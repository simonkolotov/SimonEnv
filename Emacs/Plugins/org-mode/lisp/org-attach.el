;;; org-attach.el --- Manage file attachments to Org tasks -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2018 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@newartisans.com>
;; Keywords: org data task

;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Org manual for information on how to use it.
;;
;; Attachments are managed in a special directory called "data", which
;; lives in the same directory as the org file itself.  If this data
;; directory is initialized as a Git repository, then org-attach will
;; automatically commit changes when it sees them.
;;
;; Attachment directories are identified using a UUID generated for the
;; task which has the attachments.  These are added as property to the
;; task when necessary, and should not be deleted or changed by the
;; user, ever.  UUIDs are generated by a mechanism defined in the variable
;; `org-id-method'.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)
(require 'vc-git)

(declare-function dired-dwim-target-directory "dired-aux")

(defgroup org-attach nil
  "Options concerning entry attachments in Org mode."
  :tag "Org Attach"
  :group 'org)

(defcustom org-attach-directory "data/"
  "The directory where attachments are stored.
If this is a relative path, it will be interpreted relative to the directory
where the Org file lives."
  :group 'org-attach
  :type 'directory
  :safe #'stringp)

(defcustom org-attach-commit t
  "If non-nil commit attachments with git.
This is only done if the Org file is in a git repository."
  :group 'org-attach
  :type 'boolean
  :version "26.1"
  :package-version '(Org . "9.0"))

(defcustom org-attach-git-annex-cutoff (* 32 1024)
  "If non-nil, files larger than this will be annexed instead of stored."
  :group 'org-attach
  :version "24.4"
  :package-version '(Org . "8.0")
  :type '(choice
	  (const :tag "None" nil)
	  (integer :tag "Bytes")))

(defcustom org-attach-auto-tag "ATTACH"
  "Tag that will be triggered automatically when an entry has an attachment."
  :group 'org-attach
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "Tag")))

(defcustom org-attach-file-list-property "Attachments"
  "The property used to keep a list of attachment belonging to this entry.
This is not really needed, so you may set this to nil if you don't want it.
Also, for entries where children inherit the directory, the list of
attachments is not kept in this property."
  :group 'org-attach
  :type '(choice
	  (const :tag "None" nil)
	  (string :tag "Tag")))

(defcustom org-attach-method 'cp
  "The preferred method to attach a file.
Allowed values are:

mv    rename the file to move it into the attachment directory
cp    copy the file
ln    create a hard link.  Note that this is not supported
      on all systems, and then the result is not defined.
lns   create a symbol link.  Note that this is not supported
      on all systems, and then the result is not defined."
  :group 'org-attach
  :type '(choice
	  (const :tag "Copy" cp)
	  (const :tag "Move/Rename" mv)
	  (const :tag "Hard Link" ln)
	  (const :tag "Symbol Link" lns)))

(defcustom org-attach-expert nil
  "Non-nil means do not show the splash buffer with the attach dispatcher."
  :group 'org-attach
  :type 'boolean)

(defcustom org-attach-allow-inheritance t
  "Non-nil means allow attachment directories be inherited."
  :group 'org-attach
  :type 'boolean)

(defvar org-attach-inherited nil
  "Indicates if the last access to the attachment directory was inherited.")

(defcustom org-attach-store-link-p nil
  "Non-nil means store a link to a file when attaching it."
  :group 'org-attach
  :version "24.1"
  :type '(choice
	  (const :tag "Don't store link" nil)
	  (const :tag "Link to origin location" t)
	  (const :tag "Link to the attach-dir location" attached)))

(defcustom org-attach-archive-delete nil
  "Non-nil means attachments are deleted upon archiving a subtree.
When set to `query', ask the user instead."
  :group 'org-attach
  :version "26.1"
  :package-version '(Org . "8.3")
  :type '(choice
	  (const :tag "Never delete attachments" nil)
	  (const :tag "Always delete attachments" t)
	  (const :tag "Query the user" query)))

(defcustom org-attach-annex-auto-get 'ask
  "Confirmation preference for automatically getting annex files.
If \\='ask, prompt using `y-or-n-p'.  If t, always get.  If nil, never get."
  :group 'org-attach
  :package-version '(Org . "9.0")
  :version "26.1"
  :type '(choice
	  (const :tag "confirm with `y-or-n-p'" ask)
	  (const :tag "always get from annex if necessary" t)
	  (const :tag "never get from annex" nil)))

;;;###autoload
(defun org-attach ()
  "The dispatcher for attachment commands.
Shows a list of commands and prompts for another key to execute a command."
  (interactive)
  (let (c marker)
    (when (eq major-mode 'org-agenda-mode)
      (setq marker (or (get-text-property (point) 'org-hd-marker)
		       (get-text-property (point) 'org-marker)))
      (unless marker
	(error "No task in current line")))
    (save-excursion
      (when marker
	(set-buffer (marker-buffer marker))
	(goto-char marker))
      (org-back-to-heading t)
      (save-excursion
	(save-window-excursion
	  (unless org-attach-expert
	    (with-output-to-temp-buffer "*Org Attach*"
	      (princ "Select an Attachment Command:

a       Select a file and attach it to the task, using `org-attach-method'.
c/m/l/y Attach a file using copy/move/link/symbolic-link method.
u       Attach a file from URL (downloading it).
n       Create a new attachment, as an Emacs buffer.
z       Synchronize the current task with its attachment
        directory, in case you added attachments yourself.

o       Open current task's attachments.
O       Like \"o\", but force opening in Emacs.
f       Open current task's attachment directory.
F       Like \"f\", but force using dired in Emacs.

d       Delete one attachment, you will be prompted for a file name.
D       Delete all of a task's attachments.  A safer way is
        to open the directory in dired and delete from there.

s       Set a specific attachment directory for this entry or reset to default.
i       Make children of the current entry inherit its attachment directory.")))
	  (org-fit-window-to-buffer (get-buffer-window "*Org Attach*"))
	  (message "Select command: [acmlzoOfFdD]")
	  (setq c (read-char-exclusive))
	  (and (get-buffer "*Org Attach*") (kill-buffer "*Org Attach*"))))
      (cond
       ((memq c '(?a ?\C-a)) (call-interactively 'org-attach-attach))
       ((memq c '(?c ?\C-c))
	(let ((org-attach-method 'cp)) (call-interactively 'org-attach-attach)))
       ((memq c '(?m ?\C-m))
	(let ((org-attach-method 'mv)) (call-interactively 'org-attach-attach)))
       ((memq c '(?l ?\C-l))
	(let ((org-attach-method 'ln)) (call-interactively 'org-attach-attach)))
       ((memq c '(?y ?\C-y))
	(let ((org-attach-method 'lns)) (call-interactively 'org-attach-attach)))
       ((memq c '(?u ?\C-u))
        (let ((org-attach-method 'url)) (call-interactively 'org-attach-url)))
       ((memq c '(?n ?\C-n)) (call-interactively 'org-attach-new))
       ((memq c '(?z ?\C-z)) (call-interactively 'org-attach-sync))
       ((memq c '(?o ?\C-o)) (call-interactively 'org-attach-open))
       ((eq c ?O)            (call-interactively 'org-attach-open-in-emacs))
       ((memq c '(?f ?\C-f)) (call-interactively 'org-attach-reveal))
       ((memq c '(?F))       (call-interactively 'org-attach-reveal-in-emacs))
       ((memq c '(?d ?\C-d)) (call-interactively
			      'org-attach-delete-one))
       ((eq c ?D)            (call-interactively 'org-attach-delete-all))
       ((eq c ?q)            (message "Abort"))
       ((memq c '(?s ?\C-s)) (call-interactively
			      'org-attach-set-directory))
       ((memq c '(?i ?\C-i)) (call-interactively
			      'org-attach-set-inherit))
       (t (error "No such attachment command %c" c))))))

(defun org-attach-dir (&optional create-if-not-exists-p)
  "Return the directory associated with the current entry.
This first checks for a local property ATTACH_DIR, and then for an inherited
property ATTACH_DIR_INHERIT.  If neither exists, the default mechanism
using the entry ID will be invoked to access the unique directory for the
current entry.
If the directory does not exist and CREATE-IF-NOT-EXISTS-P is non-nil,
the directory and (if necessary) the corresponding ID will be created."
  (let (attach-dir uuid)
    (setq org-attach-inherited (org-entry-get nil "ATTACH_DIR_INHERIT"))
    (cond
     ((setq attach-dir (org-entry-get nil "ATTACH_DIR"))
      (org-attach-check-absolute-path attach-dir))
     ((and org-attach-allow-inheritance
	   (org-entry-get nil "ATTACH_DIR_INHERIT" t))
      (setq attach-dir
	    (org-with-wide-buffer
	     (if (marker-position org-entry-property-inherited-from)
		 (goto-char org-entry-property-inherited-from)
	       (org-back-to-heading t))
	     (let (org-attach-allow-inheritance)
	       (org-attach-dir create-if-not-exists-p))))
      (org-attach-check-absolute-path attach-dir)
      (setq org-attach-inherited t))
     (t					; use the ID
      (org-attach-check-absolute-path nil)
      (setq uuid (org-id-get (point) create-if-not-exists-p))
      (when (or uuid create-if-not-exists-p)
	(unless uuid (error "ID retrieval/creation failed"))
	(setq attach-dir (expand-file-name
			  (format "%s/%s"
				  (substring uuid 0 2)
				  (substring uuid 2))
			  (expand-file-name org-attach-directory))))))
    (when attach-dir
      (if (and create-if-not-exists-p
	       (not (file-directory-p attach-dir)))
	  (make-directory attach-dir t))
      (and (file-exists-p attach-dir)
	   attach-dir))))

(defun org-attach-check-absolute-path (dir)
  "Check if we have enough information to root the attachment directory.
When DIR is given, check also if it is already absolute.  Otherwise,
assume that it will be relative, and check if `org-attach-directory' is
absolute, or if at least the current buffer has a file name.
Throw an error if we cannot root the directory."
  (or (and dir (file-name-absolute-p dir))
      (file-name-absolute-p org-attach-directory)
      (buffer-file-name (buffer-base-buffer))
      (error "Need absolute `org-attach-directory' to attach in buffers without filename")))

(defun org-attach-set-directory (&optional arg)
  "Set the ATTACH_DIR node property and ask to move files there.
The property defines the directory that is used for attachments
of the entry.  When called with `\\[universal-argument]', reset \
the directory to
the default ID based one."
  (interactive "P")
  (let ((old (org-attach-dir))
        (new
         (progn
           (if arg (org-entry-delete nil "ATTACH_DIR")
             (let ((dir (read-directory-name
                         "Attachment directory: "
                         (org-entry-get nil
                                        "ATTACH_DIR"
                                        (and org-attach-allow-inheritance t)))))
               (org-entry-put nil "ATTACH_DIR" dir)))
           (org-attach-dir t))))
    (unless (or (string= old new)
                (not old))
      (when (yes-or-no-p "Copy over attachments from old directory? ")
        (copy-directory old new t nil t))
      (when (yes-or-no-p (concat "Delete " old))
        (delete-directory old t)))))

(defun org-attach-set-inherit ()
  "Set the ATTACH_DIR_INHERIT property of the current entry.
The property defines the directory that is used for attachments
of the entry and any children that do not explicitly define (by setting
the ATTACH_DIR property) their own attachment directory."
  (interactive)
  (org-entry-put nil "ATTACH_DIR_INHERIT" "t")
  (message "Children will inherit attachment directory"))

(defun org-attach-use-annex ()
  "Return non-nil if git annex can be used."
  (let ((git-dir (vc-git-root (expand-file-name org-attach-directory))))
    (and org-attach-git-annex-cutoff
         (or (file-exists-p (expand-file-name "annex" git-dir))
             (file-exists-p (expand-file-name ".git/annex" git-dir))))))

(defun org-attach-annex-get-maybe (path)
  "Call git annex get PATH (via shell) if using git annex.
Signals an error if the file content is not available and it was not retrieved."
  (let* ((default-directory (expand-file-name org-attach-directory))
	 (path-relative (file-relative-name path)))
    (when (and (org-attach-use-annex)
	       (not
		(string-equal
		 "found"
		 (shell-command-to-string
		  (format "git annex find --format=found --in=here %s"
			  (shell-quote-argument path-relative))))))
      (let ((should-get
	     (if (eq org-attach-annex-auto-get 'ask)
		 (y-or-n-p (format "Run git annex get %s? " path-relative))
	       org-attach-annex-auto-get)))
	(if should-get
	    (progn (message "Running git annex get \"%s\"." path-relative)
		   (call-process "git" nil nil nil "annex" "get" path-relative))
	  (error "File %s stored in git annex but it is not available, and was not retrieved"
		 path))))))

(defun org-attach-commit ()
  "Commit changes to git if `org-attach-directory' is properly initialized.
This checks for the existence of a \".git\" directory in that directory."
  (let* ((dir (expand-file-name org-attach-directory))
	 (git-dir (vc-git-root dir))
	 (use-annex (org-attach-use-annex))
	 (changes 0))
    (when (and git-dir (executable-find "git"))
      (with-temp-buffer
	(cd dir)
        (dolist (new-or-modified
                 (split-string
                  (shell-command-to-string
                   "git ls-files -zmo --exclude-standard") "\0" t))
          (if (and use-annex
                   (>= (nth 7 (file-attributes new-or-modified))
                       org-attach-git-annex-cutoff))
              (call-process "git" nil nil nil "annex" "add" new-or-modified)
            (call-process "git" nil nil nil "add" new-or-modified))
	    (cl-incf changes))
	(dolist (deleted
		 (split-string
		  (shell-command-to-string "git ls-files -z --deleted") "\0" t))
	  (call-process "git" nil nil nil "rm" deleted)
	  (cl-incf changes))
	(when (> changes 0)
	  (shell-command "git commit -m 'Synchronized attachments'"))))))

(defun org-attach-tag (&optional off)
  "Turn the autotag on or (if OFF is set) off."
  (when org-attach-auto-tag
    (save-excursion
      (org-back-to-heading t)
      (org-toggle-tag org-attach-auto-tag (if off 'off 'on)))))

(defun org-attach-untag ()
  "Turn the autotag off."
  (org-attach-tag 'off))

(defun org-attach-store-link (file)
  "Add a link to `org-stored-link' when attaching a file.
Only do this when `org-attach-store-link-p' is non-nil."
  (setq org-stored-links
	(cons (list (org-attach-expand-link file)
		    (file-name-nondirectory file))
	      org-stored-links)))

(defun org-attach-url (url)
  (interactive "MURL of the file to attach: \n")
  (org-attach-attach url))

(defun org-attach-attach (file &optional visit-dir method)
  "Move/copy/link FILE into the attachment directory of the current task.
If VISIT-DIR is non-nil, visit the directory with dired.
METHOD may be `cp', `mv', `ln', `lns' or `url' default taken from
`org-attach-method'."
  (interactive
   (list
    (read-file-name "File to keep as an attachment:"
                    (or (progn
                          (require 'dired-aux)
                          (dired-dwim-target-directory))
                        default-directory))
    current-prefix-arg
    nil))
  (setq method (or method org-attach-method))
  (let ((basename (file-name-nondirectory file)))
    (when (and org-attach-file-list-property (not org-attach-inherited))
      (org-entry-add-to-multivalued-property
       (point) org-attach-file-list-property basename))
    (let* ((attach-dir (org-attach-dir t))
           (fname (expand-file-name basename attach-dir)))
      (cond
       ((eq method 'mv) (rename-file file fname))
       ((eq method 'cp) (copy-file file fname))
       ((eq method 'ln) (add-name-to-file file fname))
       ((eq method 'lns) (make-symbolic-link file fname))
       ((eq method 'url) (url-copy-file file fname)))
      (when org-attach-commit
        (org-attach-commit))
      (org-attach-tag)
      (cond ((eq org-attach-store-link-p 'attached)
             (org-attach-store-link fname))
            ((eq org-attach-store-link-p t)
             (org-attach-store-link file)))
      (if visit-dir
          (dired attach-dir)
        (message "File %S is now a task attachment." basename)))))

(defun org-attach-attach-cp ()
  "Attach a file by copying it."
  (interactive)
  (let ((org-attach-method 'cp)) (call-interactively 'org-attach-attach)))
(defun org-attach-attach-mv ()
  "Attach a file by moving (renaming) it."
  (interactive)
  (let ((org-attach-method 'mv)) (call-interactively 'org-attach-attach)))
(defun org-attach-attach-ln ()
  "Attach a file by creating a hard link to it.
Beware that this does not work on systems that do not support hard links.
On some systems, this apparently does copy the file instead."
  (interactive)
  (let ((org-attach-method 'ln)) (call-interactively 'org-attach-attach)))
(defun org-attach-attach-lns ()
  "Attach a file by creating a symbolic link to it.

Beware that this does not work on systems that do not support symbolic links.
On some systems, this apparently does copy the file instead."
  (interactive)
  (let ((org-attach-method 'lns)) (call-interactively 'org-attach-attach)))

(defun org-attach-new (file)
  "Create a new attachment FILE for the current task.
The attachment is created as an Emacs buffer."
  (interactive "sCreate attachment named: ")
  (when (and org-attach-file-list-property (not org-attach-inherited))
    (org-entry-add-to-multivalued-property
     (point) org-attach-file-list-property file))
  (let ((attach-dir (org-attach-dir t)))
    (org-attach-tag)
    (find-file (expand-file-name file attach-dir))
    (message "New attachment %s" file)))

(defun org-attach-delete-one (&optional file)
  "Delete a single attachment."
  (interactive)
  (let* ((attach-dir (org-attach-dir t))
	 (files (org-attach-file-list attach-dir))
	 (file (or file
		   (completing-read
		    "Delete attachment: "
		    (mapcar (lambda (f)
			      (list (file-name-nondirectory f)))
			    files)))))
    (setq file (expand-file-name file attach-dir))
    (unless (file-exists-p file)
      (error "No such attachment: %s" file))
    (delete-file file)
    (when org-attach-commit
      (org-attach-commit))))

(defun org-attach-delete-all (&optional force)
  "Delete all attachments from the current task.
This actually deletes the entire attachment directory.
A safer way is to open the directory in dired and delete from there."
  (interactive "P")
  (when (and org-attach-file-list-property (not org-attach-inherited))
    (org-entry-delete (point) org-attach-file-list-property))
  (let ((attach-dir (org-attach-dir)))
    (when
	(and attach-dir
	     (or force
		 (y-or-n-p "Are you sure you want to remove all attachments of this entry? ")))
      (shell-command (format "rm -fr %s" attach-dir))
      (message "Attachment directory removed")
      (when org-attach-commit
        (org-attach-commit))
      (org-attach-untag))))

(defun org-attach-sync ()
  "Synchronize the current tasks with its attachments.
This can be used after files have been added externally."
  (interactive)
  (when org-attach-commit
    (org-attach-commit))
  (when (and org-attach-file-list-property (not org-attach-inherited))
    (org-entry-delete (point) org-attach-file-list-property))
  (let ((attach-dir (org-attach-dir)))
    (when attach-dir
      (let ((files (org-attach-file-list attach-dir)))
	(org-attach-tag (not files))
	(when org-attach-file-list-property
	  (dolist (file files)
	    (unless (string-match "^\\.\\.?\\'" file)
	      (org-entry-add-to-multivalued-property
	       (point) org-attach-file-list-property file))))))))

(defun org-attach-file-list (dir)
  "Return a list of files in the attachment directory.
This ignores files ending in \"~\"."
  (delq nil
	(mapcar (lambda (x) (if (string-match "^\\.\\.?\\'" x) nil x))
		(directory-files dir nil "[^~]\\'"))))

(defun org-attach-reveal (&optional if-exists)
  "Show the attachment directory of the current task.
This will attempt to use an external program to show the directory."
  (interactive "P")
  (let ((attach-dir (org-attach-dir (not if-exists))))
    (and attach-dir (org-open-file attach-dir))))

(defun org-attach-reveal-in-emacs ()
  "Show the attachment directory of the current task in dired."
  (interactive)
  (let ((attach-dir (org-attach-dir t)))
    (dired attach-dir)))

(defun org-attach-open (&optional in-emacs)
  "Open an attachment of the current task.
If there are more than one attachment, you will be prompted for the file name.
This command will open the file using the settings in `org-file-apps'
and in the system-specific variants of this variable.
If IN-EMACS is non-nil, force opening in Emacs."
  (interactive "P")
  (let* ((attach-dir (org-attach-dir t))
	 (files (org-attach-file-list attach-dir))
	 (file (if (= (length files) 1)
		   (car files)
		 (completing-read "Open attachment: "
				  (mapcar #'list files) nil t)))
         (path (expand-file-name file attach-dir)))
    (org-attach-annex-get-maybe path)
    (org-open-file path in-emacs)))

(defun org-attach-open-in-emacs ()
  "Open attachment, force opening in Emacs.
See `org-attach-open'."
  (interactive)
  (org-attach-open 'in-emacs))

(defun org-attach-expand (file)
  "Return the full path to the current entry's attachment file FILE.
Basically, this adds the path to the attachment directory."
  (expand-file-name file (org-attach-dir)))

(defun org-attach-expand-link (file)
  "Return a file link pointing to the current entry's attachment file FILE.
Basically, this adds the path to the attachment directory, and a \"file:\"
prefix."
  (concat "file:" (org-attach-expand file)))

(defun org-attach-archive-delete-maybe ()
  "Maybe delete subtree attachments when archiving.
This function is called by `org-archive-hook'.  The option
`org-attach-archive-delete' controls its behavior."
  (when org-attach-archive-delete
    (org-attach-delete-all (not (eq org-attach-archive-delete 'query)))))


;; Attach from dired.

;; Add the following lines to the config file to get a binding for
;; dired-mode.

;; (add-hook
;;  'dired-mode-hook
;;  (lambda ()
;;    (define-key dired-mode-map (kbd "C-c C-x a") #'org-attach-dired-to-subtree))))

(defun org-attach-dired-to-subtree (files)
  "Attach FILES marked or current file in dired to subtree in other window.
Takes the method given in `org-attach-method' for the attach action.
Precondition: Point must be in a dired buffer.
Idea taken from `gnus-dired-attach'."
  (interactive
   (list (dired-get-marked-files)))
  (unless (eq major-mode 'dired-mode)
    (user-error "This command must be triggered in a dired buffer."))
  (let ((start-win (selected-window))
        (other-win
         (get-window-with-predicate
          (lambda (window)
            (with-current-buffer (window-buffer window)
              (eq major-mode 'org-mode))))))
    (unless other-win
      (user-error
       "Can't attach to subtree.  No window displaying an Org buffer"))
    (select-window other-win)
    (dolist (file files)
      (org-attach-attach file))
    (select-window start-win)))



(add-hook 'org-archive-hook 'org-attach-archive-delete-maybe)

(provide 'org-attach)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; org-attach.el ends here
