; set hot-key for modes

(global-set-key (kbd "C-M-t") 'text-mode)
(global-set-key (kbd "C-M-C") 'c++-mode)

;lines truncation
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

;SimonInit
(global-set-key (kbd "C->") '(lambda () (interactive) 
                                 (open-init-file)))


;Reload buffer
(global-set-key "\C-x\C-r" 'revert-buffer)

(global-set-key "\M-S-[" 'c-beginning-of-defun)
(global-set-key "\M-S-]" 'c-end-of-defun)

(global-set-key "\M-[" 'find-matching-keyword)

;===================================
;gdb and gud-gdb
(add-hook 'gud-mode-hook
          '(lambda ()
             (local-set-key [home] ; move to beginning of line, after prompt
                            'comint-bol)
             (local-set-key [up] ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-matching-input-from-input 1)
                                 (previous-line 1))))
             (local-set-key [down] ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-matching-input-from-input 1)
                                 (forward-line 1))))

             (define-key gud-mode-map [(alt n)] 'gud-next) ; External Buffer Commands
             (define-key gud-mode-map [(alt s)] 'gud-step)
             (define-key gud-mode-map [(alt f)] 'gud-finish)

                                  ; Load history file
             ;;;;;WHY DOESN'T THIS WORK?!;;;;;
;             (make-local-variable 'comint-input-ring-file-name) 
;             (setq comint-input-ring-file-name ((concat emacs-persistance-dir "/comint-history"))) 
;             (setq comint-input-ring-size 10000)
;             (set History)
             (comint-read-input-ring)
             (make-local-variable 'kill-buffer-hook)
             (add-hook 'kill-buffer-hook 'comint-write-input-ring)
             
             ))

;Is this needed? I'm using (?) gud
;(defun gdb-keys (map) 
;  "Set key bindings for gdb debugging"
;  (interactive)
;  (define-key map [(alt n)] 'gdb-next)
;  (define-key map [(alt s)] 'gdb-step)
;  (define-key map [(alt f)] 'gdb-finish)
;  (define-key map [(alt h)] 'gdb-cont-to))


;;;;;;;;;;;;;; Fill-Column Indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq-default fill-column 100)

; Lines Truncation
(set-default 'truncate-lines nil)

;;;;;;;;;;;;;;;;; Subword Mode - Jump by camelback
(global-subword-mode t)

;;;;;;;;;;;;;;;;;; Automatic Parentheses completion  Mode
;(electric-pair-mode f)

;; Set some auto modes
(setq auto-mode-alist
      (append
       (list (cons "\\.cmake$" 'cmake-mode))

       (list (cons "\\.pro$" 'makefile-mode))
       
       (list (cons "SConstruct" 'python-mode))
       (list (cons "SConscript" 'python-mode))
       (list (cons "\\.py$" 'python-mode))
       (list (cons "\\.run$" 'python-mode))

       (list (cons "\\.md$" 'markdown-mode))
       
       (list (cons "\\.h$" 'c++-mode))
       (list (cons "\\.hh$" 'c++-mode))
       (list (cons "\\.H$" 'c++-mode))
       (list (cons "\\.cxx$" 'c++-mode))
       (list (cons "\\.cc$" 'c++-mode))
       (list (cons "\\.cpp$" 'c++-mode))       

       (list (cons "\\.json$" 'js2-mode))

       (list (cons "\\.xml$" 'xml-mode)) 

       (list (cons "\\.txt$" 'text-mode))
       
       (list (cons "\\.org" 'org-mode))

       (list (cons "\\.init" 'lisp-mode))
       (list (cons "\\.emacs" 'lisp-mode))
       (list (cons "\\.el" 'lisp-mode))

       (list (cons "\\.nsi" 'nsis-mode))

       (list (cons "\\.bat" 'bat-mode))

       (list (cons "\\.txt" 'text-mode))
       
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;Text mode indent
(defun newline-and-indent-relative()
  "Do a newline and a relative indent."
  (interactive)
  (newline)
  (indent-relative-maybe))

    
(define-key text-mode-map [return] 'newline-and-indent-relative)
(define-key text-mode-map "\C-m" 'newline-and-indent-relative)



; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")

; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)

; don't split windows
;(setq py-split-windows-on-execute-p nil)

; try to automagically figure out indentation
(setq py-smart-indentation t)

;TODO: Make this work?
;(defun annotate-todo ()
;  "put fringe marker on TODO: lines in the curent buffer"
;  (interactive)
;  (save-excursion
;    (goto-char (point-min))
;    (while (re-search-forward "TODO:" nil t)
;      (let ((overlay (make-overlay (- (point) 5) (point))))
;        (overlay-put overlay 'before-string (propertize "A"
;                                                        'display '(left-fringe right-triangle)))))))
;
;(add-hook 'python-mode-hook 'annotate-todo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;c/c++ mode
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c h") 'ff-find-other-file)))

;=================================


; Tabulation etc
(setq-default tab-width 8)    ;I never use tabs. but if tabs are present - they should be very visible
(setq python-indent 2)
(setq py-indent-offset 2)
(setq perl-indent-level 2)
(setq c-basic-indent 2)
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)

(defun update-indent-mode ()
  (setq c-basic-offset my-indent)
  (c-set-offset 'substatement my-substatement)
  (c-set-offset 'substatement-open my-substatement-open)
  (c-set-offset 'access-label my-access-label)
  (c-set-offset 'topmost-intro my-topmost-intro))

(defun my-indent-mode ()
  "Set indent tabs to the xjet indent mode"
  (interactive)
  ;; C++
  (setq my-indent 2)
  (setq my-substatement 2)
  (setq my-substatement-open 0)
  (setq my-access-label 0)
  (setq my-topmost-intro 0)
  (update-indent-mode)

  ;; Python
  (setq python-indent-offset 2)

  )

(add-hook 'c++-mode-hook
  (lambda ()
    (my-indent-mode))
  )

(add-hook 'c-mode-hook
(lambda ()
;(setq indent-line-function (quote insert-tab))     ;<<<<<<<<<<<<<<<<<<
(my-indent-mode() )
))



                                        ; Advanced Search Functions:
;; git grep
(load "dov-git-grep")
(global-set-key [(control c) ?f] 'dov-git-grep)
;; git find file
;;;;I'm experimenting with Ivy for a while
(load "git-find-file.el")
;;;;(global-set-key [(control c) ?g] 'git-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Allow sudo in ido-find-file  !!!
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

                                        ; Allow sudo in shell command  !!!
;(defun sudo-shell-command (command)
;  (shell-command (concat "echo " (read-passwd "Password: ") " | sudo -S " command)))

;(global-set-gkey (kbd "C-M-!") 'sudo-shell-command(command))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Show full(er) buffer name in status bar
;(if (boundp 'InitialBufferName) () (setq InitialBufferName mode-line-buffer-identification))
;(setq-default mode-line-buffer-identification
;              (cons 'default-directory
;               ;'(:eval (replace-regexp-in-string "^.*/\\(.*\\)/" "\\1/" default-directory))
;               InitialBufferName))

                                        ;Show BufferName and Path in Upper Tab
;;;;; Disable loading of “default.el” at startup,
;;;;; in Fedora all it does is fix window title which I rather configure differently
;;;(setq inhibit-default-init t)
;;;
;;;;; SHOW FILE PATH IN FRAME TITLE
;;;(setq-default frame-title-format "%b (%f)")
;;;

;;;(setq frame-title-format
;;;      (list (format "%s %%S: %%j " (system-name))
;;;        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;; Org Mode
(setq my-default-family "InconsolataDov")
(setq my-default-font "InconsolataDov 11")

(load "org-bullets.el")

; This is a bug work around
(defun org-element-cache-reset (&optional all) (interactive))

(require 'org)
(require 'org-crypt)
(defun my-org-hook ()
  (local-set-key [(control c) (control ?.)] 'org-time-stamp)
  (local-set-key "\M-I" 'org-toggle-inline-images);org-toggle-iimage-in-org)
  (local-set-key "\C-c\C-pp" 'org-toggle-pretty-entities)
  (local-set-key "\C-c\C-pi" 'org-toggle-iimage-inorg)
  (local-set-key "\C-c\C-pi" 'org-toggle-iimage-inorg)
  (local-set-key (kbd "C-<tab>") 'next-buffer) ; C-Tab: Next Buffer
  (setq org-export-with-sub-superscripts "{}")
  (variable-pitch-mode t)
  (set-face-attribute 'org-table nil :family my-default-family)
  (set-face-attribute 'org-checkbox nil :family my-default-family)
  (set-face-attribute 'org-block nil :family my-default-family)
  (set-face-attribute 'org-verbatim nil :family my-default-family :foreground "green4")
  (org-bullets-mode)
  (setq org-bullets-bullet-list
        '("▸"
          "•"
          "•"
          "•"
          "•"
          "•"
          "•"
          ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
          ;;; Small
          ;; ► • ★ ▸
    ))

  (setq org-hide-emphasis-markers nil)
  (setq org-confirm-babel-evaluate nil)
  (xmsi-mode)
  (org-toggle-pretty-entities)
  (setq bidi-paragraph-direction nil)
  (setq org-export-html-postamble nil)
  (setq org-export-html-validation-link "")
  ;; Use journal theme if requested
  (if (>= emacs-major-version 24)
      (if (string-match "notes.org" (buffer-name) )
          (progn
            (disable-theme 'org-default)
            (load-theme-buffer-local 'org-journal))
        (load-theme-buffer-local 'org-default)))
  (setq org-entities-user '(
    ("models" "\\models" t "&8872;" "[models]" "models" "⊨")
    ("indf" "{\bf 1}" t "&#120128;" "[indf]" "indf" "𝟙")
    ("ell" "\\ell" t "&#2113;" "[ell]" "indf" "ℓ")
    ))

  )
(add-hook 'org-mode-hook 'my-org-hook)

;;export to html-slidy
(require 'ox-slidy)

;; Make all font-lock faces fonts use inconsolata
(dolist (face '(font-lock-builtin-face 	
                font-lock-comment-delimiter-face
                font-lock-comment-face 	
                font-lock-constant-face
                font-lock-doc-face 	
                font-lock-function-name-face
                font-lock-keyword-face 	
                font-lock-negation-char-face
                font-lock-preprocessor-face 	
                font-lock-regexp-grouping-backslash
                font-lock-regexp-grouping-construct 	
                font-lock-string-face
                font-lock-type-face 	
                font-lock-variable-name-face
                font-lock-warning-face))
  (set-face-attribute face nil :family my-default-family))


; Source Languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (perl . t)
   (emacs-lisp . t)
   (python . t)
   (ditaa . t)
   (dot . t)
   (asymptote . t)
   (plantuml . t)
   (octave . t)
   (R . t)
   (C . t)
   )) 
(setq org-plantuml-jar-path
      (concat emacs-git "/Plugins/plantuml.jar"))


; convert lines into checkbox
(defun org-set-line-checkbox (arg)
  (interactive "P")
  (let ((n (or arg 1)))
    (when (region-active-p)
      (setq n (count-lines (region-beginning)
                           (region-end)))
      (goto-char (region-beginning)))
    (dotimes (i n)
      (beginning-of-line)
      (skip-chars-forward "[:blank:]")
      (insert "- [ ] ")
      (if (> n 1)
        (forward-line))
    (end-of-line))))

(global-set-key (kbd "C-M-]") 'org-set-line-checkbox)

;;Define programs to open files
(if (string-match "mingw-nt" system-configuration)
    (progn
      (setq org-file-apps
            (append
             '(("png" . "c:/progra~2/IrfanView/i_view32.exe %s"))
             '(("doc" . "\"c:/Program Files (x86)/OpenOffice.org 3/program/soffice.exe\" %s"))

             org-file-apps
             ))
      )
  (progn 
    (setq org-file-apps
          (append
           '(("png" . "eog %s"))
           '(("pdf" . "evince %s"))
           '(("svg" . "inkscape %s"))
           '(("net" . "/usr/local/samiam/runsamiam %s"))
           '(("xcf" . "gimp %s"))
           '(("giv" . "giv %s"))
           '(("doc" . "libreoffice -norestore %s"))
           '(("odt" . "libreoffice -norestore %s"))
           '(("gnumeric" . "gnumeric %s"))
           '(("html" . (concat my-chrome-command " %s")))
           org-file-apps))))

(setq org-src-lang-modes 
     '(("elisp" . emacs-lisp)
        ("ditaa" . artist)
        ("asymptote" . asy)
        ("dot" . fundamental)
        ("perl" . cperl)
        ("python" . python)
        ))

