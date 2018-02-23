;This is done to safely ignore local-variables warnings
(setq enable-local-variables :safe)

                                        ;Setting basic location and variables - override those if needed
(defvar emacs-git
  "/home/simon/github/SimonEnv/Emacs/"
  "The location of my Emacs environment settings")

(defvar default-work-notes-file
  "/home/simon/github/XJetWorkNotes/XJetWorkNotebook.org"
  "The location of my Work Notebook (somewhere on the work server?)")

(defvar default-personal-notes-file
  "/home/simon/Notebook/NoteBook.org"
  "The location of my personal notebook (mounted from EncFS)")

(defvar default-init-file 
  (concat emacs-git "SimonNewInit.org") 
  "My Init File")

(defvar my-emacs-monitors-num 1
  "The number of monitors to take into account when calculating the Emacs window size (2/3 of the
  available width)")

                                        ;emacs persistance directory
(if (not (boundp 'emacs-persistance-dir))
    (setq emacs-persistance-dir "/home/simon/.emacs.d")
)

;;Loading the rest of the configuration from my environment
(if (string-equal (file-name-extension default-init-file) "org")
  ;;The new way: via org mode
  (org-babel-load-file default-init-file)
  
  ;;Else: The old way
  (load default-init-file)
  )
