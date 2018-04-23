; -*- Encoding: utf-8 -*-
;;======================================================================
;;   emacs (not Xemacs) mode
;;
;; To use this file, add lines similar to the following to ~/.emacs:
;;
;; ; -*- Encoding: utf-8 -*-
;; 
;; (setq emacs-git "/home/simon/github/SimonEnv/Emacs/")
;; (setq default-work-notes-file "/home/simon/github/Notes/WorkNoteBook.org")
;; (setq default-personal-notes-file "/home/simon/Notes/NoteBook.org")
;; (setq my-emacs-monitors-num 2) ; Number of monitors attached
;; 
;; ; emacs persistance directory
;; (if (not (boundp 'emacs-persistance-dir))
;;     (setq emacs-persistance-dir "/home/simon/.emacs.d")
;; )
;; 
;; (load (concat emacs-git "SimonInit.el"))
;;
;;  The default font used is InconsolataDov. copy it from /home/simon/github/SimonEnv/Emacs/ to ~/.fonts/
;;----------------------------------------------------------------------

;;Win vs Linux
(if (string-match "mingw" system-configuration)
    (progn
      ;On Windows
      (if (not (boundp 'emacs-git))
          (setq emacs-git "D:/Simon/github/SimonEnv/Emacs/"))
      (if (not (boundp 'emacs-persistance-dir))
          (setq emacs-persistance-dir "C:/Documents and Settings/simon/Application Data/.emacs.d"))
      
      ;; don't use Hebrew locale!
      (setq system-time-locale "C")
      
      ;; Load windows utilities - those include send buffer to VS by Dov.
      ;; TODO: Check this out
                                        ;      (load (concat emacs-git "win-utils.el")))

      ;; Various settings to use utf-8 (From Dov, I don't understand them all)
      (setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
      (set-language-environment 'utf-8)
      (set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
      (setq locale-coding-system 'utf-8)
      (set-default-coding-systems 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (prefer-coding-system 'utf-8)

      ;; Add M-F4 to closing emacs, as apparently it is not caught by the windows manager
      (global-set-key (kbd "M-<f4>") 'save-buffers-kill-terminal) ; Close EMACS (Microsoft style)

      ;; Chrome command for windows (assumes C:\Program Files (x86)\Google\Chrome\Application is in path)
      (if (not (boundp 'my-chrome-command))
               (setq my-chrome-command "chrome"))
       
      )
  (progn
    ;On Linux
    (if (not (boundp 'emacs-git))
        (setq emacs-git "/home/simon/github/SimonEnv/Emacs/"))
    (if (not (boundp 'emacs-persistance-dir))
        (setq emacs-persistance-dir "/home/simon/.emacs.d"))

      ;; Use Miriam mono font for Hebrew
    (set-fontset-font "fontset-default" '(#x5d0 . #x5ff) "Miriam Mono CLM:bold")
    (set-face-font 'default "fontset-default")
    (setq load-path (append (list
                             "/usr/local/share/emacs/site-lisp"
                             ) load-path))

    ;; Chrome command for linux
    (if (not (boundp 'my-chrome-command))
        (setq my-chrome-command "google-chrome"))
    (setq browse-url-generic-program "google-chrome")    
   ) 
  )

;;;;;;;;Font
(add-to-list 'default-frame-alist '(font .   "InconsolataDov 11"))
(set-face-attribute 'default t :font  "InconsolataDov 11" )

(set-face-attribute 'default nil :font  "InconsolataDov 11" )
(set-frame-font   "InconsolataDov 11" nil t)
    
(setq my-default-family "InconsolataDov 11")
(setq my-default-font "InconsolataDov 11")


;;;;;;;;;;EXTERNAL PLUGINS
(add-to-list 'load-path (concat emacs-git "Plugins/"))

;various modes required for magit
(add-to-list 'load-path (concat emacs-git "Plugins/git-modes")) 
(add-to-list 'load-path (concat emacs-git "Plugins/magit"))

(add-to-list 'load-path (concat emacs-git "Plugins/yasnippet"))

;for python notebook. I didn't manage to make it work
;(add-to-list 'load-path (concat emacs-git "Plugins/ein")) 

(add-to-list 'load-path (concat emacs-git "Plugins/org-mode"))
(add-to-list 'load-path (concat emacs-git "Plugins/org-mode/lisp"))
(add-to-list 'load-path (concat emacs-git "Plugins/org-mode/contrib/lisp"))
(add-to-list 'load-path (concat emacs-git "Plugins/autocomplete"))
(add-to-list 'load-path (concat emacs-git "Plugins/fill-column-indicator-1.83"))
(add-to-list 'load-path (concat emacs-git "Plugins/swiper"))
(add-to-list 'load-path (concat emacs-git "Plugins/plantuml-mode"))



(load "scott.emacs") ;I think this is used for setting the Emacs window size

(load "magit")

;NSIS mode
(autoload 'nsis-mode "nsis-mode" "NSIS mode" t)
        
;;;;;;;;;;Env Vars
(defun my-reload-env-vars ()
; set env vars to be reloaded here
; e.g.:  
;  (setenv "METALJET" "$PE_HOME/XjetApps/MetalJet/Apps/Project/qt/" t)
  )



(setenv "QMAKE" "qmake-qt5")
(setenv "QTDIR" "/usr")
; add env vars according to bashrc...

`my-reload-env-vars()

;;;;;;;;;;ein for IPython Notebooks in emacs
;;(require 'ein-ipynb-mode)

;;;;;;;;;;plantuml-mode for editing plant-uml diagrams
(require 'plantuml-mode)
(setq plantuml-jar-path (concat emacs-git "Plugins/plantuml-mode/bin/"))

;;;;;;;;;;XSMI for math symbols
(require 'xmsi-math-symbols-input)
(xmsi-mode)

;;;;;;;;;;bb-mode for bit-bake
(require 'bb-mode)

;;;;;;;;;;smex for using IDO for M-x commands
(require 'smex) ; Not needed if you use package.el
  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;                     when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;;;;;;;;;Ivy mode - like Ido but better in SOME regards
(require 'counsel)

(define-key ivy-minibuffer-map (kbd "C-j") #'ivy-immediate-done)
(define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)

(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)

(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)

(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)

;;;;;;;;;;Undo-Tree
(require 'undo-tree)
(global-undo-tree-mode)
(set-default 'undo-tree-auto-save-history t)

;;;;;;;;;Git for emacs
(require 'magit)
(global-set-key "\C-ci" 'magit-status)
(global-set-key "\C-c\C-b" 'magit-blame-mode)

;;;;;;;;;; ido-mode is the cool autocompletion mode in the lower buffer
(require 'ido)
(ido-mode t)

;;;;;;;;;; js2 mode for json
(require 'js2-mode)

;;;;;;;;;; yas for programming templates
;; TODO: is this working?
(require 'yasnippet)
(setq yas-snippet-dirs (list (concat emacs-git "Plugins/yasnippet/snippets")))
(yas-global-mode 1)

;; Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
    (let ((original-point (point)))
      (while (and
              (not (= (point) (point-min) ))
              (not
               (string-match "[[:space:]\n]" (char-to-string (char-before)))))
        (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))


(define-key yas-minor-mode-map (kbd "C-<return>")     'yas-ido-expand)
(define-key yas-minor-mode-map (kbd "C-<kp-enter>")     'yas-ido-expand)

(yas-reload-all)

;; Lexical completion with M-RET
(define-key yas-minor-mode-map (kbd "M-<return>")     'dabbrev-expand)
(define-key yas-minor-mode-map (kbd "M-<kp-enter>")     'dabbrev-expand)

;;;;;;;;;;auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (concat emacs-git "Plugins/autocomplete/ac-dict"))
(ac-config-default)

;;;;;;;;;;lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 2)

;;;;;;;;;;Toolbars
(menu-bar-mode 't)
(tool-bar-mode 'nil)

;;;;;;;;;;Misc
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(safe-local-variable-values (quote ((Encoding . utf-8))))
 '(show-paren-mode t))

; Overwrite Selection
 (delete-selection-mode t)

 ; reload changed files
(global-auto-revert-mode t)

; Don't add newline at end of file
(setq mode-require-final-newline nil)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;;Open notebook
(defun open-work-notes-file ()
  "Load my work notebook"
  (interactive)
  (find-file default-work-notes-file)
  (font-lock-fontify-buffer)
  (end-of-buffer)
  )


(defun open-personal-notes-file ()
  "Load my personal notebook"
  (interactive)
  (find-file default-personal-notes-file)
  (font-lock-fontify-buffer)
  (end-of-buffer)
  )

;;Open shell
(defun open-shell ()
  "Load the emacs shell"
  (interactive)
  (shell)
  )

;; Init File
(setq default-init-file (concat emacs-git "/SimonInit.el"))

;;Open Init File
(defun open-init-file ()
  "Load my personal init file"
  (interactive)
  (find-file default-init-file)
  )

;; Most Recent Buffers
(defun find-first-buffer-match (buffers pattern)
  (dolist (f buffers)
    (when (string-match pattern (buffer-name f))
      (return f))))

(defun find-most-recent-pattern-buffer (pattern)
  "find the most recent code buffer in the history and switch to it"
  (let ((f (find-first-buffer-match (cdr (buffer-list)) pattern)))
    (if (not (eq f nil))
        (switch-to-buffer f)
      )
    )
  )

(defun find-most-recent-python-buffer ()
  "find the most recent python buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.py"))

(defun find-most-recent-c-buffer ()
  "find the most recent c/c++ buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.\\(cpp\\|h\\|cc\\|hh|hpp\\)$"))

(defun find-most-recent-emacs-buffer ()
  "find the most recent emacs init buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.el\\$\\|dov.emacs|SimonInit.el"))

(defun find-most-recent-magit-buffer ()
  "find the most recent magit buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "magit"))

(defun find-most-recent-org-buffer ()
  "find the most recent code buffer in the history and switch to it"
  (interactive)
  (find-most-recent-pattern-buffer "\\.org\$"))

;; qt docs lookup
(require `info-look)
(load "qtdoc")
(setq qtdoc-html-root "http://doc.qt.io/qt-5/")

(load "google-look")

;;;;;;;;;;KEYBOARD SHORTCUTS
; Undo-Redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo) ; 【Ctrl+z】
(global-set-key (kbd "C-S-z") 'redo) ; 【Ctrl+Shift+z】;  Mac style

;;Disable default exit command
;;(I trip over it accidentally while undoing and redoing too much)
(global-set-key "\C-x\C-c" nil)

;;(global-set-key "\C-o" 'find-file)  ; Open file (Microsoft style)
(global-set-key "\C-o" 'counsel-find-file)  ; Open file (Microsoft style)
(global-set-key (kbd "C-<f4>") 'kill-this-buffer) ; Close Buffer (Microsoft style)

;; For programming language modes
(define-key global-map (kbd "RET") 'newline-and-indent) 

(global-set-key (kbd "M-g") 'goto-line) ; Goto-line

(global-set-key (kbd "C-<f9>") 'compile) ; Compile

; Previous/Next Error (and also previous/next file in dov-git-grep)
(global-set-key "\M-`" 'next-error) 
(global-set-key "\M-~" 'previous-error)

(global-set-key (kbd "C-<tab>") 'next-buffer) ; C-Tab: Next Buffer
;; C-S-Tab: Previous Buffer
(global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer) 
(global-set-key (kbd "C-S-<tab>") 'previous-buffer)

; Move between Windows
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

; Scroll with Ctrl+Up/Down
(defun scroll-dont-move-cursor (dist)
  ""
  (let ((p (point)))
    (scroll-up dist)
    (goto-char p)))
  
(defun scroll-up-line ()
  (interactive)
  (scroll-dont-move-cursor 1))

(defun scroll-down-line ()
  (interactive)
  (scroll-dont-move-cursor -1))

; NB: scrolling down = moving the window up...
(global-set-key (kbd "<C-up>")   'scroll-down-line)
(global-set-key (kbd "<C-kp-up>")   'scroll-down-line)
(global-set-key (kbd "<C-down>") 'scroll-up-line)
(global-set-key (kbd "<C-kp-down>") 'scroll-up-line)
                                        ; Scroll Other Window with Alt-Up/Down
(global-set-key (kbd "<M-up>")
                (lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "<M-kp-up>")
                (lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "<M-down>")
                (lambda () (interactive) (scroll-other-window-down -1)))
(global-set-key (kbd "<M-kp-down>")
                (lambda () (interactive) (scroll-other-window-down -1)))

; page-up down works with C- or M- in the same way as the rest
(global-set-key (kbd "<C-prior>")   'scroll-down-command) ;prior = page-up
(global-set-key (kbd "<C-kp-prior>")   'scroll-down-command)

(global-set-key (kbd "<C-next>")   'scroll-up-command) ;next = page-down
(global-set-key (kbd "<C-kp-next>")   'scroll-up-command)

; Change C-arrows to be the same as M-f/b
(global-set-key (kbd "C-<right>")   'forward-word)
(global-set-key (kbd "C-<kp-right>")   'forward-word)

(global-set-key (kbd "C-<left>")   'backward-word)
(global-set-key (kbd "C-<kp-left>")   'backward-word)

(global-set-key (kbd "<C-kp-home>")   'beginning-of-buffer)
(global-set-key (kbd "<C-kp-end>")   'end-of-buffer)

(global-set-key (kbd "<C-kp-delete>")   'kill-word)


; Set M-arrows to be the same as C-arrows, but by full words
(global-set-key (kbd "M-<kp-right>")   'right-word)
(global-set-key (kbd "M-<kp-left>")   'left-word)


; Command History Completion
(define-key minibuffer-local-map
  (kbd "M-p") 'previous-complete-history-element)
(define-key minibuffer-local-map
  (kbd "M-n") 'next-complete-history-element)
(define-key minibuffer-local-map
  (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map
  (kbd "<down>") 'next-complete-history-element)

; set hot-key for modes
(global-set-key (kbd "C-M-p") 'python-mode)
(global-set-key (kbd "C-M-t") 'text-mode)
(global-set-key (kbd "C-M-C") 'c++-mode)

;lines truncation
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

;Personal Notebook
(global-set-key [f4] 'open-personal-notes-file)

(global-set-key (kbd "C-S-n") '(lambda () (interactive)
                                 (switch-to-buffer "NoteBook.org")))

;Work Notebook
(global-set-key [f5] 'open-work-notes-file)

(global-set-key (kbd "C-S-w") '(lambda () (interactive)
                                 (switch-to-buffer "WorkNoteBook.org")))

;Shell
(global-set-key [f6] 'open-shell)

(global-set-key (kbd "C-S-s")
                '(lambda () (interactive) 
                   (switch-to-buffer
                    (find-most-recent-pattern-buffer "\\*shell"))))

;SimonInit
(global-set-key (kbd "C->") '(lambda () (interactive) 
                                 (open-init-file)))


;Reload buffer
(global-set-key "\C-x\C-r" 'revert-buffer)

(global-set-key "\M-S-[" 'c-beginning-of-defun)
(global-set-key "\M-S-]" 'c-end-of-defun)

(global-set-key "\M-[" 'find-matching-keyword)


;Most Recent Buffers
(global-set-key (kbd "S-C-c") 'find-most-recent-c-buffer)
(global-set-key (kbd "S-C-e") 'find-most-recent-emacs-buffer)
(global-set-key (kbd "S-C-p") 'find-most-recent-python-buffer)
(global-set-key (kbd "S-C-m") 'find-most-recent-magit-buffer)
(global-set-key (kbd "S-C-o") 'find-most-recent-org-buffer)

;Help and documentation
(global-set-key [(control h) (control q)] 'qtdoc-lookup)
(global-set-key [(control h) (control g)] 'google-lookup)
(global-set-key [(control h) (control p)] 'python-lookup)
(global-set-key [(control h) (control c)] 'cpp-lookup)

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

             ; External Buffer Commands
             (define-key gud-mode-map [(alt n)] 'gud-next) 
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

;comint (?)
(defun comint-write-input-ring-all-buffers ()
  (mapc-buffers 'comint-write-input-ring))

 (add-hook 'kill-emacs-hook 'comint-write-input-ring-all-buffers)


(add-hook 'comint-mode-hook
  (lambda()
    (define-key comint-mode-map [(meta p)] 'comint-previous-matching-input-from-input)
    (define-key comint-mode-map [(kbd "<up>")] 'comint-previous-matching-input-from-input)
    
    (define-key comint-mode-map [(meta n)] 'comint-next-matching-input-from-input)
    (define-key comint-mode-map [(kbd "<down>")] 'comint-next-matching-input-from-input)
    
    (define-key comint-mode-map [(control c) (control o)] 'comint-kill-output-to-kill-ring)
    (define-key comint-mode-map [(control x) (control ?\\)] 'toggle-backslash-line)
    (define-key comint-mode-map [(tab)] 'comint-dynamic-complete)

    ; Save history when the shell is killed
    (make-local-variable 'comint-input-ring-file-name)
    (setq comint-input-ring-file-name (concat emacs-persistance-dir "/comint-history"))
    (setq comint-input-ring-size 10000)
    (setq comint-process-echoes 't)
    (comint-read-input-ring)
    (make-local-variable 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook 'comint-write-input-ring)
  ))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; The following is based on:
;; http://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/
(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; Save history between emacs sessions
(savehist-mode t)


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

       (list (cons "\\.bat" 'bat-mode))

       (list (cons "\\.txt" 'text-mode))

       (list (cons "\\.bb$" 'bb-mode))
       (list (cons "\\.inc$" 'bb-mode))
       (list (cons "\\.bbappend$" 'bb-mode))
       (list (cons "\\.bbclass$" 'bb-mode))
       (list (cons "\\.conf$" 'bb-mode))

       (list (cons "\\.lua$" 'lua-mode))

       (list (cons "\\.\\([Nn][Ss][Ii]\\)$" 'nsis-mode))
       (list (cons "\\.\\([Nn][Ss][Hh]\\)$" 'nsis-mode))
       
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

;;;;;;;;;;;;; python-mode - I'll try using the built-in mode that comes with emacs
;;;;;;;(setq py-install-directory (concat emacs-git "Plugins/python-mode.el-6.1.2"))
;;;;;;;(add-to-list 'load-path py-install-directory)
;;;;;;;(setq py-outline-minor-mode-p nil)
;;;;;;;(require 'python-mode)

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


;;;;;;;;;;;;;;Window size

(setq left-two-thirds-screen-pos (list 0.6 my-height-fraction 0 0))    ; 0 from left, 0 from top
(defun frame-occupy-left-two-thirds-screen ()
  "Move and resize the frame so it occupies the left half of the screen."
  (interactive)
  ;(apply 'frame-move-resize left-half-screen-pos)
  (apply 'frame-move-resize left-two-thirds-screen-pos))

(setq left-third-screen-pos (list 0.278 my-height-fraction 0 0))    ; 0 from left, 0 from top
(setq left-sixth-screen-pos (list 0.21 my-height-fraction 0 0))    ; 0 from left, 0 from top
(defun frame-occupy-left-third-screen ()
  "Move and resize the frame so it occupies the left half of the screen."
  (interactive)
  ;(apply 'frame-move-resize left-half-screen-pos)
  (apply 'frame-move-resize left-third-screen-pos))
(defun frame-occupy-left-sixth-screen ()
  "Move and resize the frame so it occupies the left half of the screen."
  (interactive)
  ;(apply 'frame-move-resize left-half-screen-pos)
  (apply 'frame-move-resize left-sixth-screen-pos))

; Set Window size by environment type
(if (not (boundp 'my-emacs-monitors-num))(setq my-emacs-monitors-num 1))
(if (window-system)
    (cond
     ((= my-emacs-monitors-num 1) 
        (frame-occupy-left-two-thirds-screen))
     ((= my-emacs-monitors-num 2)
      (frame-occupy-left-third-screen))
     ((= my-emacs-monitors-num 3)
      (frame-occupy-left-sixth-screen))     
     (t
      (frame-occupy-left-third-screen))
    )
  )

; Invert Colors only on the first time emacs is run
(if (not (boundp 'my-do-invert-color))(setq my-do-invert-color t))
    
(if my-do-invert-color
    (progn 
      (invert-face 'default)
      (setq my-do-invert-color nil)
    )
)


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

(defun my-indent-mode-xjet ()
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

(defun my-indent-mode-cree ()
  "Set indent tabs to the xjet indent mode"
  (interactive)
  ;; C++
  (setq my-indent 4)
  (setq my-substatement 4)
  (setq my-substatement-open 0)
  (setq my-access-label 0)
  (setq my-topmost-intro 0)
  (update-indent-mode)

  ;; Python
  (setq python-indent-offset 2)

  )

(add-hook 'c++-mode-hook
  (lambda ()
    (my-indent-mode-cree))
  )

(add-hook 'c-mode-hook
(lambda ()
;(setq indent-line-function (quote insert-tab))     ;<<<<<<<<<<<<<<<<<<
(my-indent-mode-cree() )
))

(add-hook 'python-mode-hook
  (lambda ()
    (my-indent-mode-cree))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "white")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;(require 'ox-slidy)

(require 'ox-latex)
(setq org-latex-create-formula-image-program 'dvipng)
(org-babel-do-load-languages 'org-babel-load-languages '((latex . t)))

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
(if (string-match "mingw" system-configuration)
    (progn
      (setq org-file-apps
            (append
             '(("png" . "\"c:/Program Files (x86)/giv/bin/giv.exe\" %s"))
             '(("doc" . "\"c:/Program Files (x86)/Microsoft Office/root/Office16/WINWORD.EXE\" %s"))

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
        ("plantuml" . plantuml)
        ))

;;;;;;;;;;;;;;;;;;;;

; magit-diff-file was written by dov, and requsted to be merged into magit.
; See: https://github.com/magit/magit/issues/2553
(defun magit-diff-file (rev-or-range &optional file args)
  "Show changes between a file from another branch"
  (interactive (list (magit-diff-read-range-or-commit "File diff for range" nil current-prefix-arg)
                     (if current-prefix-arg
                       (read-file-name "File: ")
                       buffer-file-name))) 
  (magit-diff-setup rev-or-range nil args
                    (list (replace-regexp-in-string (magit-toplevel) "" (expand-file-name file)))))

(global-set-key (kbd "C-c d") 'magit-diff-file)

; magit settings
(setq magit-push-always-verify nil) ; by default push to updtream branch
(setq git-commit-summary-max-length 256) ; length of commit-msg above which a warning is shown
(load "magit-blame")
(load "markdown-mode")
(setq magit-diff-options '("-w"))
(load "mo-git-blame")
