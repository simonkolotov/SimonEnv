; -*- Encoding: utf-8 -*-
;;======================================================================
;;   emacs (not Xemacs) mode
;;
;; To use this file, add lines similar to the following to ~/.emacs:
;;
;;  (setq emacs-git "/home/simon/github/SimonEnv/Emacs/")
;;  (setq default-notes-file "/mnt/xjetsrv/public/Groups/Software/Users/Simon/Notes/notes.org")
;;  (load (concat emacs-git "/SimonInit.el"))
;;
;;  The default font used is InconsolataDov. copy it from /home/simon/github/SimonEnv/Emacs/ to ~/.fonts/
;;----------------------------------------------------------------------

;;;;;;;;;;EXTERNAL PLUGINS
(add-to-list 'load-path (concat emacs-git "Plugins/"))
(add-to-list 'load-path (concat emacs-git "Plugins/git-modes")) ;various modes required for magit
(add-to-list 'load-path (concat emacs-git "Plugins/magit"))
(add-to-list 'load-path (concat emacs-git "Plugins/yasnippet"))
(add-to-list 'load-path (concat emacs-git "Plugins/ein"))
(add-to-list 'load-path (concat emacs-git "Plugins/org-mode"))
(add-to-list 'load-path (concat emacs-git "Plugins/org-mode/lisp"))
(add-to-list 'load-path (concat emacs-git "Plugins/org-mode/contrib/lisp"))
(add-to-list 'load-path (concat emacs-git "Plugins/autocomplete"))
(add-to-list 'load-path (concat emacs-git "Plugins/fill-column-indicator-1.83"))

;;;;;;;;;;Env Vars for MetalJet compilation
(setenv "QMAKE" "qmake-qt5")
(setenv "QTDIR" "/usr")
(setenv "XJETQTVERSION" "QT5")

(setenv "PE_HOME" "/home/simon/git/MetalJet")
(setenv "METALJET" "$PE_HOME/XjetApps/MetalJet/Apps/Project/qt/" t)
(setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages:/usr/local/lib64/python2.7/site-packages")
(setenv "LD_LIBRARY_PATH" "$METALJET/BinLinux/:/usr/local/lib" t)

;;;;;;;;;;ein for IPython Notebooks in emacs
(require 'ein-ipynb-mode)
;;;;;;;;;;XSMI for math symbols
(require 'xmsi-math-symbols-input)
(xmsi-mode)
;;;;;;;;;;Undo-Tree
(require 'undo-tree)
(global-undo-tree-mode)
(set-default 'undo-tree-auto-save-history t)

;;;;;;;;;Git for emacs
(require 'magit)

(global-set-key "\C-ci" 'magit-status)
(global-set-key "\C-c\C-b" 'magit-blame-mode)

;;;;;;;;;; ido-mode
(require 'ido)
(ido-mode t)



;;;;;;;;;; yas for programming templates
;(add-to-list 'load-path
;              (concat emacs-git "yasnippet"))
(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs (list "/home/simon/github/SimonEnv/Emacs/Plugins/yasnippet/snippets"
                             "/home/simon/github/SimonEnv/Emacs/Plugins/snippets"))

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

;;;;;;;;;;Toolbars
(menu-bar-mode 't)
(tool-bar-mode 'nil)  

;;;;;;;;;;KEYBOARD SHORTCUTS
                                        ; Undo-Redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo) ; 【Ctrl+z】
(global-set-key (kbd "C-S-z") 'redo) ; 【Ctrl+Shift+z】;  Mac style

(global-set-key "\C-o" 'find-file)  ; Open file (Microsoft style)
(global-set-key (kbd "C-<f4>") 'kill-this-buffer) ; Close Buffer (Microsoft style)

(define-key global-map (kbd "RET") 'newline-and-indent) ; For programming language modes

(global-set-key (kbd "M-g") 'goto-line) ; Goto-line

(global-set-key (kbd "C-<f9>") 'compile) ; Compile

(global-set-key "\M-`" 'next-error) ; Next Error (and also next file in dov-git-grep)
(global-set-key "\M-~" 'previous-error) ; Previous Error (and also previous file in dov-git-grep)

                                        ; C-Tab: Next Buffer
(global-set-key (kbd "C-<tab>") 'next-buffer)

                                        ; C-S-Tab: Previous Buffer
(global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer)


                                        ; Scroll with Alt-Up/Down
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

; NB: scrolling down = moving the window up...
(global-set-key (kbd "<M-up>")   (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "<M-kp-up>")   (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "<M-down>") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "<M-kp-down>") (lambda () (interactive) (scroll-up 1)))
                                        ; Scroll Other Window with Alt-Up/Down
(global-set-key (kbd "<C-up>")   (lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "<C-kp-up>")   (lambda () (interactive) (scroll-other-window-down 1)))
(global-set-key (kbd "<C-down>") (lambda () (interactive) (scroll-other-window-down -1)))
(global-set-key (kbd "<C-kp-down>") (lambda () (interactive) (scroll-other-window-down -1)))


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
(define-key minibuffer-local-map (kbd "M-p") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "M-n") 'next-complete-history-element)
(define-key minibuffer-local-map (kbd "<up>") 'previous-complete-history-element)
(define-key minibuffer-local-map (kbd "<down>") 'next-complete-history-element)


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
             (define-key gud-mode-map [(alt u)] 'gud-finish)

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



(defun gdb-keys (map) 
  "Set key bindings for gdb debugging"
  (interactive)
  (define-key map [(alt n)] 'gdb-next)
  (define-key map [(alt s)] 'gdb-step)
  (define-key map [(control c) (control s)] 'gdb-step)
  (define-key map [(alt u)] 'gdb-finish)
;;  (define-key map [µ] 'gdb-finish)
  (define-key map [(alt f)] 'gdb-finish)
  (define-key map [(alt h)] 'gdb-cont-to)
  (define-key map [(hebrew_finalkaph)] 'gdb-next)
  (define-key map [(hebrew_finalpe)] 'gdb-step)
  (define-key map [(iso-next-group)] nil))
             
;;============================================

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat emacs-git "autocomplete/ac-dict"))
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Save history between emacs sessions
(savehist-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Fill-Column Indicator
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq-default fill-column 100)

                                        ; Lines Truncation
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)
(set-default 'truncate-lines 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Subword Mode - Jump by camelback
(global-subword-mode t)

;                                        ; Automatic Parentheses completion  Mode
;(electric-pair-mode f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set some auto modes
(setq auto-mode-alist
      (append
       (list (cons "\\.cmake$" 'cmake-mode))

       (list (cons "\\.pro$" 'text-mode))
       
       (list (cons "SConstruct" 'python-mode))
       (list (cons "SConscript" 'python-mode))
       (list (cons "\\.py$" 'python-mode))
       (list (cons "\\.run$" 'python-mode))
       
       (list (cons "\\.hh$" 'c++-mode))
       (list (cons "\\.H$" 'c++-mode))
       (list (cons "\\.cxx$" 'c++-mode))

       (list (cons "\\.json$" 'js2-mode))

       (list (cons "\\.xml$" 'xml-mode)) 

       (list (cons "\\.txt$" 'text-mode))
       
       (list (cons "\\.org" 'org-mode))
       
       auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


                                        ; python-mode
(setq py-install-directory "/home/simon/github/SimonEnv/Emacs/Plugins/python-mode.el-6.1.2")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

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

                                        ; set hot-key for python mode
(global-set-key (kbd "C-M-p") 'python-mode)


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

                                        ; set hot-key for text mode
(global-set-key (kbd "C-M-t") 'text-mode)

                                        ; set hot-key for C++ mode
(global-set-key (kbd "C-M-C") 'c++-mode)

(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c h") 'ff-find-other-file)))


                                        ; Overwrite Selection
(delete-selection-mode t)

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
;;'(cua-mode t nil (cua-base))       

                                        ; Start In Window sized 125x60 for work
                                        ;    (for netbook - 101x23)
(if (window-system) (set-frame-size (selected-frame) 125 60))

;; Start In Full Screen Mode
                                        ;(initial-frame-alist (quote ((fullscreen . maximized)))))


                                        ; Invert Colors
(invert-face 'default)


                                        ; Change TAB to 2 spaces
                                        ;(setq c-basic-indent 2)
(setq-default tab-width 2)
(setq python-indent tab-width)
(setq perl-indent-level tab-width)
                                        ;(setq tab-width 2)

(setq indent-line-function 'insert-tab)
(setq standard-indent 2)
(setq-default indent-tabs-mode nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Indentation


(add-hook 'python-mode-hook
(lambda ()
(setq indent-tabs-mode nil)
(setq tab-width 2)

                                        ;(setq indent-line-function (quote insert-tab))

))


(defun update-indent-mode ()
  (setq c-basic-offset my-indent)
  (c-set-offset 'substatement my-substatement)
  (c-set-offset 'substatement-open my-substatement-open)
  (c-set-offset 'access-label my-access-label)
  (c-set-offset 'topmost-intro my-topmost-intro))



(defun xjet-indent-mode ()
  "Set indent tabs to the xjet indent mode"
  (interactive)
  ;; C++-python
  (setq my-indent 2)
  (setq my-substatement 2)
  (setq my-substatement-open 0)
  (setq my-access-label 0)
  (setq my-topmost-intro 0)
  (update-indent-mode)

  ;; Python
  (setq py-indent-offset 2)
  )

(add-hook 'c++-mode-hook
(lambda ()
(xjet-indent-mode)))
  
(add-hook 'c-mode-hook
(lambda ()
;(setq indent-tabs-mode nil)
;(setq tab-width 2)
                                        ;(setq indent-line-function (quote insert-tab))
(xjet-indent-mode() )
))


                                        ;(add-hook 'python-mode-hook
                                        ;          '(lambda ()
                                        ;             (setq tab-width 2)
                                        ;             (setq python-indent 2))
                                        ;             (setq indent-tabs-mode nil))


;;;                                        ; BackTab
;;;(global-set-key [backtab] 'unindent-for-tab-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                                        ; Don't add newline at end of file
(setq mode-require-final-newline nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "white")))))

                                        ; reload changed files
(global-auto-revert-mode t)

                                        ; Cua Mode
                                        ;(cua-mode)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Advanced Search Functions:
;; git grep
(load "dov-git-grep")
(global-set-key [(control c) ?f] 'dov-git-grep)
;; git find file
(load "git-find-file.el")
(global-set-key [(control c) ?g] 'git-find-file)

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

;(global-set-key (kbd "C-M-!") 'sudo-shell-command(command))


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
                                        ; Org Mode
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
  (local-set-key "\C-c\M-c" 'org-screenshot)
  (local-set-key "\C-c\C-pe" 'org-toggle-emphasis-markers)
  (local-set-key "\C-c\C-pp" 'org-toggle-pretty-entities)
  (local-set-key "\C-c\C-pi" 'org-toggle-iimage-inorg)
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
           '(("html" . "firefox %s"))
           org-file-apps))))

(setq org-src-lang-modes
      '(("elisp" . emacs-lisp)
        ("ditaa" . artist)
        ("asymptote" . asy)
        ("dot" . fundamental)
        ("perl" . cperl)
        ("python" . python)
        ))
