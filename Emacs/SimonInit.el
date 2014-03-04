; -*- Encoding: utf-8 -*-

;;;;;;;;;;EXTERNAL PLUGINS
(add-to-list 'load-path "/home/simon/github/SimonEnv/Emacs/Plugins/")
(add-to-list 'load-path "/home/simon/github/SimonEnv/Emacs/Plugins/magit")

;;;;;;;;;;Undo-Tree
(require 'undo-tree)
(global-undo-tree-mode)
(set-default 'undo-tree-auto-save-history t)

;;;;;;;;;Git for emacs
(require 'magit)

;;;;;;;;;; ido-mode
(require 'ido)
(ido-mode t)

;;;;;;;;;;KEYBOARD SHORTCUTS
                                        ; Undo-Redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo) ; 【Ctrl+z】
(global-set-key (kbd "C-S-z") 'redo) ; 【Ctrl+Shift+z】;  Mac style

(global-set-key "\C-o" 'find-file)  ; Open file (Microsoft style)
(global-set-key (kbd "C-<f4>") 'kill-this-buffer) ; Close Buffer (Microsoft style)

(define-key global-map (kbd "RET") 'newline-and-indent) ; For programming language modes

(global-set-key (kbd "C-<f9>") 'compile) ; Compile

(global-set-key "\M-`" 'next-error) ; Next Error (and also next file in dov-git-grep)

                                        ; C-Tab: Next Buffer
(global-set-key (kbd "C-<tab>") 'next-buffer)

                                        ; C-S-Tab: Previous Buffer
(global-set-key (kbd "C-S-<iso-lefttab>") 'previous-buffer)


                                        ; Scroll with Alt-Up/Down
(global-set-key (kbd "<M-up>")   (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "<M-kp-up>")   (lambda () (interactive) (scroll-down 1)))
(global-set-key (kbd "<M-down>") (lambda () (interactive) (scroll-up 1)))
(global-set-key (kbd "<M-kp-down>") (lambda () (interactive) (scroll-up 1)))

                                        ; Change C-arrows to be the same as M-f/b
(global-set-key (kbd "C-<right>")   'forward-word)
(global-set-key (kbd "C-<kp-right>")   'forward-word)

(global-set-key (kbd "C-<left>")   'backward-word)
(global-set-key (kbd "C-<kp-left>")   'backward-word)

(global-set-key (kbd "<C-kp-home>")   'beginning-of-buffer)
(global-set-key (kbd "<C-kp-end>")   'end-of-buffer)

(global-set-key (kbd "<C-kp-delete>")   'kill-word)

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
(add-to-list 'load-path "~/.emacs.d/Plugins/autocomplete")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/autocomplete/ac-dict")
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Save history between emacs sessions
(savehist-mode t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Fill-Column Indicator
(add-to-list 'load-path "~/.emacs.d/Plugins/fill-column-indicator-1.83")
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
                                        ; python-mode
(setq py-install-directory "/home/simon/.emacs.d/Plugins/python-mode.el-6.1.2")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

                                        ; use python-mode for .run files
(add-to-list 'auto-mode-alist '("\\.run\\'" . python-mode))

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

                                        ; Start In Window sized 124x40
(if (window-system) (set-frame-size (selected-frame) 125 40))

;; Start In Full Screen Mode
                                        ;(initial-frame-alist (quote ((fullscreen . maximized)))))


                                        ; Invert Colors
(invert-face 'default)


                                        ; Change TAB to 2 spaces
                                        ;(setq c-basic-indent 2)
(setq-default tab-width 2)
(setq python-indent 2)
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

(add-hook 'c-mode-hook
(lambda ()
(setq indent-tabs-mode nil)
(setq tab-width 2)

                                        ;(setq indent-line-function (quote insert-tab))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ; Show full(er) buffer name in status bar
(setq-default mode-line-buffer-identification
              (cons
               '(:eval (replace-regexp-in-string "^.*/\\(.*\\)/" "\\1/" default-directory))
               mode-line-buffer-identification))
