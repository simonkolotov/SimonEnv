; -*- Encoding: utf-8 -*-

; emacs persistance directory
(if (not (boundp 'emacs-persistance-dir))
    (setq emacs-persistance-dir "/home/simon/.emacs.d"))

;;;;;;;;;;EXTERNAL INIT FILE
(load-file "/home/simon/github/SimonEnv/Emacs/SimonInit.el")