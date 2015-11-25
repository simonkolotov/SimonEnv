; -*- Encoding: utf-8 -*-

(setq emacs-git "/home/simon/github/SimonEnv/Emacs/")
(setq default-notes-file "/home/simon/github/Notes/NoteBook.org")
(setq my-emacs-monitors-num 2) ; Number of monitors attached

; emacs persistance directory
(if (not (boundp 'emacs-persistance-dir))
    (setq emacs-persistance-dir "/home/simon/.emacs.d")
)

(load (concat emacs-git "SimonInit.el"))