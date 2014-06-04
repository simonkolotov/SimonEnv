; -*- Encoding: utf-8 -*-

  (setq emacs-git "/home/simon/github/SimonEnv/Emacs/")
  (setq default-notes-file "/mnt/xjetsrv/public/Groups/Software/Users/Simon/Notes/notes.org")
  (load (concat emacs-git "SimonInit.el"))


; emacs persistance directory
(if (not (boundp 'emacs-persistance-dir))
    (setq emacs-persistance-dir "/home/simon/.emacs.d")
)
