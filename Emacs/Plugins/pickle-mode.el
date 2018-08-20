(setq my-python-interpreter "python")

(define-derived-mode pkl-mode fundamental-mode "pkl"
  "Major mode for viewing pkl files."
  (delete-region (point-min) (point-max))
  (call-process my-python-interpreter nil t t (concat emacs-git  "scripts/pkl-cat.py") buffer-file-name)
  (set-buffer-modified-p nil)
  (read-only-mode)
  (toggle-truncate-lines)
  (beginning-of-buffer))
(add-to-list 'auto-mode-alist '("\\.pkl\\'" . pkl-mode))
