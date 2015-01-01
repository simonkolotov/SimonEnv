;;; org-solarjet.el - Open files in the solarjet git repo

(require 'org)

(org-add-link-type "solarjet" 'org-solarjet-open)

(defun org-solarjet-open (path)
  "Visit the file in solarjet-git"
  (git-find-file-in-repo solarjet-git path))

;;; org-solarjet.el ends here