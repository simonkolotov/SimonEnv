;;; org-learning.el - Support for my learning git

(require 'org)

(org-add-link-type "learning" 'org-learning-open)

(defun org-learning-open (path)
  "Visit the file in learning-git"
  (git-find-file-in-repo learning-git path))

;;; org-learning.el ends here