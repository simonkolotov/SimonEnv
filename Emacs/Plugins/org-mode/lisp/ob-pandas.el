 ;; -*- emacs-lisp -*-
(defun org-babel-execute:pandas (body params)
  (let ((results
         (org-babel-execute:python
          (concat "import pandas as pd; import numpy as np;" body) (org-babel-merge-params '((:results . "scalar")) params))))
    (org-babel-result-cond (cdr (assoc :result-params params))
      results
      (let ((tmp-file (org-babel-temp-file "sh-")))
        (with-temp-file tmp-file (insert results))
        (org-babel-import-elisp-from-file tmp-file)))))
