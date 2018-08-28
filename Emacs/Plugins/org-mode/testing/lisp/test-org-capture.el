;;; test-org-capture.el --- Tests for org-capture.el -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2017  Nicolas Goaziou

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Unit tests for Org Capture library.

;;; Code:

(require 'org-capture)

(ert-deftest test-org-capture/fill-template ()
  "Test `org-capture-fill-template' specifications."

  ;; When working on these tests consider to also change
  ;; `test-org-feed/fill-template'.

  ;; %(sexp) placeholder.
  (should
   (equal "success!\n"
	  (org-capture-fill-template "%(concat \"success\" \"!\")")))
  ;; It is possible to include other place holders in %(sexp).  In
  ;; that case properly escape \ and " characters.
  (should
   (equal "Nested string \"\\\"\\\"\"\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "%(concat \"%i\")"
				       "Nested string \"\\\"\\\"\""))))
  ;; %<...> placeholder.
  (should
   (equal (concat (format-time-string "%Y") "\n")
	  (org-capture-fill-template "%<%Y>")))
  ;; %t and %T placeholders.
  (should
   (equal (concat (format-time-string (org-time-stamp-format nil nil)) "\n")
	  (org-capture-fill-template "%t")))
  (should
   (equal (concat (format-time-string (org-time-stamp-format t nil)) "\n")
	  (org-capture-fill-template "%T")))
  ;; %u and %U placeholders.
  (should
   (equal
    (concat (format-time-string (org-time-stamp-format nil t)) "\n")
    (org-capture-fill-template "%u")))
  (should
   (equal
    (concat (format-time-string (org-time-stamp-format t t)) "\n")
    (org-capture-fill-template "%U")))
  ;; %i placeholder.  Make sure sexp placeholders are not expanded
  ;; when they are inserted through this one.
  (should
   (equal "success!\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "%i" "success!"))))
  (should
   (equal "%(concat \"no \" \"evaluation\")\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template
	     "%i" "%(concat \"no \" \"evaluation\")"))))
  ;; When %i contents span over multiple line, repeat initial leading
  ;; characters over each line.
  (should
   (equal "> line 1\n> line 2\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "> %i" "line 1\nline 2"))))
  ;; Test %-escaping with \ character.
  (should
   (equal "%i\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\%i" "success!"))))
  (should
   (equal "\\success!\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\\\%i" "success!"))))
  (should
   (equal "\\%i\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "\\\\\\%i" "success!"))))
  ;; More than one placeholder in the same template.
  (should
   (equal "success! success! success! success!\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template "%i %i %i %i" "success!"))))
  ;; %(sexp) placeholder with an input containing the traps %, " and )
  ;; all at once which is complicated to parse.
  (should
   (equal "5 % Less (See Item \"3)\" Somewhere)\n"
	  (let ((org-store-link-plist nil))
	    (org-capture-fill-template
	     "%(capitalize \"%i\")"
	     "5 % less (see item \"3)\" somewhere)")))))

(ert-deftest test-org-capture/refile ()
  "Test `org-capture-refile' specifications."
  ;; When refiling, make sure the headline being refiled is the one
  ;; being captured.  In particular, empty lines after the entry may
  ;; be removed, and we don't want to shift onto the next heading.
  (should
   (string-prefix-p
    "** H1"
    (org-test-with-temp-text-in-file "* A\n* B\n"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Todo" entry (file+headline ,file "A") "** H1 %?"))))
	(org-capture nil "t")
	(insert "\n")
	(cl-letf (((symbol-function 'org-refile)
		   (lambda ()
		     (interactive)
		     (throw :return
			    (buffer-substring-no-properties
			     (line-beginning-position)
			     (line-end-position))))))
	  (catch :return (org-capture-refile)))))))
  ;; When the entry is refiled, `:jump-to-captured' moves point to the
  ;; refile location, not the initial capture target.
  (should
   (org-test-with-temp-text-in-file "* Refile target"
     (let ((file1 (buffer-file-name)))
       (org-test-with-temp-text-in-file "* A"
	 (let* ((file2 (buffer-file-name))
		(org-capture-templates
		 `(("t" "Todo" entry (file+headline ,file2 "A")
		    "** H1 %?" :jump-to-captured t))))
	   (org-capture nil "t")
	   (cl-letf (((symbol-function 'org-refile-get-location)
		      (lambda (&rest args)
			(list (file-name-nondirectory file1) file1 nil nil))))
	     (org-capture-refile)
	     (list file1 file2 (buffer-file-name)))))))))

(ert-deftest test-org-capture/insert-at-end-abort ()
  "Test that capture can be aborted after inserting at end of capture buffer."
  (should
   (equal
    "* A\n* B\n"
    (org-test-with-temp-text-in-file "* A\n* B\n"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Todo" entry (file+headline ,file "A") "** H1 %?"))))
	(org-capture nil "t")
	(goto-char (point-max))
	(insert "Capture text")
	(org-capture-kill))
      (buffer-string)))))

(ert-deftest test-org-capture/table-line ()
  "Test `table-line' type in capture template."
  ;; When a only file is specified, use the first table available.
  (should
   (equal "Text

| a |
| x |

| b |"
	  (org-test-with-temp-text-in-file "Text\n\n| a |\n\n| b |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file) "| x |"
		       :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When an entry is specified, find the first table in the
  ;; corresponding section.
  (should
   (equal "* Foo
| a |
* Inbox
| b |
| x |
"
	  (org-test-with-temp-text-in-file "* Foo\n| a |\n* Inbox\n| b |\n"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file+headline ,file "Inbox")
		       "| x |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  (should
   (equal "* Inbox
| a |
| x |

| b |
"
	  (org-test-with-temp-text-in-file "* Inbox\n| a |\n\n| b |\n"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file+headline ,file "Inbox")
		       "| x |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When a precise location is specified, find the first table after
  ;; point, down to the end of the section.
  (should
   (equal "| a |


| b |
| x |
"
	  (org-test-with-temp-text-in-file "| a |\n\n\n| b |\n"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file+function ,file forward-line)
		       "| x |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; Create a new table with an empty header when none can be found.
  (should
   (equal "|   |   |\n|---+---|\n| a | b |\n"
	  (org-test-with-temp-text-in-file ""
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file) "| a | b |"
		       :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; Properly insert row with formulas.
  (should
   (equal "| 1 |\n| 2 |\n#+TBLFM: "
	  (org-test-with-temp-text-in-file "| 1 |\n#+TBLFM: "
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| 2 |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When `:prepend' is nil, add the row at the end of the table.
  (should
   (equal "| a |\n| x |\n"
	  (org-test-with-temp-text-in-file "| a |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When `:prepend' is non-nil, add it as the first row after the
  ;; header, if there is one, or the first row otherwise.
  (should
   (equal "| a |\n|---|\n| x |\n| b |"
	  (org-test-with-temp-text-in-file "| a |\n|---|\n| b |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t :prepend t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  (should
   (equal "| x |\n| a |"
	  (org-test-with-temp-text-in-file "| a |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t :prepend t))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; When `:table-line-pos' is set and is meaningful, obey it.
  (should
   (equal "| a |\n|---|\n| b |\n| x |\n|---|\n| c |"
	  (org-test-with-temp-text-in-file "| a |\n|---|\n| b |\n|---|\n| c |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t :table-line-pos "II-1"))))
	      (org-capture nil "t"))
	    (buffer-string))))
  (should
   (equal "| a |\n|---|\n| x |\n| b |\n|---|\n| c |"
	  (org-test-with-temp-text-in-file "| a |\n|---|\n| b |\n|---|\n| c |"
	    (let* ((file (buffer-file-name))
		   (org-capture-templates
		    `(("t" "Table" table-line (file ,file)
		       "| x |" :immediate-finish t :table-line-pos "I+1"))))
	      (org-capture nil "t"))
	    (buffer-string))))
  ;; Throw an error on invalid `:table-line-pos' specifications.
  (should-error
   (org-test-with-temp-text-in-file "| a |"
     (let* ((file (buffer-file-name))
	    (org-capture-templates
	     `(("t" "Table" table-line (file ,file)
		"| x |" :immediate-finish t :table-line-pos "II+99"))))
       (org-capture nil "t")
       t)))
  ;; Update formula when capturing one or more rows.
  (should
   (equal
    '(("@3$1" . "9"))
    (org-test-with-temp-text-in-file "| 1 |\n|---|\n| 9 |\n#+tblfm: @2$1=9"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Table" table-line (file ,file)
		 "| 2 |" :immediate-finish t :table-line-pos "I-1"))))
	(org-capture nil "t")
	(org-table-get-stored-formulas)))))
  (should
   (equal
    '(("@4$1" . "9"))
    (org-test-with-temp-text-in-file "| 1 |\n|---|\n| 9 |\n#+tblfm: @2$1=9"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Table" table-line (file ,file)
		 "| 2 |\n| 3 |" :immediate-finish t :table-line-pos "I-1"))))
	(org-capture nil "t")
	(org-table-get-stored-formulas)))))
  ;; Do not update formula when cell in inserted below affected row.
  (should-not
   (equal
    '(("@3$1" . "9"))
    (org-test-with-temp-text-in-file "| 1 |\n|---|\n| 9 |\n#+tblfm: @2$1=9"
      (let* ((file (buffer-file-name))
	     (org-capture-templates
	      `(("t" "Table" table-line (file ,file)
		 "| 2 |" :immediate-finish t))))
	(org-capture nil "t")
	(org-table-get-stored-formulas))))))


(provide 'test-org-capture)
;;; test-org-capture.el ends here
