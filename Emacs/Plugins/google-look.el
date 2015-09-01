;;; google-look.el --- lookup Gtk and Gnome documentation.

;; Copyright 2013, Dov Grobgeld
;;
;; google-look.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; google-look.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; http://www.gnu.org/licenses/gpl.txt, or you should have one in the file
;; COPYING which comes with GNU Emacs and other GNU programs.  Failing that,
;; write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA 02110-1301 USA.
;;
;; google-look.el allows looking up items on google and finds documentation
;; in the python and cpp web sites.

(defvar google-lookup-history '())
(defvar google-search "http://google.com/search?q=")
(defun google-lookup ()
  "Search google"
  (interactive)
  (let* ((info-lookup-mode 'lisp-mode)  
         (default (info-lookup-guess-default 'symbol info-lookup-mode))
         (name (read-string "Google: " default 'google-lookup-history 0 nil)))
    (browse-url-generic (concat google-search name))))

(defvar python-lookup-history '())
(defvar python-search "http://docs.python.org/release/2.7/search.html?q=")
(defun python-lookup ()
  "Search the python documentation"
  (interactive)
  (let* ((info-lookup-mode 'c-mode)
         (default (info-lookup-guess-default 'symbol info-lookup-mode))
         (name (read-string "Python doc search: " default 'python-lookup-history 0 nil)))
    (browse-url-generic (concat python-search name))))

(defvar cpp-lookup-history '())
;(defvar cpp-search "http://www.cplusplus.com/search.do?q=")
(setq cpp-search "http://en.cppreference.com/mwiki/index.php?search=")
(defun cpp-lookup ()
  "Search the cpp documentation"
  (interactive)
  (let* ((info-lookup-mode 'c-mode)
         (default (info-lookup-guess-default 'symbol info-lookup-mode))
         (name (read-string "C++ doc search: " default 'cpp-lookup-history 0 nil)))
    (browse-url-generic (concat cpp-search name))))

(provide 'google-look)
;;; qtdoc.el ends here
