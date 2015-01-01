(require 'ox-s5)
(org-export-define-derived-backend 'slidy 's5
  :menu-entry
  '(?S "Export to SLIDY HTML Presentation"
       ((?H "To temporary buffer" org-slidy-export-as-html)
	(?h "To file" org-slidy-export-to-html)
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-slidy-export-to-html t s v b)
		(org-open-file (org-slidy-export-to-html nil s v b)))))))
  :options-alist
  '((:html-link-home "HTML_LINK_HOME" nil nil)
    (:html-link-up "HTML_LINK_UP" nil nil)
    (:slidy-background "SLIDY_BACKGROUND" nil org-slidy-background newline)
    (:html-head-include-default-style "HTML_INCLUDE_DEFAULT_STYLE" nil nil)
    (:html-head-include-scripts "HTML_INCLUDE_SCRIPTS" nil nil))
  :translate-alist
  '((template . org-slidy-template)))

(defgroup org-export-slidy nil
  "Options for exporting Org mode files to Slidy HTML Presentations."
  :tag "Org Export Slidy"
  :group 'org-export-html)

(defcustom org-slidy-background
  "<img id=\"head-icon\" alt=\"graphic with four colored squares\"
  src=\"http://www.w3.org/Talks/Tools/Slidy2/graphics/icon-blue.png\" />"
  "Contents for the background div"
  :group 'org-export-slidy
  :type 'string)

(defcustom org-slidy-style
  "http://www.w3.org/Talks/Tools/Slidy2/styles/w3c-blue.css"
  "Slidy theme stylesheet url."
  :group 'org-export-slidy
  :type 'string)

(defun org-slidy-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (let ((org-html--pre/postamble-class "background")
        (info (plist-put
		(plist-put info :html-preamble (plist-get info :slidy-background))
		:html-postamble nil)))
    (mapconcat
     'identity
     (list
      (org-html-doctype info)
      (format "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\">"
	      (plist-get info :language) (plist-get info :language))
      "<head>"
      "<link rel='stylesheet'"
      " href='http://www.w3.org/Talks/Tools/Slidy2/styles/slidy.css'"
      " media='screen, projection, print'"
      " type='text/css' />"
      (when org-slidy-style
        (format "<link rel='%s' type='%s' media='%s' href='%s' />"
         "stylesheet" "text/css" "screen, projection, print"
         org-slidy-style))
      "<script src='http://www.w3.org/Talks/Tools/Slidy2/scripts/slidy.js'"
      " type='text/javascript'></script>"
      (org-html--build-meta-info info)
      (org-html--build-head info)
      (org-html--build-mathjax-config info)
      "</head>"
      "<body>"
      (org-html--build-pre/postamble 'preamble info)
      (org-html--build-pre/postamble 'postamble info)
      (format "<%s id=\"%s\" class=\"presentation\">"
	      (nth 1 (assq 'content org-html-divs))
	      (nth 2 (assq 'content org-html-divs)))
      ;; title page
      (format "<%s id='title-slide' class='slide cover'>"
	      (plist-get info :html-container))
      (format-spec org-s5-title-slide-template (org-html-format-spec info))
      (format "</%s>" (plist-get info :html-container))
      ;; table of contents.
      (let ((depth (plist-get info :with-toc)))
	(when depth (org-slidy-toc depth info)))
      contents
      (format "</%s>" (nth 1 (assq 'content org-html-divs)))
      "</body>"
      "</html>\n") "\n")))

(defun org-slidy-export-as-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to an HTML buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org SLIDY Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'slidy "*Org SLIDY Export*"
    async subtreep visible-only body-only ext-plist (lambda () (nxml-mode))))

(defun org-slidy-export-to-html
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a SLIDY HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'slidy file
      async subtreep visible-only body-only ext-plist)))

(defun org-slidy-publish-to-html (plist filename pub-dir)
  "Publish an org file to SLIDY HTML Presentation.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'slidy filename ".html" plist pub-dir))

(provide 'ox-slidy)

;;; ox-slidy.el ends here
