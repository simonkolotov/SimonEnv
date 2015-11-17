; scott.emacs.el
; Scott McPeak's attempt to convince emacs to use some reasonable key bindings


; file organization:  The file is divided into sections, where each
; section defines functions and variables related to a specific
; topic.  The second-to-last section contains preferences and
; policies (other than those set by M-x configure, which are in
; another file).  The last section has all the keybindings.


; ------------ what version of emacs? -----------------
; true if we're under NT
(setq version-os-nt     (equal (getenv "OS") "Windows_NT"))

; true if we're under XEmacs (www.xemacs.org)
(setq version-xemacs    (string-match "XEmacs\\|Lucid" emacs-version))

; true if we're under regular emacs
(setq version-emacs     (not version-xemacs))

; true if we're running under anything other than a text terminal
(setq version-not-term  (not (not window-system)))

; true under regular emacs and X windows
(setq version-emacs-x   (and (not version-xemacs) (equal window-system 'x)))


; ------------------ version smoothing ------------------------
; this section tries to provide a consistent environment for elisp
; code across versions of emacs, to reduce the number of explicit
; version dependencies in code that follows

; xemacs doesn't have these functions, so I define them myself
(if version-xemacs (progn
  (defun frame-char-width ()
    "Return the width of a column."
    (round (/ (frame-pixel-width) (frame-width))))

  (defun frame-char-height ()
    "Return the height of a row."
    (round (/ (frame-pixel-height) (frame-height))))
    
  (defun transient-mark-mode (flag)
    "Turn on transient-mark-mode (xemacs nop)."
    (interactive)
    t)   ; nop

  (defun set-default-font (font)
    "Set the font for a frame (xemacs nop)."
    t)

  (defun set-background-color (color)
    "Set frame's background color (xemacs nop)."
    t)
    
  (defun set-foreground-color (color)
    "Set frame's foregroun color (xemacs nop)."
    t)
    
  (defun set-cursor-color (color)
    "Set cursor color (xemacs nop)."
    t)

  (defun toggle-debug-on-error ()
    "Toggle the `debug-on-error' variable."
    (interactive)
    (setq debug-on-error (not debug-on-error)))
))


; ---------- general-purpose utilities -----------------------
; to match with set-mark..
(defun set-point (pos)
  "Set point to 'pos'."
  (goto-char pos))

(defun line-of-position (pos)
  "Return the 1-based line number of the given position."
  (count-lines (point-min)
               (if (>= pos (point-max))
                   (point-max)
                   (+ pos 1))
  ))

; why is this fn missing?  it should be standard!
(defun current-line ()
  "Return the current line number of point \(the cursor\)."
  (line-of-position (point)))

; workaround for annoying "L??" crap
(defun print-curline ()
  "Print to status line the current line number."
  (interactive)
  (princ (current-line)))

; I need a fn that behaves the way kill-region does when called with
; no arguments, interactively
(defun cut-text ()
  "Cut the selected text, placing it in the clipboard \(kill ring\)."
  (interactive)
  (let ((p (point))
        (m (mark)))
    (if (< p m)
      (kill-region p m)
      (kill-region m p))
  ))

; and another for copy-text
(defun copy-text ()
  "Copy the selected text to the clipboard \(kill ring\)."
  (interactive)
  (let ((p (point))
        (m (mark)))
    (if (< p m)
      (kill-ring-save p m)
      (kill-ring-save m p))
  ))

(defun get-sel-or-word ()
  "Return either the selected text, or if it's not active,
the word under the cursor."
  (if (is-mark-active)
    (let ((p (point))
          (m (mark)))
      (if (< p m)
        (buffer-substring p m)
        (buffer-substring m p)
      ))
    (progn
      (re-search-backward "[;:\*() \t\n]")      ; go to the place before the name
      (re-search-forward "[^;:\*() \t\n]+")
      (match-string 0)
    )
  )
)

; untabify a file
(defun untabify-buffer ()
  "Untabify the whole buffer."
  (interactive)
  (untabify (point-min) (point-max))
  (princ "Untabified buffer.")
)

; tabify a file
(defun tabify-buffer ()
  "Tabify the whole buffer."
  (interactive)
  (tabify (point-min) (point-max))
  (princ "Tabified buffer.")
)

; string replacement noninteractively
(defun noninteractive-replace (from to)
  (set-mark (beginning-of-buffer))
  (while (search-forward from nil t)
    (replace-match to nil nil)))

; version that matches regular expressions
(defun noninteractive-re-replace (from to)
  (set-mark (beginning-of-buffer))
  (while (re-search-forward from nil t)
    (replace-match to nil nil)))

; toggle case sensitivity
(defun toggle-search-case-sensitivity ()
  "Toggle whether searches are case-sensitive"
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (if case-fold-search
    (princ "Searches are now case-*in*sensitive.")
    (princ "Searches are now case-*sensitive*."))
)


(defun strcmp (s1 s2)
  "String comparison; return value is as for C's strcmp."
  ; (I had originally written this with `compare-strings', but it turns
  ; out that fn isn't available in older versions of emacs)
  (if (string-equal s1 s2)
      0
      (if (string-lessp s1 s2)
          -1
          +1
      )))


(defun filter-list (list predicate)
  "Return a list containing only those elements from 'list' which
  cause 'predicate' to return true."
  (if (not list)
      nil          ; recursion base case
      (if (funcall predicate (car list))
          ; keep the item
          (cons (car list) (filter-list (cdr list) predicate))
          ; else, remove it
          (filter-list (cdr list) predicate)
      )))

; example of calling filter-list with a lambda function
;(filter-list '(1 2 3 4) #'(lambda (n) (> n 2)))


(defun begins-with (haystack needle)
  "Return true if the first argument begins with the second argument,
  as strings."
  (and (>= (length haystack)
           (length needle))
       (equal (substring haystack 0 (length needle))
              needle)
  ))


; useful for evaluating forms and getting the result in the working
; buffer instead of buried in the messages window
(defun princ-buffer (value)
  (insert (prin1-to-string value)))

  
; whenever I do M-x revert-buffer I am annoyed by having to type 'yes'
(defun revert-buffer-yes ()
  "Revert the buffer without asking for confirmation."
  (interactive)
  (if (buffer-modified-p)                                                
    ; what I really want is a y/n prompt, but I don't know how
    ; to do that (and am too lazy to look now)
    (princ "The buffer has been modified; kill it instead of reverting.")
    ; else, ..
    (let
      ((cur (current-line)))
      (revert-buffer t t)

      ; if "readonly" appears near the beginning, then make the buffer read-only
      (goto-char 0)
      (if (re-search-forward "readonly" 100 t)   ; returns true only if it matches
        (toggle-read-only 1))                    ; means set to read-only

      ; go back to where we were before the revert
      (goto-line cur)
    )
  ))


(defun revert-all-buffers ()
  "Revert all open, unmodified buffers."
  (interactive)
  (revert-all-buffers-helper (buffer-list))
)

; joy! recursion! ...
; more joy! this hits the max recursion depth limit! ........
(defun revert-all-buffers-helper (list)
  (if (consp list)
    (progn
      (save-excursion
        (set-buffer (car list))
        (if (and buffer-file-name
                 (not (buffer-modified-p)))
          (revert-buffer-yes)
        ))
      (revert-all-buffers-helper (cdr list))
    )))


; a way to find out how to name a key in elisp code
; (doesn't work all that well...)
(defun get-key-name ()
  "Prompts user to type a key, and prints the key's name."
  (interactive)
  (princ (read-key-sequence-vector "Key: ")))


(defun my-font-lock-fontify-buffer ()
  "Fontify buffer, unless we're in fundamental mode, in which
   case *unfontify* buffer"
  (interactive)           
  (if (eq major-mode 'fundamental-mode)
      (font-lock-unfontify-buffer)
      (font-lock-fontify-buffer))
)

(defmacro copy-fn-defn (oldname newname)
  "Given an existing function 'oldname', define a new function 'newname'
which has exactly the text of 'oldname'.  Then, even if 'oldname' is
redefined, 'newname' will behave like the old 'oldname'."
  (fset newname (symbol-function oldname))
)

;  (let ((fn-defn (symbol-function oldname)))        ; grab function
;    (append (list 'defun newname) (cdr fn-defn))    ; new fn; cdr removes 'lambda
;  ))

(defun string-at-cursor ()
  "Return the string the cursor is on."
  (let* ((chars "A-Za-z\\-")
         (left (save-excursion
                 (skip-chars-backward chars)
                 (point)))
         (right (save-excursion
                   (skip-chars-forward chars)
                   (point)))
        )
    (buffer-substring left right)
  ))


; ---------------- frame move/resize (i.e. X window) -----------------------
(defun frame-move-resize
         (width-fraction height-fraction      ; fractions of screen width, height
          horiz-pos vert-pos)                 ; pels from left, top (neg: right, bot)
  "Move and resize the frame so its width and height are the given
  fractional amounts of the screen width and height, and it is positioned
  from the left (negative: right) and top (negative: bottom) edges by
  the specified number of pixels."
  (if version-not-term
    (let* ((char-width (frame-char-width))          ; character width in pels
           (char-height (frame-char-height))        ; char height
           (disp-width (x-display-pixel-width))     ; screen width in pels
           (disp-height (x-display-pixel-height))   ; screen height
           (frame (selected-frame))                 ; frame to act upon
           (new-columns
             (round (/ (* disp-width width-fraction) char-width)))
           (new-lines
             (round (/ (* disp-height height-fraction) char-height)))
          )
      (set-frame-size frame new-columns new-lines)  ; do actual resize
      (sleep-for 0.5)                               ; see below
      (set-frame-position frame horiz-pos vert-pos) ; and move (after resize)

      ; There is a bug where sometimes, inexplicably, the window appears
      ; shifted about halfway off the right edge of the screen.  I have no
      ; idea which software the bug is in (emacs, X server, KDE, ...).
      ; It can be reliably reproduced by running, in sequence:
      ;   `frame-occupy-normal-position'
      ;   `frame-occupy-normal-position'
      ;   `frame-occupy-right-half-screen'
      ;   `frame-occupy-normal-position'        ; here it will be bad
      ;   `frame-occupy-normal-position'        ; one more gets it back ok
      ;
      ; Update:  On a hunch it was (at least partially) a race condition
      ; I've inserted a 0.1 second delay between resize and move, and this
      ; has cured the problem (at least when running on my system with emacs
      ; running on the same machine as the X server).
      ;
      ; Faster machine, longer delay (?): 0.2 secs works now..
      ; Then again, no delay at all also works!  cool.
      ; no it doesn't.. but the delay doesn't help..
      ; nothing works except doing it twice...
    )
  ))  

(defun frame-occupy-right-half-screen ()
  "Move and resize the selected frame so it occupies roughly
  the right-hand half of the screen."
  (interactive)                                                                  
  ; doing it twice just makes it mess up other times..
  ;(apply 'frame-move-resize right-half-screen-pos)   ; twice to work around bug?
  (apply 'frame-move-resize right-half-screen-pos))

(defun frame-occupy-left-half-screen ()
  "Move and resize the frame so it occupies the left half of the screen."
  (interactive)
  ;(apply 'frame-move-resize left-half-screen-pos)
  (apply 'frame-move-resize left-half-screen-pos))

(defun frame-occupy-normal-position ()
  "Move frame to my normal emacs position."
  (interactive)
  ;(apply 'frame-move-resize normal-screen-pos)
  (apply 'frame-move-resize normal-screen-pos))


; --------------------- window scrolling ----------------------------------
; window-start is position at top of window
(defun topmost-line ()
  "The topmost displayed line number."
  (line-of-position (window-start)))

; note that "scroll-up" actually moves the view lower in the file --
; I think emacs' terminology is backwards, but I am not going to try
; to switch, in hopes of avoiding confusion
(defun scroll-up-1 ()
  "Scroll text up \(view moves down\) by 1 line."
  (interactive)
  (while (<= (current-line) (topmost-line))
    (move-down))     ; move down before scrolling, so it doesn't move to left
  (scroll-up 1)
)

(defun bottommost-line ()
  "The bottom-most displayed line number."
  (line-of-position (- (window-end) 1)))

; keeps cursor *two* lines above bottommost because of bug in scrolling logic,
; which appears to be a race condition between scrolling and sexp evaluation
(defun scroll-down-1 ()
  "Scroll text down \(view moves up\) by 1 line."
  (interactive)
  (while (>= (current-line) (- (bottommost-line) 2))
    (move-up))
  (scroll-down 1)
)


(defun rightmost-column ()
  "The rightmost column \(0-based\) currently displayed."
  (let ((hscroll (window-hscroll))
        (width (window-width)))
    (if (= hscroll 0)
      (- (window-width) 1)                     ; 0-based column value
      (+ (window-width) (window-hscroll) -2))  ; hscroll counts $'s at left
  ))

; keeps cursor one line shy of right edge because a bug in the scrolling
; code makes it not work at the absolute edge
(defun scroll-right-1 ()
  "Scroll the window text right \(view moves left\) by 1 space."
  (interactive)
  (while (>= (current-column) (- (rightmost-column) 1))
    (move-left))           ; cursor is at right edge -- move it left by 1
  (scroll-right 1)
  (if (= (window-hscroll) 1)
    (scroll-right-1))      ; do it again if we end up in the wierd mode
)

(defun leftmost-column ()
  "The leftmost column currently displayed."
  (window-hscroll))

(defun scroll-left-1 ()
  "Scroll the window text left \(view moves right\) by 1 space."
  (interactive)
  (while (<= (current-column) (leftmost-column))
    (move-right))           ; cursor is at left edge -- move it right by 1
  (scroll-left 1)
  (if (= (window-hscroll) 1)
    (scroll-left-1))   ; do it again, because hscroll == 1 is really same view position
)


; fixed number of iterations
(defun do-n-times (n func)
  "Execute func n times."
  (while (> n 0)
    (funcall func)
    (setq n (- n 1))
  ))
;(do-n-times 3 '(lambda () (princ "foo")))


; and these are for scrolling faster (I resort to calling the
; single-place fns over and over, instead of folding the repetition
; count into the scrolling code, because the scrolling code is
; already pretty hacked up to get around various limitations and
; bugs in Emacs, and I don't want to relive the hacking)
(defun scroll-left-several () (interactive)
  (do-n-times ctl-shift-scroll-h-amt 'scroll-left-1))
(defun scroll-right-several () (interactive)
  (do-n-times ctl-shift-scroll-h-amt 'scroll-right-1))
(defun scroll-down-several () (interactive)
  (do-n-times ctl-shift-scroll-v-amt 'scroll-down-1))
(defun scroll-up-several () (interactive)
  (do-n-times ctl-shift-scroll-v-amt 'scroll-up-1))



; ---------------- managing multiple windows -----------------------
(defun toggle-buffer ()
  "Switch to the most-recently visited buffer."
  (interactive)
  (switch-to-buffer (other-buffer))
)

(defun other-window-or-buffer-toggle ()
  "If multiple windows are present, switch among them as
   `other-window' does.  If not, do `toggle-buffer'."
  (interactive)
  (if (> (count-windows) 1)
    (other-window 1)
    (toggle-buffer)
  )
)


(defun enlarge-window-both-ways ()
  "Make the current window one line taller and one line fatter,
  if possible."
  (interactive)
  (if (< (window-height) (- (frame-height) 1))
   ; only do this if the window is shorter than the full
   ; height (minus 1 for minibuffer) of the frame
   (enlarge-window 1)
  )
  (if (< (window-width) (frame-width))
   ; and only this if it's thinner than full frame width
   ; (both checks are to prevent the annoying beeping that
   ; occurs otherwise)
   (enlarge-window-horizontally 1)
  )
)


; usable buffer list!
(defun usable-buffer-list ()
  "Replaces the current buffer with the buffer list buffer."
  (interactive)
  (set-window-buffer (selected-window) (list-buffers-noselect)))

  
; let's try this...
;(defun my-alt-tab-fn ()
;  "go back one buffer"
;  (interactive)
;  (switch-to-buffer (other-buffer)))
;(global-set-key [M-tab] 'my-alt-tab-fn)


; count the number of windows in the current frame, ignoring the
; minibuffer; turns out `count-windows' already does this, so I'm
; leaving this here for now as an example
;(defun my-count-windows-helper-fn (w)
;  ; 'count' will be a variable defined in `my-count-windows', and
;  ; accessed via dynamic scoping
;  (setq count (+ 1 count)))
;
;(defun my-count-windows ()
;  (let ((count 0))    ; passed to `my-count-windows-helper-fn' via dynamic scope
;    (walk-windows 'my-count-windows-helper-fn)
;    count
;  ))


; F5: until I can implement full maximize/restore, this will do
;(global-set-key [f5] 'delete-other-windows)


; clear this initially
(setq my-saved-window-config-list nil)

; the F5 behavior I want is essentially Windows MDI maximize/restore
; toggle -- i.e. what Borland does.
(defun mdi-maximize-restore-toggle ()
  "When called in a multi-window frame it will save the window
  configuration by calling `current-window-configuration', then call
  `delete-other-windows'.  When called in a single-window frame it will
  restore the frame configuration by calling `set-window-configuration'."
  (interactive)
  (if (> (count-windows) 1)
    (progn    ; multi-window case: maximize
      ; throw away and configs that apply to deleted frames or
      ; the current frame
      (gc-my-window-config-list (selected-frame))

      ; add the config for this frame
      (setq my-saved-window-config-list
        (cons (current-window-configuration) my-saved-window-config-list))

      ; and go to single-window mode
      (delete-other-windows)
    )

    (progn    ; single-window case: restore
      ; search for the config that applies to this frame
      (restore-applicable-window-configuration my-saved-window-config-list)
    )
  ))

(defun gc-my-window-config-list (frame)
  "Remove any saved configs that apply to deleted frames or to
  the 'frame' argument."
  (setq my-saved-window-config-list
    (filter-list my-saved-window-config-list
      #'(lambda (config)
          (and
            (member (window-configuration-frame config) (frame-list))
            (not (eq (window-configuration-frame config) frame))
          ))
    )))

(defun restore-applicable-window-configuration (list)
  "Look through 'list' for a window config that applies to the selected
  frame.  If found, restore via that config.  If not, say so."
  (if (not list)
    (princ "There is no saved window config for this frame.")
    (if (eq (window-configuration-frame (car list)) (selected-frame))
      ; restore it
      (set-window-configuration (car list))

      ; else, proceed down list
      (restore-applicable-window-configuration (cdr list))
    )))


; what I plan to bind f5 to
(defun intended-mdi-maximize-restore-toggle ()
  "Calls `mdi-maximize-restore-toggle' if it works, otherwise
  calls `delete-other-windows'."
  (interactive)
  (if (functionp 'window-configuration-frame)
    (mdi-maximize-restore-toggle)      ; recent enough emacs
    (delete-other-windows)             ; not recent enough.. use old behavior
  ))


; ---------------------- typing normal characters -------------------
(defun my-insert-tab ()
  "Insert a real tab character"
  (interactive)
  (insert "\t"))


; cool next-line that moves to the end of the next line automatically!
(defun go-to-end-of-next-line ()               
  (interactive) (move-down) (move-end))


; to bind something to do nothing but global-unset-key doesn't work
(defun no-op ()
  "Do nothing."
  (interactive)
  t)


; ----------------- insertion macros --------------------
; insert current date/time
;   %m   month in [01..12]
;   %-m  month in [1..12]
;   %d   day in [01..31]
;   %y   year in [00..99]
;   %Y   full year
;   %H   hour in [00..23]
;   %M   minute in [00..59]
; see format-time-string for more info on formatting options
(defun my-time-string ()
  (format-time-string "%Y-%m-%d %H:%M"))
(defun insert-time-string ()
  "Insert time and date at cursor."
  (interactive)
  (insert (my-time-string)))
  

(defun insert-random-integer ()
  "insert a random 28-bit number"
  (interactive)
  (insert (format "%d" (abs (random))))
)

; randomize the above
(random 't)


(defun to-hex-digit (n)
  "convert 0 <= 'n' <= 15 to a hex digit string (capital if letter)"
  (if (< n 10)
    (format "%d" n)
    (format "%c" (+ (- n 10) ?A))
  ))

(defun insert-random-hex-integer ()
  "insert a random 32-bit hexadecimal integer"
  (interactive)
  (insert "0x")    ; C notation!
  (let ((i 8))
    (while (> i 0)
      (insert (to-hex-digit (random 16)))
      (setq i (- i 1))
    )))


; ---------------- portable mark functions ---------------
(defun is-mark-active ()
  "True if the selection is displayed."
  (cond
    (version-emacs
      mark-active)           ; (mark) causes error if inactive under regular emacs
    (version-xemacs
      (not (not (mark))))    ; but nil with xemacs, and mark-active doesn't exist
    (t t)
  ))

(defun turn-off-mark ()
  "Turn off the mark selection."
  (cond
    (version-emacs
      (setq mark-active nil))
    (version-xemacs
      (zmacs-deactivate-region))
    (t t)
  ))

(defun turn-on-mark ()
  "Turn on the mark selection, with mark equal to cursor."
  (cond
    (version-emacs
      (set-mark (point)))      ; push-mark doesn't work for this!!!
    (version-xemacs
      (push-mark (point))
      (zmacs-activate-region))
    (t t)
  ))

(defun keep-region-selected ()
  "Allow the selected region to remain selected after this command."
  (cond
    (version-emacs   t)
    (version-xemacs  (setq zmacs-region-stays t))
    (t t)
  ))


; ------------------ copy and paste -------------------------------
; relevant fns
;   x-get-selection: X selection -> string
;   x-own-selection: string -> X clipboard
;   insert-string: string -> buffer

; copy to *both* emacs clipboard and X clipboard
; (xemacs doesn't do this by default!!)
(defun my-copy-cmd ()
  "Copy currently selected region to clipboard."
  (interactive)
  (if (not (is-mark-active))
    (princ "Nothing is selected.")
    (cond
      (version-emacs
        (let ((p (point)) (m (mark)))
          (if (> p m)
            (kill-ring-save m p)
            (kill-ring-save p m)
          )))
      (version-xemacs
        ; grab selection since we'll need it in a moment, and
        ; copy-primary-selection clobbers mark
        (let ((selString (buffer-substring (mark) (point))))
          ; make selection available within XEmacs
          (copy-primary-selection)

          ; make it available to other programs
          ;(x-own-selection selString)     ; this is very flaky
          (x-store-cutbuffer selString)    ; appears to work

        ))
    )
  ))


(defun my-paste-cmd ()
  "Insert contents of clipboard."
  (interactive)
  (cond
    (version-emacs
      (yank))
    (version-xemacs
      ; get X selection and insert it
      ;(insert-string (x-get-selection))   ; flaky
      (insert-string (x-get-cutbuffer))    ; works?
    )
  ))


(defun my-cut-cmd ()
  "Remove selected text, copying it to the clipboard."
  (interactive)
  (if (not (is-mark-active))
    (princ "Nothing is selected.")

    ; grab text's extent
    (let ((m (mark)) (p (point)))
      ; do normal copy command
      (my-copy-cmd)

      ; and then delete the text from the buffer
      (delete-named-text m p)      ; defined below
    )
  )
)


; in many situations, I want my clipboard functions to operate on the
; current line of text, if nothing is explicitly selected
(defun default-selection ()
  "If no text is selected, select the current line."
  (interactive)
  (if (not (is-mark-active))
    (progn
      (beginning-of-line)        ; move cursor to line start
      (next-line 1)              ; move cursor to next line
      (turn-on-mark)             ; start selecting
      (previous-line 1)          ; move up, thereby selecting the line
  )))


; this is what Pico does in response to C-k
; actually, when the mark is active (text is selected), let's make
;   it behave as cut
(defun my-ctl-k ()
  "If no text selected, kill the entire current line, including the EOL.
   Otherwise, cut selected text."
  (interactive)
  (default-selection)
  (my-cut-cmd)             ; cut selected text
)


; copy-to-other: when working with multiple panes, I am often collecting
; text from one pane and dumping it into another.  this speeds-up that
; task, by copying the selected region to the other pane, then returning
(defun copy-to-other ()
  "Copy selected text to other pane."
  (interactive)
  (default-selection)
  (my-copy-cmd)       ; copy to clipboard
  (other-window 1)    ; move to other pane
  (my-paste-cmd)      ; paste from clipboard
  (other-window -1)   ; move to original pane
  (next-line 1)       ; move down one line
)


; a natural extension of the previous fn is one that moves text
(defun move-to-other ()
  "Move selected text to other pane."
  (interactive)
  (default-selection)
  (my-cut-cmd)        ; cut to clipboard
  (other-window +1)   ; move to other pane
  (my-paste-cmd)      ; paste from clipboard
  (other-window -1)   ; return to original pane
)


; useful for interleaving results from one window into another
(defun take-from-other ()
  "Move line at cursor in other pane, into this one."
  (interactive)
  (other-window -1)   ; other pane (-1 so undone by +1)

  (move-home)         ; workaround: if I don't do this, it only works once.. emacs bug?
  (move-end)          ; workaround: need this otherwise it only works twice!
  (move-home)         ; workaround: for good measure...

  (move-to-other)     ; take it
  (other-window +1)   ; back here
)




; -------------------- emacsclient stuff -----------------------------
; this looks like a good idea: lets the 'emacsclient' program act
; like an editor, but it just sends the buffer to emacs for editing;
; when done, we use C-x # to quit editing and let the invoker
; of 'emacsclient' proceed (see my c-x # sequence, below)
(if version-emacs-x
    (server-start))

; my C-x # sequence: tell emacsclient to exit, and close the associated emacs buffer!
(defun my-server-edit ()
  "Tell emacsclient to exit, and close buffer.  See also server-edit."
  (interactive)
  (let ((curbuf (current-buffer)))
    (server-edit)          ; tell emacsclient to exit
    (kill-buffer curbuf)   ; close buffer
  ))


; ------------------- lateral (left/right) cursor movement ----------------
(defun query-destructive-cursor-movement ()
  "Print whether we're doing destructive moves."
  (interactive)
  (if (destructive-cursor-movement)          ; defined in the policy section
    (princ "Destructive moves are ON.")
    (princ "Destructive moves are OFF.")
  ))

; right-arrow should move to the right, extending EOL if necessary
(defun right-forward-char ()
  "move the cursor to the right, extending EOL if necessary"
  (interactive)
  (move-to-column
    (+ (current-column) 1)             ; move one char right
    (destructive-cursor-movement))     ; maybe force it
)

(defun left-backward-char ()
  "move the cursor to the left, stopping at BOL"
  (interactive)
  (if (= (current-column) 0)
     t                   ; nop if at BOL
     (if (destructive-cursor-movement)
         (move-to-column                 ; forceful move
           (- (current-column) 1)          ; one char left
           t)                              ; force
         (backward-char)                 ; nonforceful
           ; if we did same thing as above, can't move left over tabs
     )
  )
)


; ----------------- shift-move to select text --------------------------
; passing a cursor-movement fn returns a fn that will turn on the region
; highlighting if it is off, and then exec the cursor-movement
(defmacro make-extend-region-fn (name func)
  "Define a function called 'name' which behaves like 'func'
  except it extends the selected region."
  `(defun ,name ()
      ,(concat "Like " (prin1-to-string func) ", but extends the region.")
      (interactive)
      (if (not (is-mark-active))
        (turn-on-mark))
      ,func
      ,(if version-xemacs               ; this 'if' is tested at macro-expand time
         '(setq zmacs-region-stays t)     ; xemacs: make the region stay selected
         t)                               ; emacs: no need to do anything special
   ))

;(princ-buffer
;  (macroexpand '(make-extend-region-fn "move-select-left" (left-backward-char))))

(make-extend-region-fn move-select-left   (left-backward-char))
(make-extend-region-fn move-select-right  (forward-char))
(make-extend-region-fn move-select-up     (previous-line 1))
(make-extend-region-fn move-select-down   (next-line 1))
(make-extend-region-fn move-select-home   (beginning-of-line))
(make-extend-region-fn move-select-end    (end-of-line))
(make-extend-region-fn move-select-pgup   (my-page-up))
(make-extend-region-fn move-select-pgdn   (my-page-down))
(make-extend-region-fn move-select-top    (beginning-of-buffer))
(make-extend-region-fn move-select-bottom (end-of-buffer))



; -------------------- page-up / page-down -------------------------
; all I want is for the cursor to remain on the same onscreen row
; across pageup/pagedown.  emacs does this already by simply setting
; the variable `scroll-preserve-screen-position', but xemacs doesn't
; have equivalent functionality.  so I will write it.

(defun get-onscreen-row ()
  "Return the 0-based onscreen row that the cursor is currently on."
  (- (current-line) (topmost-line)))

(defun set-onscreen-row (row)
  "Put the cursor on the named 0-based onscreen row."
  (goto-line (+ (topmost-line) row)))

(defun get-onscreen-col ()
  "Return 0-based onscreen column of the cursor.
  \(Has problems when the view is scrolled to the right...\)."
  (- (current-column) (leftmost-column)))

(defun set-onscreen-col (col)
  "Put the cursor on the named 0-based onscreen column."
  (move-to-column col (destructive-cursor-movement)))


(cond
  (version-emacs
    (defun my-page-up ()
      "Page-up while preserving cursor's onscreen position."
      (interactive)
      (scroll-down)     ; easy (and up/down sense is reversed in RMS's mind)
    )
    (defun my-page-down ()
      "Page-down while preserving cursor's onscreen position."
      (interactive)
      (scroll-up)
    )
  )

  ; now for the tricky part
  (version-xemacs
    (defun my-page-up ()
      "Page-up while preserving cursor's onscreen position."
      (interactive)
      (let ((row (get-onscreen-row))     ; remember where we are
            (col (get-onscreen-col)))
        (scroll-down)
        (set-onscreen-row row)           ; restore
        (set-onscreen-col col)
      ))
    (defun my-page-down ()
      "Page-down while preserving cursor's onscreen position."
      (interactive)
      (let ((row (get-onscreen-row))     ; remember where we are
            (col (get-onscreen-col)))
        (scroll-up)
        (set-onscreen-row row)           ; restore
        (set-onscreen-col col)
      ))
  ))


; --------------- same-column cursor movement --------------------
;                      (aka picture mode)
; this is part of my same-column cursor movement, because Emacs represents
; its current-cursor position as a byte offset from the beginning of the file
(defun trim-trailing-spaces ()
  "Trim trailing spaces from the current line."
  (interactive)

  (beginning-of-line)                       ; move to BOL before doing the search
  (if (re-search-forward "[ \t]*$" nil t)     ; returns true only if it matches
      ; I had originally used " +$", thinking this more efficient if sometimes
      ; it didn't match; however, when I had that, it would search beyond the
      ; current line...
    (replace-match "" nil nil))             ; replace trailing spaces with nothing
)


; this is tricky.. I want a cursor-movement function that will stay in the
; same column.  this entails two basic operations:
;   1. when I arrive on a line, if I'm in a column that is beyond that
;      line's EOL, add spaces to that line (pad it) until it is wide enough
;   2. when I leave a line, remove the extra spaces (which is a useful
;      trim function in its own right, usually)
(defmacro iso-col-cursor-move (func)
  "template for iso-column cursor-movement functions"
  `(lambda ()
    "Move the cursor while maintaining the same column #"
    (interactive)
    (if (not (destructive-cursor-movement))
      (progn ,func)                    ; don't force; just do it
      (let ((col (current-column))     ; force it; col = column we want to end up in
           )
        ; trim spaces from the end of this line
        (trim-trailing-spaces)

        ; move the cursor
        ,func

        ; pad spaces if necessary
        ; aha!  move-to-column has a 2nd arg that will do the necessary padding
        ; (but note! it inserts both tabs and spaces during padding!)
        (move-to-column col t)                  ; move to that column
      )
    )
  )
)


(defun trim-end-of-line ()
  "Trim trailing whitespace before going to EOL."
  (interactive)
  (if (not (destructive-cursor-movement))
    (end-of-line)         ; don't modify it, do it normally
    (progn
      (trim-trailing-spaces)
      (end-of-line))      ; end of trimmed line
  )
)


; passing a cursor-movement fn returns a fn that will turn off the region
; highlighting if it is on, and then exec the cursor-movement
(defmacro make-turn-off-region-fn (func)
  "template for turn-off-region functions"
  `(lambda ()
    "turn off region and move cursor"
    (interactive)
    (if (is-mark-active)
        (turn-off-mark))
    ,func))


(defun move-left () "Move one char left" (interactive)
  (funcall (make-turn-off-region-fn (left-backward-char))))
(defun move-right () "Move one char right" (interactive)
  (funcall (make-turn-off-region-fn (right-forward-char))))
(defun move-up () "Move one line up" (interactive)
  (if (= (current-line) 1)
    t    ; don't do anything.. the normal behavior moves the cursor to col 0
    (funcall (iso-col-cursor-move (funcall (make-turn-off-region-fn (previous-line 1)))))
  ))
(defun move-down () "Move one line down" (interactive)
  (funcall (iso-col-cursor-move (funcall (make-turn-off-region-fn (next-line 1))))))
(defun move-home () "Move to the start of the line" (interactive)
  (funcall (make-turn-off-region-fn (beginning-of-line))))
(defun move-end () "Move to the end of the line" (interactive)
  (funcall (make-turn-off-region-fn (trim-end-of-line))))
(defun move-pgup () "Move one page up" (interactive)
  (if (= (topmost-line) 1)
    t    ; don't do because moves cursor to end of line
    (funcall (iso-col-cursor-move (funcall (make-turn-off-region-fn (scroll-down)))))
  ))
(defun move-pgdn () "Move one page down" (interactive)
  (funcall (iso-col-cursor-move (funcall (make-turn-off-region-fn (scroll-up))))))
(defun move-top () "Move to the top of the document" (interactive)
  (funcall (make-turn-off-region-fn (beginning-of-buffer))))
(defun move-bottom () "Move to the bottom of the document" (interactive)
  (funcall (make-turn-off-region-fn (end-of-buffer))))


; ------------------ parenthesis matching -------------------
; attempts at explicit paren matching -- found one that works!

; this was in lisp/emulation/vi.el in the emacs source tree; it does
; most of what I want!
(defun vi-find-matching-paren ()
  "Locate the matching paren.  It's a hack right now."
  (interactive)
  (cond ((looking-at "[[({]") (forward-sexp 1) (backward-char 1))
        ((looking-at "[])}]") (forward-char 1) (backward-sexp 1))
        (t t)))   ; this used to (ding) on error


(defun find-innermost-delims (how-many search-func delim-set bound)
  "Search either forwards or backwards \(depending on 'search-func'\) for
  the innermost 'how-many' delimiters in 'delim-set'.  The total set
  of delimiters is hardcoded; delimiters not in 'delim-set' are taken
  as balancers, for skipping pairs.  The returned list has the innermost
  delimiter's position first.  Search stops at 'bound'.

  'search-func' is assumed to be either `re-search-backward' or
  `re-search-forward'.

  'delim-set' is a regular expression."
  ; the following comments assume we're looking for lparens
  (save-excursion        ; restore point after all this
    (let ((nest-count 0)   ; used to count nesting level of rparens
          (retval nil))    ; return value if we don't find any lparens
      (while (progn
        (if (not (funcall search-func "[(){}]\\|\\[\\|\\]" bound t))   ; sets point
          nil               ; break out of loop because no more lparens
          (if (looking-at delim-set)
            ; found lparen
            (if (eq nest-count 0)
              (progn                              ; found one
                (setq retval                        ; add to return set
                  (append retval (list (point))))
                (setq how-many (- how-many 1))      ; decrement number to find
                (if (eq how-many 0)                 ; done?
                  nil                                 ; yes: break out of loop
                  t)                                  ; no: keep going
              )
              (progn                              ; balanced by an rparen
                (setq nest-count (- nest-count 1))
                t)                                  ; stay in loop
            )

            ; found rparen
            (progn
              (setq nest-count (+ nest-count 1))
              t)                                    ; stay in loop
          )
        )
      ))

      retval     ; return whatever was set in loop
    )))


;(setq debug-on-signal nil)

;(re-search-backward "a" (point-max) t)


; -------- crucial one -------
;(add-hook 'post-command-hook 'mark-innermost-lparen)
; -------- crucial one -------

;(princ post-command-hook)


(defun find-innermost-lparens (howmany)
  "Search backwards from point for up to the first 'how-many' left-parens \(or
  other paired delimiter\) that isn't balanced by something else.
  Returns a list of positions, where the first one is the innermost."
  (find-innermost-delims howmany 're-search-backward "[[({]"
    (max
      (- (window-start) 500)     ; go a ways backward to work around an emacs bug..?
      (point-min)                ; but not past beginning of buffer
    )))

(defun find-innermost-lparen ()
  "Return position of single innermost lparen, or nil if none."
  (let ((list (find-innermost-lparens 1)))
    (if (not list)
      nil
      (car list)
    )))



(global-set-key [f11] '(lambda () (interactive)
  (mark-innermost-lparen)
;  (princ (find-innermost-delims 5 're-search-backward "[[({]"))
;  (princ (find-innermost-lparens 5))
;  (let ((list (find-innermost-lparens 5)))
;    (if list
;      (goto-char (nth 0 list))
;      (princ "No innermost lparen.")
;    )
;  )
))


(defun mark-innermost-lparen ()
  "Set the `paren-ovl' marker on the innermost lparen."
  (interactive)
  (let ((innermost (find-innermost-lparen)))
    (if innermost
      (mark-paren-character innermost)
      (hide-paren-ovl)
    )))


; we will have one overlay per buffer to mark the paren of interest
(set-default 'paren-ovl nil)      ; means it hasn't been created for a given buffer
(make-variable-buffer-local 'paren-ovl)

(defun make-paren-ovl ()
  "make `paren-ovl' if it doesn't exist"
  (if (not paren-ovl)
    (progn
      (setq paren-ovl (make-overlay (point-min) (point-min)))    ; somewhere out of the way
      (overlay-put paren-ovl 'face 'font-lock-paren-ovl-face)    ; assign the key property now
    )))

(defun mark-paren-character (char)
  "Put the `paren-ovl' marker on 'char'."
  (make-paren-ovl)
  (move-overlay paren-ovl char (1+ char))
)

(defun hide-paren-ovl ()
  "Make the `paren-ovl' go away temporarily."
  (make-paren-ovl)
  (move-overlay paren-ovl 1 1)
)




; -- experiments with overlays --
;(setq ovl (make-overlay 5 100))
;(overlay-put ovl 'face 'font-lock-constant-face)
;(overlay-put ovl 'face nil)
;(delete-overlay ovl)
;(setq ovls (overlays-in (point-min) (point-max)))
;(delete-overlay (car ovls))
;(setq paren-ovl nil)
;(mark-paren-character 33969)
;(hide-paren-ovl)
(princ paren-ovl)
;(overlay-properties paren-ovl)
;(overlay-put paren-ovl 'face 'font-lock-constant-face)
;(overlay-put paren-ovl 'face 'font-lock-paren-ovl-face)
;(overlay-put paren-ovl 'face nil)

; -- experiments with extents --
;(setq my-ext (make-extent 33214 33215))
;(set-extent-face my-ext 'font-lock-keyword-face)
;(set-extent-priority my-ext 1)
;(princ my-ext)


;(global-set-key [f12] '(lambda () (interactive)
;  (goto-char (find-innermost-lparen))
;  (princ (point))
;))


;(goto-char (scan-sexpsf(point) -1))
;(point)

; -- experiments with hooks --
;(setq toy-hook-calls 0)
;(defun toy-hook ()
;  (setq toy-hook-calls (+ 1 toy-hook-calls))
;  (princ (format "toy-hook call %d" toy-hook-calls))
;)
;(add-hook 'post-command-hook 'toy-hook)
;(remove-hook 'post-command-hook 'toy-hook)
;(princ post-command-hook)
;(princ (format "a %d" 5))


; ---------------- matching word pairs ------------------
; The idea here is that while emacs has built-in support for matching
; things like parentheses, I work with a variety of syntaxes that use
; balanced keyword pairs, such as "begin" and "end", or "#if" and
; "#endif".  So this mechanism searches for the balanced element
; of such ad-hoc constructions.
;
; TODO: Currently, there is no support for skipping things that are
; in string literals, comments, etc.  I think that would be possible
; just by having appropriate regexs for them and skipping them when
; they occur, but I haven't tried yet.
(defun find-matching-element (search-func offset open-regex close-regex)
  "Search forwards or backwards (depending on `search-func') to find
   the matching pair identified by `open-regex' and `close-regex'."
  (let ((nesting 1)                ; number of pairs we are inside
        (orig-point (point))       ; original cursor loc
        (orig-case-fold-search case-fold-search))
    (setq case-fold-search nil)        ; case-sensitive search
    (goto-char (+ (point) offset))     ; skip the `open-regex' at cursor
    (while (and (> nesting 0)
                (funcall search-func
                  (concat "\\(" open-regex "\\)\\|\\(" close-regex "\\)") nil t))
      (if (string-match open-regex (match-string 0))
        (setq nesting (+ nesting 1))
        (setq nesting (- nesting 1))
      ))
    (setq case-fold-search orig-case-fold-search)
    (if (eq nesting 0)
      ; found the matching word, move cursor to the beginning of the match
      (goto-char (match-beginning 0))
      ; did not find the matching word, report the nesting depth at EOF
      (progn
        (goto-char orig-point)
        (error (format "Did not find match; nesting at file end is %d" nesting))
      )
    )))

; This is what I bind to Alt-[ and Alt-].
(defun find-matching-keyword ()
  "Find the matching keyword of a balanced pair."
  (interactive)
  (cond
    ; these first two come from lisp/emulation/vi.el
    ((looking-at "[[({]") (forward-sexp 1) (backward-char 1))
    ((looking-at "[])}]") (forward-char 1) (backward-sexp 1))

    ; TODO: Should the set of pairs be sensitive to the mode of
    ; the current file?

    ; Kettle CVC
    ((looking-at "ASSERT")
     (find-matching-element 're-search-forward 6 "ASSERT" "RETRACT"))
    ((looking-at "RETRACT")
     (find-matching-element 're-search-backward 0 "RETRACT" "ASSERT"))
                
    ; Kettle CVC
    ;
    ; "\\b": word boundary assertion, needed because one delimiter is
    ; a substring of the other
    ((looking-at "BLOCK")
     (find-matching-element 're-search-forward 5 "\\bBLOCK\\b" "ENDBLOCK"))
    ((looking-at "ENDBLOCK")
     (find-matching-element 're-search-backward 0 "ENDBLOCK" "\\bBLOCK\\b"))

    ; Simplify
    ((looking-at "BG_PUSH")
     (find-matching-element 're-search-forward 7 "BG_PUSH" "BG_POP"))
    ((looking-at "BG_POP")
     (find-matching-element 're-search-backward 0 "BG_POP" "BG_PUSH"))

    ; C/C++
    ((looking-at "#if")
     (find-matching-element 're-search-forward 3 "#if" "#endif"))
    ((looking-at "#endif")
     (find-matching-element 're-search-backward 0 "#endif" "#if"))

    ; ML
    ;
    ; this does not quite work because e.g. "struct" is also terminated
    ; with "end" ..
    ((looking-at "begin")
     (find-matching-element 're-search-forward 5 "\\bbegin\\b" "\\bend\\b"))
    ((looking-at "end")
     (find-matching-element 're-search-backward 0 "\\bend\\b" "\\bbegin\\b"))

    ;(t (error "Cursor is not on ASSERT nor RETRACT"))
    (t t)
  ))


; ------------------- inferior shell stuff ---------------------
; fix for ^M in shell
;(add-hook 'comint-output-filter-functions
;          'shell-strip-ctrl-m nil t)
;(remove-hook 'comint-output-filter-functions
;             'shell-strip-ctrl-m)

;(setq comint-process-echoes nil)

; fix for echoing commands in shell
;(defun my-comint-init ()
;  (setq comint-process-echoes t))
;(add-hook 'comint-mode-hook 'my-comint-init)

; trying to get sml to work in shell process
(defun my-sml-mode ()
  "My SML mode"
  (interactive)
  (setq comint-prompt-regexp "^[\-=] ")
    ; this matches "- " and "= "
)

; will this take precedence over the defn in shell.el?
(defvar shell-font-lock-keywords
  '((eval . (cons shell-prompt-pattern 'font-lock-shell-prompt-face))
    ("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-shell-cmd-args-face)
;    ("^[^ \t\n]+:.*" . font-lock-string-face)
;    ("^\\[[1-9][0-9]*\\]" . font-lock-string-face)
   )
  "Additional expressions to highlight in Shell mode."
)


; ------------------ indentation -------------------------
; alt-left and alt-right: indent/outdent
(defun my-indent-fn (cols)
  (cond               ; I tried hard to write a portable version but..
    (version-emacs
      ; since this fn modifies the contents of the buffer,
      ; transient-mark-mode turns off the mark afterward,
      ; and there appears to be nothing I can do about it;
      ; so I must turn it off first, modify the buffer, then
      ; turn it on.  yay Gnu.
      (transient-mark-mode nil)
      (let ((m (mark))
            (p (point)))
         (if (< m p)
           (indent-rigidly m p cols)   ; do the real in/outdent
           (indent-rigidly p m cols)
         )
         ; this doesn't work...
         ;(let ((new-point (point)))
         ;  (goto-char m)               ; return to previous mark
         ;  (turn-on-mark)              ; start selecting
         ;  (goto-char new-point)       ; back to where we ended up after in/outdent
         ;)
      )
      (transient-mark-mode t)
    )

    (version-xemacs
      (default-selection)               ; nice shortcut, wish it worked above
      (interactive)
      (let ((m (mark))
            (p (point)))
         (if (< m p)
           (indent-rigidly m p cols)    ; do the real in/outdent
           (indent-rigidly p m cols)
         )
      )
      (keep-region-selected)
    )
  ))


; by the way, if I haven't said it already: GNU SUCKS!
; the only way the indent functions don't invisibilize the selected
; text is if the handler fn causes an error.  that's right, the fn must
; cause an error for it to work right.  what the hell.  's not worth it.

(defun my-indent ()
  "indent region by 2 columns"
  (interactive)
  (my-indent-fn 2))
(defun my-outdent ()
  "outdent region by 2 columns"
  (interactive)
  (my-indent-fn -2))


; ------------------ proper delete behavior --------------------
(defun delete-named-text (start end)
  "Delete text named by start and end positions; will automatically
  detect if start>end, and reverse them appropriately."
  (if (> start end)
    (delete-region end start)
    (delete-region start end)
  ))

(defun delete-text ()
  "Delete the selected text -- does not put it into the clipboard."
  (interactive)
  (delete-named-text (mark) (point))
)

; step 1 of making del do the right thing
; (step 2 is working on selected regions)
(defun my-delete-func ()
  "my Delete handler: delete a selected region, or normal ^D action"
  (interactive)
  (if (is-mark-active)
    (delete-text)
    (delete-char 1))           ; else
)


; ------------------ syntax highlighting ------------------------
; I want my definitions to override the defaults, and so that
; means loading the defaults now so mine follow them
(require 'font-lock)


; name the modes where highlighting is acceptable (since xemacs often
; highlights things it shouldn't and the colors make the text unreadable)
;(setq font-lock-global-modes
;  '(emacs-lisp-mode c++-mode LaTeX-mode texinfo-mode perl-mode
;    sh-mode makefile-mode))


; this is the default value for new faces I create; I don't understand
; what the below does, exactly, and I am going to use M-x customize
; to change it anyway
;(setq default-new-face-desc
;  '((((class color) (background light)) (:foreground "Red" :bold t))
;    (((class color) (background dark)) (:foreground "Pink" :bold t))
;    (t (:inverse-video t :bold t))))
;


; experimenting with macros that define symbols
(defmacro set-some-symbol (name)
  "experiment"
  `(progn
    (setq ,(intern (concat "prefix-" name "-suffix")) 4)
  )
)
;(set-some-symbol "four")     ; works
;(princ prefix-four-suffix)   ; works


;; ; define a face; this macro is a bit hairy because it has to construct
;; ; new symbols (done with `intern')
;; (defmacro define-font-lock-face (name)
;;   "for defining a new syntax highlighting font face"
;;   `(progn
;;     (defvar ,(intern (concat "font-lock-" name "-face"))
;;       (quote ,(intern (concat "font-lock-" name "-face")))
;;       (concat "Face name to use for " ,name ".")
;;     )
;;     (defface ,(intern (concat "font-lock-" name "-face"))
;;       default-new-face-desc
;;       (concat "Font Lock mode face for " ,name ".")
;;       :group 'font-lock-highlighting-faces
;;     )
;;   ))

;; ; this is what (define-font-lock-face "octal") expands to:
;; ;(defvar font-lock-octal-face 'font-lock-octal-face
;; ;  "Face name to use for octal.")
;; ;(defface font-lock-octal-face default-new-face-desc
;; ;  "Font Lock mode face for octal."
;; ;  :group 'font-lock-highlighting-faces)
;; 
;; ; list of faces for my highlighting code; defining them here lets me
;; ; refer to them below, and also causes them to appear in M-x customize
;; ; (can use M-x customize-face to get there more-or-less directly)
;; (define-font-lock-face "preprocessor")  ; #include, etc.
;; (define-font-lock-face "comment")       ; C or C++ comments
;; (define-font-lock-face "keyword")       ; language keywords
;; (define-font-lock-face "constant")      ; literals in the code
;; (define-font-lock-face "variable")      ; words that are not keywords
;; (define-font-lock-face "octal")         ; octal literals are a language gotcha
;; (define-font-lock-face "operator")      ; symbols like '+'
;; (define-font-lock-face "tabs")          ; for highlighting tabs in files
;; (define-font-lock-face "paren-ovl")
;; (define-font-lock-face "shell-prompt")
;; (define-font-lock-face "todo")          ; for TODO where it appears
;; (define-font-lock-face "trailing-ws")   ; ws at end of line
;; 
;; 
;; ; C++ highlighting
;; (setq c++-keywords
;;   ; regexp-opt takes a list of alternatives and builds a regular
;;   ; expression to match any one of them; the regexp it returns is
;;   ; supposedly more efficient than the trivial one you would write
;;   ; as "alt1|alt2|alt3|..."
;;   (regexp-opt
;;    '(
;;       ; C keywords
;;       "asm" "auto" "break" "case" "cdecl" "char" "const"
;;       "continue" "default" "delete" "do" "double" "else" "enum"
;;       "extern" "float" "for" "goto" "if" "int" "long" "pascal"
;;       "register" "return" "short" "signed" "sizeof" "static"
;;       "struct" "switch" "typedef" "union" "unsigned" "void"
;;       "volatile" "wchar_t" "while"
;; 
;;       ; C++ keywords
;;       "bool" "catch" "class" "complex" "const_cast"
;;       "dynamic_cast" "explicit" "export" "friend" "inline"
;;       "mutable" "namespace" "new" "operator" "private"
;;       "protected" "public" "reinterpret_cast" "static_cast"
;;       "template" "this" "throw" "try" "typename" "using"
;;       "virtual" "typeid"
;; 
;;       ; some more that I like
;;       "EXPLICIT" "MUTABLE" "STATICDEF"
;;       "atom" "byte" "point" "rect" "string" "owner" "serf" "nullable" "rostring"
;;       "override"
;; 
;;       ; for linux kernel code
;;       "Err" "size_t" "ssize_t" "loff_t"
;; 
;;    ) t))
;; 
;; (setq c++-operators
;;   (regexp-opt
;;     '(
;;       "+" "-" "*" "/" "%" "^" "&" "|" "~" "!" "=" "<" ">" "+=" "-="
;;       "*=" "/=" "%=" "^=" "&=" "|=" "<<" ">>" ">>=" "<<=" "==" "!="
;;       "<=" ">=" "&&" "||" "++" "--" "->*" "," "->" "[" "]"
;;       "(" ")" ".*" "." "{" "}" ";" ":" "?"
;;     )))
;; 
;; ; things I want handled the same way in C++ and Java modes
;; (setq c++-and-java-font-lock-keywords (list
;;   ;; identifiers
;;   ;'("\\(^[_a-zA-Z][_a-zA-Z0-9]*\\)" 1 font-lock-variable-name-face)
;;   ;'("[^0-9a-zA-Z]\\([_a-zA-Z][_a-zA-Z0-9]*\\)" 1 font-lock-variable-name-face)
;;     ;     ^^^^^^ this is to prevent it from seeing the "abc" in "0xabc"
;;     ;            as an identifier
;;   '("\\<\\([_a-zA-Z][_a-zA-Z0-9]*\\)\\>" 1 font-lock-variable-name-face)
;;     ; \\< and \\> are word delimiters.. not sure what criteria is used to decide what
;;     ; is or is not a word..
;; 
;;   ; octal literals
;;   ;'("\\(0[0-7]*[89a-wyzA-WYZ]+\\)" 1 font-lock-warning-face)   ; illegal octals
;;    ; illegal octals are nice, but difficult given the way this thing
;;    ; matches regexps.. what we need is the usual maximal-match rules,
;;    ; but those don't seem to be implemented here (rather, it does maximal
;;    ; match only within a single regexp, and first-match between regexps)
;;   '("[^0-9a-zA-Z.]\\(0[0-7]+\\)" 1 font-lock-octal-face)            ; legal octals
;;   '("\\(^0[0-7]+\\)" 1 font-lock-octal-face)                        ; legal octals
;; 
;;   ; hexadecimal literals
;;   ; (must preceed float/int because otherwise the leading "0" is treated
;;   ; as an int, and the rest fails to match)
;;   '("\\(0x[0-9a-fA-F]+\\)" 1 font-lock-constant-face)
;; 
;;   ; floating-point (also includes decimal literals now)
;;   ;'("\\([0-9]*\\.[0-9]+\\)" 1 font-lock-constant-face)
;;    ; .4 or 3.4
;;   ;'("\\([0-9]+e\\(\\+\\|-\\)?[0-9]+\\)" 1 font-lock-constant-face)
;;    ; 3e4 or 3e+4 oe 3e-4
;;   ;'("\\([0-9]*\\.[0-9]+e\\(\\+\\|-\\)?[0-9]+\\)" 1 font-lock-constant-face)
;;    ; .4e5 or .4e+5 or .4e-5 or 3.4e5 or 3.4e+5 or 3.4e-5
;;   '("\\(\\([0-9]*\\.\\)?[0-9]+\\(e\\(\\+\\|-\\)?[0-9]+\\)?\\)"
;;     1 font-lock-constant-face)
;;     ; all of them?
;; 
;;   ; decimal integers
;;   ; (I folded them into the above regexp)
;;   ;'("\\([0-9]+\\)" 1 font-lock-constant-face)
;; ))
;; 
;; (setq c++-font-lock-keywords-1 (append
;;   (list
;;     ; preprocessor directives (only catches the first line)
;;     '("^\\([ \t]*#.*\\)" 1 font-lock-preprocessor-face t)
;;       ; 4th arg 't' means override existing fontification!
;; 
;;     ;'("^\\([ \t]*#.*\\(\\\\\n.*\\)*\\)" 1 font-lock-preprocessor-face)
;;       ; this 2nd line is my attempt to get it to recognize multiline macros
;;       ; and highlight them entirely as preprocessor (doesn't work..)
;; 
;;     ; ok, now I want comments formatted in includes
;;     ; first, C++ style:
;;     '("^[ \t]*#.*\\(//.*\\)" 1 font-lock-comment-face t)
;; 
;;     ; now, C style; it's complicated because:
;;     ;  - I want to highlight comments while they're being written, before
;;     ;    the final "*/" is seen
;;     ;  - I want to highlight the first line of multi-line comments; the
;;     ;    syntactic-highlighting engine takes care of all the lines beyond
;;     ;    the first, but the 't' override in font-lock-preprocessor-face
;;     ;    (above) undoes the highlighting of the first line
;;     ;  - I want to *not* highlight (as comment) text after the "*/"
;;     ; this regexp is responsible for many "stack overflow in regexp matcher"
;;     ; messages, but I've tried to fix it a few times and can't seem to get
;;     ; it right .... I hate emacs' use of regexps.. a DFA would be so much
;;     ; simpler to build and predict..
;;     (list (concat
;;              "\\("        ; group everything together
;;                "/\\*"       ; first, "/*"
;;                "\\("        ; then
;;                  "[^*]\\|"    ; things other than "*", or
;;                  "\\**[^*/]"  ; any # of "*"s followed by something other than "/" or "*"
;;                "\\)*"         ; 0 or more times
;;                "\\("        ; then
;;                  "\\*$\\|"    ; "*" at the end of the line, or
;;                  "\\*/\\|"    ; "*/", or
;;                  "$"          ; end of line
;;                "\\)"          ; once
;;              "\\)"
;;           )
;;       1 'font-lock-comment-face t)
;; 
;;     ; older attempts at C-style comments in preprocessor
;;     ;'("\\(/\\*.*\\*/\\)" 1 font-lock-comment-face t)
;;       ; the problem with this one is it doesn't shade until I finish the '*/'
;;     ;'("^[ \t]*#.*\\(/\\*.*\\)" 1 font-lock-comment-face t)
;;       ; so I catch the first line of multi-line comments.. assumes comments
;;       ; on preproc lines don't have non-comment chars after them on same line..
;; 
;;     ; keywords, from above
;;     (list (concat "\\(\\<" c++-keywords "\\>\\)") 1 'font-lock-keyword-face)
;; 
;;     ; word-ish literals
;;     ;   false/true: C++ language
;;     ;   FALSE/TRUE: MS-win booleans
;;     ;   False/True: X-win booleans
;;     ;   NULL: stdio.h macro for 0
;;     '("\\<\\(false\\|true\\|FALSE\\|TRUE\\|False\\|True\\|NULL\\)\\>"
;;       1 font-lock-constant-face)
;; 
;;     ; TODO (don't know how to format text inside comments..)
;;     ;'("\\<\\(TODO\\)\\>" 1 font-lock-todo-face)
;;   )
;; 
;;   ; now pull in common stuff; must be before operators section unless I'm
;;   ; willing to turn on the 'override' flag of floats
;;   c++-and-java-font-lock-keywords
;; 
;;   (list
;;     ;; ------ pieces of original code ------
;;     ;; Fontify error directives.
;;     ;'("^#[ \t]*error[ \t]+\\(.+\\)" 1 font-lock-warning-face prepend)
;;     ;;
;;     ;; Fontify filenames in #include <...> preprocessor directives as strings.
;;     ;'("^#[ \t]*\\(import\\|include\\)[ \t]*\\(<[^>\"\n]*>?\\)"
;;     ;  2 font-lock-string-face)
;;     ;;
;;     ;; Fontify function macro names.
;;     ;'("^#[ \t]*define[ \t]+\\(\\sw+\\)(" 1 font-lock-function-name-face)
;;     ;;
;;     ;; Fontify symbol names in #elif or #if ... defined preprocessor directives.
;;     ;'("^#[ \t]*\\(elif\\|if\\)\\>"
;;     ;  ("\\<\\(defined\\)\\>[ \t]*(?\\(\\sw+\\)?" nil nil
;;     ;   (1 font-lock-builtin-face) (2 font-lock-variable-name-face nil t)))
;;     ;;
;;     ;; Fontify otherwise as symbol names, and the preprocessor directive names.
;;     ;'("^#[ \t]*\\(\\sw+\\)\\>[ \t!]*\\(\\sw+\\)?"
;;     ;  (1 font-lock-builtin-face) (2 font-lock-variable-name-face nil t))
;;     ;; ------- end kept pieces ------
;; 
;;     ; operators, from above
;;     (list (concat "\\(" c++-operators "\\)") 1 'font-lock-operator-face)
;;   )
;; ))
;; 
;; ; make all levels of highlighting the same
;; (setq c++-font-lock-keywords c++-font-lock-keywords-1)
;; (setq c++-font-lock-keywords-2 c++-font-lock-keywords)
;; (setq c++-font-lock-keywords-3 c++-font-lock-keywords)
;; 
;; 
;; ; Java highlighting
;; (setq java-keywords
;;   (regexp-opt '(
;; 
;;     "abstract" "boolean" "break" "byte" "case" "catch" "char"
;;     "class" "continue" "default" "do" "double" "else" "extends"
;;     "final" "finally" "float" "for" "if" "implements" "import"
;;     "instanceof" "int" "interface" "long" "native" "new"
;;     "package" "private" "protected" "public" "return" "short"
;;     "static" "super" "switch" "synchronized" "this" "throw"
;;     "throws" "transient" "try" "void" "volatile" "while"
;; 
;;   ) t))
;; 
;; (setq java-font-lock-keywords-1 (append
;;   (list
;;     ; keywords
;;     (list (concat "\\<" java-keywords "\\>") 1 'font-lock-keyword-face)
;; 
;;     ; Fontify class names.
;;     ;'("\\<\\(class\\)\\>[ \t]*\\(\\sw+\\)?"
;;     ;  (1 font-lock-type-face) (2 font-lock-function-name-face nil t))
;; 
;;     ; Fontify package names in import directives.
;;     '("\\<\\(import\\|package\\)\\>[ \t]*\\(.*\\)?;"
;;      (1 font-lock-keyword-face) (2 font-lock-preprocessor-face nil t))
;; 
;;     ; word-like literals
;;     '("\\<\\(false\\|null\\|true\\)\\>" . font-lock-constant-face)
;; 
;;     ; Javadoc tags within comments.
;;     ;'("@\\(author\\|exception\\|return\\|see\\|version\\)\\>"
;;     ;  (1 font-lock-constant-face prepend))
;;     ;'("@\\(param\\)\\>[ \t]*\\(\\sw+\\)?"
;;     ;  (1 font-lock-constant-face prepend)
;;     ;  (2 font-lock-variable-name-face prepend t))
;;   )
;; 
;;   c++-and-java-font-lock-keywords
;; 
;;   (list
;;     ; operators, from above; use C++ operators since there isn't sufficient
;;     ; difference between them to justify another list
;;     (list (concat "\\(" c++-operators "\\)") 1 'font-lock-operator-face)
;;   )
;; ))
;; 
;; ; all levels the same
;; (setq java-font-lock-keywords java-font-lock-keywords-1)
;; (setq java-font-lock-keywords-2 java-font-lock-keywords)
;; (setq java-font-lock-keywords-3 java-font-lock-keywords)
;; 
;; 
;; ; my grammar file format
;; (setq gr-keywords
;;   (regexp-opt '(
;;     "terminals" "nonterm" "formGroup" "form" "attr"
;;     "action" "condition" "include" "empty"
;;   ) t))
;; 
;; (setq gr-font-lock-keywords (append
;;   (list
;;     ; keywords, from above
;;     (list (concat "\\(\\<" gr-keywords "\\>\\)") 1 'font-lock-keyword-face)
;;   )
;; 
;;   ; now pull in common stuff; must be before operators section unless I'm
;;   ; willing to turn on the 'override' flag of floats
;;   c++-and-java-font-lock-keywords
;; 
;;   (list
;;     ; operators, from above
;;     (list (concat "\\(" c++-operators "\\)") 1 'font-lock-operator-face)
;;   )
;; ))

; doesn't work...
(defun use-gr-keywords ()
  "Switch to my grammar file format keywords."
  (interactive)
  (setq font-lock-keywords gr-keywords))


; modified version of dired highlighting (from dired.el)
(defun my-dired-mode-hook ()
  "Tweaks to dired mode."
  (interactive)
  (setq dired-font-lock-keywords
    (list
     ;;
     ;; Directory headers.
     (list dired-subdir-regexp '(1 font-lock-type-face))
     ;;
     ;; We make heavy use of MATCH-ANCHORED, since the regexps don't identify the
     ;; file name itself.  We search for Dired defined regexps, and then use the
     ;; Dired defined function `dired-move-to-filename' before searching for the
     ;; simple regexp ".+".  It is that regexp which matches the file name.
     ;;
     ;; Dired marks.
     (list dired-re-mark
           '(0 font-lock-constant-face)
           '(".+" (dired-move-to-filename) nil (0 font-lock-warning-face)))
     ;; People who are paranoid about security would consider this more
     ;; important than other things such as whether it is a directory.
     ;; But we don't want to encourage paranoia, so our default
     ;; should be what's most useful for non-paranoids. -- rms.
  ;;;   ;;
  ;;;   ;; Files that are group or world writable.
  ;;;   (list (concat dired-re-maybe-mark dired-re-inode-size
  ;;;            "\\([-d]\\(....w....\\|.......w.\\)\\)")
  ;;;    '(1 font-lock-comment-face)
  ;;;    '(".+" (dired-move-to-filename) nil (0 font-lock-comment-face)))
     ;;
     ;; Subdirectories.
     (list dired-re-dir
           '(".+" (dired-move-to-filename) nil (0 font-lock-keyword-face)))
     ;;
     ;; Symbolic links.
     (list dired-re-sym
           '(".+" (dired-move-to-filename) nil (0 font-lock-function-name-face)))
     ;;
     ;; Files suffixed with `completion-ignored-extensions'.
     '(eval .
       (let ((extensions (mapcar 'regexp-quote completion-ignored-extensions)))
         ;; It is quicker to first find just an extension, then go back to the
         ;; start of that file name.  So we do this complex MATCH-ANCHORED form.
         (list (concat "\\(" (mapconcat 'identity extensions "\\|") "\\|#\\)$")
               '(".+" (dired-move-to-filename) nil (0 font-lock-string-face))))))
  )
)


; --------------------- file-fixing utilities ------------------
; macro to strip ^Ms from a file
(defun remove-ctlm ()
  "Remove ^M (control-M, decimal 13) characters from the file"
  (interactive)
  (beginning-of-buffer)       ; start search from the top
  (while (re-search-forward "\015" nil t)
    (replace-match "" nil nil)
  )
  (beginning-of-buffer)       ; finish at top
)

(defun rmctlm ()
  "Alias for `remove-ctlm'"
  (interactive)
  (remove-ctlm))


; macro to strip char-^Hs from a file (these show up in nroff
; output (like man pages) as formatting commands, that I want
; to remove)
(defun remove-ctlh ()
  "Remove char-^Hs characters from the file"
  (interactive)
  (beginning-of-buffer)       ; start search from the top
  (while (re-search-forward ".\010" nil t)
    (replace-match "" nil nil)
  )
  (beginning-of-buffer)       ; finish at top
)

(defun rmctlh ()
  "Alias for `remove-ctlh'"
  (interactive)
  (remove-ctlh))


; ------------------- navigating #line directives ----------------
; search backwards in a file for the nearest #line directive that
; mentions the filename
(defun find-enclosing-hashline ()
  "Search backwards for #line directive with filename"
  (interactive)
  (re-search-backward "^/?/?#\\(line\\)? [0-9]+ ")
)

(defun find-enclosing-hashline-any ()
  "Search backwards for any #line directive"
  (interactive)
  (re-search-backward "^/?/?#\\(line\\)? [0-9]+")
)

; I don't know why I wrote this.. there's a built-in function by
; the same name with the same semantics...
;(defun match-string (n)
;  "Return string matched by most recent search; 'n' is parethesis
;   grouping number, where 0 means entire search string."
;  (buffer-substring (match-beginning n) (match-end n))
;)

(defun current-hashline-line ()
  "Extract and return line number of #line directive at cursor."
  (re-search-forward "#\\(line\\)? \\([0-9]+\\)")
  (string-to-number (match-string 2))
)

(defun current-hashline-file ()
  "Extract and return file name of #line directive at cursor."
  (re-search-forward "#\\(line\\)? [0-9]+ \"\\(.*\\)\"")
  (match-string 2)
)

(defun map-hashline-back ()
  "Given a #line directive at cursor, open file it refers to, at
   the line position given."
  (interactive)
  (let ((curPosn (point))        ; save current cursor position
        (origLine (current-line)))
    (find-enclosing-hashline-any)
    (let ((lineNumber (+ (current-hashline-line)       ; value in #line directive
                         (- origLine (current-line))   ; plus # lines we had to go back
                         (- 0 1))                      ; minus one because #line refers to next line
          ))
      (goto-char curPosn)         ; restore original buffer's position
      (find-enclosing-hashline)
      (let ((fileName (current-hashline-file)))
        (goto-char curPosn)       ; restore original buffer's position

        ; list of prefixes to try; the ones other than ".." are to support
        ; some unorthodox moving of files that happens during the CCured
        ; for C++ build process
        (setq prefixes '("" "../" "../../ace/"))
        (setq foundIt nil)
        (while prefixes
          (if (file-exists-p (concat (car prefixes) fileName))
              (progn              ; file exists, open it
                (find-file (concat (car prefixes) fileName))
                (setq foundIt t)
                (setq prefixes nil))    ; stop iterating
              (progn              ; file doesn't exist, try next prefix
                (setq prefixes (cdr prefixes)))
          ))
        (if (not foundIt)
            (error "File does not exist: %s" fileName))
        (goto-line lineNumber)    ; go to right line

        ; print where we went
        (princ fileName)
        (princ ":")
        (princ lineNumber)
      )
    )
  )
)

; this doesn't work as advertised, but it's close enough
(defun run-previous-M-x-command ()
  "Run the last command interactively entered with M-x."
  (interactive)
  (let ((prevCmd (car command-history)))
    (princ prevCmd)
    (eval prevCmd)
  )
)




; ------------------ navigating CCured *infer.c files ---------------
(defun get-wild-source ()
  "Search forward to the next WILD/<reason>(<id>) string, and
   extract and return that node id."
  (re-search-forward "WILD/[A-Za-z_]+(\\([0-9]+\\))")
  (string-to-number (match-string 1))
)

(defun goto-infer-node (n)
  "Find the information section for node 'n'."
  (goto-char 0)
  (re-search-forward (format "^%d :" n))
)

(defun goto-prev-wild ()
  "Go to the node which caused this one to be WILD."
  (interactive)
  (let ((source (get-wild-source)))
    (princ (format "Node %d" source))
    (goto-infer-node source)
  )
)


; ------------------- simplifying input files -------------------
(defun get-sym-at-cursor ()
  "Return the symbol name starting at the cursor"
  (save-excursion
    (re-search-forward "[_A-Za-z]\\([_A-Za-z0-9]\\)*")
    (match-string 0)
  ))

(defun replace-sym-at-cursor (replacement)
  "Replace symbol at cursor with argument, throughout buffer to end"
  (interactive "sReplacement: ")
  (save-excursion
    (let ((orig (get-sym-at-cursor)))
      (while (re-search-forward orig nil t)
        (replace-match replacement nil nil))
    )))


; ------------------------- HTML stuff -------------------
(defun modify-name-or-sel (func)
  "Change the name under the cursor, or selection (if active),
in the way specified by func."
  ; can't quite use `get-sel-or-word' b/c I need to *replace* the word...
  (if (is-mark-active)              ; selection -> href
    (let ((sel (buffer-substring (point) (mark))))
      (delete-region (point) (mark))
      (insert (funcall func sel))
    )
    (progn                          ; cursor word -> href
      (re-search-backward "[ \n]")      ; go to the place before the name
      (re-search-forward "[^ \n]+")
      (let ((name (match-string 0)))
        (replace-match (funcall func name)))
    ))
)

(defun make-name-into-href ()
  "Change the URL or filename under the cursor into an HREF; or,
if something is selected, change that."
  (interactive)
  (modify-name-or-sel
    #'(lambda (text) (concat "<a href=\"" text "\">" text "</a>")))
)

(defun surround-with-tt ()
  "Surround the word under the cursor with <tt> and </tt>"
  (interactive)
  (modify-name-or-sel
    #'(lambda (name) (concat "<tt>" name "</tt>")))
)

(defun insert-html-toc ()
  "Run insert-html-toc on this file."
  (interactive)
  (call-program-then-revert (list "insert-html-toc"))
)

; copied and modified from sgml-mode.el; the use of 'defvar' in
; both places means this one (which is the first one emacs will
; see) is the one that remains in effect
(defvar html-tag-face-alist
  '(("b" . bold)
    ("big" . bold)
    ("blink" . highlight)
    ("cite" . italic)
    ("em" . italic)
    ("h1" . font-lock-constant-face)       ; red
    ("h2" . font-lock-keyword-face)        ; green
    ("h3" . font-lock-function-name-face)  ; italics
    ("h4" . font-lock-variable-name-face)  ; regular
    ("h5" . font-lock-comment-face)
    ("h6" . font-lock-constant-face)
    ("i" . italic)
    ("rev"  . modeline)
    ("s" . underline)
    ("small" . default)
    ("strong" . bold)
    ("title" bold underline)
    ("tt" . default)
    ("u" . underline)
    ("var" . italic))
  "Value of `sgml-tag-face-alist' for HTML mode.")


; ------------- c++ mode tweaks ------------------------
(defun turn-off-electric-keys ()
  "Tweaks the various C modes after entering the mode.  Mainly
   used to turn *off* the c-electric-* functions, because I really
   dislike them."
  (interactive)
  (local-unset-key ">")
  (local-unset-key "<")
  (local-unset-key "/")
  (local-unset-key "*")
  (local-unset-key ",")
  (local-unset-key [delete])
  (local-unset-key [DEL])    ; backspace
  (local-unset-key [tab])
  (local-unset-key [esc])
  (local-unset-key ":")
  (local-unset-key "#")
  (local-unset-key ";")
  (local-unset-key "{")
  (local-unset-key "}")
  (local-unset-key "(")
  (local-unset-key ")")
  (local-unset-key [(control c)])
  (local-unset-key [(control i)])
  (local-unset-key [(meta a)])

  (if version-xemacs (progn
    ; in xemacs, local-unset-key doesn't appear to function at all, so
    ; I will instead directly set the local bindings
    (local-set-key ">" 'self-insert-command)
    (local-set-key "<" 'self-insert-command)
    (local-set-key "/" 'self-insert-command)
    (local-set-key "*" 'self-insert-command)
    (local-set-key "," 'self-insert-command)
    (local-set-key [delete] 'my-delete-func)
    (local-set-key [tab] 'my-insert-tab)
    ;(local-set-key [esc] 'self-insert-command)
      ; what is normal binding?? C-h c won't tell me
    (local-set-key ":" 'self-insert-command)
    (local-set-key "#" 'self-insert-command)
    (local-set-key ";" 'self-insert-command)
    (local-set-key "{" 'self-insert-command)
    (local-set-key "}" 'self-insert-command)
    (local-set-key "(" 'self-insert-command)
    (local-set-key ")" 'self-insert-command)
    (local-set-key [(control c)] 'my-copy-cmd)
  ))
)

; `run-hooks' reads the symbol value, and then looks that up
; as a function (?), so this extra level of indirection is needed
; (unfortunately, this still isn't enough for xemacs to work..)
;(setq c-mode-common-hook 'c-mode-common-hook)


; ----------- misc mode tweaks ----------------------------
; my extension-to-mode mappings (minimal for now)
(setq auto-mode-alist '(
  ("\\.el\\'"    . emacs-lisp-mode)
  ("\\.emacs\\'" . emacs-lisp-mode)
  ("\\.c\\'"     . c++-mode)    ; use c++ even for c!
  ("\\.cc\\'"    . c++-mode)
  ("\\.cpp\\'"   . c++-mode)
  ("\\.C\\'"     . c++-mode)
  ("\\.h\\'"     . c++-mode)
  ("\\.hpp\\'"   . c++-mode)
  ("\\.y\\'"     . c++-mode)      ; bison
  ("\\.ast\\'"   . c++-mode)      ; astgen
  ("\\.lex\\'"   . c++-mode)      ; flex
  ("\\.gr\\'"    . c++-mode)      ; my grammar files
  ("\\.tex\\'"   . LaTeX-mode)
  ("\\.texi"     . texinfo-mode)
  ("\\.pl\\'"    . perl-mode)
  ("\\.pm\\'"    . perl-mode)
  ("\\.java\\'"  . java-mode)
  ("\\.spec\\'"  . java-mode)     ; escjava spec files
  ("\\.mk\\'"    . makefile-mode)
  ("Makefile*"   . makefile-mode)
  ("makefile*"   . makefile-mode)
  ("\\.ml\\'"    . caml-mode)
  ("\\.mli\\'"   . caml-mode)
  ("\\.m3\\'"    . caml-mode)
  ("\\.i3\\'"    . caml-mode)
  ("\\.html\\'"  . html-mode)
  ("\\.sx\\'"    . lisp-mode)
  ("\\.thy\\'"   . lisp-mode)
  ("\\.v$"       . coq-mode)
  ("\\.vc$"      . c++-mode)
  ("\\.vml$"     . c++-mode)
  ("\\.model$"   . c++-mode)
  ("\\.gdbinit\\'" . sh-mode)
))


(defun my-help-mode-hook ()
  "Tweaks to help mode."
  (interactive)
  (local-unset-key [(control c)])
  (local-set-key "\015" 'apropos-follow)
    ; in xemacs apropos buffers seem to really be in help mode.. strange..
)

(defun my-apropos-mode-hook ()
  "Tweaks to apropos mode."
  (interactive)
  (local-set-key "\015" 'apropos-follow)
  ; intent is to put cursor in search results window after doing
  ; a search.. doesn't work....
  ;(if version-emacs
  ;  ; interestingly, this kill xemacs' apropos!
  ;  (select-window (get-buffer-window "*Apropos*")))
  ;(princ "done in my-apropos-mode-hook")
)

(defun my-shell-mode-hook ()
  "Tweaks to shell mode."
  (interactive)
  (local-set-key [up] 'comint-previous-input)
  (local-set-key [down] 'comint-next-input)
  (local-unset-key [(control c)])
)

; I can't figure out why this doesn't work....
(defun my-makefile-mode-hook ()
  "Tweaks to makefile mode."
  (interactive)
;  (princ "in makefile-mode-hook")
  (highlight-tabs-on)
  (local-set-key [13] 'newline)
)
;(setq makefile-mode-hook 'makefile-mode-hook)

(defun my-latex-mode-hook ()
  "Tweaks to LaTeX mode."
  (interactive)
  (local-unset-key [(control return)])
  (local-unset-key [(control j)])
)

(defun my-perl-mode-hook ()
  "Tweaks to Perl mode."
  (turn-off-electric-keys)
)


; ----------------- ocaml mode? ----------------------
; tell emacs where to find the ocaml stuff
(setq load-path (cons (concat (getenv "HOME") "/elisp/ocaml")
                      load-path))
(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)
(autoload 'my-camldebug "camldebug" "Caml debug." t)

;(if window-system (require 'caml-hilit))    ; doesn't work
;(if window-system (require 'caml-font "elisp/ocaml/caml-font.el" 'ok-error))
(if (file-readable-p "~/elisp/ocaml/caml-font.el")
  (if window-system (require 'caml-font))
)

(defun my-caml-mode-hook ()
  "My tweaks to caml mode"
  (interactive)
  (local-unset-key [?|])
  (setq case-fold-search t)       ; default to not-case-sens searching!
)

; todo for ocaml mode:
;   - highlight the operators better
;   - make it properly re-fontify comments ..
;   - strange hilighting of 'cstruct'


; ----------------- coq mode ----------------------
(setq load-path (cons (concat (getenv "HOME") "/elisp/coq")
                      load-path))
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
(autoload 'run-coq "coq-inferior" "Run an inferior Coq process." t)
(autoload 'run-coq-other-window "coq-inferior"
  "Run an inferior Coq process in a new window." t)
(autoload 'run-coq-other-frame "coq-inferior"
  "Run an inferior Coq process in a new frame." t)


; ---------------- games with tabs ----------------------------------
; In general, I *hate* literal tab characters to be in my files.
; It messes up everything when I'm trying to align things, edit
; things, etc.  It's frustrating that the soup of spaces and tabs
; in files comes from somebody's mistaken presumption that somehow
; this improves "efficiency".

; mark the buggers
; (how?)

; please don't insert them
;(setq indent-tabs-mode nil)      ; means don't use tabs..?
;(setq tabify-regexp "^$")        ; intended to not match anything with whitespace
;(setq tab-width 0)              ; no help

; I finally succeeded in getting it to not use tabs!  by examining
; the source, I see that indent-tabs-mode is what turns it off, but
; any time that variable is changed, it is made buffer-local, which
; means it can't be set normally by .emacs.  Instead, customize
; wrote something below, which did the trick.

; someone posted this as a way of highlighting tabs
;(require 'font-lock)
;(font-lock-make-face '(my-tab-face            nil "Red"))
;(font-lock-make-face '(my-trailing-space-face nil "Cyan"))
;(setq font-lock-keywords
;      (append font-lock-keywords
;              '(("\t+"     (0 'highlighted-tabs t))
;                ("[ \t]+$" (0 'my-trailing-space-face t))
;               )))
;(global-font-lock-mode 1)

; there is a tab here: ->       <-
; so here's my version
(setq highlight-tabs-keywords
  '("\t+" (0 'font-lock-tabs-face t)))
(setq highlight-trailing-ws-keywords
  '("[\t ]+$" (0 'font-lock-trailing-ws-face t)))

(defun highlight-tabs-on ()
  "Turn on tab-highlighting."
  (interactive)
  ; add the specifier for tabs
  (setq font-lock-keywords
    (append font-lock-keywords (list highlight-tabs-keywords)))
  (setq font-lock-keywords
    (append font-lock-keywords (list highlight-trailing-ws-keywords)))
  (font-lock-fontify-buffer)        ; re-fontify
)

(defun highlight-tabs-off ()
  "Turn off tab-highlighting."
  (interactive)
  ; remove the specifier for tabs
  (setq font-lock-keywords
    (filter-list font-lock-keywords
      #'(lambda (element) (and (not (eq element highlight-tabs-keywords))
                               (not (eq element highlight-trailing-ws-keywords))
                          ))
    ))
  (font-lock-fontify-buffer)        ; re-fontify
)

(defun highlight-tabs-toggle ()
  "Toggle whether tabs are currently highlighted."
  (interactive)

  (if (filter-list font-lock-keywords
        #'(lambda (element) (eq element highlight-tabs-keywords))
      )
    (highlight-tabs-off)
    (highlight-tabs-on)
  )
)


; ----------------- auto-indent 'enter' key -------------------------
(defun auto-align-newline ()
  "Inserts a newline, plus enough space to put the cursor beneath
   the first non-whitespace character in the closest preceeding
   nonblank line."
  (interactive)
  (let*
    ((start 0)     ; need a temp var
    ; first, compute the column we want to end up on, and store
    ; that value in 'col'
     (indentString (save-excursion
        (skip-chars-backward " \t\n") ; move to closest preceeding nonblank line
        (beginning-of-line)           ; move to start of that line
        (setq start (point))          ; grab that location
        (skip-chars-forward  " \t")   ; move to the first nonblank character
        (if (looking-at "\n")         ; blank line anyway?  i.e., top line?
          ""                                 ; don't indent
          (buffer-substring start (point))   ; get prev line's indentation
        ))))

    ; now, insert the carriage return
    (newline)

    ; finally, adjust column so we are in 'col'
    (insert indentString)
    ;(insert-char 32 col)
    ;(move-to-column col t)    ; force-move
  ))

; This binding was the result of hours of frustration and searching for
; answers.. I originally was binding it to [return], which seemed the
; most natural way to do it.  However, creating a binding for [return]
; causes Emacs to now treat the Enter key and C-m differently.  Further
; all the normal bindings for Enter (e.g. minibuffer's exit-minibuffer)
; effectively use C-m, so binding [return] prevented the Enter key from
; working in all contexts outside simple editing.
;
; I finally discovered that it was doing this (info node "Named ASCII
; Chars"), and realized that I have to bind C-m itself (decimal 13,
; octal 15) to avoid breaking Enter's meaning everywhere else.
;
; Yet again, evil GNU...
;
; binding I'm referring to is now in the bindings section; was:
;   (global-set-key "\015" 'auto-align-newline)


; ----------------------- applying external filters -----------------
; I often have a block of text that I want transformed by some external
; program such as awk.  I do this now by copying the text to a temp file,
; running the program on that temp file outputting to another temp file,
; read the 2nd temp, copy+paste into original buffer, and delete original
; text.  This is inconvenient.  So I'm going to try to write something
; to do it all from the emacs UI.

; my documentation for call-process-region:
;   (call-process-region start end program delete-src dest-buffer redisp args...)
;     start, end: region of source text to use
;     program: name of program to apply as a filter
;     delete-src: t means delete source text, nil means leave it
;     dest-buffer: t means current buffer, nil means discard it, 0 means discard and don't wait
;       can also be list, (dest-buffer stderr-file), where the first arg is interpreted
;       as above for stdout and 2nd arg is as above for stderr, or is a file name
;       string
;     redisp: t means incrementally update buffer during the run
;     args...: additional arguments to pass to the program

(defvar apply-filter-program-history-list 'filter-history
  "Name of a history list to use for interactive prompting for a filter
  program.")

(defun apply-filter-to-selection (filter)
  "Transform selected text by passing it through `filter' (a shell
  command line).  If no text is selected, just insert the command's
  output at the cursor."

  (if (is-mark-active)
    (let (; deal with possible transposition of point and mark
          (p (point))
          (m (mark))
         )
      (if (< p m)
        ; use /bin/sh so I get all the usual shell interpretations
        (call-process-region p m "/bin/sh" t t nil "-c" filter)
        (call-process-region m p "/bin/sh" t t nil "-c" filter)
      )
    )
    ; mark is not active, run the program with no input
    (call-process-region (point) (point) "/bin/sh" t t nil "-c" filter)
  ))

(defun apply-filter-program ()
  "Transform selected text by passing it through a filter program."
  (interactive)

  ; prompt for the program to run
  (let ((program (read-from-minibuffer "Program to apply: "
                                       nil nil nil
                                       apply-filter-program-history-list
                                       nil t))
       )
    (apply-filter-to-selection program)
  ))

(defun apply-cvt-filter ()
  "Transform selected text (or current line if nothing selected) by
  passing it through ./cvt.pl."
  (interactive)

  ; make sure something is selected
  (default-selection)

  ; run the conversion
  (apply-filter-to-selection "./cvt.pl")
)

; turns out emacs already has reverse-region... but I leave this here anyway
(defun reverse-lines ()
  "Reverse the order of the selected lines."
  (interactive)

  ; final sed removes the numbering *plus* the leading tab
  (apply-filter-to-selection "nl -p -ba -nrz | sort -r | sed 's/^.......//'")
)

(defun empty-header ()
  "Insert the usual boilerplate for a header file."
  (interactive)

  (apply-filter-to-selection (concat "empty-header "
    (basename (buffer-file-name()))))
)


; ----------------- some refactoring ---------------------
(defvar refactor-history-list 'refactor-history
  "Name of a history list to use for interactive prompting for a
  refactoring parameter.")

(defun unmethodify ()
  "Replace x->foo(a, b, c) with foo(x, a, b, c), where foo is
  prompted for interactively."
  (interactive)

  (let ((pointer (read-from-minibuffer "Pointer to unmethodify: "
                                       nil nil nil
                                       refactor-history-list
                                       nil t))
       )
    (query-replace-regexp (concat pointer "->\\([a-zA-Z_][a-zA-Z_0-9]*\\)(")
                          (concat "\\1(" pointer ", "))
  ))


; -------------------- etags stuff -----------------------
(defun find-next-tag ()
  "Go to next match of `find-tag'."
  (interactive)
  (find-tag nil t)
)


(defun grep-sources ()
  "Grep source files for the selection, or word at cursor."
  (interactive)
  (let ((text (get-sel-or-word)))
    (grep (concat "grep -n '" text "' *.h *.c *.cc *.ast *.gr"))
  ))


; ----------- indentation manipulation ---------
(defun normalize-point-mark ()
  "If mark is after point, swap them; otherwise leave them."
  (let ((start (min (point) (mark)))
        (end (max (point) (mark))))
     (set-point start)
     (set-mark end)
  ))

(defun replace-indents-in-region (srcIndent srcIndentChars destIndent)
  "Replace sequences of one kind of indentor (srcIndent, as a regexp)
  with sequences of another kind of indentor (destIndent);
  srcIndentChars says how many characters each instance of the source
  indentor is, so we know how many copies of destIndent to replace it
  with."
  (if (not (is-mark-active))
    (princ "Nothing is selected.")                                                    
    (let ((orig-point (copy-marker (point) nil))    ; nil: ins at mark doesn't move it
          (orig-mark (copy-marker (mark) t)))       ; t: ins at mark moves it
      (normalize-point-mark)
      ;(princ (format "(point %s) (mark %s) " (point) (mark)))
      (let ((end (make-marker)))
        (set-marker end (mark))     ; use a marker so insertions don't mess it up
        (while (re-search-forward srcIndent end t)
          (let ((e (match-end 0))
                (b (match-beginning 0)))
            ;(princ (format "(e=%s b=%s end=%s) " e b (+ end 0)))
            (replace-match
              (n-copies destIndent (/ (- e b) srcIndentChars))
            )
          )
        )
        (set-marker end nil)        ; point it at nothing for performance reasons
      )
      
      ; this doesn't really work the way I want it to (in particular,
      ; the region doesn't stay set.. same old crap..)
      (set-point orig-point)
      (set-marker orig-point nil)
      (set-mark orig-mark)
      (set-marker orig-mark nil)
    )
  )
)

(defun indent-tabs-to-2spaces ()
  "Replace sequences of leading tabs with sequences of
  leading 2-spaces, in selected text."
  (interactive)
  (replace-indents-in-region "^\t+" 1 "  "))

(defun indent-2spaces-to-tabs ()
  "Replace sequences of leading 2-spaces with sequences
  if leading tabs, in selected text."
  (interactive)
  (replace-indents-in-region "^\\(  \\)+" 2 "\t"))

(defun indent-8spaces-to-2spaces ()
  "Replace sequences of leading 8-spaces with sequences
  if leading 2-spaces, in selected text."
  (interactive)
  (replace-indents-in-region "^\\(        \\)+" 8 "  "))

(defun indent-2spaces-to-8spaces ()
  "Replace sequences of leading 2-spaces with sequences
  if leading 8-spaces, in selected text."
  (interactive)
  (replace-indents-in-region "^\\(  \\)+" 2 "        "))

; (defun indent-tabs-to-2-spaces ()
;   "Replace sequences of leading tabs with sequences of
;   leading 2-spaces, in selected text."
;   (interactive)
;   (if (not (is-mark-active))
;     (princ "Nothing is selected.")
;     (progn
;       (normalize-point-mark)
;       ;(princ (format "(point %s) (mark %s) " (point) (mark)))
;       (let ((end (make-marker)))
;         (set-marker end (mark))     ; use a marker so insertions don't mess it up
;         (while (re-search-forward "^\t+" end t)
;           (let ((e (match-end 0))
;                 (b (match-beginning 0)))
;             ;(princ (format "(e=%s b=%s end=%s) " e b (+ end 0)))
;             (replace-match
;               (n-copies "  " (- e b))    ; # of tabs -> # of " "
;             )))
;         (set-marker end nil)        ; point it at nothing for performance reasons
;       ))
;   ))



(defun n-copies (str n)
  "Return a string that is 'n' copies of 'str' concatenated."
  (let ((origStr str))
    (while (> n 1)
      (setq str (concat str origStr))
      (setq n (- n 1))
    )
    str
  )
)


; why must I go out of my way to define this??
(defun uncomment-region ()
  "Uncomment region."
  (interactive)
  (comment-region (point) (mark) -1))


      






; -------------------- keybindings menu ---------------------
; the code here is processes a keybindings menu, like the one that
; appears at the end of this file, and installs the bindings it
; finds in that menu
(defun install-keybindings-menu (menu)
  "Install a menu of keybindings.
  
  'menu' is a list of lists.  Each contained list begins with one
  of several symbols; which symbol it is determines what follows.
  
  The list of symbols, and what is expected to follow, is:

  (menu <menu title> <menu description> items...)
    A (sub) menu.  The 'items' is another keybindings menu.

  (item <item title> <action> bindings...)
    A menu item.  'action' is the function to execute when the menu
    item is selected, or key is pressed; if it is missing, the
    menu item is ignored.  'bindings' is a list of key binding
    vectors.
    
  (separator)
    Visual separator."

  (interactive)
  (if (eq menu ())
    t    ; done
    (progn
      (install-keybindings-item (car menu))   ; process 1 item
      (install-keybindings-menu (cdr menu))   ; process rest
    )))

(defun install-keybindings-item (list)
  "Helper for `install-keybindings-menu'."
  (let ((tag (car list)))
    (cond ((eq tag 'menu)
             (install-keybindings-menu (nthcdr 3 list)))   ; process items after description
          ((eq tag 'item)
             (if (>= (length list) 4)                      ; see if there are any bindings here
               (bind-some-keys (nth 2 list) (nthcdr 3 list))   ; yes, process them
             ))
          ((eq tag 'separator)
             t)      ; nop for now
          (t
             (error "Invalid menu tag: %s" tag))
    )))

(defun is-bindable-key (binding)
  "Return t if 'binding' is a key we can bind; this just checks for
  a few special cases that are known to be problems -- it is not the
  case that every key this fn accepts can be bound."
  ; the only one at this time is [(control shift s)] on xemacs
  (if (and version-xemacs 
           (equal (car bindings) [(control shift s)]))
    nil    ; this is the one we can't bind
    t      ; assume the rest are ok
  ))

(defun bind-some-keys (action bindings)
  "Call `global-set-key', binding every key that appears in the
  'bindings' list to 'action'."
  (if (eq bindings ())
      t   ; done
      (progn
        (if (is-bindable-key (car bindings))     ; check for problematic keys
          (global-set-key (car bindings) action)   ; normal behavior
        )
        
        ; recursively process rest of bindings
        (bind-some-keys action (cdr bindings))
      )))
;(bind-some-keys 'save-buffer '([f2] [(control o)]))


; ----------------------- emacs todo list ---------------------------
; legend: * means done, ~ means half-done, ! means stopped working,
;         - means won't do, (nothing) means still yet to do,
;         -> means this is a high priority!

; priorities for emacs acceptance:
;  *end/home properly defined
;  *C-z, C-w for scroll up/down
;  *C--, C-| for window split
;  -MC-right, MC-left, MC-up, MC-down to move topologically (not a good idea)
;  *acceptable default colors (white on blue, or black on gray)
;  *acceptable syntax highlighting (hacked the hell out of font-lock.el!)
;  *convenient buffer switching (more convenient than c-x c-f filename): f4
;  *explicit paren matching via m-[ and m-]
;  *usable from home
;  *command to open file the cursor is on (c-i)
;  ~fix the wrapping or horiz-scroll for thin windows
;  *allow cursor into blank space (not possible) (update: got it!)
;  *bind c-r to replace
;  *shift-cursor to select text; ctrl-ins to copy, shift-del to cut,
;     shift-ins to paste
;  *indent/outdent with m-left and m-right
;  -reduce the paren-matching delay (not possible)
;  *alt-backspace to undo
;  *unbind Ins key
;  ~F5 to maximize/restore windows inside emacs
;  *usable buffer list
;  *usable subshell
;  *Del when region selected should do shift-del
;  *fix the fucked-up auto-indent that the C++ mode uses! what the hell is up with that shit?  goddamn gnu!!!!
;  *make sane the interaction with emacsclient (in particular, close the buffer!)
;  *get it to not beep when C-^ on a window that is full-size in either direction
;  *way to display the current column #.. mode line!: (column-number-mode)
;   fix Latex mode so C-j works

; new, though hopefully small, issues:
;  *allow arrow keys in readonly
;  *don't insert tab characters when moving forcefully
;     instead, it forcefully converts them to spaces... acceptable I guess
;  *pgup/pgdn so the cursor stays in same place on screen
;  *scroll-right and scroll-left the same way my up and down work with c-z and c-w
;     (I think I should bind C-arrow to be a general scrolling interface)
;  *eliminate some mode-specific rebindings:
;     help-mode ^c
;  *problem with shell..
;  *C-k should cut if text is selected
;  *mode to show all control characters, including tabs
;   don't jump around as much when the cursor moves beyond the
;     top or bottom of the window
;  *move-up is broken at or near end of file..?
;     was because I was calling count-lines with point-max+1 because of the +1
;     hack.. I just limited the 2nd arg to point-max, and now it works
;  *it should delete the *~ files when it closes them successfully
;     handled by putting backups elsewhere so I don't care
;   shell prompt regexp that doesn't misinterpret '$'
;  *my-new-frame (font, esp.)
;  *key or fn to insert time/date: Alt-D

; more ambitious emacs ideas:
;  ~truly useful buffer list: like alt-tab interface
;     having several frames comes close..
;  *cursor beyond EOL
;   usable file-position bookmarks: C-1..9 to goto, and C-S-1..9 to set
;  *number the columns starting with 1
;  *f5 maximize/restore; would really like per-frame.. got it!
;  ~compile/debug as inferior processes working correctly
;   allow rectangular selections, deletions, insertions
;  *prevent insertion of Tab characters except when literally pressing Tab
;     -> now even Tab doesn't insert Tab characters!
;     fixed
;   undo that undoes cursor movement, not just buffer modification
;   redo independent from undo!!
;   subshell with history retrieval instead of rampant buffer modification
;   tolerable perl syntax highlighting

; auto-indent ideas (actually just Borland's ideas):
;  *when I press Enter, should go to column of first non-whitespace
;     char in most recent nonblank line
;
;       this_is_foo();   [enter]
;       _  <--- cursor goes here
;
;   when I press Backspace in a line that has no non-whitespace characters,
;     it should move left to the first (going left) column where an above
;     line starts, such that all intervening lines start in higher-numbered
;     columns
;
;       while (1) {
;         while (2) {
;           this_is_bar();
;           _      [backspace]
;       ^ ^   <--- backspace first goes to 'w' in while (2), then another
;                  press goes to 'w' in while (1), and finally to BOL

; another idea: different color for tabs in Makefile mode
; yet more:
;   get that symlink crap working right

; more:
;   better paren matching interface
;  -start creating my own menu -- would be useless because there
;     is no (usable) keyboard interface to the menus!
;   apply-filter-program should end with output selected
;   shift-control-n (e.g.) like shift-dnarrow
;   figure out how to read email in emacs
;  *revert-buffer-yes
;   M-x apropos should switch to other window (already does in xemacs..)
;   search where the default text is taken from the buffer
;   don't print the autosave messages
;   pgup/pgdown don't clear selection?
;   ctl-shift-right doesn't work
;   multiple ctl-k in a row should act like a single cut
;   perl-mode disable electric keys

; how to set the emacs window's title programmatically?

; still more:
;   on Alt-F9, move cursor to bottom of compilation window, then
;     back to original window, so I see end of messages
;   fix syntax highlighting!

; xemacs:
;  *pgup/pgdown doesn't keep cursor position properly
;  *startup in strange place?
;  *typing to replace selection!  
;     (got it from the menu.. Options | Editing Options | Auto Delete Selection)

; ---------------- info from others -----------------
; how to rebind tricky keys:
;    Hit <insert>, then C-h l.  Post what you see.
;
;    (The output should include what Emacs thinks the <insert> key does.
;    Suppose that Emacs saw xyzzy, then you can bind that key as follows:
;    (global-set-key (kbd "<xyzzy>") 'forward-char).)


; ------------------- trash ---------------------


; ---------- various M-x configure related things ----------
; 'M-x configure' writes some stuff to the end of .emacs, but
; older Emacs' don't understand the stuff it writes; this should
; suffice
(if (< emacs-major-version 20)
    (progn
       (defun custom-set-variables (&rest args) t)
       (defun custom-set-faces (&rest args) t))
)


; this is some crap the 'configure' command wrote...
; (had to wrap a version check around it, which may screw up
; future changes to 'configure'..)
;(if (>= emacs-major-version 20)
;  (custom-set-variables
;   '(truncate-lines t)
;   '(truncate-partial-width-windows t)
;   '(standard-indent 2))
;  (custom-set-faces
;   '(font-lock-comment-face ((((class color) (background light)) (:italic t :foreground "Black"))))
;   '(font-lock-string-face ((t (:foreground "Black"))))
;   '(font-lock-type-face ((((class color) (background light)) (:foreground "MediumSpringGreen")))))
;)


; my backup of my font-lock faces (10/24/99)
;(custom-set-faces
; '(font-lock-operator-face ((t (:bold t :foreground "#e0000000b000"))))
; '(font-lock-comment-face ((((class color) (background light)) (:italic t :foreground "#300030006800"))))
; '(font-lock-string-face ((((class color) (background light)) (:foreground "Black"))))
; '(font-lock-octal-face ((((class color) (background light)) (:foreground "DarkRed"))))
; '(font-lock-keyword-face ((t (:foreground "#000070000000"))))
; '(font-lock-warning-face ((((class color) (background light)) (:foreground "White" :background "Red"))))
; '(font-lock-constant-face ((((class color) (background light)) (:foreground "Red"))))
; '(font-lock-shell-cmd-args-face ((((class color) (background light)) (:foreground "Blue"))))
; '(font-lock-type-face ((((class color) (background light)) (:foreground "Blue"))))
; '(font-lock-shell-prompt-face ((((class color) (background light)) (:bold t :foreground "Black"))))
; '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Blue"))))
; '(vhdl-font-lock-prompt-face ((((class color) (background light)) (:foreground "White" :background "Black"))))
; '(font-lock-builtin-face ((((class color) (background light)) (:foreground "#800000007000")))))

(defun crt-colors ()
  "Use colors that look good on a CRT."
  (interactive)
  (custom-set-faces
   '(font-lock-operator-face ((t (:bold t :foreground "#e0000000b000"))))
   '(font-lock-comment-face ((((class color) (background light)) (:italic t :foreground "#300030006800"))))
   '(font-lock-string-face ((((class color) (background light)) (:foreground "Black"))))
   '(font-lock-octal-face ((((class color) (background light)) (:foreground "DarkRed"))))
   '(font-lock-keyword-face ((t (:foreground "#000070000000"))))
   '(font-lock-warning-face ((((class color) (background light)) (:foreground "White" :background "Red"))))
   '(font-lock-constant-face ((((class color) (background light)) (:foreground "Red"))))
   '(font-lock-shell-cmd-args-face ((((class color) (background light)) (:foreground "Blue"))))
   '(font-lock-type-face ((((class color) (background light)) (:foreground "Blue"))))
   '(font-lock-shell-prompt-face ((((class color) (background light)) (:bold t :foreground "Black"))))
   '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Blue"))))
   '(vhdl-font-lock-prompt-face ((((class color) (background light)) (:foreground "White" :background "Black"))))
   '(font-lock-preprocessor-face ((((class color) (background light)) (:foreground "#800000007000")))))
  (if version-emacs-x (progn
    (setq my-bg-color "#f000f000f000")
    (set-background-color my-bg-color)
    (set-foreground-color "Black")
  ))
)

(defun lcd-colors ()
  "Colors that look good on LCD"
  (interactive)
  (if version-emacs-x
    (set-background-color "Blue") ;"#f000f000f000")
    (set-foreground-color "White"))
  (set-cursor-color "White")
  (custom-set-variables
   '(truncate-lines t)
   '(truncate-partial-width-windows t)
   '(standard-indent 2)
   '(global-font-lock-mode t nil (font-lock)))

  ; this set of faces unfortunately does not include the color
  ; for highlighting selected text, which should be #000000008fff
  (custom-set-faces
   '(font-lock-operator-face ((t (:bold t :foreground "#e0000000b000"))))
   '(font-lock-comment-face ((((class color) (background light)) (:italic t :foreground "#300030006800"))))
   '(font-lock-string-face ((((class color) (background light)) (:foreground "Black"))))
   '(font-lock-octal-face ((((class color) (background light)) (:foreground "DarkRed"))))
   '(font-lock-keyword-face ((t (:foreground "#6000ffff7000"))))
   '(font-lock-warning-face ((((class color) (background light)) (:foreground "White" :background "Red"))))
   '(font-lock-constant-face ((((class color) (background light)) (:foreground "Red"))))
   '(font-lock-shell-cmd-args-face ((((class color) (background light)) (:foreground "Blue"))))
   '(font-lock-type-face ((((class color) (background light)) (:foreground "Blue"))))
   '(font-lock-shell-prompt-face ((((class color) (background light)) (:bold t :foreground "Black"))))
   '(font-lock-variable-name-face ((((class color) (background light)) (:foreground "Blue"))))
   '(vhdl-font-lock-prompt-face ((((class color) (background light)) (:foreground "White" :background "Black"))))
   '(font-lock-preprocessor-face ((((class color) (background light)) (:foreground "#800000007000")))))
)
;(lcd-colors)

; I don't even know what this does in emacs; but in xemacs, it causes
; a cryptic error message on startup; it was in the custom-set-variables list
;'(global-font-lock-mode t nil (font-lock))
; let's try this as an alternative
(if version-emacs-x
  (global-font-lock-mode 1)     ; turn it on
)


; ----------------- preferences and policies -----------------------
; ---- misc ----
; apparently, emacs was envious of MS-Word's ability to propagate
; viruses through attached macros, so they implemented a something to
; do this..  grr... see etc/FAQ.. this apparently disables
; document-attached macros, though it's hard to get verification of
; this since I can't find docs on how to *create* document-attached
; macros!
(setq inhibit-local-variables t)


; don't ask about symlink stuff.. I still don't understand the
; implications of this, but the help on vc-follow-symlink seems
; to indicate I want to not follow them...                         
(setq vc-follow-symlinks nil)


; instead of making a backup of file a.b as ~a.b in the same directory,
; create it in ~/bak as a.b
(defun make-backup-file-name (file)
  (let ((dir (concat (getenv "HOME") "/bak")))
    (if (not (file-exists-p dir))
      (make-directory dir))             ; make the directory if it doesn't exist
    (concat dir "/" (file-name-nondirectory file))
  ))

  
; and for extending the line numbering limit
; (note: this is a byte count of file size)
(setq line-number-display-limit 99999999)


; ---- appearance ----
; fonts and colors
(cond 
  ((and version-not-term (<= (x-display-pixel-width) 800))
   ; 800x600 or less; use a smaller font
   (setq my-preferred-font "-*-fixed-medium-r-*-*-13-*-*-*-*-*-*-*"))
  ((and version-not-term (<= (x-display-pixel-width) 1280))
   ; 1280x1024 or less; medium font
   (setq my-preferred-font "-*-courier-medium-r-*-*-14-*-*-*-*-*-*-*"))
  (t (setq my-preferred-font "-*-courier-medium-r-*-*-18-*-*-*-*-*-*-*"))
)
;(setq my-preferred-background-color "Blue")
(setq my-preferred-background-color "#00000000a000")
(setq my-preferred-foreground-color "White")
(setq my-preferred-cursor-color "White")

;(setq my-preferred-font "-adobe-courier-medium-r-*-*-14-101-100-*-*-*-*-*")

;(setq my-preferred-font "-scott-courier-medium-r-*-*-18-*-*-*-*-*-*-*")

; this is what I was using for 1600x1200
;(setq my-preferred-font "-*-courier-medium-r-*-*-18-*-*-*-*-*-*-*")

; normal 1280x1024
(setq my-preferred-font "-*-editor-medium-r-*-*-14-*-*-*-*-*-*-*")

; this is the default emacs font, and the one which does *not* cause
; a multi-second delay when being set..
;(setq my-preferred-font "-Misc-Fixed-Medium-R-Normal--15-140-75-75-C-90-ISO8859-1")

; ! " # $ `i i'

; set these things for the current frame
;(set-my-frame-font)

; since my font is not on many machines, detect when it didn't work
; and choose another
;(if (not (begins-with (cdr (assoc 'font (frame-parameters))) "-Scott"))
;  (progn
;    (setq my-preferred-font "-*-courier-medium-r-*-*-14-*-*-*-*-*-*-*")
;    (set-my-frame-font))
;)

; switch to a given font
(defun switch-to-font (f)
  "Switch to font `f'"
  (setq my-preferred-font f)
  (set-my-frame-font)
)

; good for making the font larger
(setq my-larger-font "-adobe-courier-medium-r-*-*-20-*-*-*-*-*-*-*")
(defun use-larger-font ()
  "Switch to a larger screen font."
  (interactive)
  (switch-to-font my-larger-font)
)

; this is the expansion of set-my-frame-font:
;(modify-frame-parameters (selected-frame)
;                         (list (cons 'font my-preferred-font)))
;(frame-update-faces (selected-frame))


;(set-my-frame-colors)


; add column number to mode line
; (I want columns numbered starting with 1, but emacs numbers
; them starting at 0.  The only way I found to fix it is to
; change the emacs source, which I've done on my machine.)
(column-number-mode t)


; and line number!  cripes why does xemacs default to not having this?
(line-number-mode t)


; put scrollbars on the right side of windows
(if (and (not version-xemacs)
         (> (strcmp emacs-version "20.2.2") 0))    ; 20.2.1 & 20.2.2 set-scroll-bar-mode is broken
    (set-scroll-bar-mode 'right)
)

; ; get rid of that useless (because I can't use the keyboard to
; ; access it) menu
; (if (not version-xemacs)
;     (menu-bar-mode -1))
; 
; ; similarly for toolbar
; (if (and version-emacs-x
;          (> (strcmp emacs-version "21") 0))
;   (tool-bar-mode 0))

; and turn off that damn beeping!  same for tcsh...
; this works for i-search beeps, but not for ^Q beeps.. arg..
; for xemacs, this definition prevents error messages from being displayed (!!)
(if version-emacs-x
  (defun ding (&optional arg)
    nil)
)


; ---- frame management ----
; set normal height fraction; different for different versions because
; this is the fraction the *text area* occupies -- it doesn't count the
; space used by menu, titlebar, toolbar, etc.
(setq my-height-fraction
  (cond
    (version-emacs-x 0.85)      ; no decorations
    (version-xemacs 0.78)       ; toolbar and menu..
    (t 0.7)                     ; conservative
  ))

; preset positions: width-fraction, height-fraction, x-origin-pels, y-origin-pels
; (see `frame-move-resize')

(setq right-half-screen-pos (list 0.6 my-height-fraction -4 30))   ; 1 from right, 25 from top
(setq left-half-screen-pos (list 0.5 my-height-fraction 50 30))    ; 50 from left, 25 from top
(setq normal-screen-pos (list 0.83 my-height-fraction -4 30))      ; 1 from right, 25 from top

; old (KDE1) settings
;(setq right-half-screen-pos (list 0.6 my-height-fraction -1 25))   ; 1 from right, 25 from top
;(setq left-half-screen-pos (list 0.5 my-height-fraction 50 25))    ; 50 from left, 25 from top
;(setq normal-screen-pos (list 0.83 my-height-fraction -1 25))      ; 1 from right, 25 from top

; I don't set the initial window position here, because it will get
; changed (at least in xemacs) when cust.settings.el are loaded, since
; that sets the font.  Instead, I will set the window position from
; the .emacs itself.


; ---- window scrolling ----
; disable horizontal-scrolling "snap" behavior, which prevents scrolling
; when cursor is within a certain distance from the left edge
(setq hscroll-snap-threshold 0)

; allow cursor to be at a scrolled edge
(setq hscroll-margin 0)

; automatically scroll horizontally when the cursor reaches the
; right edge (since this doesn't work in all versions, and doesn't
; work that well in any version, I may just re-implement it myself)

; don't insert newlines when the cursor moves beyond EOF
(setq next-line-add-newlines nil)

; vertically/up/down scrolling amount when scrolling is
; caused by cursor moving beyond edge of visible area
; (this appears not to work?  moving down it works,
; but moving up it doesn't...)
;(setq scroll-step 1)

; how many places should scroll-{left,right}-several scroll horizontally?
(setq ctl-shift-scroll-h-amt 10)

; and what about scroll-{up,down}-several, vertically?
(setq ctl-shift-scroll-v-amt 5)


; ---- cursor movement ----
; policy controlling whether cursor movement is destructive
(defun destructive-cursor-movement ()
  "Policy; returns t if we want cursor movement to potentially change
  the file"
  (interactive)

  (not                               ; any of these conditions turn off destructive:
    (or buffer-read-only               ; if it's read-only, we just get beeps
        (eq major-mode 'custom-mode)   ; M-x customize also doesn't like it
        (eq major-mode 'makefile-mode) ; and Makefiles are very sensitive to such changes
        (not (buffer-modified-p))      ; if we have made no changes, don't start
    )))


; aha!  this takes care of pgup/dn annoying stuff!
; unfortunately, it only has effect on emacs; xemacs'
; src/window.c/window_scroll is completely different, and it isn't
; clear how to give it the same functionality
(setq scroll-preserve-screen-position 1)


; ---- selecting text ----
; turn on transient mark mode (that is, we highlight the selected text)
(transient-mark-mode 't)

; I found about this by reading pc-select.el, which turns out to implement
; a great deal of what I already did below.. however, my code is much
; smaller (owing to macro usage), so I'll keep mine, thanks!
;
; The following causes typing to replace the selection.
(if (not version-xemacs)       ; TODO: xemacs doesn't support this.. ?
    (delete-selection-mode 1))

; ---- automatic paragraph wrap ----
; tolerable wrapping (auto-fill-mode to activate)
(set-fill-column 78)    ; doesn't take effect, it's buffer-local ..
(setq colon-double-space 't)


; ----------- gnus stuff -----------
; load `message' now so I can override functions
(require 'message)
(defun message-check-news-syntax ()
  "Check the syntax of the message."
  (and
    (save-excursion
      (save-restriction
        (widen)
        (and
         ;; We narrow to the headers and check them first.
         (save-excursion
           (save-restriction
             (message-narrow-to-headers)
             (message-check-news-header-syntax)))
         ;; Check the body.
         (message-check-news-body-syntax))))
    ; sm: this last line is my addition
    (y-or-n-p "Post the message? ")
  ))

; allow comments (real name) in my addr
(defun message-make-address ()
  "Make the address of the user."
  (or user-mail-address
      (concat (user-login-name) "@" (message-make-domain))))

(defun my-gnus-summary-mode-hook ()
  "Tweaks to gnus summary mode."
  (local-set-key "r" 'gnus-summary-followup-with-original)
)




; ---- mode hooks ----
;(add-hook 'c-mode-common-hook 'turn-off-electric-keys)
;(add-hook 'help-mode-hook 'my-help-mode-hook)
;(add-hook 'apropos-mode-hook 'my-apropos-mode-hook)
;(add-hook 'shell-mode-hook 'my-shell-mode-hook)
;(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)
;(add-hook 'apropos-mode-hook 'my-apropos-mode-hook)
;(add-hook 'latex-mode-hook 'my-latex-mode-hook)
;(add-hook 'perl-mode-hook 'my-perl-mode-hook)
;(add-hook 'caml-mode-hook 'my-caml-mode-hook)
;(add-hook 'dired-mode-hook 'my-dired-mode-hook)
;
;(add-hook 'gnus-summary-mode-hook 'my-gnus-summary-mode-hook)

; ---- compiling ----
(defun my-compile ()
  "My command to do a build."
  (interactive)
  
  (if (file-executable-p "emacs-make")
    ; an emacs-make script in the current directory overrides
    (compile "./emacs-make")
    ; just do make
    (compile "make")     ; the default is "make -k", which I don't want
  )
)

(defun basename (path)
  "Return the filename part of a pathname."
  (let ((path (split-string path "/")))     ; path as list
    (car (last path))
  )
)

(defun strip-extension (name)
  "Return the name, without the extension."
  (let ((parts (split-string name "\\.")))    ; parts as list
    (substring name 0 (- (length name) 1 (length (car (last parts)))))
  )
)

(defun my-compile-this-module ()
  "Compile just the module for this buffer."
  (interactive)
  (let ((name (strip-extension (basename (buffer-file-name)))))     ; name w/o path or extension
    (compile (concat "remake " name ".o"))
  )
)

; since compiling is done by invoking $SHELL, and my shell likes to
; set the xterm title, and that looks funny in an emacs buffer, I
; want to disable that feature of my .cshrc
(setq process-environment
  (append process-environment (list "DONT_STITLE=1")))


; to make the compilation output scroll so I can always see the
; bottom (which should be the default!), use M-x customize to
; set compilation-scroll-output to true


(defun debug-compile-mode ()
  "Set environment variables so the Makefile will do a debug build.
   Intended (eventually) to work with Makefiles from any of my projects."
  (interactive)
  (setenv "ALLOW_DEBUG" 1)
  (princ "Compile mode is now DEBUG")
)

(defun unsetenv (varname)
  "Remove the given variable from the environment."
  (let ((varnameEq (concat varname "=")))
    (setq process-environment
      (filter-list process-environment
      #'(lambda (s)
          (not (begins-with s varnameEq))
        ))
    )
  ))

(defun ndebug-compile-mode ()
  "Set env vars for release (fast) compile.  See `debug-compile-mode'."
  (interactive)
  (unsetenv "ALLOW_DEBUG")
  (princ "Compile mode is now RELEASE")
)


; ------ desktops -------
; desktops let you save a particular set of open files as a "desktop"
; in a project directory, so you can go back to that same set of open
; files later

; make the desktop functions available
;(load "desktop")

; something about getting modes correct?
;(desktop-load-default)

; if there is a desktop file in the starting directory, load it
; this as been doing some screwy things..?
;(desktop-read)


; ---------------- debugger stuff ---------------------
(setq gdb-use-5 't)       ; C-M-z here to activate
(setq gdb-use-5 nil)      ; default is 4.18

(defun gdb-no-args ()
  "Run gdb without any additional args; relies on .gdbinit"
  (interactive)
  (if gdb-use-5
    (gdb "gdb-5")
    (gdb "gdb"))
)


; so I can override `gud-display-line', below
(require 'gud)

; this is a modified version of `gud-display-line', with just one
; small tweak: when we call `get-buffer-window', we pass 't' as the
; 'frame' argument, so that if the source is already displayed in
; another frame, it will use that instead of always making a window
; in the current frame
(defun gud-display-line (true-file line)
  (let* ((last-nonmenu-event t)  ; Prevent use of dialog box for questions.
         (buffer
          (save-excursion
            (or (eq (current-buffer) gud-comint-buffer)
                (set-buffer gud-comint-buffer))
            (gud-find-file true-file)))
         (window (and buffer (or (get-buffer-window buffer t)   ; <-- tweak
                                 (display-buffer buffer))))
         (pos))
    (if buffer
        (progn
          (save-excursion
            (set-buffer buffer)
            (save-restriction
              (widen)
              (goto-line line)
              (setq pos (point))
              (setq overlay-arrow-string "=>")
              (or overlay-arrow-position
                  (setq overlay-arrow-position (make-marker)))
              (set-marker overlay-arrow-position (point) (current-buffer)))
            (cond ((or (< pos (point-min)) (> pos (point-max)))
                   (widen)
                   (goto-char pos))))
          (set-window-point window overlay-arrow-position)))))



; -------------------- outline mode -----------------
; start with a mostly-empty (?) key map
(setq outline-mode-map (make-sparse-keymap))
;(setq outline-minor-mode-map (make-sparse-keymap))

(defun show-direct-children()
  "Show everything directly beneath a heading (body, and headings)"
  (interactive)
  (show-children)     ; headings
  (show-entry)        ; body attached to this heading
)
(define-key outline-mode-map [(kp-add)] 'show-direct-children)

; show everything beneath a heading, at all levels
(define-key outline-mode-map [(kp-multiply)] 'show-subtree)

; hide this entire subtree
(define-key outline-mode-map [(kp-subtract)] 'hide-subtree)

; hide all non-headings
(define-key outline-mode-map [(kp-divide)] 'hide-body)


; highlighting for outline mode (copied & modified from outline.el)
(setq outline-font-lock-keywords
  '(;;
    ;; Highlight headings according to the level.
    (eval . (list (concat "^" outline-regexp ".+")
                  0 '(or (cdr (assq (outline-font-lock-level)
                                    '((1 . font-lock-constant-face)       ; red
                                      (2 . font-lock-string-face)         ; yellow
                                      (3 . font-lock-keyword-face)        ; green
                                      (4 . font-lock-function-name-face)  ; italics
                                      (5 . font-lock-variable-name-face)  ; regular
                                      (6 . font-lock-comment-face)
                                      (7 . font-lock-constant-face)
                                      (8 . font-lock-type-face))))
                         font-lock-warning-face)
                  nil t)))
)


; this binding at the global level helps me tell whether the
; local map bindings above are working correctly
(global-set-key [(control kp-add)] '(lambda () (interactive)
  (princ "You pressed Ctrl+keypad-plus")))


(defun my-outline-minor-mode-hook ()
  "Tweaks to outline-minor mode."
  (interactive)
;  (local-unset-key [(control c)])
;  (local-set-key "\015" 'apropos-follow)

  ; outline-mode keybindings

  ; show children of current node
  (local-set-key [(kp-add)] 'show-direct-children)

  ; show all children, recursively
  (local-set-key [(kp-multiply)] 'show-subtree)

  ; hide children of current node
  (local-set-key [(kp-subtract)] 'hide-subtree)

  ; hide all non-heading entries
  (local-set-key [(kp-divide)] 'hide-body)
)
(add-hook 'outline-minor-mode-hook 'my-outline-minor-mode-hook)


; ; -------------------- underscore ------------------
; ; the underscore character is annoying to type, so I'm experimenting
; ; with alternative keybindings..
; (setq space-is-underscore nil)      ; when true, space will type underscore

; (defun toggle-space-is-underscore()
;   "Toggle `space-is-underscore', which when true, causes the spacebar
;   to insert an underscore character."
;   (interactive)
;   (setq space-is-underscore (not space-is-underscore))
;   (if space-is-underscore
;     (princ "Pressing spacebar now inserts underscores")
;     (princ "Pressing spacebar now inserts spaces"))
; )

; (defun my-spacebar ()
;   "Insert a space character, unless `space-is-underscore', in which
;   case insert an underscore."
;   (interactive)
;   (if space-is-underscore
;     (insert "_")
;     (insert " "))
; )

; these definitions are now part of the menu
; (global-set-key [? ] 'my-spacebar)
; (global-set-key [? ] 'self-insert-command)

; (global-set-key [(control ? )]
;   '(lambda () (interactive)
;      (toggle-space-is-underscore)
;      (princ (format "toggle-space-is-underscore is now %s" space-is-underscore))
;    ))


; ---------------------- unique buffer names ---------------------
; Emacs likes to append "<1>", "<2>", etc to buffer names to make them
; unique.  The numbers are meaningless and ugly.  This is my attempt
; to automate the process of renaming the buffers to use part of their
; path names to make the names both unique and meaningful.

(defun is-buffer-name (name)
  "Return true if the given name is the name of a buffer."
  (if (get-buffer name) t nil))

; this is a list of file names that shouldn't ever be used as the
; name of a buffer because their meaning is very dependent on the
; directory in which they exist; thus my naming routines below
; will prepend at least one level of their path suffix
(setq dependent-names '("Makefile" "Makefile.in" "configure" "configure.in"))

(defun can-be-buffer-name (name)
  "Return true if NAME would be an acceptable buffer name, given
  what the other buffer names currently are."
  (and (not (is-buffer-name name))            ; must be unique
       (not (member name dependent-names)))   ; should convey information
)

(defun split-path-name (name)
  "Return all of the components in the name, as a list of strings.
  For example, input a/b/c yields a list of three strings.  A leading
  / will cause the first string to be empty."
  (let ((ret (split-string name "/")))
    (if (equal (substring name 0 1) "/")
      (cons "" ret)      ; prepend an empty string (`split-string' won't)
      ret                ; leave as-is
    ))
)
;(split-path-name "a/b/c")
;(split-path-name "/a/b/c")

(defun join (list sep)
  "Make a string by joining the elements of LIST together,
  separated by SEP."
  (if (and list (cdr list))
    ; at least two elements
    (concat (car list) sep (join (cdr list) sep))
    ; one or zero elements
    (if list
      (car list)    ; one element, return it
      "")           ; zero elements, return empty string
  ))
;(join '("") "/")
;(join '("a") "/")
;(join '("a" "b") "/")
;(join '("a" "b" "c") "/")
;(join '("" "a" "b" "c") "/")

(defun last-n-elements (list n)
  "Return a list consisting of LIST's last N elements."
  (if (<= (length list) n)
    list           ; list has no more than N elements, return it all
    (last-n-elements (cdr list) n)    ; don't include first element
  ))
;(last-n-elements '(a) 2)
;(last-n-elements '(a b) 2)
;(last-n-elements '(a b c) 2)

(defun make-unique-buffer-name-helper (components n i)
  "Recursive helper for `make-unique-buffer-name'.  I is the number
  of trailing components to try first, N the total number of components
  available."
  (let ((possible (join (last-n-elements components i) "/")))
    (if (can-be-buffer-name possible)
      ; name is unique, use it
      possible
      ; name cannot be used because it's already in use
      (if (= i n)
        ; punt to default
        (generate-new-buffer-name (car (last components)))
        ; recursively consider a longer suffix
        (make-unique-buffer-name-helper components n (+ i 1))
      )
    ))
)

(defun make-unique-buffer-name (filename)
  "Given the name of a file that will be edited, make a name for its
  buffer.  The buffer name will be the smallest suffix of the file's
  fully-qualified path name such that it is unique."
  ; it appears that whenever Emacs calls this, it has already added
  ; the necessary path qualifiers
  (let* ((components (split-path-name filename))
         (n (length components)))
    (make-unique-buffer-name-helper components n 1)
  ))
(make-unique-buffer-name "~/wrk/cplr/elkhound/c/configure")

; This function is by default defined in files.el in the Emacs
; distribution.  I redefine it so I can insert calls to my naming
; functions.
(defun create-file-buffer (filename)
  "Create a suitably named buffer for visiting FILENAME, and return
  it.  This version has been redefined to call `make-unique-buffer-name'."
  ;(princ (concat "create-file-buffer: " filename))
  (let ((name (make-unique-buffer-name filename)))
    (generate-new-buffer name))
)


; ------------------- yes-or-no-p ---------------------
; There are a number of situations where Emacs wants to ask me a question,
; but the answer is always the same (or, it's easy to get the effect of
; the other answer afterwards).  The main example is the question:
;
;   File foo.txt has changed on disk.  Reread from disk?
;
; This question is annoying because it gets asked while I'm moving around
; in a debugger stack trace, and often don't care about the file I happen
; to be at (because I want to move past that frame anyway).  Moreover,
; my F12 binding lets me re-read files with a single keystroke, so if I
; actually *do* want to re-read it's easy to do.

; First, I need the original definition of yes-or-no-p so I can call it
; after I've replaced its definition.  In case .emacs gets re-read
; after startup, avoid losing the original definition.
(if (fboundp 'orig-yes-or-no-p)
  nil        ; it's already bound, don't re-bind
  (fset 'orig-yes-or-no-p (symbol-function 'yes-or-no-p))
)

; Now, define my version in terms of `orig-yes-or-no-p'.
(defun yes-or-no-p (prompt)
  "Ask user a yes-or-no question.  Return t if answer is yes, nil if no.
This is a wrapper around `orig-yes-or-no'."
  (if (string-match
       ; This message is created in lisp/files.el, and there are four
       ; variations.  I'm intentionally matching two of them.
       "File .* changed on disk.  Reread from disk"
       prompt)

    ; it's that question; the answer is no, but I *do* want to know 
    ; that it has changed
    (progn (message "Note: File has changed on disk.") nil)

    ; it's a different question; for now, just ask me; I'll probably
    ; add more patterns to the above as I think of other questions that
    ; I don't want asked
    (orig-yes-or-no-p prompt)
  )
)


; ------------------- cyclic insertion ------------------
; The `insert-from-cycle' command lets me paste several pieces of text
; in a cycle.  To use it, copy the definition of `insert-cycle-list'
; into the scratch buffer, change its contents, and eval the setq.

; current element to insert
(setq insert-cycle-index 0)

; list of elements
(setq insert-cycle-list (list "<!--\n" "-->\n"))

; insert and cycle
(defun insert-from-cycle()
  "Insert text from a cyclic list."
  (interactive)
  (if (>= insert-cycle-index (length insert-cycle-list))
    (setq insert-cycle-index 0))
  (insert (nth insert-cycle-index insert-cycle-list))
  (setq insert-cycle-index (+ 1 insert-cycle-index))
)


; ----------------------- dired fix ---------------------
(require 'dired)

; this expands the regexp to handle YYYY-MM-DD date format
(setq dired-move-to-filename-regexp
  (let* ((l "\\([A-Za-z]\\|[^\0-\177]\\)")
         ;; In some locales, month abbreviations are as short as 2 letters,
         ;; and they can be padded on the right with spaces.
         ;; weiand: changed: month ends potentially with . or , or .,
;;old    (month (concat l l "+ *"))
         (month (concat l l "+[.]?,? *"))
         ;; Recognize any non-ASCII character.
         ;; The purpose is to match a Kanji character.
         (k "[^\0-\177]")
         ;; (k "[^\x00-\x7f\x80-\xff]")
         (s " ")
         (yyyy "[0-9][0-9][0-9][0-9]")
         (mm "[ 0-1][0-9]")
;;old    (dd "[ 0-3][0-9]")
         (dd "[ 0-3][0-9][.]?")
         (HH:MM "[ 0-2][0-9]:[0-5][0-9]")

         ; sm: I added these
         (YYYY-MM-DD "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
         (intl-date (concat YYYY-MM-DD s HH:MM))

         (western (concat "\\(" month s dd "\\|" dd s month "\\)"
         ;; weiand: changed: year potentially unaligned
;;old                     s "\\(" HH:MM "\\|" s yyyy "\\|" yyyy s "\\)"))
;;sm:old                  s "\\(" HH:MM "\\|" s "?" yyyy "\\|" yyyy s "\\)"))
                          s "\\(" HH:MM "\\|" s "?" yyyy "\\|" yyyy s "\\)"))
         (japanese (concat mm k s dd k s "\\(" s HH:MM "\\|" yyyy k "\\)")))
         ;; The "[0-9]" below requires the previous column to end in a digit.
         ;; This avoids recognizing `1 may 1997' as a date in the line:
         ;; -r--r--r--   1 may      1997        1168 Oct 19 16:49 README
         ;; The ".*" below finds the last match if there are multiple matches.
         ;; This avoids recognizing `jservice  10  1024' as a date in the line:
         ;; drwxr-xr-x  3 jservice  10  1024 Jul  2  1997 esg-host
    (concat ".*[0-9]" s "\\(" western "\\|" japanese "\\|" intl-date "\\)" s))
)


; --------------------- SCCS -----------------------
(defun checkout-this-file ()
  "Check out from SCCS the current buffer's file for editing."
  (interactive)
  (bk-edit)
)

(defun bk-edit ()
  "Run bk edit on this file."
  (interactive)
  (call-program-then-revert (list "bk" "edit"))
)

(defun bk-unedit ()
  "Run bk unedit on this file."
  (interactive)
  (call-program-then-revert (list "bk" "unedit"))
)

(defun call-program-then-revert (command)
  "Run `command', which is a list of strings (beginning with program "
  "name), then revert the current buffer.  The current file's name "
  "is appended to the command before running it."

  (let* ((name (basename (buffer-file-name)))        ; name w/o path
         (full-cmd (append command (list name)))     ; command with appended arg
         ; call-process arguments:
         ;   arg 1: command to run
         ;   arg 2: input file, nil means /dev/null
         ;   arg 3: output file, nil means /dev/null
         ;   arg 4: argv[0] (usually same as arg 1)
         ;   arg 5: argv[1]
         ;    ...
         ;   returns exit status or signal string
         (dummy (princ (concat "running " (concat-with-spaces full-cmd)  " ...")))
         (result (apply 'call-process "runlog" nil nil "runlog" full-cmd))
        )
    (if (not (equal result '0))
      (error (concat "call-process failed: " result))
      (progn
        (princ " done")
        (revert-buffer-yes)
      ))
  ))

(defun concat-with-spaces (list)
  "Concatenate the strings in `list' with spaces in between."
  (cond ((not list) "")
        ((not (cdr list)) (car list))
        (t (concat (car list) " " (concat-with-spaces (cdr list))))
  ))

(defun bk-command (command extension jump)
  "Run bk `command' on the current file, and open a new buffer to hold "
  "its output, which goes in tmp/<buffer-name>.`extension'.  If `jump' "
  "is true, then jump to the same line in the output as the current "
  "line of the input."

  (let* ( ; name of current buffer w/o path
          (name (basename (buffer-file-name)))
          ; current line number
          (curline (current-line))
          ; output filename
          (outfname (concat "tmp/" name "." extension))
          ; command to run
          (cmd (concat "mkdir tmp; bk " command " " name " >" outfname))
          ; debug print
          (dummy (princ (concat "running " cmd " ...")))
          ; run it
          (result (call-process "runlog" nil nil "runlog" "/bin/sh" "-c" cmd))
        )
    (if (not (equal result '0))
      (error (concat "call-process failed: " result))
      (progn
        (find-file outfname)
        (if jump
          (goto-line curline))
      )
    )
  )
)

(defun bk-annotate ()
  "Run bk annotate on the current file, and open a new buffer to hold "
  "its output."
  (interactive)
  (bk-command "annotate -a -u -m" "ann" 't)
)

(defun bk-diffs ()
  "Run bk diffs on the current file, and open a new buffer to hold "
  "its output."
  (interactive)
  (bk-command "diffs -u" "diff" nil)
)


; --------------------- key bindings --------------------
; key-binding syntax from:
;   http://www.linuxgazette.com/issue31/marsden.html
; it's mostly self-evident from below; the one tricky thing is
; certain characters like '[' need to be escaped, and that is
; done with the '?' character -- so, e.g. ?[ names the '[' key

; some of the bindings have a comment "for xterm" -- these are
; extra bindings for when running emacs inside a terminal emulator
; (like xterm) instead of X windows, because in that situation some
; key combinations don't make it through the terminal to emacs


; for some keys, making a new binding requires first unbinding the
; old one
;(global-unset-key [(control c)])
;(local-unset-key [(meta g)])

; argh, can't unbind these!!  it just doesn't work!
;(global-unset-key [(meta 8)])
;(local-unset-key [(meta 8)])


; as an experiment, I'm going to arrange all the key bindings into
; a giant menu definition
;(setq my-bindings-menu '(
;  (menu "&File" "Commands that apply to whole files"
;    (item "&New")
;    (item "&Open..." find-file                           [f3])
;    (item "Open file at cursor" find-file-at-point       [(control i)])
;    (item "&Save" save-buffer                            [f2] [(control o)])
;    (item "Save &As..." write-buffer                     [(control f2)])
;    (item "Save a&ll")
;    (item "&Close" kill-buffer                           [f4] [(control f4)])
;    (separator)
;    (item "Revert to saved" revert-buffer-yes            [f12])
;    (item "Finish server edit" my-server-edit            [(control x) (?#)])
;    (separator)
;    (item "Remove all tabs" untabify-buffer              ) ; [f9])
;    (item "Spaces -> tabs" tabify-buffer                 ) ; [(control f9)])
;    (separator)
;    (item "Buffer list..." usable-buffer-list            [(control tab)] [f8])
;    (separator)
;    (item "E&xit" save-buffers-kill-emacs                [(meta f4)])
;  )
;
;  (menu "&View" "Options for how files appear onscreen"
;    (item "Toggle syntax highlighting" font-lock-mode)
;;    (item "Toggle whether tabs are highlighted" highlight-tabs-toggle  [(control f10)])
;    (item "Refontify" my-font-lock-fontify-buffer        [f10])
;  )
;
;  (menu "&Edit" "Commands to manipulate text"
;    (item "&Undo" undo                         [(meta backspace)] [(control backspace)])
;    (item "&Redo")
;    (separator)
;    (item "Cu&t" my-cut-cmd                    [(shift delete)])
;    (item "&Copy" my-copy-cmd                  [(control insert)] [(control c)])
;    (item "&Paste" my-paste-cmd                [(shift insert)] [(control y)] [(control v)])
;    (item "Paste from cycle" insert-from-cycle [(meta control v)])
;    (item "C&lear" my-delete-func              [delete] [(control d)])
;    (item "Kill line (or cut)" my-ctl-k        [(control k)])
;    (item "&Select all")
;    (separator)
;    (item "Copy to other window" copy-to-other     [(meta c)])
;    (item "Move to other window" move-to-other     [(meta m)])
;    (item "Take from other window" take-from-other [(meta t)])
;    (separator)
;    (item "Indent selected" my-indent          [(meta right)])
;    (item "Outdent selected" my-outdent        [(meta left)])
;    (separator)
;    (item "Reformat paragraph" fill-paragraph  [(control j)])
;    (separator)
;    (item "Apply program to selected" apply-filter-program     [(meta a)])
;    (item "Apply ./cvt.pl to sel/line" apply-cvt-filter        [(meta z)])
;    (item "Make name into HREF" make-name-into-href            [(control meta l)])
;    (item "Surround with <tt>" surround-with-tt                [(control meta t)])
;    (separator)
;    (menu "Typing" "Typing letters, or change the way typing behaves"
;      (item "Toggle insert / overwrite" overwrite-mode         [(meta insert)])
;      (separator)
;      (item "Go to end of next line" go-to-end-of-next-line    [(control return)])
;      (separator)
;      (item "Insert Tab character" my-insert-tab               [tab])
;      (item "Insert Newline character" auto-align-newline      [13])
;      (separator)
;      (item "Insert &date / time" insert-time-string           [(meta d)])
;      (item "Insert random hex integer" insert-random-hex-integer      [(control meta r)])
;      (separator)
;;      (item "Type a space or underscore" my-spacebar           [? ])
;      (item "Toggle whether spacebar types underscores"
;            toggle-space-is-underscore                         [(control ? )])
;    )
;    (item "Run previous M-x command" run-previous-M-x-command  [(control meta p)])
;  )
;
;  (menu "&Search" "Navigating in files"
;    (item "&Find..." isearch-forward                [(control s)])
;    (item "&Regexp Find..." search-forward-regexp   [(meta s)])
;    (item "Find backwards..." isearch-backward      [(control shift s)] [(control meta s)])
;    (item "&Search again")
;    (separator)
;    (item "&Replace..." query-replace               [(control r)])
;    (item "Replace regexp..." query-replace-regexp  [(meta r)])
;    (separator)
;    (item "&Goto line..." goto-line                 [(meta g)])
;    (item "Grep for sel/word" grep-sources          [(meta control g)])
;    (item "Find matching delimiter" find-matching-keyword   [(meta ?[)] [(meta ?])])
;    (separator)
;    (menu "Cursor movement" "Moving the cursor in a file"
;      (menu "Ordinary" "Moves the cursor; text is unselected"
;        (item "Left" move-left                [left]    [(control b)])
;        (item "Right" move-right              [right]   [(control f)])
;        (item "Up" move-up                    [up]      [(control p)])
;        (item "Down" move-down                [down]    [(control n)])
;        (item "Beginning of line" move-home   [home]    [(control a)])
;        (item "End of line" move-end          [end]     [(control e)])
;        (item "Page up" my-page-up            [prior]   [(meta v)])
;        (item "Page down" my-page-down        [next])
;        (item "Top of file" move-top          [(control prior)])
;        (item "End of file" move-bottom       [(control next)])
;      )
;      (menu "Selecting" "Selects text while moving"
;        (item "Left" move-select-left                [(shift left)])
;        (item "Right" move-select-right              [(shift right)])
;        (item "Up" move-select-up                    [(shift up)])
;        (item "Down" move-select-down                [(shift down)])
;        (item "Beginning of line" move-select-home   [(shift home)])
;        (item "End of line" move-select-end          [(shift end)])
;        (item "Page up" move-select-pgup             [(shift prior)])
;        (item "Page down" move-select-pgdn           [(shift next)])
;        (item "Top of file" move-select-top          [(shift control prior)])
;        (item "End of file" move-select-bottom       [(shift control next)])
;      )
;      (menu "Scrolling" "Change which part of the file shows up in the window"
;        (item "Up" scroll-down-1                        [(control w)] [(control up)])
;        (item "Down" scroll-up-1                        [(control z)] [(control down)])
;        (item "Left" scroll-right-1                     [(control left)])
;        (item "Right" scroll-left-1                     [(control right)])
;        (item "Up several lines" scroll-down-several    [(control shift up)])
;        (item "Down several lines" scroll-up-several    [(control shift down)])
;        (item "Left several cols" scroll-right-several  [(control shift left)])
;        (item "Right several cols" scroll-left-several  [(control shift right)])
;      )
;    )
;  )
;
;  (menu "Compile" "Compiling under Emacs"
;    (item "Compile" my-compile                [(meta f9)] [f9])
;    (item "Compile this module" my-compile-this-module   [(control f9)])
;    (item "Evaluate expression" eval-defun    [(meta control z)])
;    (separator)
;    (item "&Previous message" previous-error  [(meta f7)])
;    (item "&Next message" next-error          [(meta f8)])
;    (separator)
;    (item "Find tag" find-tag                 [(meta ?.)])
;    (item "Next tag" find-next-tag            [(meta ?,)])
;    (item "Back from tag" pop-tag-mark        [(meta ?/)])
;    (item "Pick tag file" visit-tags-table)
;    (separator)
;    (item "Start debugger" gdb-no-args        [(meta f10)])
;    (item "Set breakpoint" gud-break          [(meta f2)])
;    ;(item "Run to this point" ???            [(meta f3)])
;    (item "Continue execution" gud-cont       [(f7)])
;  )
;
;  (menu "Window" "Panes and X Windows"
;    (item "Horizontal divider" split-window-vertically    [(control -)])
;    (item "Vertical divider" split-window-horizontally    [(control \\)])
;    (separator)
;    (item "Enlarge pane" enlarge-window-both-ways         [(control ^)])
;    (item "Remove pane" delete-window                     [(meta control d)])
;    (separator)
;    (item "Switch pane" other-window-or-buffer-toggle     [f6] [(control =)])
;    (item "Maximize or restore pane" intended-mdi-maximize-restore-toggle   [f5])
;    (separator)
;    (item "New X Window" new-frame)
;    (item "Occupy left half-screen" frame-occupy-left-half-screen   [(control meta left)])
;    (item "Occupy right half-screen" frame-occupy-right-half-screen [(control meta right)])
;    (item "Occupy most of screen" frame-occupy-normal-position      [(control meta up)])
;  )
;))

; cause the above menu to take effect
;(install-keybindings-menu my-bindings-menu)


; smaller version for testing
;(setq small-bindings-menu '(
;  (menu "&File" "Commands that apply to whole files"
;    (item "&New")
;    (item "&Open..." find-file                           [f3])
;  )
;))
;(install-keybindings-menu small-bindings-menu)


; ---- bindings local to modes ----
(define-key minibuffer-local-completion-map [tab] 'minibuffer-complete)


; ---- *un*bindings ----
; unbind some keys I don't want bound to anything, since I hit them
; by mistake often
;(global-unset-key [(control u)])                ; reserve C-u until I find a suitable purpose
;(global-unset-key [(control t)])                ; reserved
;(global-unset-key [insert])
;(global-unset-key [(control x) (control z)])   ; normally suspends or minimizes emacs (!)
;(global-unset-key [(control ?0)])    ; <sarcasm>good to know that the "better" keybinding
;(global-unset-key [(control ?1)])    ; syntax has its own f-ing peculiarities too</sarcasm>
;(global-unset-key [(control ?2)])
;(global-unset-key [(control ?3)])
;(global-unset-key [(control ?4)])
;(global-unset-key [(control ?5)])
;(global-unset-key [(control ?6)])
;(global-unset-key [(control ?7)])
;(global-unset-key [(control ?8)])
;(global-unset-key [(control ?9)])
;(global-set-key [(control x) (control x)] 'no-op)
;(global-set-key [(control /)] '(lambda () (interactive)     ; normally undo
;  (princ "Use Alt-Backsp or Ctl-Backsp for Undo")))


;(global-set-key [f12] '(lambda () (interactive)
;  (noninteractive-replace "foo" "bar")))




; the actual M-x customize stuff has been moved to
; "~/elisp/cust.settings.el" so that the majority of this
; file can be independent of personal preferences
