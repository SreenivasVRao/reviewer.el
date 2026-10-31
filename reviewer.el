;;; reviewer.el --- Annotate files and diffs, render as org for agent handoff  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; URL: https://github.com/SreenivasVRao/reviewer.el
;; Package-Requires: ((emacs "29.1") (transient "0.4.0"))

;;; Commentary:

;; reviewer-mode is a minor mode for annotating a buffer (a file or a
;; diff) without changing its content.  C-c r a creates an annotation
;; on the active region (or the word at point), and edits the
;; annotation under point if one already exists; clearing the text
;; deletes it.  C-c r R adds a reply to the thread at point instead of
;; replacing the note.
;;
;; Annotations are overlays, session-local only -- no persistence yet.
;; The data itself is just a buffer position range plus a note, author,
;; timestamp, status, thread id, and a list of replies, kept
;; deliberately plain so it can be serialized later without a redesign.
;; The thread id in particular is meant to outlive the overlay: once
;; persistence lands, it's what lets an annotation on a since-deleted
;; region still be found.
;;
;; Rendering annotations (file, line number(s), quoted span, note) as
;; org for handing off to an agent is a separate, not-yet-written piece
;; built on top of `reviewer-all-annotations'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'thingatpt)
(require 'org)
(require 'transient)
(require 'color)
(require 'project)

(defgroup reviewer nil
  "Annotate buffers without changing their content."
  :group 'convenience)

(defface reviewer-highlight-face
  '((t :inherit highlight))
  "Face used to highlight an annotated region."
  :group 'reviewer)

(defcustom reviewer-confirm-before-clear t
  "Whether to prompt for confirmation before a bulk annotation delete.
Guards `reviewer-clear-annotations', `reviewer-clear-project-annotations',
and `reviewer-delete-region-annotations' -- the commands that can delete
more than one annotation at once.  Set to nil to skip the prompt."
  :type 'boolean
  :group 'reviewer)

(defun reviewer--annotation-p (overlay)
  "Return non-nil if OVERLAY is a reviewer annotation."
  (overlay-get overlay 'reviewer-note))

(defun reviewer--annotation-at (pos)
  "Return the reviewer annotation overlay at POS, if any."
  (seq-find #'reviewer--annotation-p (overlays-at pos)))

(defun reviewer-all-annotations ()
  "Return all reviewer annotation overlays in the buffer, sorted by position."
  (sort (seq-filter #'reviewer--annotation-p (overlays-in (point-min) (point-max)))
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun reviewer-annotation-text (overlay)
  "Return the note text stored on annotation OVERLAY."
  (overlay-get overlay 'reviewer-note))

(defun reviewer-annotation-author (overlay)
  "Return the author who created annotation OVERLAY."
  (overlay-get overlay 'reviewer-author))

(defun reviewer-annotation-time (overlay)
  "Return the creation time of annotation OVERLAY, as a string."
  (overlay-get overlay 'reviewer-time))

(defun reviewer-annotation-status (overlay)
  "Return the status of annotation OVERLAY, e.g. \"open\" or \"resolved\"."
  (overlay-get overlay 'reviewer-status))

(defun reviewer-annotation-thread-id (overlay)
  "Return the thread id of annotation OVERLAY.
Assigned once, when the annotation is created, and stable across
edits -- meant to keep identifying an annotation even after its region
is deleted, once that's persisted."
  (overlay-get overlay 'reviewer-thread-id))

(defun reviewer-annotation-replies (overlay)
  "Return the replies on annotation OVERLAY's thread, oldest first.
Each reply is a plist with :author, :time, and :text."
  (overlay-get overlay 'reviewer-replies))

(defun reviewer--region-or-word-bounds ()
  "Return (START . END) for the active region, or the word at point."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'word)))

(defconst reviewer--edit-buffer-name "*reviewer-annotation*"
  "Name of the buffer used to edit an annotation's note text.")

(defvar reviewer-annotation-edit-buffer-action
  '((display-buffer-reuse-window display-buffer-below-selected)
    (window-height . 20))
  "Action used to display the annotation edit buffer, unless overridden.
Add an entry keyed on `reviewer--edit-buffer-name' to
`display-buffer-alist' to customize placement instead; that takes
precedence over this.")

(defvar-local reviewer--edit-target nil
  "What `reviewer-annotation-edit-finish' saves this buffer's text onto.
One of: the overlay of an existing annotation being edited; a list of
\(BUFFER START END\) describing where to create a new one; or a cons
of (reply . OVERLAY) to append a reply to an existing thread.")

(defvar-local reviewer--edit-window-config nil
  "Window configuration to restore when this edit buffer is closed.")

(defun reviewer--edit-cleanup (buffer wconf)
  "Restore WCONF and kill BUFFER, the annotation edit buffer."
  (set-window-configuration wconf)
  (kill-buffer buffer))

(defun reviewer--generate-thread-id ()
  "Return a new, likely-unique id for an annotation thread."
  (format "%s-%04x" (format-time-string "%Y%m%dT%H%M%S") (random #x10000)))

(defvar reviewer-annotation-change-hook nil
  "Hook run after an annotation is created, edited, replied to, or deleted.
Runs with no arguments and carries no information about which
annotation or buffer changed; a listener that needs to know should
just re-derive it, e.g. via `reviewer-all-annotations' across
`(buffer-list)'.")

(defun reviewer--append-reply (overlay author text)
  "Append a reply by AUTHOR with TEXT to annotation OVERLAY's thread."
  (overlay-put overlay 'reviewer-replies
               (append (overlay-get overlay 'reviewer-replies)
                       (list (list :author author
                                   :time (format-time-string "%H:%M")
                                   :text text))))
  (run-hooks 'reviewer-annotation-change-hook))

(defun reviewer-annotation-edit-finish ()
  "Save this buffer's text onto the annotation being edited, then close it.
Deletes the annotation instead if the text was left empty; a reply
left empty is simply dropped instead."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (target reviewer--edit-target))
    (reviewer--edit-cleanup (current-buffer) reviewer--edit-window-config)
    (cond
     ((and (consp target) (eq (car target) 'reply))
      (unless (string-empty-p text)
        (reviewer--append-reply (cdr target) (user-login-name) text)))
     ((overlayp target)
      (if (string-empty-p text)
          (delete-overlay target)
        (overlay-put target 'reviewer-note text)))
     (t
      (unless (string-empty-p text)
        (seq-let (buffer start end) target
          (with-current-buffer buffer
            (let ((ov (make-overlay start end)))
              (overlay-put ov 'reviewer-note text)
              (overlay-put ov 'reviewer-author (user-login-name))
              (overlay-put ov 'reviewer-time (format-time-string "%H:%M"))
              (overlay-put ov 'reviewer-status "open")
              (overlay-put ov 'reviewer-thread-id (reviewer--generate-thread-id))
              (overlay-put ov 'face 'reviewer-highlight-face)
              (overlay-put ov 'evaporate t))
            (deactivate-mark))))))
    (run-hooks 'reviewer-annotation-change-hook)))

(defun reviewer-annotation-edit-abort ()
  "Abandon this annotation edit without changing anything."
  (interactive)
  (reviewer--edit-cleanup (current-buffer) reviewer--edit-window-config))

(defvar reviewer-annotation-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'reviewer-annotation-edit-finish)
    (define-key map (kbd "C-c C-k") #'reviewer-annotation-edit-abort)
    map)
  "Keymap for `reviewer-annotation-edit-mode'.")

(define-derived-mode reviewer-annotation-edit-mode text-mode "Reviewer-Edit"
  "Major mode for editing a `reviewer-mode' annotation's note text.
\\{reviewer-annotation-edit-mode-map}"
  (setq header-line-format
        (substitute-command-keys
         (concat "Edit annotation.  "
                 "\\[reviewer-annotation-edit-finish] to save, "
                 "\\[reviewer-annotation-edit-abort] to cancel."))))

(defun reviewer--open-edit-buffer (target initial-text)
  "Pop up the annotation edit buffer for TARGET, pre-filled with INITIAL-TEXT.
See `reviewer--edit-target' for the shapes TARGET can take."
  (let ((wconf (current-window-configuration)))
    (pop-to-buffer (get-buffer-create reviewer--edit-buffer-name)
                    reviewer-annotation-edit-buffer-action)
    (reviewer-annotation-edit-mode)
    (erase-buffer)
    (when initial-text (insert initial-text))
    (goto-char (point-max))
    (setq reviewer--edit-target target)
    (setq reviewer--edit-window-config wconf)))

(defun reviewer--create-annotation ()
  "Open the edit buffer to create a new annotation on the region/word."
  (let ((bounds (reviewer--region-or-word-bounds)))
    (unless bounds
      (user-error "No region or word at point to annotate"))
    (reviewer--open-edit-buffer (list (current-buffer) (car bounds) (cdr bounds)) nil)))

(defun reviewer--edit-annotation (overlay)
  "Open the edit buffer to edit the note text on annotation OVERLAY."
  (reviewer--open-edit-buffer overlay (overlay-get overlay 'reviewer-note)))

(defun reviewer-annotate ()
  "Create an annotation on the active region/word, or edit the one at point."
  (interactive)
  (let ((existing (reviewer--annotation-at (point))))
    (if existing
        (reviewer--edit-annotation existing)
      (reviewer--create-annotation))))

(defun reviewer-reply-to-annotation ()
  "Add a reply to the annotation thread at point."
  (interactive)
  (let ((ov (reviewer--annotation-at (point))))
    (unless ov
      (user-error "No annotation at point"))
    (reviewer--open-edit-buffer (cons 'reply ov) nil)))

(defun reviewer-clear-annotations ()
  "Delete all annotations in the current buffer."
  (interactive)
  (let ((annotations (reviewer-all-annotations)))
    (unless annotations
      (user-error "No annotations in this buffer"))
    (when (or (not reviewer-confirm-before-clear)
              (y-or-n-p (format "Delete %d annotation%s in this buffer? "
                                 (length annotations) (if (= (length annotations) 1) "" "s"))))
      (mapc #'delete-overlay annotations)
      (run-hooks 'reviewer-annotation-change-hook))))

(defun reviewer-clear-project-annotations ()
  "Delete all annotations in the current project."
  (interactive)
  (let ((root (reviewer--project-root)))
    (unless root
      (user-error "No project found (Projectile/project.el) for this buffer"))
    (let* ((buffers (reviewer--project-buffers root))
           (annotations (mapcan (lambda (buffer)
                                  (with-current-buffer buffer
                                    (reviewer-all-annotations)))
                                buffers)))
      (unless annotations
        (user-error "No annotations in any buffer under %s" root))
      (when (or (not reviewer-confirm-before-clear)
                (y-or-n-p (format "Delete %d annotation%s across project %s? "
                                   (length annotations) (if (= (length annotations) 1) "" "s") root)))
        (mapc #'delete-overlay annotations)
        (run-hooks 'reviewer-annotation-change-hook)
        (message "Cleared %d project annotation%s"
                 (length annotations) (if (= (length annotations) 1) "" "s"))))))

(defun reviewer-delete-annotation-at-point ()
  "Delete the annotation at point."
  (interactive)
  (let ((ov (reviewer--annotation-at (point))))
    (unless ov
      (user-error "No annotation at point"))
    (delete-overlay ov)
    (run-hooks 'reviewer-annotation-change-hook)))

(defun reviewer-delete-region-annotations ()
  "Delete all annotations overlapping the active region."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((annotations (seq-filter #'reviewer--annotation-p
                                  (overlays-in (region-beginning) (region-end)))))
    (unless annotations
      (user-error "No annotations in region"))
    (when (or (not reviewer-confirm-before-clear)
              (y-or-n-p (format "Delete %d annotation%s in region? "
                                 (length annotations) (if (= (length annotations) 1) "" "s"))))
      (mapc #'delete-overlay annotations)
      (deactivate-mark)
      (run-hooks 'reviewer-annotation-change-hook))))

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-workable-p "posframe")
(declare-function magit-toplevel "magit-git")
(declare-function magit-file-at-point "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-rev-parse "magit-git")
(declare-function projectile-project-root "projectile")

(defconst reviewer--posframe-buffer " *reviewer-posframe*"
  "Buffer used by posframe to display annotation text.")

(defface reviewer-posframe-face
  '((t :inherit tooltip))
  "Face used for the posframe that displays annotation text."
  :group 'reviewer)

(defface reviewer-posframe-border-face
  '((t :inherit font-lock-function-name-face))
  "Face whose foreground colors the posframe's border."
  :group 'reviewer)

(defface reviewer-posframe-author-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for the author name in the annotation posframe header."
  :group 'reviewer)

(defface reviewer-posframe-time-face
  '((t :inherit font-lock-comment-face))
  "Face used for the timestamp in the annotation posframe header."
  :group 'reviewer)

(defun reviewer--author-color (author)
  "Return a hex color string deterministically derived from AUTHOR.
Same AUTHOR name always yields the same color, so each participant in
a thread stays visually distinct across annotations without needing
to store a color anywhere.  Saturation and lightness are fixed and
only the hue varies, so every author's color stays readable against
the posframe background."
  (let ((hue (/ (float (mod (sxhash-equal author) 360)) 360.0)))
    (apply #'color-rgb-to-hex (append (color-hsl-to-rgb hue 0.55 0.6) (list 2)))))

(defcustom reviewer-posframe-parameters
  '(:border-width 2 :fringe 16 :width 40 :min-height 5)
  "Geometry of the annotation posframe, as a plist.

:BORDER-WIDTH  width in pixels of the border drawn around the posframe.
:FRINGE        width in pixels of the blank margin posframe leaves on
               the left and right inside the frame's edge -- the same
               idea as a window's fringe, just applied to this popup
               frame instead.
:WIDTH         fixed width of the posframe, in characters; long notes
               wrap instead of widening the frame.
:MIN-HEIGHT    minimum height of the posframe, in lines."
  :type '(plist :key-type (choice (const :border-width) (const :fringe)
                                   (const :width) (const :min-height))
                 :value-type integer)
  :group 'reviewer)

(defun reviewer--format-thread-entry (author time text)
  "Format one thread entry as an \"author (time)\" header plus TEXT.
AUTHOR is colored via `reviewer--author-color' so each participant in
a thread is visually distinct."
  (concat (propertize author 'face (list :inherit 'reviewer-posframe-author-face
                                          :foreground (reviewer--author-color author)))
          " "
          (propertize (format "(%s)" time) 'face 'reviewer-posframe-time-face)
          "\n" text))

(defun reviewer--annotation-content (overlay)
  "Format annotation OVERLAY's whole thread: its note, then any replies."
  (string-join
   (cons (reviewer--format-thread-entry (reviewer-annotation-author overlay)
                                         (reviewer-annotation-time overlay)
                                         (reviewer-annotation-text overlay))
         (mapcar (lambda (reply)
                   (reviewer--format-thread-entry (plist-get reply :author)
                                                   (plist-get reply :time)
                                                   (plist-get reply :text)))
                 (reviewer-annotation-replies overlay)))
   "\n\n"))

(defun reviewer--display-annotation (content)
  "Display CONTENT via posframe if available, otherwise the echo area.
Forces a redisplay first so posframe positions itself against point's
actual on-screen location -- without it, a caller that just moved
point somewhere off-screen (e.g. jumping to an annotation elsewhere in
the buffer) gets a posframe placed using the window's stale,
pre-scroll geometry instead of where point ends up."
  (if (and (require 'posframe nil t) (posframe-workable-p))
      (let ((fringe (plist-get reviewer-posframe-parameters :fringe)))
        (redisplay t)
        (posframe-show reviewer--posframe-buffer
                        :string (format "\n%s\n\n" content)
                        :position (point)
                        :background-color (face-attribute 'reviewer-posframe-face :background nil t)
                        :foreground-color (face-attribute 'reviewer-posframe-face :foreground nil t)
                        :internal-border-width (plist-get reviewer-posframe-parameters :border-width)
                        :internal-border-color (face-attribute 'reviewer-posframe-border-face :foreground nil t)
                        :left-fringe fringe
                        :right-fringe fringe
                        :width (plist-get reviewer-posframe-parameters :width)
                        :min-height (plist-get reviewer-posframe-parameters :min-height)
                        :accept-focus nil))
    (message "%s" content)))

(defun reviewer--hide-annotation-display ()
  "Hide the posframe used to display annotations, if any is showing."
  (when (and (fboundp 'posframe-hide) (get-buffer reviewer--posframe-buffer))
    (posframe-hide reviewer--posframe-buffer)))

(defun reviewer--hide-annotation-display-once ()
  "Hide the annotation posframe, then remove self from `pre-command-hook'."
  (reviewer--hide-annotation-display)
  (remove-hook 'pre-command-hook #'reviewer--hide-annotation-display-once t))

(defun reviewer-show-annotation-at-point ()
  "Display the annotation at point, until the next command."
  (interactive)
  (let ((ov (reviewer--annotation-at (point))))
    (unless ov
      (user-error "No annotation at point"))
    (reviewer--display-annotation (reviewer--annotation-content ov))
    (add-hook 'pre-command-hook #'reviewer--hide-annotation-display-once nil t)))

(defun reviewer--org-lang ()
  "Best-effort org src-block language tag for the current buffer.
Derived by stripping \"-mode\" off `major-mode', which also happens to
be exactly what org needs back to find that mode's font-lock table.
Magit buffers (status, diff, revision, stash, ...) are diff-flavored
under the hood but don't use `diff-mode', so map those to \"diff\"
explicitly."
  (if (derived-mode-p 'magit-mode)
      "diff"
    (string-remove-suffix "-mode" (symbol-name major-mode))))

(defun reviewer--line-range (overlay)
  "Return \"Line N\" or \"Lines N-M\" for OVERLAY's line span."
  (let ((start (line-number-at-pos (overlay-start overlay)))
        (end   (line-number-at-pos (overlay-end overlay))))
    (if (= start end)
        (format "Line %d" start)
      (format "Lines %d-%d" start end))))

(defun reviewer--magit-commit ()
  "Return the commit hash for the current magit diff/revision buffer, if any."
  (or (bound-and-true-p magit-buffer-revision-hash)
      (bound-and-true-p magit-buffer-revision)))

(defun reviewer--magit-branch ()
  "Return the current branch, or nil if HEAD is detached."
  (and (fboundp 'magit-get-current-branch) (magit-get-current-branch)))

(defun reviewer--magit-head ()
  "Return the commit hash HEAD currently points to."
  (and (fboundp 'magit-rev-parse) (magit-rev-parse "HEAD")))

(defun reviewer--magit-file-at (pos)
  "Return the file the diff hunk at POS belongs to, if any."
  (and (fboundp 'magit-file-at-point)
       (save-excursion (goto-char pos) (magit-file-at-point))))

(defun reviewer--render-file (file)
  "Return the /tmp/ path used to persist FILE's rendered review."
  (expand-file-name (format "reviewer-%s.org" (file-name-base file))
                    temporary-file-directory))

(defun reviewer--buffer-label ()
  "Return a human-readable label for the current buffer's annotation source.
Prefers the visited file name; falls back to the magit repo (with the
diffed commit, if any) for magit buffers, then the buffer name."
  (let* ((magit-p (derived-mode-p 'magit-mode))
         (repo    (and magit-p (fboundp 'magit-toplevel) (magit-toplevel)))
         (commit  (and magit-p (reviewer--magit-commit))))
    (cond (buffer-file-name)
          ((and repo commit)
           (format "%s@%s" (directory-file-name repo)
                   (substring commit 0 (min 8 (length commit)))))
          (repo (directory-file-name repo))
          (t (buffer-name)))))

(defun reviewer-render-next-annotation ()
  "Move point to the next annotation heading."
  (interactive)
  (end-of-line)
  (unless (re-search-forward "^\\* " nil t)
    (user-error "No next annotation"))
  (beginning-of-line))

(defun reviewer-render-previous-annotation ()
  "Move point to the previous annotation heading."
  (interactive)
  (beginning-of-line)
  (unless (re-search-backward "^\\* " nil t)
    (user-error "No previous annotation")))

(defvar reviewer-render-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'reviewer-render-next-annotation)
    (define-key map (kbd "p") #'reviewer-render-previous-annotation)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `reviewer-render-mode'.")

(define-minor-mode reviewer-render-mode
  "Minor mode for navigating a reviewer render buffer with `n'/`p'."
  :lighter " Reviewer-Render"
  :keymap reviewer-render-mode-map)

(defvar-local reviewer--render-path nil
  "Path of the /tmp/ org file this render buffer was persisted to.")

(defun reviewer--buffer-entries ()
  "Return this buffer's annotations as a list of plists for rendering.
Each has :range, :text, :thread-id, :author, :time, :note, and
:replies, plus :file -- the diff hunk's file, for a magit buffer's
per-hunk annotations; nil otherwise."
  (let ((magit-p (derived-mode-p 'magit-mode)))
    (mapcar (lambda (ov)
              (list :range     (reviewer--line-range ov)
                    :text      (buffer-substring-no-properties
                                (overlay-start ov) (overlay-end ov))
                    :thread-id (reviewer-annotation-thread-id ov)
                    :author    (reviewer-annotation-author ov)
                    :time      (reviewer-annotation-time ov)
                    :note      (reviewer-annotation-text ov)
                    :replies   (reviewer-annotation-replies ov)
                    :file      (and magit-p (reviewer--magit-file-at
                                              (overlay-start ov)))))
            (reviewer-all-annotations))))

(defun reviewer--flatten-text (text)
  "Collapse TEXT's newlines to spaces, so it prints on a single line."
  (replace-regexp-in-string "\n+" " " text))

(defun reviewer--insert-thread-summary (entry)
  "Insert ENTRY's thread id, then one \"author (time): text\" line per
participant -- the root note, then each reply, oldest first -- as a
quick digest of the whole thread ahead of its full text below."
  (insert (format "threadId: %s\n" (plist-get entry :thread-id)))
  (insert (format "  %s (%s): %s\n" (plist-get entry :author) (plist-get entry :time)
                   (reviewer--flatten-text (plist-get entry :note))))
  (dolist (reply (plist-get entry :replies))
    (insert (format "  %s (%s): %s\n" (plist-get reply :author) (plist-get reply :time)
                     (reviewer--flatten-text (plist-get reply :text)))))
  (insert "\n"))

(defun reviewer--insert-entries (entries lang level)
  "Insert ENTRIES (as returned by `reviewer--buffer-entries') as org headings.
LANG tags each entry's quoted span as an org src block; LEVEL is the
heading depth for each entry's own heading (an integer, 1 for a
top-level `*') -- so a single-buffer render can start entries at the
top level while a multi-buffer one nests them under a `* <buffer>'
heading instead.  The thread-id/participant digest from
`reviewer--insert-thread-summary' is the only rendering of an entry's
note/replies; there's no separate `Note'/`Reply' sub-heading
duplicating that text."
  (let ((stars (make-string level ?*)))
    (dolist (entry entries)
      (insert (format "%s %s\n\n" stars
                       (if (plist-get entry :file)
                           (format "%s: %s" (plist-get entry :file) (plist-get entry :range))
                         (plist-get entry :range))))
      (insert (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n\n" lang (plist-get entry :text)))
      (reviewer--insert-thread-summary entry))))

(defun reviewer--render-to-buffer ()
  "Render this buffer's annotations as org into a `*reviewer:...*' buffer.
Return that buffer, left in `reviewer-render-mode' and also persisted
to /tmp/ as a side effect, with that file's path in the returned
buffer's `reviewer--render-path'.  Signals a `user-error' if there are
no annotations."
  (unless (reviewer-all-annotations)
    (user-error "No annotations in this buffer"))
  ;; Overlay positions only mean something in this buffer, so resolve
  ;; everything derived from them here, before switching to OUT below.
  (let* ((magit-p (derived-mode-p 'magit-mode))
         (repo    (and magit-p (fboundp 'magit-toplevel) (magit-toplevel)))
         (branch  (and magit-p (reviewer--magit-branch)))
         (head    (and magit-p (reviewer--magit-head)))
         (commit  (and magit-p (reviewer--magit-commit)))
         (file    (reviewer--buffer-label))
         (dir     default-directory)
         (lang    (reviewer--org-lang))
         (entries (reviewer--buffer-entries))
         (out     (get-buffer-create (format "*reviewer:%s*" (file-name-nondirectory file)))))
    (with-current-buffer out
      (setq default-directory dir)
      (setq buffer-file-name nil)
      (setq buffer-read-only nil)
      (erase-buffer)
      (org-mode)
      ;; default-directory intentionally doesn't match this buffer's
      ;; (nonexistent) file, so path-based modeline styles would render
      ;; garbage; fall back to a plain buffer-name display if available.
      ;; Set after `org-mode' since major modes wipe non-permanent
      ;; buffer-local variables via `kill-all-local-variables'.
      (when (boundp 'doom-modeline-buffer-file-name-style)
        (setq-local doom-modeline-buffer-file-name-style 'buffer-name))
      (insert "#+STARTUP: showall\n")
      (insert (format "#+TITLE: Review: %s\n" file))
      (when repo (insert (format "#+REPO: %s\n" repo)))
      (when branch (insert (format "#+BRANCH: %s\n" branch)))
      (when head (insert (format "#+HEAD: %s\n" head)))
      (when commit (insert (format "#+COMMIT: %s\n" commit)))
      (insert "\n")
      (reviewer--insert-entries entries lang 1)
      (setq reviewer--render-path (reviewer--render-file file))
      (write-region (point-min) (point-max) reviewer--render-path)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (reviewer-render-mode 1))
    out))

(defun reviewer--project-root ()
  "Return the current buffer's project root, or nil if it has none.
Prefers Projectile when available and it finds one; falls back to
`project.el'."
  (or (and (fboundp 'projectile-project-root) (projectile-project-root))
      (when-let* ((proj (project-current)))
        (expand-file-name (project-root proj)))))

(defun reviewer--project-buffers (root)
  "Return live buffers under project ROOT with at least one annotation.
A buffer counts as under ROOT if its visited file is, or -- for a
buffer with no file, e.g. a magit diff -- its `default-directory' is."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (and (reviewer-all-annotations)
                       (file-in-directory-p (or buffer-file-name default-directory) root))))
              (buffer-list)))

(defun reviewer--project-buffer-heading ()
  "Return this buffer's `* ...' heading label for the project-wide render.
Its file path if it has one; otherwise \"buffer:<name>\" -- e.g. for a
magit diff or a scratch buffer -- so a project export's headings, file
paths and buffer names mixed together, can still tell the two apart at
a glance."
  (or buffer-file-name (format "buffer:%s" (buffer-name))))

(defun reviewer--render-project-to-buffer ()
  "Render every annotated buffer under the current project as one org buffer.
One `* <buffer label>' heading per buffer, its own annotations nested
under that.  Return that buffer, as `reviewer--render-to-buffer' does
for a single buffer.  Signals a `user-error' if there's no current
project, or no annotations under it."
  (let ((root (reviewer--project-root)))
    (unless root
      (user-error "No project found (Projectile/project.el) for this buffer"))
    (let ((buffers (reviewer--project-buffers root))
          (label (directory-file-name root)))
      (unless buffers
        (user-error "No annotations in any buffer under %s" root))
      (let ((out (get-buffer-create (format "*reviewer:%s*" (file-name-nondirectory label)))))
        (with-current-buffer out
          (setq default-directory root)
          (setq buffer-file-name nil)
          (setq buffer-read-only nil)
          (erase-buffer)
          (org-mode)
          (when (boundp 'doom-modeline-buffer-file-name-style)
            (setq-local doom-modeline-buffer-file-name-style 'buffer-name))
          (insert "#+STARTUP: showall\n")
          (insert (format "#+TITLE: Review: %s\n\n" label))
          (dolist (buf buffers)
            (let (buf-label lang entries)
              (with-current-buffer buf
                (setq buf-label (reviewer--project-buffer-heading))
                (setq lang (reviewer--org-lang))
                (setq entries (reviewer--buffer-entries)))
              (insert (format "* %s\n\n" buf-label))
              (reviewer--insert-entries entries lang 2)))
          (setq reviewer--render-path (reviewer--render-file label))
          (write-region (point-min) (point-max) reviewer--render-path)
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (reviewer-render-mode 1))
        out))))

(defun reviewer-render ()
  "Render all annotations in the current buffer as an org file in /tmp/."
  (interactive)
  (pop-to-buffer (reviewer--render-to-buffer)))

(defun reviewer-yank-render ()
  "Render annotations into a hidden buffer and copy the result to the kill ring.
Unlike `reviewer-render', the render buffer is created but not displayed."
  (interactive)
  (let ((out (reviewer--render-to-buffer)))
    (kill-new (with-current-buffer out (buffer-string)))
    (message "Rendered annotations copied to kill ring")))

(defun reviewer-yank-render-file-name ()
  "Render annotations to /tmp/ and copy the render file's path to the kill ring.
Unlike `reviewer-render', the render buffer is created but not displayed."
  (interactive)
  (let* ((out  (reviewer--render-to-buffer))
         (path (buffer-local-value 'reviewer--render-path out)))
    (kill-new path)
    (message "Copied as %s" path)))

(defun reviewer-render-project ()
  "Render annotations across the current project as an org file in /tmp/.
See `reviewer--project-root' for how the project is found."
  (interactive)
  (pop-to-buffer (reviewer--render-project-to-buffer)))

(defun reviewer-yank-render-project ()
  "Render every annotated buffer under the current project into a hidden
buffer and copy the result to the kill ring.
See `reviewer--project-root' for how the project is found."
  (interactive)
  (let ((out (reviewer--render-project-to-buffer)))
    (kill-new (with-current-buffer out (buffer-string)))
    (message "Rendered project annotations copied to kill ring")))

(defun reviewer-yank-render-project-file-name ()
  "Render every annotated buffer under the current project to /tmp/ and
copy the render file's path to the kill ring.
See `reviewer--project-root' for how the project is found."
  (interactive)
  (let* ((out  (reviewer--render-project-to-buffer))
         (path (buffer-local-value 'reviewer--render-path out)))
    (kill-new path)
    (message "Copied as %s" path)))

(defvar reviewer-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'reviewer-annotate)
    (define-key map (kbd "R") #'reviewer-reply-to-annotation)
    (define-key map (kbd "r") #'reviewer-render)
    (define-key map (kbd "s") #'reviewer-show-annotation-at-point)
    (define-key map (kbd "e") #'reviewer-yank-render-file-name)
    (define-key map (kbd "y") #'reviewer-yank-render)
    (define-key map (kbd "P") #'reviewer-render-project)
    (define-key map (kbd "E") #'reviewer-yank-render-project-file-name)
    (define-key map (kbd "Y") #'reviewer-yank-render-project)
    (define-key map (kbd "x") #'reviewer-delete-annotation-at-point)
    (define-key map (kbd "k") #'reviewer-delete-region-annotations)
    (define-key map (kbd "c") #'reviewer-clear-annotations)
    (define-key map (kbd "C") #'reviewer-clear-project-annotations)
    map)
  "Prefix keymap for `reviewer-mode' commands, bound to `C-c r'.
Only used when `reviewer-use-transient' is nil; otherwise `C-c r' is
bound to `reviewer-transient' instead.")

;;;###autoload (autoload 'reviewer-transient "reviewer" nil t)
(transient-define-prefix reviewer-transient ()
  "Transient menu for `reviewer-mode' commands."
  ["reviewer-mode\n"
   ["review"
    ("a" "annotate region"              reviewer-annotate)
    ("R" "reply to annotation"          reviewer-reply-to-annotation)
    ("s" "view annotation"              reviewer-show-annotation-at-point)
    ("x" "delete annotation"            reviewer-delete-annotation-at-point)
    ("k" "clear annotations in region"   reviewer-delete-region-annotations)
    ("c" "clear all annotations in file" reviewer-clear-annotations)]
   ["buffer"
    ("r" "show all annotations"       reviewer-render)
    ("y" "copy annotations"           reviewer-yank-render)
    ("e" "export annotations as file" reviewer-yank-render-file-name)]
   ["project"
    ("P" "show all annotations"       reviewer-render-project)
    ("Y" "copy annotations"           reviewer-yank-render-project)
    ("E" "export annotations as file" reviewer-yank-render-project-file-name)
    ("C" "clear all annotations"              reviewer-clear-project-annotations)]]
  [""])

(defun reviewer--set-use-transient (symbol value)
  "Set SYMBOL to VALUE and rebind `C-c r' in `reviewer-mode-map' accordingly."
  (set-default symbol value)
  (when (boundp 'reviewer-mode-map)
    (define-key reviewer-mode-map (kbd "C-c r")
      (if value #'reviewer-transient reviewer-command-map))))

(defcustom reviewer-use-transient t
  "Whether `C-c r' opens a transient menu of `reviewer-mode' commands.
When nil, `C-c r' is a plain prefix key instead: press `C-c r' then
one of the individual command keys bound in `reviewer-command-map'."
  :type 'boolean
  :group 'reviewer
  :set #'reviewer--set-use-transient)

(defun reviewer--set-enable-treemacs (symbol value)
  "Set SYMBOL to VALUE, requiring `reviewer-treemacs' when VALUE is non-nil.
Loading it is what makes it append its own \"t\" toggle suffix to
`reviewer-transient' -- see `reviewer-treemacs's Commentary.  There's
no matching un-require when VALUE is set back to nil, since Emacs
can't cleanly unload a feature; that suffix, once added, stays added
for the rest of the session."
  (set-default symbol value)
  (when value
    (require 'reviewer-treemacs)))

(defcustom reviewer-enable-treemacs nil
  "Whether to enable the treemacs tree/detail view for `reviewer-mode'.
Treemacs is a much heavier dependency than the mode's other optional
integrations (posframe, magit), so `reviewer-treemacs' stays unloaded
-- and `reviewer-transient' shows no \"t\" toggle suffix for it --
until this is turned on, e.g. via `setopt' or a `:custom' entry in
`use-package'."
  :type 'boolean
  :group 'reviewer
  :set #'reviewer--set-enable-treemacs)

(defvar reviewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r")
      (if reviewer-use-transient #'reviewer-transient reviewer-command-map))
    map)
  "Keymap for `reviewer-mode'.")

;;;###autoload
(define-minor-mode reviewer-mode
  "Minor mode for annotating a buffer without changing its content."
  :lighter " Reviewer"
  :keymap reviewer-mode-map
  (dolist (ov (reviewer-all-annotations))
    (overlay-put ov 'face (when reviewer-mode 'reviewer-highlight-face)))
  (unless reviewer-mode
    (reviewer--hide-annotation-display)))

;;;###autoload
(define-globalized-minor-mode reviewer-global-mode reviewer-mode reviewer-mode
  :group 'reviewer)

(provide 'reviewer)
;;; reviewer.el ends here
