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
;; deletes it.
;;
;; Annotations are overlays, session-local only -- no persistence yet.
;; The data itself is just a buffer position range plus a string, kept
;; deliberately plain so it can be serialized later without a redesign.
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

(defgroup reviewer nil
  "Annotate buffers without changing their content."
  :group 'convenience)

(defface reviewer-highlight-face
  '((t :inherit highlight))
  "Face used to highlight an annotated region.")

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

(defun reviewer--region-or-word-bounds ()
  "Return (START . END) for the active region, or the word at point."
  (if (use-region-p)
      (cons (region-beginning) (region-end))
    (bounds-of-thing-at-point 'word)))

(defun reviewer--create-annotation ()
  "Prompt for a note and create a new annotation overlay."
  (let ((bounds (reviewer--region-or-word-bounds)))
    (unless bounds
      (user-error "No region or word at point to annotate"))
    (let ((note (read-string "Annotation: ")))
      (when (string-empty-p note)
        (user-error "Annotation text can not be empty"))
      (let ((ov (make-overlay (car bounds) (cdr bounds))))
        (overlay-put ov 'reviewer-note note)
        (overlay-put ov 'reviewer-author (user-login-name))
        (overlay-put ov 'reviewer-time (format-time-string "%H:%M"))
        (overlay-put ov 'face 'reviewer-highlight-face)
        (overlay-put ov 'evaporate t))
      (deactivate-mark))))

(defun reviewer--edit-annotation (overlay)
  "Prompt to edit, or delete on empty input, the note on annotation OVERLAY."
  (let ((note (read-string "Annotation: " (overlay-get overlay 'reviewer-note))))
    (if (string-empty-p note)
        (delete-overlay overlay)
      (overlay-put overlay 'reviewer-note note))))

(defun reviewer-annotate ()
  "Create an annotation on the active region/word, or edit the one at point."
  (interactive)
  (let ((existing (reviewer--annotation-at (point))))
    (if existing
        (reviewer--edit-annotation existing)
      (reviewer--create-annotation))))

(defun reviewer-clear-annotations ()
  "Delete all annotations in the current buffer."
  (interactive)
  (let ((annotations (reviewer-all-annotations)))
    (unless annotations
      (user-error "No annotations in this buffer"))
    (mapc #'delete-overlay annotations)))

(defun reviewer-delete-annotation-at-point ()
  "Delete the annotation at point."
  (interactive)
  (let ((ov (reviewer--annotation-at (point))))
    (unless ov
      (user-error "No annotation at point"))
    (delete-overlay ov)))

(defun reviewer-delete-region-annotations ()
  "Delete all annotations overlapping the active region."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((annotations (seq-filter #'reviewer--annotation-p
                                  (overlays-in (region-beginning) (region-end)))))
    (unless annotations
      (user-error "No annotations in region"))
    (mapc #'delete-overlay annotations)
    (deactivate-mark)))

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")
(declare-function posframe-workable-p "posframe")
(declare-function magit-toplevel "magit-git")
(declare-function magit-file-at-point "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-rev-parse "magit-git")

(defconst reviewer--posframe-buffer " *reviewer-posframe*"
  "Buffer used by posframe to display annotation text.")

(defface reviewer-posframe-face
  '((t :inherit tooltip))
  "Face used for the posframe that displays annotation text.")

(defface reviewer-posframe-border-face
  '((t :inherit font-lock-function-name-face))
  "Face whose foreground colors the posframe's border.")

(defface reviewer-posframe-author-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face used for the author name in the annotation posframe header.")

(defface reviewer-posframe-time-face
  '((t :inherit font-lock-comment-face))
  "Face used for the timestamp in the annotation posframe header.")

(defun reviewer--annotation-content (overlay)
  "Format annotation OVERLAY as an \"author (time)\" header plus its note."
  (concat (propertize (reviewer-annotation-author overlay) 'face 'reviewer-posframe-author-face)
          " "
          (propertize (format "(%s)" (reviewer-annotation-time overlay)) 'face 'reviewer-posframe-time-face)
          "\n\n" (reviewer-annotation-text overlay)))

(defun reviewer--display-annotation (content)
  "Display CONTENT via posframe if available, otherwise the echo area."
  (if (and (require 'posframe nil t) (posframe-workable-p))
      (posframe-show reviewer--posframe-buffer
                      :string (format "\n%s\n\n" content)
                      :position (point)
                      :background-color (face-attribute 'reviewer-posframe-face :background nil t)
                      :foreground-color (face-attribute 'reviewer-posframe-face :foreground nil t)
                      :internal-border-width 2
                      :internal-border-color (face-attribute 'reviewer-posframe-border-face :foreground nil t)
                      :left-fringe 16
                      :right-fringe 16
                      :min-width 40
                      :min-height 5
                      :accept-focus nil)
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

(defun reviewer--render-to-buffer ()
  "Render this buffer's annotations as org into a `*reviewer:...*' buffer.
Return that buffer, left in `reviewer-render-mode' and also persisted
to /tmp/ as a side effect, with that file's path in the returned
buffer's `reviewer--render-path'.  Signals a `user-error' if there are
no annotations."
  (let ((annotations (reviewer-all-annotations)))
    (unless annotations
      (user-error "No annotations in this buffer"))
    ;; Overlay positions only mean something in this buffer, so resolve
    ;; everything derived from them here, before switching to OUT below.
    (let* ((magit-p (derived-mode-p 'magit-mode))
           (repo    (and magit-p (fboundp 'magit-toplevel) (magit-toplevel)))
           (branch  (and magit-p (reviewer--magit-branch)))
           (head    (and magit-p (reviewer--magit-head)))
           (commit  (and magit-p (reviewer--magit-commit)))
           (file    (cond (buffer-file-name)
                           ((and repo commit)
                            (format "%s@%s" (directory-file-name repo)
                                    (substring commit 0 (min 8 (length commit)))))
                           (repo (directory-file-name repo))
                           (t (buffer-name))))
           (dir     default-directory)
           (lang    (reviewer--org-lang))
           (entries (mapcar (lambda (ov)
                              (list :range (reviewer--line-range ov)
                                    :text  (buffer-substring-no-properties
                                            (overlay-start ov) (overlay-end ov))
                                    :note  (reviewer-annotation-text ov)
                                    :file  (and magit-p (reviewer--magit-file-at
                                                          (overlay-start ov)))))
                            annotations))
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
        (dolist (entry entries)
          (insert (format "* %s\n\n"
                           (if (plist-get entry :file)
                               (format "%s: %s" (plist-get entry :file) (plist-get entry :range))
                             (plist-get entry :range))))
          (insert (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n\n" lang (plist-get entry :text)))
          (insert (format "** Note\n\n%s\n\n" (plist-get entry :note))))
        (setq reviewer--render-path (reviewer--render-file file))
        (write-region (point-min) (point-max) reviewer--render-path)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t)
        (reviewer-render-mode 1))
      out)))

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
    (message "Render file path copied to kill ring: %s" path)))

(defvar reviewer-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'reviewer-annotate)
    (define-key map (kbd "r") #'reviewer-render)
    (define-key map (kbd "s") #'reviewer-show-annotation-at-point)
    (define-key map (kbd "e") #'reviewer-yank-render-file-name)
    (define-key map (kbd "y") #'reviewer-yank-render)
    (define-key map (kbd "x") #'reviewer-delete-annotation-at-point)
    (define-key map (kbd "k") #'reviewer-delete-region-annotations)
    (define-key map (kbd "c") #'reviewer-clear-annotations)
    map)
  "Prefix keymap for `reviewer-mode' commands, bound to `C-c r'.
Only used when `reviewer-use-transient' is nil; otherwise `C-c r' is
bound to `reviewer-transient' instead.")

;;;###autoload (autoload 'reviewer-transient "reviewer" nil t)
(transient-define-prefix reviewer-transient ()
  "Transient menu for `reviewer-mode' commands."
  ["reviewer-mode\n"
   ["review"
    ("a" "annotate/edit at point" reviewer-annotate)
    ("s" "show at point"         reviewer-show-annotation-at-point)
    ("x" "delete at point"       reviewer-delete-annotation-at-point)
    ("k" "delete in region"      reviewer-delete-region-annotations)
    ("c" "clear all"             reviewer-clear-annotations)]
   ["summarize"
    ("r" "render"                reviewer-render)
    ("y" "yank render"           reviewer-yank-render)
    ("e" "yank render file name" reviewer-yank-render-file-name)]])

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
