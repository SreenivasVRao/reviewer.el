;;; reviewer-treemacs.el --- Treemacs tree/detail view for reviewer.el  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; URL: https://github.com/SreenivasVRao/reviewer.el
;; Package-Requires: ((emacs "29.1") (treemacs "3.0") (reviewer "0"))

;;; Commentary:

;; Prototype of a persistent, two-pane alternative to the posframe popup:
;; a treemacs tree of annotation threads docked to one side of the
;; frame, with a plain detail pane stacked directly beneath it showing
;; the selected thread's full text.  The two windows share a side
;; (`left' or `right', see `reviewer-treemacs-side') at adjacent slots,
;; are marked dedicated, `no-delete-other-windows', and excluded from
;; `other-window' (mirroring vanilla treemacs's own
;; `treemacs-is-never-other-window', on by default here rather than
;; configurable), and are opened, switched to, and closed together by
;; `reviewer-treemacs-toggle' (also bound to "q" in both panes) -- open
;; if not already showing, switch focus there if showing but some
;; other window is selected, close only if a window of the pane is
;; already selected -- so from the user's perspective they're one
;; locked unit, not two independent side windows that happen to be
;; adjacent.
;;
;; Moving point onto a thread -- with n/p, arrows, or a mouse click --
;; previews it in the detail pane immediately, no RET required.  File
;; nodes are just group headers, not independently selectable, so n/p
;; skip over them straight to the next/previous thread; arrows and
;; mouse clicks still land on them as ordinary treemacs branch nodes,
;; e.g. to expand/collapse.  RET jumps to the thread's source location
;; and shows it there too -- highlight, popup -- but by default leaves
;; the tree window selected, so n/p keep driving straight through a
;; string of threads without an explicit switch back; set
;; `reviewer-treemacs-ret-moves-focus' non-nil to have RET switch
;; focus there instead.  "o o" always jumps and switches focus
;; regardless of that setting, "o v"/"o h" open it in a fresh
;; vertical/horizontal split instead, and "o c" jumps then closes the
;; tree/detail pane, mirroring vanilla treemacs's own "o" submap
;; without the file-management variants that don't apply to a thread.
;; "r" on a thread opens a small reply pane, stacked below the other
;; two, to append to it.
;; "q" (or "C-g", bound the same in both panes) kills the tree/detail
;; buffers outright rather than just burying them, so reopening always
;; starts from a clean `treemacs-initialize'.  `reviewer-transient'
;; also gets a "t" suffix for the same toggle.
;;
;; The tree groups every live buffer with at least one annotation --
;; found via `reviewer--project-buffers' and calling
;; `reviewer-all-annotations' in each -- under a file/buffer label from
;; `reviewer-treemacs--buffer-label', with each annotation's overlay
;; itself as the thread node underneath.  Scoped to the project
;; (`reviewer--project-root') of the buffer the pane was opened from,
;; captured once in `reviewer-treemacs--project-root'; opening it from
;; a buffer with no project falls back to every annotated buffer,
;; unscoped.  Rebuilt fresh -- same as toggling the pane closed and
;; back open, expand/collapse state and all -- on
;; `reviewer-annotation-change-hook', so an annotation created,
;; edited, replied to, or deleted anywhere, not just through this
;; pane's own commands, shows up here without waiting on a manual
;; toggle.
;;
;; Not loaded by `reviewer.el' itself -- treemacs is a much heavier
;; dependency than the mode's other optional integrations (posframe,
;; magit), so opt in explicitly with `(require 'reviewer-treemacs)'.

;;; Code:

(require 'reviewer)
(require 'treemacs)
(require 'treemacs-treelib)

(defgroup reviewer-treemacs nil
  "Treemacs-based tree/detail view for `reviewer-mode' annotations."
  :group 'reviewer)

(defcustom reviewer-treemacs-side 'right
  "Which side of the frame the tree/detail pane pair docks to."
  :type '(choice (const :tag "Left" left) (const :tag "Right" right))
  :group 'reviewer-treemacs)

(defcustom reviewer-treemacs-slot 90
  "Side-window slot the tree pane occupies on `reviewer-treemacs-side'.
The detail pane takes the next slot up, the reply pane the one after
that.  Emacs's side windows are addressed by (side . slot), and a
`display-buffer' call that targets an already-occupied (side . slot)
just takes over that window outright -- dedication and
`no-other-window' don't protect against it, since that reuse happens
beneath those checks.  Slot 0 is the overwhelmingly common default
other packages fall back to (e.g. this Emacs's own
`display-buffer-alist' entry routes `*magit: '  buffers to
`(side . right) (slot . 0)'), so this defaults far out of that range;
change it if it still collides with something in your own setup."
  :type 'integer
  :group 'reviewer-treemacs)

(defcustom reviewer-treemacs-width 0.3
  "Width of the tree/detail pane pair.
An integer is an absolute width in columns; a float is a fraction of
the frame's width, e.g. the default 0.3 caps the pair at 30% of the
frame."
  :type '(choice (float :tag "Fraction of frame width")
                  (integer :tag "Columns"))
  :group 'reviewer-treemacs)

(defcustom reviewer-treemacs-summary-length 40
  "Number of characters a thread's tree label is truncated to.
Real `reviewer-mode' annotations have no separate title -- just note
text -- so the tree label is always a summary of the root note, never
the note in full."
  :type 'integer
  :group 'reviewer-treemacs)

(defcustom reviewer-treemacs-ret-moves-focus nil
  "Whether RET on a thread switches focus to its source location.
When nil (the default), RET jumps to the thread's source and shows it
there -- highlight, popup -- but leaves the tree window selected, so
n/p keep driving straight through a string of threads.  When non-nil,
RET instead switches focus there too, the same as \"o o\"."
  :type 'boolean
  :group 'reviewer-treemacs)

(defconst reviewer-treemacs--tree-buffer "*reviewer-tree*"
  "Buffer name for the treemacs tree pane.")

(defconst reviewer-treemacs--detail-buffer-default-name "*reviewer-detail*"
  "Detail pane buffer name used when no thread is selected yet.")

(defvar reviewer-treemacs--detail-buf nil
  "The detail pane's buffer object.
Its name changes to \"reviewer-thread-<key>\" as point moves between
threads -- see `reviewer-treemacs--show-detail' -- so hold onto this
object rather than a fixed name, e.g. via `get-buffer-window'.")

(defun reviewer-treemacs--detail-buffer ()
  "Return the detail pane's buffer, creating it if necessary."
  (unless (buffer-live-p reviewer-treemacs--detail-buf)
    (setq reviewer-treemacs--detail-buf
          (generate-new-buffer reviewer-treemacs--detail-buffer-default-name)))
  reviewer-treemacs--detail-buf)

(defvar reviewer-treemacs--project-root nil
  "Project root the tree pane is scoped to, captured when it's opened.
Nil means show annotations from every live buffer, unscoped -- what
`reviewer-treemacs--open' falls back to when `reviewer--project-root'
finds no project for the buffer it's opened from.")

;; --- annotation data -------------------------------------------------------
;; The tree's data is (LABEL . OVERLAYS) tuples, one per distinct LABEL
;; with at least one annotation: LABEL from `reviewer-treemacs--buffer-label',
;; OVERLAYS the merged `reviewer-all-annotations' of every buffer that
;; maps to it -- so each thread node's underlying item is a real
;; annotation overlay, not a copy of its data.

(defun reviewer-treemacs--buffer-label ()
  "Return this buffer's label for the reviewer tree.
Deliberately its own thing rather than `reviewer--buffer-label' (used
for org rendering, where a full path or repo@commit is more useful):
a magit buffer -- status, diff, revision, log, stash, all diff-flavored
under the hood -- is labeled \"git-diff <repo>:<worktree>\", <repo>
being the repo root's directory name and <worktree> its current
branch, or a short commit hash if HEAD is detached.  Every other
buffer, file-visiting or not, is labeled by its own `buffer-name' --
short rather than a full path, since it only has to be readable in a
narrow side pane."
  (if (derived-mode-p 'magit-mode)
      (let* ((repo (and (fboundp 'magit-toplevel) (magit-toplevel)))
             (commit (reviewer--magit-commit))
             (worktree (or (reviewer--magit-branch)
                           (and commit (substring commit 0 (min 8 (length commit))))
                           "detached")))
        (if repo
            (format "git-diff %s:%s" (file-name-nondirectory (directory-file-name repo)) worktree)
          (buffer-name)))
    (buffer-name)))

(defun reviewer-treemacs--collect-files ()
  "Return (LABEL . OVERLAYS) for every live buffer with annotations.
Buffers that map to the same `reviewer-treemacs--buffer-label' -- e.g.
several magit buffers against the same repo and branch -- are merged
into a single entry rather than colliding as separate tree nodes with
the same key.  Sorted by LABEL for a stable tree order across
rebuilds."
  (let ((buffers (if reviewer-treemacs--project-root
                      (reviewer--project-buffers reviewer-treemacs--project-root)
                    (buffer-list)))
        files)
    (dolist (buf buffers)
      (with-current-buffer buf
        (when-let* ((annotations (reviewer-all-annotations)))
          (let* ((label (reviewer-treemacs--buffer-label))
                 (existing (assoc label files)))
            (if existing
                (setcdr existing (append (cdr existing) annotations))
              (push (cons label annotations) files))))))
    (sort files (lambda (a b) (string< (car a) (car b))))))

;; --- faces ---------------------------------------------------------------

(defface reviewer-treemacs-file-face
  '((t :inherit font-lock-variable-name-face :weight bold))
  "Face used for file labels in the reviewer tree pane.
Deliberately a different color from `reviewer-treemacs-thread-face' so
files and threads are distinguishable by color alone, not just by
indentation and icon."
  :group 'reviewer-treemacs)

(defface reviewer-treemacs-thread-face
  '((t :inherit font-lock-doc-face))
  "Face used for a thread's label in the reviewer tree pane."
  :group 'reviewer-treemacs)

;; --- thread data accessors -------------------------------------------------
;; All read from the button's automatically-stored `:item' -- for a
;; thread node, the annotation overlay itself, so a reply appended
;; straight onto it via `reviewer-treemacs--append-reply' is immediately
;; reflected wherever these are called again, with no separate refresh
;; step.

(defun reviewer-treemacs--item (btn)
  "Return BTN's underlying item -- for a thread node, its annotation overlay.
`treemacs-button-get' only works when the current buffer is BTN's own
buffer, so this switches there first -- needed here since BTN is a
marker that outlives the tree buffer being selected, e.g. once control
reaches the reply pane or the detail pane."
  (treemacs-with-button-buffer btn (treemacs-button-get btn :item)))

(defun reviewer-treemacs--key (btn)
  "Return BTN's key.  See `reviewer-treemacs--item' for why."
  (treemacs-with-button-buffer btn (treemacs-button-get btn :key)))

(defun reviewer-treemacs--thread-entries (btn)
  "Return thread node BTN's entries: its root note, then replies, oldest first.
Each entry is a plist with :author, :time, and :text -- the root note
reshaped to match the shape of `reviewer-annotation-replies' already."
  (let ((ov (reviewer-treemacs--item btn)))
    (cons (list :author (reviewer-annotation-author ov)
                :time (reviewer-annotation-time ov)
                :text (reviewer-annotation-text ov))
          (reviewer-annotation-replies ov))))

(defun reviewer-treemacs--format-thread (entries)
  "Format ENTRIES (a thread's plists) the same way the posframe view does.
Reuses `reviewer--format-thread-entry' so the two views stay visually
consistent: bold, per-author-colored name, muted timestamp, then text."
  (mapconcat (lambda (entry)
               (reviewer--format-thread-entry (plist-get entry :author)
                                               (plist-get entry :time)
                                               (plist-get entry :text)))
             entries "\n\n"))

(defun reviewer-treemacs--thread-detail (btn)
  "Format thread node BTN's full conversation for the detail pane."
  (reviewer-treemacs--format-thread (reviewer-treemacs--thread-entries btn)))

(defun reviewer-treemacs--summarize (text)
  "Truncate TEXT to `reviewer-treemacs-summary-length' characters.
Collapses newlines to spaces first, since a summary is meant to render
on a single tree line, then appends an ellipsis if still truncated."
  (let ((flat (replace-regexp-in-string "\n+" " " text)))
    (if (> (length flat) reviewer-treemacs-summary-length)
        (concat (substring flat 0 reviewer-treemacs-summary-length) "…")
      flat)))

(defun reviewer-treemacs--item-summary (item)
  "Return the tree label for thread ITEM, an annotation overlay: its
root note, summarized.
Takes the raw ITEM rather than a button, since node-type label forms
only have that -- see the node type definitions below."
  (reviewer-treemacs--summarize (reviewer-annotation-text item)))

;; --- detail pane -------------------------------------------------------

(defconst reviewer-treemacs--placeholder-text
  "Select a thread to see it here."
  "Detail pane text shown when point isn't on a thread node.")

(defvar-local reviewer-treemacs--last-preview-key nil
  "Key of the node last previewed in the detail pane by cursor movement.")

(defun reviewer-treemacs--show-detail (text &optional key)
  "Show TEXT in the detail pane, named after thread KEY.
Renames the pane to \"reviewer-thread-<KEY>\" (KEY being the
annotation's own `reviewer-annotation-thread-id') so it's identifiable
on its own.  With no KEY (nothing selected yet), the pane keeps its
generic default name."
  (with-current-buffer (reviewer-treemacs--detail-buffer)
    (rename-buffer (if key (format "reviewer-thread-%s" key)
                      reviewer-treemacs--detail-buffer-default-name)
                    t)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (goto-char (point-min)))
    (special-mode)
    ;; Set after `special-mode' since major modes wipe non-permanent
    ;; buffer-local variables via `kill-all-local-variables'.
    (setq truncate-lines nil)
    (reviewer-treemacs--local-keymap (current-local-map)
                                      '(("q" . reviewer-treemacs-toggle)
                                        ("C-g" . reviewer-treemacs-toggle)))))

(defun reviewer-treemacs--preview-at-point ()
  "Preview the thread at point in the detail pane as the cursor moves.
Runs on `post-command-hook' in the tree buffer so any navigation --
n/p, arrow keys, a mouse click -- updates the detail pane immediately,
without waiting for RET."
  (let* ((btn (treemacs-current-button))
         (key (and btn (treemacs-button-get btn :key))))
    (unless (equal key reviewer-treemacs--last-preview-key)
      (setq reviewer-treemacs--last-preview-key key)
      (if (and btn (treemacs-button-get btn :leaf))
          (reviewer-treemacs--show-detail (reviewer-treemacs--thread-detail btn) key)
        (reviewer-treemacs--show-detail reviewer-treemacs--placeholder-text)))))

;; --- navigation ----------------------------------------------------------

(defun reviewer-treemacs--move-to-leaf (move-fn)
  "Call MOVE-FN, then keep calling it while point rests on a file node.
File nodes are just group headers in this tree -- there's nothing to
preview or act on for one -- so n/p should never leave point sitting
on one; skip straight through to the next thread in that direction.
Stops as soon as MOVE-FN stops making progress, so a file node with no
threads under it (or the top/bottom of the tree) doesn't loop forever."
  (let ((pos (point)))
    (funcall move-fn 1)
    (while (and (/= (point) pos)
                (let ((btn (treemacs-current-button)))
                  (and btn (not (treemacs-button-get btn :leaf)))))
      (setq pos (point))
      (funcall move-fn 1))))

(defun reviewer-treemacs-next-line ()
  "Move to the next thread in the tree, skipping over file nodes."
  (interactive)
  (reviewer-treemacs--move-to-leaf #'treemacs-next-line))

(defun reviewer-treemacs-previous-line ()
  "Move to the previous thread in the tree, skipping over file nodes."
  (interactive)
  (reviewer-treemacs--move-to-leaf #'treemacs-previous-line))

;; --- jump to source ------------------------------------------------------

(defun reviewer-treemacs--main-window ()
  "Return a window to open a visited annotation's buffer in, or nil.
The most-recently-used window that isn't dedicated -- i.e. not one of
the tree/detail/reply side windows -- so RET reuses whatever \"main\"
window the user was last working in instead of leaving the choice to
`pop-to-buffer's own heuristics, which -- combined with this user's
`display-buffer-alist' -- were splitting a new window inconsistently."
  (seq-find (lambda (w) (not (window-dedicated-p w)))
            (sort (window-list nil 'no-minibuf)
                  (lambda (a b) (> (window-use-time a) (window-use-time b))))))

(defun reviewer-treemacs--visit-buffer (buf)
  "Visit BUF in `reviewer-treemacs--main-window', or split if none exists."
  (let ((window (reviewer-treemacs--main-window)))
    (if window
        (progn (select-window window) (switch-to-buffer buf))
      (pop-to-buffer buf))))

(defun reviewer-treemacs--visit-buffer-split (buf side)
  "Visit BUF in a new window, splitting on SIDE (`right' or `below').
Splits `reviewer-treemacs--main-window' when there is one, so the new
window lands next to the user's actual working window rather than
next to the tree/detail pane; falls back to `reviewer-treemacs--visit-buffer'
when there's no other window to split."
  (let ((window (reviewer-treemacs--main-window)))
    (if (not window)
        (reviewer-treemacs--visit-buffer buf)
      (select-window (split-window window nil side))
      (switch-to-buffer buf))))

(defun reviewer-treemacs--jump-to-context (btn &optional visit-fn)
  "Jump to thread node BTN's annotation and display it there.
VISIT-FN opens the resolved buffer, defaulting to
`reviewer-treemacs--visit-buffer'; callers pass an alternative to open
it elsewhere instead, e.g. a fresh split."
  (let* ((ov (reviewer-treemacs--item btn))
         (buf (overlay-buffer ov)))
    (unless buf
      (user-error "This annotation no longer exists"))
    (funcall (or visit-fn #'reviewer-treemacs--visit-buffer) buf)
    (unless reviewer-mode (reviewer-mode 1))
    (goto-char (overlay-start ov))
    (reviewer-show-annotation-at-point)))

(defun reviewer-treemacs--visit-annotation (&optional _arg)
  "Jump to the source location of the thread node at point, switching
focus to it; an \"o o\" binding."
  (interactive)
  (when-let* ((btn (treemacs-current-button)))
    (reviewer-treemacs--jump-to-context btn)))

(defun reviewer-treemacs--peek-annotation (&optional _arg)
  "Show the source location of the thread node at point, without
leaving the tree window selected; a `:ret-action'.
Runs the same jump as `reviewer-treemacs--visit-annotation' -- visit
the buffer, pop up the annotation -- but wrapped in
`save-selected-window' so the tree stays selected afterward and n/p
keep driving straight through a string of threads."
  (interactive)
  (when-let* ((btn (treemacs-current-button)))
    (save-selected-window
      (reviewer-treemacs--jump-to-context btn))))

(defun reviewer-treemacs--ret-annotation (&optional arg)
  "Jump to the source location of the thread node at point; a `:ret-action'.
Dispatches to `reviewer-treemacs--visit-annotation' or
`reviewer-treemacs--peek-annotation' depending on
`reviewer-treemacs-ret-moves-focus'."
  (interactive)
  (if reviewer-treemacs-ret-moves-focus
      (reviewer-treemacs--visit-annotation arg)
    (reviewer-treemacs--peek-annotation arg)))

(defun reviewer-treemacs--visit-annotation-btn (btn)
  "Jump to the source location of BTN; a `:visit-action'."
  (reviewer-treemacs--jump-to-context btn))

(defun reviewer-treemacs--thread-at-point ()
  "Return the thread node button at point, or signal a `user-error'."
  (let ((btn (treemacs-current-button)))
    (unless (and btn (treemacs-button-get btn :leaf))
      (user-error "No thread at point"))
    btn))

(defun reviewer-treemacs-open-vertical-split ()
  "Jump to the thread at point's source, opened in a new vertical split."
  (interactive)
  (reviewer-treemacs--jump-to-context
   (reviewer-treemacs--thread-at-point)
   (lambda (buf) (reviewer-treemacs--visit-buffer-split buf 'right))))

(defun reviewer-treemacs-open-horizontal-split ()
  "Jump to the thread at point's source, opened in a new horizontal split."
  (interactive)
  (reviewer-treemacs--jump-to-context
   (reviewer-treemacs--thread-at-point)
   (lambda (buf) (reviewer-treemacs--visit-buffer-split buf 'below))))

(defun reviewer-treemacs-open-and-close ()
  "Jump to the thread at point's source, then close the tree/detail pane."
  (interactive)
  (reviewer-treemacs--jump-to-context (reviewer-treemacs--thread-at-point))
  (reviewer-treemacs--close))

;; --- reply pane ------------------------------------------------------------

(defconst reviewer-treemacs--reply-buffer "*reviewer-reply*"
  "Buffer used to compose a reply to a thread in the tree pane.")

(defvar-local reviewer-treemacs--reply-target nil
  "The treemacs button `reviewer-treemacs-reply-finish' appends this reply to.")

(defvar-local reviewer-treemacs--reply-saved-heights nil
  "Tree/detail window heights captured before this reply pane opened.
An alist of (WINDOW . HEIGHT); see `reviewer-treemacs--save-heights'.
Restored once the reply pane closes, since inserting and removing a
third window in the same side column otherwise leaves Emacs's own
space redistribution in place rather than reverting it.")

(defun reviewer-treemacs--save-heights ()
  "Return an alist of (WINDOW . HEIGHT) for the tree/detail windows."
  (mapcar (lambda (window) (cons window (window-height window)))
          (reviewer-treemacs--windows)))

(defun reviewer-treemacs--restore-heights (saved)
  "Resize each window in SAVED back to its recorded height.
SAVED is an alist as returned by `reviewer-treemacs--save-heights'."
  (dolist (entry saved)
    (when (window-live-p (car entry))
      (window-resize (car entry) (- (cdr entry) (window-height (car entry))) nil t))))

(defun reviewer-treemacs--close-reply-window ()
  "Close the reply pane, if open, restoring the heights it disturbed."
  (when-let* ((buf (get-buffer reviewer-treemacs--reply-buffer)))
    (let ((saved (buffer-local-value 'reviewer-treemacs--reply-saved-heights buf))
          (window (get-buffer-window buf)))
      (when window
        (reviewer-treemacs--unlock-window window)
        (delete-window window))
      (kill-buffer buf)
      (reviewer-treemacs--restore-heights saved))))

(defun reviewer-treemacs--append-reply (btn text)
  "Append TEXT as a reply, from the current user, to thread node BTN's annotation.
Appends straight onto BTN's `:item', the annotation overlay itself --
see the note on thread data accessors above -- so the detail pane can
be refreshed by simply re-reading it."
  (reviewer--append-reply (reviewer-treemacs--item btn) (user-login-name) text))

(defun reviewer-treemacs-reply-finish ()
  "Append this buffer's text as a reply, then close the reply pane."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (btn reviewer-treemacs--reply-target))
    (reviewer-treemacs--close-reply-window)
    (unless (string-empty-p text)
      (reviewer-treemacs--append-reply btn text)
      (reviewer-treemacs--show-detail (reviewer-treemacs--thread-detail btn)
                                       (reviewer-treemacs--key btn)))))

(defun reviewer-treemacs-reply-abort ()
  "Abandon this reply without appending anything."
  (interactive)
  (reviewer-treemacs--close-reply-window))

(defvar reviewer-treemacs-reply-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'reviewer-treemacs-reply-finish)
    (define-key map (kbd "C-c C-k") #'reviewer-treemacs-reply-abort)
    map)
  "Keymap for `reviewer-treemacs-reply-mode'.")

(define-derived-mode reviewer-treemacs-reply-mode text-mode "Reviewer-Reply"
  "Major mode for composing a reply to a thread in the reviewer tree pane.
\\{reviewer-treemacs-reply-mode-map}"
  ;; Header-line text doesn't wrap -- it just gets clipped at the window
  ;; edge -- and this pane is as narrow as the tree/detail pane
  ;; (`reviewer-treemacs-width'), so kept short enough to actually fit
  ;; instead of the longer, friendlier phrasing used elsewhere.
  (setq header-line-format
        (substitute-command-keys
         (concat "\\[reviewer-treemacs-reply-finish] to reply, "
                 "\\[reviewer-treemacs-reply-abort] to cancel."))))

(defun reviewer-treemacs-reply-to-thread-at-point ()
  "Open a reply pane, stacked below the detail pane, for the thread at point."
  (interactive)
  (let ((btn (treemacs-current-button)))
    (unless (and btn (treemacs-button-get btn :leaf))
      (user-error "No thread at point"))
    (let ((saved (reviewer-treemacs--save-heights)))
      (select-window
       (reviewer-treemacs--lock-window
        (display-buffer-in-side-window
         (get-buffer-create reviewer-treemacs--reply-buffer)
         `((side . ,reviewer-treemacs-side) (slot . ,(+ reviewer-treemacs-slot 2))
           (window-width . ,(reviewer-treemacs--resolved-width))
           (window-height . 8)))))
      (reviewer-treemacs-reply-mode)
      (erase-buffer)
      (setq reviewer-treemacs--reply-target btn)
      (setq reviewer-treemacs--reply-saved-heights saved))))

;; --- node types --------------------------------------------------------

(treemacs-define-leaf-node-type reviewer-treemacs-thread
  :icon "‣ "
  :label (propertize (reviewer-treemacs--item-summary item) 'face 'reviewer-treemacs-thread-face)
  :key (reviewer-annotation-thread-id item)
  :ret-action #'reviewer-treemacs--ret-annotation
  :visit-action #'reviewer-treemacs--visit-annotation-btn)

(treemacs-define-expandable-node-type reviewer-treemacs-file
  :closed-icon (propertize "▸ " 'face 'reviewer-treemacs-file-face)
  :open-icon (propertize "▾ " 'face 'reviewer-treemacs-file-face)
  :label (propertize (car item) 'face 'reviewer-treemacs-file-face)
  :key (car item)
  :children (cdr item)
  :child-type 'reviewer-treemacs-thread)

(treemacs-define-variadic-entry-node-type reviewer-treemacs-root
  :key 'reviewer-treemacs-root
  :children (reviewer-treemacs--collect-files)
  :child-type 'reviewer-treemacs-file)

;; --- locked side-window pair --------------------------------------------

(defun reviewer-treemacs--lock-window (window)
  "Mark WINDOW dedicated, protected from `delete-other-windows', excluded
from `other-window' (mirroring vanilla treemacs's own
`treemacs-is-never-other-window'), and its height preserved against
automatic resizing.
Being both dedicated and excluded from `other-window' -- not just one
or the other -- closes off the two different ways WINDOW could
otherwise get overwritten: `dedicated' stops `display-buffer' and
friends from swapping in some unrelated buffer, `no-other-window'
stops `other-window'/`ace-window'/etc. from cycling into it in the
first place.  The height preservation matters once the reply pane
opens as a third, temporary window stacked below WINDOW in the same
side column -- without it, Emacs redistributes space across all three
and doesn't restore the original split when the reply pane closes."
  (set-window-dedicated-p window t)
  (set-window-parameter window 'no-delete-other-windows t)
  (set-window-parameter window 'no-other-window t)
  (window-preserve-size window nil t)
  window)

(defun reviewer-treemacs--unlock-window (window)
  "Undo `reviewer-treemacs--lock-window' on WINDOW before deleting it."
  (set-window-parameter window 'no-delete-other-windows nil)
  (set-window-parameter window 'no-other-window nil)
  (set-window-dedicated-p window nil))

(defun reviewer-treemacs--windows ()
  "Return the live windows currently showing the tree/detail pane pair."
  (delq nil (list (get-buffer-window reviewer-treemacs--tree-buffer)
                   (and (buffer-live-p reviewer-treemacs--detail-buf)
                        (get-buffer-window reviewer-treemacs--detail-buf)))))

(defun reviewer-treemacs--local-keymap (parent-map bindings)
  "Install a local keymap over PARENT-MAP with BINDINGS added.
BINDINGS is an alist of (KEY . COMMAND); KEY is passed through `kbd'.
Buffer-local, so it doesn't touch PARENT-MAP itself -- e.g.
`treemacs-mode-map' or `special-mode-map' -- and affect unrelated
buffers."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map parent-map)
    (dolist (binding bindings)
      (define-key map (kbd (car binding)) (cdr binding)))
    (use-local-map map)))

(defun reviewer-treemacs--resolved-width ()
  "Resolve `reviewer-treemacs-width' to an absolute column count.
Side windows don't reliably honor a float `window-width' entry -- the
resize Emacs performs after the initial split silently no-ops for
side-window trees -- so the fraction is turned into columns here and
handed to `display-buffer-in-side-window' as a plain integer instead."
  (if (floatp reviewer-treemacs-width)
      (round (* reviewer-treemacs-width (frame-width)))
    reviewer-treemacs-width))

(defun reviewer-treemacs--render ()
  "(Re-)render the tree from `reviewer-treemacs-root' into the current buffer.
Shared by `reviewer-treemacs--open' (the initial render, tree window
already selected) and `reviewer-treemacs--refresh' (re-rendering an
already-open pane from a buffer that need not be selected, e.g. when
`reviewer-annotation-change-hook' fires from some other window), so
the two stay in lockstep.
`treemacs-initialize' resets the buffer to `treemacs-mode', which
wipes the local keymap and `post-command-hook' addition along with
everything else non-permanent-local, so both are reinstalled every
call; leaves point at the top of the rendered tree -- the first
file/label node, a group header with nothing to preview or act on --
so it's nudged onto the first thread the same way `n' would, and
`window-point' is synced explicitly on every window showing this
buffer since a refresh may run without any of them selected."
  (treemacs-initialize reviewer-treemacs-root
    :with-expand-depth 1)
  (when-let* ((btn (treemacs-current-button)))
    (unless (treemacs-button-get btn :leaf)
      (reviewer-treemacs-next-line)))
  (reviewer-treemacs--local-keymap treemacs-mode-map
                                    `(("q" . reviewer-treemacs-toggle)
                                      ("C-g" . reviewer-treemacs-toggle)
                                      ("r" . reviewer-treemacs-reply-to-thread-at-point)
                                      ("n" . reviewer-treemacs-next-line)
                                      ("p" . reviewer-treemacs-previous-line)
                                      ("o o" . reviewer-treemacs--visit-annotation)
                                      ("o v" . reviewer-treemacs-open-vertical-split)
                                      ("o h" . reviewer-treemacs-open-horizontal-split)
                                      ("o c" . reviewer-treemacs-open-and-close)))
  (add-hook 'post-command-hook #'reviewer-treemacs--preview-at-point nil t)
  ;; The hook above only fires on the next real command; call it once now
  ;; so the detail pane reflects wherever point ended up (the first
  ;; thread, or the placeholder if the tree turned out empty) without
  ;; waiting on that.
  (reviewer-treemacs--preview-at-point)
  (dolist (window (get-buffer-window-list (current-buffer) nil t))
    (set-window-point window (point))))

(defun reviewer-treemacs--open ()
  "Open the locked tree/detail pane pair, tree above detail."
  (setq reviewer-treemacs--project-root (reviewer--project-root))
  (let* ((side reviewer-treemacs-side)
         (width (reviewer-treemacs--resolved-width))
         (tree-window (display-buffer-in-side-window
                       (get-buffer-create reviewer-treemacs--tree-buffer)
                       `((side . ,side) (slot . ,reviewer-treemacs-slot)
                         (window-width . ,width))))
         (detail-window (display-buffer-in-side-window
                         (reviewer-treemacs--detail-buffer)
                         `((side . ,side) (slot . ,(1+ reviewer-treemacs-slot))
                           (window-width . ,width)))))
    (reviewer-treemacs--lock-window tree-window)
    (reviewer-treemacs--lock-window detail-window)
    (reviewer-treemacs--show-detail reviewer-treemacs--placeholder-text)
    (select-window tree-window)
    (reviewer-treemacs--render)))

(defun reviewer-treemacs--refresh ()
  "Re-render the tree pane from current annotations, if the pane is open.
Added to `reviewer-annotation-change-hook', so an annotation created,
edited, replied to, or deleted anywhere -- not just through this
pane's own commands -- is reflected here too, without needing to
toggle the pane closed and back open by hand.  Runs via
`with-current-buffer' rather than selecting the tree window, so it
doesn't steal focus from wherever the triggering command left it.
Resets any expand/collapse state in the tree, the same as toggling it
closed and back open would -- `treemacs-initialize' always re-renders
from scratch, there's no incremental update path in this tree's data
model to preserve it instead."
  (when (reviewer-treemacs--windows)
    (with-current-buffer reviewer-treemacs--tree-buffer
      (reviewer-treemacs--render))))

(add-hook 'reviewer-annotation-change-hook #'reviewer-treemacs--refresh)

(defun reviewer-treemacs--close ()
  "Close the tree/detail pane pair, killing their buffers.
Also closes the reply pane, if open."
  (reviewer-treemacs--close-reply-window)
  (dolist (window (reviewer-treemacs--windows))
    (let ((buf (window-buffer window)))
      (reviewer-treemacs--unlock-window window)
      (delete-window window)
      (kill-buffer buf)))
  (setq reviewer-treemacs--detail-buf nil))

;;;###autoload
(defun reviewer-treemacs-toggle ()
  "Show, switch to, or close the reviewer tree/detail pane pair.
Opens it if it isn't showing; if it's already showing but some other
window is selected, just switches focus there instead of closing it --
only closes it when a window of the pane is already the selected one,
e.g. \"q\"/\"C-g\" pressed from inside it."
  (interactive)
  (let ((windows (reviewer-treemacs--windows)))
    (cond
     ((null windows) (reviewer-treemacs--open))
     ((memq (selected-window) windows) (reviewer-treemacs--close))
     (t (select-window (get-buffer-window reviewer-treemacs--tree-buffer))))))

;; Appended to `reviewer.el's transient rather than defined alongside
;; it, since that file shouldn't have to require treemacs just to know
;; this suffix exists -- this one opts in like everything else here.
(transient-append-suffix 'reviewer-transient "e"
  '("t" "toggle tree/detail view" reviewer-treemacs-toggle))

(provide 'reviewer-treemacs)
;;; reviewer-treemacs.el ends here
