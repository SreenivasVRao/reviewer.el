;;; reviewer.el --- Annotate files and diffs, render as org for agent handoff  -*- lexical-binding: t -*-

;; Copyright (C) 2026

;; Author: Sreenivas Venkobarao
;; URL: https://github.com/SreenivasVRao/reviewer.el
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:

;; reviewer-mode is a minor mode for annotating a buffer (a file or a
;; diff) without changing its content.  C-c C-a creates an annotation
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
        (overlay-put ov 'face 'reviewer-highlight-face)
        (overlay-put ov 'evaporate t)))))

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

(defun reviewer--org-lang ()
  "Best-effort org src-block language tag for the current buffer.
Derived by stripping \"-mode\" off `major-mode', which also happens to
be exactly what org needs back to find that mode's font-lock table."
  (string-remove-suffix "-mode" (symbol-name major-mode)))

(defun reviewer--line-range (overlay)
  "Return \"Line N\" or \"Lines N-M\" for OVERLAY's line span."
  (let ((start (line-number-at-pos (overlay-start overlay)))
        (end   (line-number-at-pos (overlay-end overlay))))
    (if (= start end)
        (format "Line %d" start)
      (format "Lines %d-%d" start end))))

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

(defun reviewer-render ()
  "Render all annotations in the current buffer as an org file in /tmp/."
  (interactive)
  (let ((annotations (reviewer-all-annotations)))
    (unless annotations
      (user-error "No annotations in this buffer"))
    ;; Overlay positions only mean something in this buffer, so resolve
    ;; everything derived from them here, before switching to OUT below.
    (let* ((file    (or (buffer-file-name) (buffer-name)))
           (lang    (reviewer--org-lang))
           (entries (mapcar (lambda (ov)
                              (list :range (reviewer--line-range ov)
                                    :text  (buffer-substring-no-properties
                                            (overlay-start ov) (overlay-end ov))
                                    :note  (reviewer-annotation-text ov)))
                            annotations))
           (out     (find-file-noselect (reviewer--render-file file))))
      (with-current-buffer out
        (setq buffer-read-only nil)
        (erase-buffer)
        (org-mode)
        (insert "#+STARTUP: showall\n")
        (insert (format "#+TITLE: Review: %s\n\n" file))
        (dolist (entry entries)
          (insert (format "* %s\n\n" (plist-get entry :range)))
          (insert (format "#+BEGIN_SRC %s\n%s\n#+END_SRC\n\n" lang (plist-get entry :text)))
          (insert (format "** Note\n\n%s\n\n" (plist-get entry :note))))
        (save-buffer)
        (setq buffer-read-only t)
        (reviewer-render-mode 1))
      (pop-to-buffer out))))

(defvar reviewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'reviewer-annotate)
    (define-key map (kbd "C-c C-r") #'reviewer-render)
    map)
  "Keymap for `reviewer-mode'.")

;;;###autoload
(define-minor-mode reviewer-mode
  "Minor mode for annotating a buffer without changing its content."
  :lighter " Reviewer"
  :keymap reviewer-mode-map
  (dolist (ov (reviewer-all-annotations))
    (overlay-put ov 'face (when reviewer-mode 'reviewer-highlight-face))))

(provide 'reviewer)
;;; reviewer.el ends here
