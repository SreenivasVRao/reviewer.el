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

(defvar reviewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'reviewer-annotate)
    map)
  "Keymap for `reviewer-mode'.")

;;;###autoload
(define-minor-mode reviewer-mode
  "Minor mode for annotating a buffer without changing its content."
  :lighter " Reviewer"
  :keymap reviewer-mode-map)

(provide 'reviewer)
;;; reviewer.el ends here
