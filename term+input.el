;;; term+input.el --- term+ input field implementation

;; Author: INA Lintaro <tarao.gnn at gmail.com>
;; URL: http://github.com/tarao/term+-el
;; Version: 0.1
;; Keywords: terminal, emulation

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'term+vars)


;;; input mode

(defun term+input-beg ()
  (and (car term+input-range-markers)
       (marker-position (car term+input-range-markers))))

(defun term+input-end ()
  (and (cdr term+input-range-markers)
       (marker-position (cdr term+input-range-markers))))

(defun term+input-reset-range ()
  (when (car term+input-range-markers)
    (set-marker (car term+input-range-markers) nil))
  (when (cdr term+input-range-markers)
    (set-marker (cdr term+input-range-markers) nil))
  (setq term+input-range-markers (cons nil nil)))

(defun term+input-in-range-p (&optional pos)
  (let ((pos (or pos (point))))
    (and (car term+input-range-markers) (cdr term+input-range-markers)
         (<= (marker-position (car term+input-range-markers)) pos)
         (<= pos (marker-position (cdr term+input-range-markers))))))

(defun term+input-reset (&optional no-delete)
  (make-local-variable 'term+input-range-markers)
  (make-local-variable 'term+input-overlay)
  (make-local-variable 'term+input-map-overlay)
  (make-local-variable 'term+input-readonly-overlay)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max)
                            '(read-only nil rear-nonsticky nil)))
  ;; remove input field
  (let ((beg (term+input-beg)) (end (term+input-end)))
    (when (and beg end (<= (point-min) beg) (<= end (point-max)))
      (unless no-delete (delete-region beg end))))
  (term+input-reset-range)
  ;; remove the overlay
  (when term+input-overlay
    (delete-overlay term+input-overlay)
    (setq term+input-overlay nil))
  (when term+input-map-overlay
    (delete-overlay term+input-map-overlay)
    (setq term+input-map-overlay nil))
  (when term+input-readonly-overlay
    (delete-overlay term+input-readonly-overlay)
    (setq term+input-readonly-overlay nil)))

(defun term+input-set-range (beg end &optional no-delete)
  (term+input-reset no-delete)
  (let ((beg (or beg (point))) (end (or end (point))))
    ;; make the whole buffer readonly except the input field
    (when (< (point-min) beg)
      (add-text-properties (1- beg) beg '(rear-nonsticky t)))
    (add-text-properties (point-min) beg '(read-only t))
    (add-text-properties end (point-max) '(read-only t))
    ;; markers for the input field
    (save-excursion
      (goto-char beg)
      (setcar term+input-range-markers (point-marker))
      (set-marker-insertion-type (car term+input-range-markers) nil))
    (setcdr term+input-range-markers (point-marker))
    (set-marker-insertion-type (cdr term+input-range-markers) t)
    ;; overlay for the input field
    (setq term+input-overlay (make-overlay beg (1+ end) nil nil t))
    (overlay-put term+input-overlay 'field term+input-field)
    (overlay-put term+input-overlay 'face 'term+input-face)
    (overlay-put term+input-overlay 'priority 20)
    ;; we need an overlay for keymap to negotiate with auto-complete
    (setq term+input-map-overlay (make-overlay beg (1+ end) nil nil t))
    (overlay-put term+input-map-overlay 'keymap term+input-map)
    ;; having term+input-overlay be higher priority breaks
    ;; transient-mark-mode; this is why we have separate overlays
    (overlay-put term+input-map-overlay 'priority 12000)
    ;; overlay for readonly field
    (setq term+input-readonly-overlay
          (make-overlay (point-min) (point-max) nil nil t))
    (overlay-put term+input-readonly-overlay 'face 'term+input-readonly-face)
    (overlay-put term+input-readonly-overlay 'keymap
                 term+input-readonly-map)
    (overlay-put term+input-readonly-overlay 'priority 10)))

(defun term+input-constrain-to-field ()
  (when term+input-overlay
    (goto-char (constrain-to-field (point) (term+input-beg)
                                   nil nil term+input-field))))

(define-minor-mode term+input-mode
  "Minor mode for input field of terminal."
  :group 'term+
  (if term+input-mode
      (term+input-set-range (point) (point))
    (term+input-reset)))


;;; commands in the input field

(defun term+beginning-of-line (&optional arg)
  "Move point to beginning of current line.
If the current position is the beginning of the input field, then
it moves out the input field.  Otherwise, it stops at the field
boundary."
  (interactive "^p")
  (if (= (term+input-beg) (point))
      (let ((inhibit-field-text-motion t))
        (beginning-of-line arg))
    (beginning-of-line arg)))

(defun term+end-of-line (&optional arg)
  "Move point to end of current line.
If the current position is the end of the input field, then it
moves out the input field.  Otherwise, it stops at the field
boundary."
  (interactive "^p")
  (if (= (1+ (term+input-end)) (point))
      (let ((inhibit-field-text-motion t))
        (end-of-line arg))
    (end-of-line arg)))

(defun term+kill-line (&optional arg)
  "Kill the rest of the current line."
  (interactive "P")
  ;; this is taken from `kill-line' in simple.el
  (kill-region (point)
               (progn
                 (if arg
                     (forward-visible-line (prefix-numeric-value arg))
                   (if (eobp)
                       (signal 'end-of-buffer nil))
                   (let ((end
                          (save-excursion
                            (end-of-visible-line) (point))))
                     (if (or (save-excursion
                               (unless show-trailing-whitespace
                                 (skip-chars-forward " \t" end))
                               (= (point) end))
                             (and kill-whole-line (bolp)))
                         (forward-visible-line 1)
                       (goto-char end))))
                 (when (> (point) (term+input-end))
                   (goto-char (term+input-end)))
                 (point))))

(defun term+kill-input (&optional arg)
  "Kill whole text in the input field.
If ARG is a non-negative number, then kill point to the end of
line.  If ARG is a negative number, then kill point to the
beginning of line.  Specifying ARG by `universal-argument'
without digit arguments toggles kill to the end and kill to the
beginning."
  (interactive "P")
  (when (and (consp arg) (natnump (car arg)))
      (let ((n (floor (log (car arg) 4))))
        (cond
         ((and (natnump n) (/= (% n 2) 0)) (setq arg 1))
         ((and (natnump n) (= (% n 2) 0)) (setq arg -1)))))
  (let* ((beg (if (numberp arg) (point) (term+input-beg)))
         (end (if (and (numberp arg) (< arg 0))
                  (term+input-beg) (term+input-end))))
    (if (> end beg)
        (kill-region beg end)
      (kill-region end beg))))


;;; workarounds

(defadvice term-emulate-terminal
  (around term+input-restore-field (proc str) activate)
  "Adjust the input field not to include output of terminal
applications during `term+input-mode'."
  (let ((buffer (process-buffer proc)))
    (with-current-buffer buffer
      (if term+input-mode
          (let* ((marker (process-mark proc))
                 (marker (and (= marker (term+input-beg)) marker))
                 (inhibit-redisplay t))
              ad-do-it
            (when marker
              (let* ((beg (min (marker-position marker) (point-max)))
                     (end (min (max (term+input-end) beg) (point-max))))
                (term+input-set-range beg end t))))
        ad-do-it))))

(defadvice ac-put-prefix-overlay
  (after term+input-ac-prefix-overlay-map activate)
  "Give `auto-complete' a chance to handle its own bindings in
`term+input-mode'."
  (when (and ac-prefix-overlay
             term+input-map-overlay
             (memq ac-prefix-overlay
                   (overlays-in (term+input-beg) (term+input-end))))
    (let ((map (overlay-get ac-prefix-overlay 'keymap)))
      (set-keymap-parent (overlay-get term+input-map-overlay 'keymap) map))))

(defadvice ac-remove-prefix-overlay
  (after term+input-remove-ac-prefix-overlay-map activate)
  "Disable `auto-complete' bindings in `term+input-mode'."
  (when term+input-map-overlay
    (set-keymap-parent (overlay-get term+input-map-overlay 'keymap) nil)))

(provide 'term+input)
;;; term+input.el ends here
