;;; term+edit.el --- term+ edit mode implementation

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

(require 'term)
(require 'term+vars)
(require 'term+input)

(defun term+edit-post-command ()
  (when (and (not (term+input-in-range-p))
             (memq this-command term+edit-quit-commands))
    (term-char-mode)))

(defun term+edit-initialize-buffer (beg end text)
  (term+input-set-range beg end)
  (when text
    (delete-region beg end)
    (insert text))
  (setq term+edit-last-text text))

(define-minor-mode term+edit-mode
  "Editing terminal buffer in Emacs.
It switches to `term-line-mode' and enables `term+input-mode'.

When a command in `term+edit-quit-commands' is called, it
switches to `term-char-mode'.

Switching to `term-char-mode' leaves this mode and leaving this
mode restores the last cursor position and the last command line
text."
  :group 'term+
  (if term+edit-mode
      (progn
        (make-local-variable 'term+edit-last-text)
        (set (make-local-variable 'term+edit-last-pos) (point))
        (add-hook 'post-command-hook #'term+edit-post-command nil t)
        (unless (term-in-line-mode) (term-line-mode))
        (term+input-mode 1))
    ;; restore the last position
    (when (and term+edit-restore-last-pos term+edit-last-pos)
      (let* ((proc (get-buffer-process (current-buffer)))
             (marker (and proc (process-mark proc))))
        (dolist (win (get-buffer-window-list nil 0 t))
          (with-selected-window win
            (goto-char (or marker term+edit-last-pos))))))
    ;; quit input mode
    (term+input-mode 0)
    ;; insert the last text
    (when term+edit-last-text
      (term-send-raw-string term+edit-last-text)
      (setq term+edit-last-text nil))
    (remove-hook 'post-command-hook 'term+edit-post-command t)))

(defadvice term-char-mode (before term+edit-quit activate)
  "Disable `term+edit-mode'."
  (term+edit-mode 0))

(defadvice term-line-mode (after term+edit-enter activate)
  "Enable `term+edit-mode'."
  (term+edit-mode 1))

(defadvice term-send-input (before term+edit-send-whole-text activate)
  (goto-char (term+input-end)))

(defadvice term-send-input (after term+edit-clear-last-text activate)
  (setq term+edit-last-text nil))

(defun term+edit (&optional beg end text)
  "Switch to `term+line-mode' with making non-input area read-only.
The range between BEG and END becomes writable input area.  If
TEXT is specified, the range between BEG and END are replaced
with the TEXT."
  (interactive "r")
  (let ((beg (or beg (point)))
        (end (or end (save-excursion (end-of-line) (point)))))
    (term+edit-mode 1)
    (term+edit-initialize-buffer beg end text)))

(defun term+edit-at-point (&optional text)
  "Switch to `term-line-mode' with making non-input area read-only.
Only current position becomes writable input area."
  (interactive)
  (term+edit (point) (point) text))

(defun term+edit-to-eol (&optional text)
  "Switch to `term-line-mode' with making non-input area read-only.
The range from current position to end of line will be killed and
the beginning of range becomes writable input area."
  (interactive)
  (kill-region (point) (save-excursion (end-of-line) (point)))
  (term+edit-at-point text))

(defun term+edit-as-expected (&optional text)
  "Switch to `term-line-mode' with making non-input area read-only.
It changes the behavior depending on the value of
`term+edit-kill-to-eol'."
  (interactive)
  (if term+edit-kill-to-eol
      (term+edit-to-eol text)
    (term+edit-at-point text)))

(defun term+edit-insert (text)
  "Switch to `term-line-mode' with specified text.
This function calls `term+edit-as-expected' with TEXT.  TEXT is
properly decoded according to the current locale settings.

This function runs `term+edit-insert-hook' at the end."
  (let ((cs (coding-system-change-eol-conversion locale-coding-system nil)))
    (term+edit-as-expected (decode-coding-string text cs)))
  (run-hooks 'term+edit-insert-hook))

(defadvice term-emulate-terminal
  (around term+edit-restore-point-and-scroll (proc str) activate)
  "Prevent output of terminal applications from moving cursor and
scrolling window during `term+edit-mode'."
  (let ((buffer (process-buffer proc)))
    (with-current-buffer buffer
      (if term+edit-mode
          (let* ((point (point))
                 (window (selected-window))
                 (start (and (eq (window-buffer window) buffer)
                             (window-start window)))
                 (inhibit-redisplay t))
              ad-do-it
            (when start (set-window-start window start))
            (goto-char point))
        ad-do-it))))

(provide 'term+edit)
;;; term+edit.el ends here
