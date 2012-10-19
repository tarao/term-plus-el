;;; term+mode.el --- term+ with multiple modes

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

(require 'multi-mode-util)
(require 'term+vars)
(require 'term+input)

(term+new-protocol "mode")
(term+new-control-command (term+osc-emacs "mode") term+-st 'term+mode)

(defvar term+mode-mode nil)
(make-variable-buffer-local 'term+mode-mode)

(defun term+mode (mode)
  "Set MODE as a mode to enable when `term+mode-edit' is called."
  (when (stringp mode)
    (setq mode (and (> (length mode) 0)
                    (intern (or (and (string-match "-mode$" mode) mode)
                                (concat mode "-mode"))))))
  (setq term+mode-mode (and mode (fboundp mode) mode)))

(defun term+mode-quit ()
  (when (multi-initialized-p)
    (multi-mode-quit)
    (setq term+mode-mode nil)))
(add-hook 'term+char-mode-hook #'term+mode-quit)

(defun term+mode-chunk-fun (pos)
  (multi-with-base-buffer
    (when (and term+line-mode term+mode-mode)
      (let ((start (marker-position (car term+input-range-markers)))
            (end (marker-position (cdr term+input-range-markers))))
        (when (and (<= start pos) (<= pos end))
          (multi-make-list term+mode-mode start end))))))

(defun term+mode-install ()
  (multi-install-mode term+mode-mode 'term+mode-chunk-fun))

(defun term+mode-unbury-base-buffer ()
  (let ((buffer (current-buffer)))
    (unbury-buffer)
    (switch-to-buffer buffer)))

(defun term+mode-indirect-buffer-input ()
  (when (multi-indirect-buffer-p)
    (let (beg end)
      (multi-with-base-buffer
        (setq beg (term+input-beg)
              end (term+input-end)))
      (term+input-mode 1)
      (term+input-set-range beg end)
      (rename-buffer (concat " " (buffer-name)) t)
      (add-hook 'multi-select-mode-hook
                #'term+mode-unbury-base-buffer nil t))))

(defun term+mode-edit ()
  "Set up `multi-mode' and enable a major mode in the input field.
The major mode enabled is the one specified in `term+mode'.

This function is intended to be called when `term+edit-mode' is
enabled."
  (when term+mode-mode
    (term+mode-install)
    (multi-with-indirect-buffers (term+mode-indirect-buffer-input))
    (multi-select-buffer)))
(add-hook 'term+edit-insert-hook #'term+mode-edit)

(defun term+mode-run-in-base-buffer (func)
  "Make FUNC to be run in the base buffer (without returning to
the indirect buffer)."
  (let ((ad-sym (intern (concat (symbol-name func) "-in-base-buffer"))))
    (eval `(defadvice ,func (before ,ad-sym activate)
             (let ((pos (point)))
               (set-buffer (multi-base-buffer))
               (goto-char pos))))))

(term+mode-run-in-base-buffer 'term-char-mode)
(term+mode-run-in-base-buffer 'term-send-input)
(term+mode-run-in-base-buffer 'term+send-input-and-char-mode)

(multi-run-in-base-buffer 'term-previous-input t)
(multi-run-in-base-buffer 'term-next-input t)
(multi-run-in-base-buffer 'term+hardcopy)

(provide 'term+mode)
;;; term+mode.el ends here
