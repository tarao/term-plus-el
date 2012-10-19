;;; term+evil.el --- term+ evil integration

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
(require 'term+edit)
(require 'evil)

(term+new-protocol "evil")
(term+new-control-command (term+osc-sel "n") term+-st 'term+edit-normal)

(delq 'term-mode evil-insert-state-modes)
(add-to-list 'evil-emacs-state-modes 'term-mode)
(add-to-list 'term+edit-quit-commands 'evil-yank)

(defun term+evil-setup ()
  (let ((map term+line-map))
    (map-keymap #'(lambda (key def)
                    (evil-define-key 'normal map (vector key) def)) map)))

(defun term+evil-state-entry-constrain-to-field ()
  (when term+input-mode
    (add-hook 'evil-insert-state-entry-hook
              #'term+input-constrain-to-field nil t)))

(add-hook 'term+char-mode-hook #'evil-emacs-state)
(add-hook 'term+line-mode-hook #'evil-insert-state)
(add-hook 'term-mode-hook #'term+evil-setup)
(add-hook 'term+input-mode-hook #'term+evil-state-entry-constrain-to-field)

(defadvice evil-operator-range
  (around ad-term+evil-constrain-range-to-field activate)
  "Fit range into the input field."
  (let* ((range ad-do-it)
         (type (and (consp (cddr range)) (car (cddr range)))))
    (when (and (term+input-in-range-p) (not (eq type 'block)))
      (let ((beg (nth 0 range)) (end (nth 1 range)))
        (setq range
              (cons (max beg (term+input-beg))
                    (cons (min end (term+input-end)) (cddr range))))))
    (setq ad-return-value range)))

(defun term+edit-normal (text)
  "Switch to `term-line-mode' with making non-input area
read-only and switch to normal state."
  (term+edit-insert text)
  (evil-normal-state))

(provide 'term+evil)
;; term+evil.el ends here
