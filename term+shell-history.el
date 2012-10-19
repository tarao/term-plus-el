;;; term+shell-history.el --- term+ shell history lookup functionality

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

(defun term+set-shell-history-file (path)
  "Change the path of shell history file to PATH.
The value is used in `term+shell-history'."
  (set (make-local-variable 'term+shell-history-file) path))

(defsubst term+default-shell-history-file ()
  (or (getenv "HISTFILE")
      (cond
       ((string-match-p "\\(^\\|/\\)zsh$" shell-file-name) "~/.zsh_history")
       ((string-match-p "\\(^\\|/\\)bash$" shell-file-name) "~/.bash_history")
       ((string-match-p "\\(^\\|/\\)ksh$" shell-file-name) "~/.sh_history")
       (t "~/.history"))))

(defun term+shell-history-file ()
  (let* ((path (or term+shell-history-file (term+default-shell-history-file)))
         (path (file-truename path)))
    (when (boundp 'shell-history-file)
      (set (make-local-variable 'shell-history-file) path))
    path))

(defun term+shell-history-buffer (file)
  (or (get-file-buffer file)
      (with-current-buffer (find-file-noselect file)
        (auto-revert-mode -1)
        (buffer-disable-undo)
        (current-buffer))))

(defun term+parse-shell-history ()
  (with-current-buffer (term+shell-history-buffer (term+shell-history-file))
    (goto-char (point-min))
    (let ((str "") (extended (re-search-forward "^: [0-9]+:" nil t)) hist pt)
      (goto-char (point-min))
      (while (and (setq pt (point)) (not (eobp)) (search-forward "\n" nil t))
        (setq str (concat str (buffer-substring pt (1- (point)))))
        (when (and extended (string-match "^: [0-9]+:[0-9];" str))
          (setq str (replace-match "" t t str)))
        (when (and extended (string-match "^[ \t\r\n]+" str))
          (setq str (replace-match "" t t str)))
        (when (and extended (string-match "[ \t\r\n]+$" str))
          (setq str (replace-match "" t t str)))
        (if (string= (buffer-substring (- (point) 2) (1- (point))) "\\")
            (setq str (concat (substring str 0 (1- (length str))) "\n"))
          (add-to-list 'hist str)
          (setq str "")))
      (when (> (length str) 0) (add-to-list 'hist str))
      hist)))

(defun term+default-shell-history (initial)
  (with-current-buffer (term+shell-history-buffer (term+shell-history-file))
    (revert-buffer t t))
  (let* ((hist (term+parse-shell-history))
         (str (completing-read "Command: " hist nil nil initial)))
    (when (> (length str) 0) (concat str "\n"))))

(defun term+shell-history (&optional initial)
  "Show prompt to find a command line from shell history.
The history is read from `term+shell-history-file', which can be
set via `term+set-shell-history-file'.  The prompt is shown by a
function which is a value of `term+shell-history-function' with
INITIAL as an argument.

If `term+shell-history-dont-exec' is non-nil, the selected
command line is just pasted to the terminal.  Otherwise, the
selected command line with a new line character is inserted to
the terminal.  However, this is not the case if
`term+shell-history-function' returns a command line with no new
line character."
  (interactive)
  (term+shell-history-file)
  (let* ((str (funcall term+shell-history-function initial))
         (str (or (and term+shell-history-dont-exec
                       (string-match-p "\n$" str)
                       (substring str 0 (1- (length str))))
                  str)))
    (when str (term-send-raw-string str))))

(provide 'term+shell-history)
;;; term+shell-history.el ends here
