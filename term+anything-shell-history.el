;;; term+anything-shell-history.el --- term+ anything shell history integration

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
(require 'anything-complete)

(defun term+anything-shell-history (initial)
  (anything
   `(((name . "History")
      (action
       ("Exec" . (lambda (str) (concat str "\n")))
       ("Paste" . identity))
      ,@anything-c-source-complete-shell-history))
   initial
   nil nil nil
   "*anything shell history*"))

(setq term+shell-history-function 'term+anything-shell-history)

(provide 'term+anything-shell-history)
;; term+anything-shell-history ends here
