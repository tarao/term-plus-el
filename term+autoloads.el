;;; term+autoloads.el --- autoload settings for term+

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

;; commands to start terminal
(autoload 'term "term+"
  "Start a terminal-emulator in a new buffer." t)
(autoload 'ansi-term "term+"
  "Start a terminal-emulator in a new buffer." t)
(autoload 'term-mode "term+"
  "Major mode for interacting with an inferior interpreter." t)

;; commands to setup terminal multiplexer
(autoload 'term+mux-noselect "term+mux"
  "Open a new terminal as a new tab without selecting the tab." t)
(autoload 'term+mux-new "term+mux"
  "Open a new terminal as a new tab." t)
(autoload 'term+mux-other-window "term+mux"
  "Open a new terminal as a new tab in the other window." t)
(autoload 'term+mux-new-command "term+mux"
  "Open a new terminal as a new tab with specifying a command to run." t)
(autoload 'term+mux-new-session "term+mux"
  "Make a new session." t)
(autoload 'term+mux-remote-session "term+mux"
  "Make a new remote session." t)

;; control commands
(autoload 'term+mux-set-title "term+mux"
  "Set the title of the tab of the current terminal.")
(autoload 'term+mux-cdd "term+mux"
  "List default directories of terminals in the current session.")

(provide 'term+autoloads)
;;; term+autoloads.el ends here
