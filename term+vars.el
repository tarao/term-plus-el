;;; term+vars.el --- term+ variable definitions

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
(require 'dired)
(require 'dired-aux)

(defgroup term+ nil
  "term-mode enhancement"
  :prefix "term+"
  :group 'term)


;;; internal variables

(defvar term+char-mode-hook nil)
(defvar term+line-mode-hook nil)
(defvar term+char-mode t)
(defvar term+line-mode nil)

;; compatibility
(defsubst term+make-composed-keymap-1 (maps &optional parent)
  (let ((map (copy-sequence maps)))
    (set-keymap-parent map parent)
    map))
(defmacro term+make-composed-keymap (maps &optional parent)
  (if (fboundp 'make-composed-keymap)
      `(make-composed-keymap ,maps ,parent)
    `(term+make-composed-keymap-1 ,maps ,parent)))

(defvar term+char-map
  (let ((map (make-sparse-keymap)))
    ;; unbind
    (define-key map (kbd "C-x") nil)
    (define-key map (kbd "M-x") nil)
    (define-key map (kbd "M-:") nil)
    ;; bind
    (define-key map (kbd "C-M-j") #'term+edit-as-expected)
    (define-key map (kbd "M-RET") #'term+edit-as-expected)
    (define-key map (kbd "C-q") #'term+pass-through)
    (define-key map (kbd "C-c C-e") #'term+send-esc)
    (define-key map (kbd "C-c C-c") #'term-interrupt-subjob)
    (define-key map (kbd "C-c h") #'term+hardcopy)
    (define-key map (kbd "C-c l") #'term+toggle-buffer-log)
    (define-key map (kbd "C-c r") #'term+toggle-record)
    (define-key map (kbd "C-y") #'term+yank)
    map)
  "Keymap activated in `term-char-mode'.
It inherits the bindings of `term-raw-map'.

Keys in this keymap are available in `term-char-mode' and those
keys are not sent to the terminal directly (unless the command
bound to the key explicitly call function to send string to the
terminal such as `term-send-raw-string').

There are some keys that are marked as not to be sent directly
without any special binding (the default binding of Emacs is
used).  By default, such keys are

C-x
M-x
M-:

You can add this kind of keys by
\(define-key term+char-map KEY nil).")
(defvar term+line-map
  (let ((map (make-sparse-keymap)))
    ;; bind
    (define-key map (kbd "C-M-j") #'term-char-mode)
    (define-key map (kbd "M-RET") #'term-char-mode)
    (define-key map (kbd "RET") #'term+send-input-and-char-mode)
    (define-key map (kbd "C-a") #'term+beginning-of-line)
    (define-key map (kbd "C-e") #'term+end-of-line)
    (define-key map (kbd "M-p") #'term-previous-input)
    (define-key map (kbd "M-n") #'term-next-input)
    (define-key map (kbd "C-k") #'term+kill-line)
    (define-key map (kbd "C-c C-u") #'term+kill-input)
    (define-key map (kbd "C-c C-w") #'backward-kill-word)
    map)
  "Keymap activated in `term-line-mode'.
It inherits the bindings of `term-mode-map'.")

(defvar term+input-range-markers (cons nil nil))
(defvar term+input-overlay nil)
(defvar term+input-map-overlay nil)
(defvar term+input-readonly-overlay nil)
(defvar term+input-map term+line-map)
(defvar term+input-readonly-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " #'term+mark-or-copy)
    (define-key map (kbd "RET") #'term-char-mode)
    (define-key map (kbd "ESC") #'term-char-mode)
    map)
  "Keymap activated in read-only area in `term+edit-mode'.")

(defconst term+input-field 'term+input)

(defvar term+edit-last-pos nil)
(defvar term+edit-last-text nil)

(defvar term+buffer-log-function nil)
(defvar term+buffer-log-marker nil)
(defvar term+buffer-log-history-length nil)
(defvar term+buffer-log-file nil)
(defvar term+buffer-log-timer nil)
(defvar term+buffer-log-buffer nil)
(defvar term+record-file nil)
(defvar term+record-overlay nil)

(defconst term+osc-emacs "\033]51;%s;")
(defconst term+osc-sel "\033]52;%s;")
(defconst term+-st "\033\\")
(defconst term+-bel "\a")

(defvar term+control-commands nil)
(defvar term+control-command-pending-output nil)
(make-variable-buffer-local 'term+control-command-pending-output)
(defvar term+control-command-start-regexp nil)
(defvar term+control-command-max-len (list 0 0 0))

(defsubst term+new-protocol (protocol)
  (setq term-protocol-version (concat term-protocol-version "," protocol)))
(defsubst term+osc-emacs (name) (format term+osc-emacs name))
(defsubst term+osc-sel (name) (format term+osc-sel name))
(defsubst term+new-control-command (beg end cmd)
  (let ((len term+control-command-max-len))
    (setq term+control-command-max-len
          (list (max (string-width beg) (nth 0 len))
                (max (string-width end) (nth 1 len))
                (max (+ 2 (string-width (symbol-name cmd))) (nth 2 len)))))
  (put cmd 'function-documentation
       `(term+control-command-documentation (quote ,cmd) ,beg ,end))
  (add-to-list 'term+control-commands (list beg end cmd)))

(defvar term+shell-history-file nil
  "Shell history file.
`term+set-shell-history-file' sets this value.  If the value is
nil, then the current shell's default history file is used.  In
this case, the current shell must be properly set via
`term+set-shell' especially for a remote session.")
(defvar term+shell-history-function 'term+default-shell-history
  "Function to show prompt for `term+shell-history'.
It takes one argument, which is the initial text of the prompt.")
(defvar term+target-directory nil)

(defconst term+put-mode-buffer-name "*put multiple files*")


;; customizable variables

(defface term+input-face
  '((((class color) (min-colors 88) (background dark))
     :background "#575745")
    (((class color) (min-colors 88) (background light))
     :background "LemonChiffon")
    (((class color) (min-colors 8) (background dark))
     :background "yellow")
    (((class color) (min-colors 8) (background light))
     :background "yellow"))
  "Face of the input area in `term-line-mode'."
  :group 'term+)
(defface term+input-readonly-face
  '((((class color) (min-colors 88) (background dark))
     :background "gray30")
    (((class color) (min-colors 88) (background light))
     :background "gray70"))
  "Face of the readonly area in `term-line-mode'."
  :group 'term+)

(defcustom term+kill-buffer-at-exit t
  "Kill terminal buffer when the process exits."
  :type 'boolean
  :group 'term+)
(defcustom term+edit-kill-to-eol nil
  "t means `term+edit-as-expected' kills to end of line before it
switches to `term-line-mode'.  It is convenient to set this value
to t if you are using RPROMPT of the shell but it also kills to
end of line on non-shell programs."
  :type 'boolean
  :group 'term+)
(defcustom term+edit-restore-last-pos t
  "t means to restore the last position when `term+edit-mode'
quits."
  :type 'boolean
  :group 'term+)
(defcustom term+edit-quit-commands '(kill-ring-save)
  "Quit `term+edit-mode' after these commands."
  :type '(list function)
  :group 'term+)
(defcustom term+open-in-other-window nil
  "t means `term+open' or `term+view' shows file content in other
window."
  :type 'boolean
  :group 'term+)
(defcustom term+default-user nil
  "Default user to use for remote connection.
It is nil by default; otherwise settings in configuration files
like \"~/.ssh/config\" would be overwritten."
  :type '(choice (const nil) string)
  :group 'term+)
(defcustom term+shell-history-dont-exec nil
  "t means not to execute the selected command line but to paste
it to the terminal in `term+shell-history'."
  :type 'boolean
  :group 'term+)
(defcustom term+download-directory nil
  "The default local target directory for `term+get'."
  :type '(choice (constant nil) string)
  :group 'term+)
(defcustom term+upload-directory nil
  "The default local source directory for `term+put'."
  :type '(choice (constant nil) string)
  :group 'term+)
(defcustom term+hardcopy-visible-contents t
  "t means `term+hardcopy' saves only visible area of the buffer.
The value has no effect if the buffer is not the buffer of the
selected window."
  :type 'boolean
  :group 'term+)
(defcustom term+hardcopy-append nil
  "t means `term+hardcopy' appends a hardcopy to the end of specified file."
  :type 'boolean
  :group 'term+)
(defcustom term+hardcopy-separator '(">" "=" " %s@%s %s " "=" "<")
  "nil means `term+hardcopy' outputs no separator at all.
A non-nil value means `term+hardcopy' outputs a separator if
`term+hardcopy-append' is non-nil.  A symbol means that the
separator is a return value of the symbol's function.  The values
specified by `term+hardcopy-separator-args' are passed to the
function as arguments.  A list means that the separator is
constructed by the list elements.  The list must have five
strings (an empty string is used if there is an insufficient
element).  Each element may include a format string like \"%s\".
`format' is applied to the element and the values specified by
`term+hardcopy-separator-args'.  The resulting separator is the
concatenation of the elements.  If the length of the separator is
less than the terminal width, the second and the fourth element
are repeated to reach the terminal width."
  :type '(choice (constant nil) symbol (list string))
  :group 'term+)
(defcustom term+hardcopy-separator-args '(user host time)
  "A list of symbols used in `term+hardcopy-separator'.
Each symbol describes information available in the hardcopy
separator.  See `term+hardcopy-separator' for the detail.
Available symbols are:

user   describes the user name
host   describes the host name
dir    describes the current directory
time   describes the current time in ISO 8601 format"
  :type '(list symbol)
  :group 'term+)
(defcustom term+buffer-log-interval 5
  "Interval of auto saving terminal buffer contents.
A termnal buffer in which `term+start-buffer-log' is called is
automatically saved after this interval of idle time."
  :type 'number
  :group 'term+)
(defcustom term+record-append nil
  "t means `term+start-record' appends output to file."
  :type 'boolean
  :group 'term+)
(defcustom term+record-message
  (let ((reset '(:weight normal))
        (attrs '(:slant :underline :overline :strike-through :box))
        (color (face-foreground 'default))
        (rec-mark (string #x25cf)) (rec "REC"))
    (mapc #'(lambda (x) (setq simple (append (list x nil) reset))) attrs)
    (propertize
     (concat (propertize rec-mark 'face (append '(:foreground "red") reset))
             (propertize rec 'face (append `(:foreground ,color) reset)))
     'local-map (let ((map (make-sparse-keymap)))
                  (define-key map [mouse-1] #'term+mouse-stop-record)
                  map)
     'help-echo "Click to stop recording"
     'pointer 'hand))
  "Message shown during recording."
  :type '(choice (constant nil) 'string)
  :group 'term+)

(provide 'term+vars)
;;; term+vars.el ends here
