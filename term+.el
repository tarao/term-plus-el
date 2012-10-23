;;; term+.el --- term-mode enhancement

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
(require 'term+edit)
(require 'term+logging)
(require 'term+file-transfer)
(require 'term+shell-history)
(eval-when-compile (require 'cl))


;;; protocols

(setq term-protocol-version (concat term-protocol-version "+"))
(term+new-control-command (term+osc-emacs "host") term+-st 'term+set-hostname)
(term+new-control-command (term+osc-emacs "user") term+-st 'term+set-user)
(term+new-control-command (term+osc-emacs "cd") term+-st 'term+set-directory)
(term+new-control-command (term+osc-emacs "shell") term+-st 'term+set-shell)
(term+new-control-command (term+osc-emacs "histfile") term+-st
                          'term+set-shell-history-file)
(term+new-control-command (term+osc-emacs "open") term+-st 'term+open)
(term+new-control-command (term+osc-emacs "view") term+-st 'term+view)
(term+new-control-command (term+osc-emacs "get") term+-st 'term+get)
(term+new-control-command (term+osc-emacs "put") term+-st 'term+put)
(term+new-control-command (term+osc-sel "i") term+-st 'term+edit-insert)
(term+new-control-command (term+osc-sel "h") term+-st 'term+shell-history)


;;; commands

(defun term+pass-through (char)
  "Send a character to the terminal."
  (interactive "cCharacter: ")
  (term-send-raw-string (string char)))

(defun term+send-esc ()
  "Send ESC to the terminal."
  (interactive)
  (term+pass-through ?\e))

(defun term+yank ()
  "Reinsert (\"paste\") the last stretch of killed text to the terminal.
It is the same as `yank' except that it sends the text to the
terminal and ignores `yank-handler' property of the text."
  (interactive)
  (letf (((symbol-function 'insert-for-yank)
          (symbol-function 'term-send-raw-string)))
    (setq this-command 'yank)
    (call-interactively 'yank)))

(defun term+send-input-and-char-mode ()
  "Send input to the terminal and switch to `term-char-mode'."
  (interactive)
  (term-send-input)
  (term-char-mode))

(defun term+mark-or-copy ()
  "Copy if there is a region or set mark otherwise.
If setting mark, `set-mark-command' is called.  Otherwise,
`kill-ring-save' is called."
  (interactive)
  (if (region-active-p)
      (progn
        (setq this-command 'kill-ring-save)
        (call-interactively 'kill-ring-save))
    (setq this-command 'set-mark-command)
    (call-interactively 'set-mark-command)))


;;; control commands

(defsubst term+shorten-hostname (host)
  (car (split-string host "[.]")))
(defsubst term+system-name ()
  (term+shorten-hostname (system-name)))
(defsubst term+local-host-p (host)
  (or (equal host (term+system-name)) (equal host (system-name))))
(defsubst term+local-user-p (user)
  (let ((user (or user term+default-user)))
    (or (not user) (string= user (user-real-login-name)))))

(defun term+maybe-remote-directory (dir)
  (let ((host term-ansi-at-host)
        (user (or term-ansi-at-user term+default-user)))
    (expand-file-name
     (file-name-as-directory
      (if (and (term+local-host-p host)
               (term+local-user-p user))
          dir
        (if (not user)
            (concat "/" host ":" dir)
          (if (and (string= user "root") (term+local-host-p host))
              (concat "/sudo:root@localhost:" dir)
            (concat "/" user "@" host ":" dir))))))))

(defun term+update-default-directory ()
  (setq default-directory (term+maybe-remote-directory term-ansi-at-dir)))

(defun term+set-hostname (host)
  "Change the host part of `default-directory' to HOST."
  (setq term-ansi-at-host host)
  (term+update-default-directory))

(defun term+set-user (user)
  "Change the user part of `default-directory' to USER."
  (setq term-ansi-at-user user)
  (term+update-default-directory))

(defun term+set-directory (dir)
  "Change the directory part of `default-directory' to DIR."
  (setq term-ansi-at-dir dir)
  (term+update-default-directory))

(defun term+set-shell (shell)
  "Change the current shell to SHELL."
  (set (make-local-variable 'shell-file-name) shell))


;;; handle control commands

(defadvice term-emulate-terminal
  (around term+control-commands (proc str) activate)
  "Additional special control sequences are supported.
They are called \"control commands\" and each of them has a form
START-SEQUENCE STRING END-SEQUENCE where START-SEQUENCE and
END-SEQUENCE are fixed character streams read from a terminal
application, which designate a handler function.  STRING is an
arbitrary string passed to the handler function.  The pairs of
START-SEQUENCE and END-SEQUENCE and their corresponding handlers
are listed below.

START-SEQUENCE must be read at once.  If the terminal application
outputs START-SEQUENCE at the end of a very long text, Emacs may
split them in the middle of START-SEQUENCE and the control
command is not handled properly.  A workaround for this is to
\"sleep 0\" to flush the preceding output before writing
START-SEQUENCE."
  (unless term+control-command-start-regexp
    ;; make regexp for the all control command start markers combined
    (let ((strs (mapcar #'(lambda(x) (regexp-quote (car x)))
                        term+control-commands)))
      (setq term+control-command-start-regexp (regexp-opt strs))))
  (with-current-buffer (process-buffer proc)
    (when term+control-command-pending-output
      ;; there is pending output
      (setq str (concat term+control-command-pending-output str)))
    (while (and (> (length str) 0)
                (string-match term+control-command-start-regexp str))
      ;; there is a control command start marker
      (let* ((head (match-beginning 0))
             (start (match-string 0 str))
             (spec (cdr (assoc start term+control-commands))) pending)
        (let ((term+control-command-pending-output nil))
          ;; process characters before the control command start marker
          ;; we have no pending output this time
          (term-emulate-terminal proc (substring str 0 head)))
        ;; the rest of the string is potential pending output
        (setq pending (and (consp spec) (substring str head))
              str (substring str (+ head (length start))))
        (when (consp spec)
          (let* ((regexp (concat "\\(" (regexp-quote (car spec)) "\\)"))
                 (cmd (cadr spec)) (end (string-match regexp str))
                 (tail (match-end 1)))
            (cond (end ;; we found the end marker and no pending output
                   (save-current-buffer
                     (funcall cmd (substring str 0 end)))
                   (goto-char (process-mark proc))
                   (setq str (substring str tail) pending nil))
                  (t   ;; we have nothing to process other than pending output
                   (setq str "")))))
        (setq term+control-command-pending-output pending))))
  (when (> (length str) 0)
    ad-do-it))

(defun term+control-command-list-1 ()
  (let* ((len term+control-command-max-len)
         (start-len (nth 0 len)) (end-len (nth 1 len)) (cmd-len (nth 2 len))
         (start-head "Start") (end-head "End") (cmd-head "Handler") doc)
    (setq start-len (max start-len (string-width start-head))
          end-len (max end-len (string-width end-head))
          cmd-len (max cmd-len (string-width cmd-head))
          doc (concat "[Control Commands]\n\n"
                      (format (format "%%-%ds" start-len) start-head) "  "
                      (format (format "%%-%ds" end-len) end-head) "  "
                      cmd-head "\n"
                      (make-string start-len ?-) "  "
                      (make-string end-len ?-) "  "
                      (make-string cmd-len ?-) "\n"))
    (dolist (tuple (reverse term+control-commands))
      (setq doc
            (concat doc
                    (format (format "%%-%ds" start-len) (nth 0 tuple)) "  "
                    (format (format "%%-%ds" end-len) (nth 1 tuple)) "  "
                    "`" (symbol-name (nth 2 tuple)) "'\n")))
    doc))

(defun term+control-command-list ()
  "List all supported control commands."
  (interactive)
  (help-setup-xref (list #'term+control-command-list)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ (term+control-command-list-1))
    (with-current-buffer standard-output
      (buffer-string))))

(defmacro term+with-original-documentation (fun bind &rest body)
  (declare (indent defun))
  (let ((doc-sym (car bind)))
    `(let ((original (get ,fun 'function-documentation)) result)
       (put ,fun 'function-documentation nil)
       (let ((,doc-sym (documentation ,fun))) (setq result (progn ,@body)))
       (put ,fun 'function-documentation original)
       result)))

(defun term+control-command-documentation (&optional fun start end)
  (let* ((fun (or fun 'term-emulate-terminal))
         (msg "This function is a control command assigned to")
         (msg-end "See `term-emulate-terminal' for the detail.")
         (append (or (and fun start end
                         (format "\n\n%s \"%s...%s\".\n%s"
                                 msg start end msg-end))
                     (concat "\n\n" (term+control-command-list-1)))))
    (term+with-original-documentation fun (doc)
      (concat doc append))))
(put 'term-emulate-terminal 'function-documentation
     '(term+control-command-documentation))


;;; terminal width hack

(defadvice term-window-width (around term+window-width activate)
  "Always use entire window as terminal window.
`term' sets (1- (window-width)) in terminal-mode Emacs (emacs
-nw) because there is no way to remove truncation glyph at the
end of line.  We can use (window-width) for now since we have
zero-width-space hack for not displaying truncation glyph."
  (setq ad-return-value (window-width)))

(defun term+zero-width-truncation ()
  "Silently wrap long lines by using zero-width space as
truncation glyph."
  (setq truncate-lines t)
  (set-display-table-slot buffer-display-table 'truncation #x200b))


;;; setup

(defadvice term-char-mode (after term+enter-char-mode activate)
  "Some keys are not sent directly and handled specially by
Emacs.

\\{term+char-map}

There are some keys that are marked as not to be sent directly
and not to be handled by any special binding (the default binding
of Emacs is used).  See `term+char-map' for the detail."
  (setq term+char-mode t term+line-mode nil)
  (run-hooks 'term+char-mode-hook))
(defadvice term-line-mode (after term+enter-line-mode activate)
  "Some key bindings are available in addition to line mode map of `term-mode'.

\\{term+line-map}"
  (setq term+char-mode nil term+line-mode t)
  (run-hooks 'term+line-mode-hook))
(defadvice term-handle-exit (around term+handle-exit activate)
  (if term+kill-buffer-at-exit
      (kill-buffer)
    ad-do-it))

(defun term+setup ()
  ;; no truncation glyph
  (term+zero-width-truncation)
  ;; disable some appearance options
  (set (make-local-variable 'show-trailing-whitespace) nil)
  (set (make-local-variable 'show-paren-mode) nil)
  ;; disable backup
  (set (make-local-variable 'auto-save-default) nil)
  ;; fix variable locality and default value
  (set (make-local-variable 'term-ansi-at-host) (term+system-name))
  (make-local-variable 'term-ansi-at-user)
  ;; char/line mode
  (make-local-variable 'term+char-mode)
  (make-local-variable 'term+line-mode)
  ;; line mode map
  (use-local-map (set (make-local-variable 'term-mode-map)
                      (term+make-composed-keymap term+line-map term-mode-map)))
  ;; char mode map
  (set (make-local-variable 'term-raw-map)
       (term+make-composed-keymap term+char-map term-raw-map))
  ;; initial mode
  (run-hooks 'term+line-mode-hook))
(add-hook 'term-mode-hook #'term+setup)

(provide 'term+)
;;; term+.el ends here
