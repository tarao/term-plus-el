;;; term+logging.el --- term+ logging functionality

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


;;; hardcopy

(defun term+hardcopy (file &optional append whole-buffer)
  "Save hardcopy of the terminal to FILE.
If APPEND or `term+hardcopy-append' is non-nil, the hardcopy is
appended at the at of FILE, preceded by a separator generated
from `term+hardcopy-separator-format' and
`term+hardcopy-separator-args'.  If WHOLE-BUFFER is non-nil or
`term+hardcopy-visible-contents' is nil, the whole buffer
contents is saved.  Otherwise, the visible area of the buffer is
saved."
  (interactive "FSave: ")
  (let* ((whole-buffer (or whole-buffer (null term+hardcopy-visible-contents)
                           (not (eq (window-buffer) (current-buffer)))))
         (beg (or (and whole-buffer (point-min)) (window-start)))
         (end (or (and whole-buffer (point-max)) (window-end)))
         (user term-ansi-at-user)
         (host term-ansi-at-host)
         (dir term-ansi-at-dir)
         (time (format-time-string "%Y-%m-%dT%T%Z"))
         (append (or append term+hardcopy-append))
         (separator (and append (term+make-hardcopy-separator))))
    (when (and separator (> (length separator) 0))
      (write-region separator nil file t))
    (write-region beg end file append nil nil (not append))))

(defun term+make-hardcopy-separator-from-list (list &rest args)
  (let* ((list (mapcar #'(lambda (x) (apply #'format x args)) list))
         (start (or (nth 0 list) "")) (end (or (nth 4 list) ""))
         (mid (or (nth 2 list) ""))
         (width (or (and (boundp 'term-width) term-width) 80))
         (width (- width (length start) (length mid) (length end)))
         (width (max 0 width))
         (fill1 (apply #'concat (make-list width (or (nth 1 list) ""))))
         (fill2 (apply #'concat (make-list width (or (nth 3 list) ""))))
         (width1 (if (= 0 (length fill2)) width (/ width 2)))
         (width2 (if (= 0 (length fill1)) width (+ (/ width 2) (% width 2))))
         (fill1 (substring fill1 0 (min width1 (length fill1))))
         (fill2 (substring fill2 0 (min width2 (length fill2))))
         (separator (concat start fill1 mid fill2 end)))
    (and (> (length separator) 0) (concat "\n" separator "\n"))))

(defun term+make-hardcopy-separator ()
  (let ((args (mapcar #'(lambda (x)
                          (or (and (symbolp x)
                                   (or (and (boundp x) (symbol-value x))
                                       (symbol-name x)))
                              x))
                      term+hardcopy-separator-args)))
    (cond
     ((and (symbolp term+hardcopy-separator) (fboundp term+hardcopy-separator))
      (or (apply term+hardcopy-separator args) ""))
     ((listp term+hardcopy-separator)
      (let ((term+hardcopy-separator-args (cons term+hardcopy-separator args))
            (term+hardcopy-separator 'term+make-hardcopy-separator-from-list))
        (term+make-hardcopy-separator)))
     (t ""))))


;;; dump buffer to file

(define-minor-mode term+buffer-log-mode
  "Minor mode for terminal buffer auto save."
  :group 'term+
  (if term+buffer-log-mode
      (progn
        ;; on
        (set (make-local-variable 'term+buffer-log-marker) (point-min-marker))
        (make-local-variable 'term+buffer-log-history-length)
        (make-local-variable 'term+buffer-log-file)
        (set (make-local-variable 'term+buffer-log-timer)
             (run-with-idle-timer term+buffer-log-interval t
                                  #'term+buffer-log-save (current-buffer)))
        (add-hook 'kill-buffer-hook #'term+stop-buffer-log nil t))
    ;; off
    (remove-hook 'kill-buffer-hook #'term+stop-buffer-log t)
    (set-marker term+buffer-log-marker nil)
    (kill-local-variable 'term+buffer-log-marker)
    (kill-local-variable 'term+buffer-log-history-length)
    (kill-local-variable 'term+buffer-log-file)
    (cancel-timer term+buffer-log-timer)
    (kill-local-variable 'term+buffer-log-timer)
    (when term+buffer-log-buffer
      (kill-buffer term+buffer-log-buffer))
    (kill-local-variable 'term+buffer-log-buffer)))

(defun term+start-buffer-log (file)
  "Start logging terminal buffer contents to FILE.
The buffer contents are saved when Emacs is idle for
`term+buffer-log-interval' seconds.

If `term-buffer-maximum-size' is set, the buffer contents may be
truncated to the maximum size but the discarded part of the
contents is also saved in FILE."
  (interactive "FSave: ")
  (if term+buffer-log-mode
      (error "Already logging to %s" term+buffer-log-file)
    (term+buffer-log-mode 1)
    (setq term+buffer-log-file file)
    (write-region "" nil file nil nil nil t)
    (term+buffer-log-save)))

(defun term+stop-buffer-log ()
  "Stop logging terminal buffer contents."
  (interactive)
  (if (not term+buffer-log-mode)
      (error "Logging not started")
    (term+buffer-log-save)
    (term+buffer-log-mode 0)))

(defun term+toggle-buffer-log ()
  "Start or stop logging terminal buffer contents."
  (interactive)
  (if term+buffer-log-mode
      (term+stop-buffer-log)
    (call-interactively #'term+start-buffer-log)))

(defsubst term+truncate-available-p () (executable-find "truncate"))

(defun term+buffer-log-save (&optional buffer)
  "Save contents of BUFFER to file.
BUFFER must be a terminal buffer.  `term+buffer-log-file' is used
for a target file.

This function takes care of `term+buffer-log-marker', which
indicates that buffer contents before that point never changes
anymore.  In other words, every time saving the buffer, the
target file must be reverted to that point so that buffer
contents after that point can be saved by just appending the
contents.

The actual save action is done by `term+buffer-log-function'.
The value of `term+buffer-log-function' can be either
`term+buffer-log-save-truncate' or `term+buffer-log-save-buffer'.
The former is selected by default.

`term+buffer-log-save-truncate' requires \"truncate\" command to
revert the target file.

`term+buffer-log-save-buffer' is selected when \"truncate\"
command is not available.  This function requires potentially a
heavy overhead because it uses an intermediate buffer to hold
terminal buffer contents including discarded parts and it writes
the whole buffer into the target file every time it is called."
  (unless buffer (setq buffer (current-buffer)))
  (when (and (buffer-modified-p buffer) term+buffer-log-file)
    (with-current-buffer buffer
      (unless term+buffer-log-function
        (setq term+buffer-log-function
              (if (term+truncate-available-p)
                  #'term+buffer-log-save-truncate
                #'term+buffer-log-save-buffer)))
      (when (and term+buffer-log-file
                 (boundp 'term-home-marker) term-home-marker)
        (funcall term+buffer-log-function
                 term+buffer-log-marker term-home-marker (point-max))
        (set-marker term+buffer-log-marker (marker-position term-home-marker)))
      (set-buffer-modified-p nil))))

(defun term+buffer-log-save-truncate (beg hist-pos end)
  (when term+buffer-log-history-length
    (call-process "truncate" nil nil nil
                  "-s" (format "<%d" term+buffer-log-history-length)
                  (expand-file-name term+buffer-log-file)))
  (when (< beg hist-pos)
    (write-region beg hist-pos term+buffer-log-file t)
    (setq term+buffer-log-history-length
          (nth 7 (file-attributes term+buffer-log-file))))
  (write-region hist-pos end term+buffer-log-file t))

(defun term+buffer-log-save-buffer (beg hist-pos end)
  (let ((name (format "*termi+buffer-log:%s*" term+buffer-log-file))
        (buffer (current-buffer)) (file term+buffer-log-file))
    (unless term+buffer-log-buffer
      (generate-new-buffer name)
      (set (make-local-variable 'term+buffer-log-buffer) (get-buffer name)))
    (with-current-buffer (get-buffer-create name)
      (let ((buffer-undo-list t))
        (goto-char (point-max))
        (when term+buffer-log-history-length
          (goto-char term+buffer-log-history-length)
          (delete-region (point) (point-max)))
        (when (< beg hist-pos)
          (insert-buffer-substring-no-properties buffer beg hist-pos)
          (setq term+buffer-log-history-length (point)))
        (insert-buffer-substring-no-properties buffer hist-pos end)
        (write-region nil nil file)))))

(defadvice term-emulate-terminal
  (before term+buffer-log-save-before-truncation (proc str) activate)
  "Save buffer contents before truncation."
  (when (> term-buffer-maximum-size 0)
    (with-current-buffer (process-buffer proc)
      (when (and term+buffer-log-mode
                 (> (+ (count-lines (marker-position term+buffer-log-marker)
                                    (point-max))
                       (/ (length str)
                          (or (and (boundp 'term-width) term-width) 1)))
                    (/ term-buffer-maximum-size 2)))
        (term+buffer-log-save)))))


;;; record terminal (ttyrec compatible recording)

(define-minor-mode term+record-mode
  "Minor mode for terminal recording."
  :group 'term+
  (if term+record-mode
      (progn
        ;; on
        (when term+record-message
          (set (make-local-variable 'term+record-overlay) (make-overlay 0 0))
          (overlay-put term+record-overlay 'priority 20000)
          (term+record-show-overlay)
          (add-hook 'post-command-hook #'term+record-show-overlay nil t)
          ;; we need to move the overlay each time (window-start) is to
          ;; be updated; calling `term+record-show-overlay' at the end of
          ;; `term-emulate-terminal' is not enough because the return value
          ;; of (window-start) won't be updated until the next redisplay
          (add-hook 'window-scroll-functions #'term+record-show-overlay nil t))
        (make-local-variable 'term+record-file))
    ;; off
    (when term+record-overlay
      (remove-hook 'window-scroll-functions #'term+record-show-overlay t)
      (remove-hook 'post-command-hook #'term+record-show-overlay t)
      (delete-overlay term+record-overlay)
      (kill-local-variable 'term+record-overlay))
    (kill-local-variable 'term+record-file)))

(defun term+start-record (file)
  "Start recording output of the terminal to FILE."
  (interactive "FSave: ")
  (term+record-mode 1)
  (setq term+record-file file)
  (unless term+record-append (write-region "" nil file nil nil nil t)))

(defun term+stop-record ()
  "Stop recording output of the terminal."
  (interactive)
  (message (format "Recorded to %s" term+record-file))
  (term+record-mode 0))

(defun term+toggle-record ()
  "Start or stop recording output of the terminal."
  (interactive)
  (if term+record-mode
      (term+stop-record)
    (call-interactively #'term+start-record)))

(defun term+mouse-stop-record (event)
  "Stop recording output of the terminal."
  (interactive "@e")
  (term+stop-record)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pt (and proc (process-mark proc))))
    (when pt (goto-char pt))))

(eval-and-compile
  (unless (fboundp 'window-body-width)
    (defun window-body-width (&optional window)
      (let ((e (window-inside-edges window)))
        (- (nth 2 e) (nth 0 e))))))

(defun term+record-show-overlay (&optional window start)
  (with-selected-window (or window (selected-window))
    (when term+record-overlay
      (let ((inhibit-redisplay t) (point (point)))
        (goto-char (or start (window-start)))
        (let* ((bol (point)) (eol (line-end-position))
               (h (- (window-body-width) (string-width term+record-message) 1))
               (pos (+ bol h)) (str term+record-message)
               display after-string)
          (cond ((<= eol pos)
                 (let* ((align `(space :align-to ,h))
                        (space (propertize " " 'display align)))
                   (setq pos eol
                         after-string (concat space str)
                         display nil)))
                (t (setq display str after-string nil)))
          (overlay-put term+record-overlay 'display display)
          (overlay-put term+record-overlay 'after-string after-string)
          (move-overlay term+record-overlay pos eol)
          (goto-char point))))))

(defsubst term+record-bytes-string (&rest integers)
  (apply #'unibyte-string (mapcar #'(lambda (x) (logand x #xff)) integers)))
(defsubst term+record-short-to-bytes (integer)
  (list (logand integer #xff) (logand (lsh integer -8) #xff)))
(defsubst term+record-short-string (integer)
  (apply #'term+record-bytes-string (term+record-short-to-bytes integer)))
(defsubst term+record-long-to-bytes (integer)
  (append (term+record-short-to-bytes (logand integer #xffff))
          (term+record-short-to-bytes (logand (lsh integer -16) #xffff))))
(defsubst term+record-long-string (integer)
  (apply #'term+record-bytes-string (term+record-long-to-bytes integer)))

(defun term+record-write (time str)
  (when (> (length str) 0)
    (let ((sec (concat (term+record-short-string (nth 1 time))
                       (term+record-short-string (nth 0 time))))
          (usec (term+record-long-string (nth 2 time)))
          (len (term+record-long-string (string-bytes str))))
      (write-region (concat sec usec len str) nil term+record-file t :x))))

(defadvice term-emulate-terminal
  (around term+record-terminal-output (proc str) activate)
  "Record terminal output when `term+record-mode' is activated."
  (if (with-current-buffer (process-buffer proc)
        (or (not term+record-mode) (not term+record-file)))
      ad-do-it
    (let ((time (current-time)) ; get current time before execute
          (inhibit-redisplay t))
      (delete-overlay term+record-overlay)
      ad-do-it
      (with-current-buffer (process-buffer proc)
        (term+record-show-overlay)
        (term+record-write time str)))))

(provide 'term+logging)
;;; term+logging.el ends here
