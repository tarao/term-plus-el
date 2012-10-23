;;; xterm-256color.el --- term-mode xterm compatibility

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
(eval-when-compile (require 'cl))

(setq term-term-name "xterm-256color")
(when (and (featurep 'term+) (fboundp 'term+new-protocol))
  (term+new-protocol "256color")
  (term+new-protocol "xterm"))

(defvar term-width)
(defvar term-height)
(defvar term-home-marker)

(setq ansi-term-color-vector
      [unspecified
       "black"   "#c23621" "#25bc24" "#adad27"
       "#492ee1" "#d338d3" "#33bbc8" "#cbcccd"
       "#818383" "#fc391f" "#31e722" "#eaec23"
       "#5833ff" "#f935f8" "#14f0f0" "white"])

;; 256 colors
(defvar term-ansi-256-reset nil)
(defvar term-ansi-256-state nil)
(defvar term-ansi-current-bright nil)
(defvar term-ansi-current-italic nil)
(defvar term-ansi-current-strike nil)
(defvar term-ansi-current-overline nil)
(defvar term-ansi-current-frame nil)
(defvar term-ansi-current-256-color nil)
(defvar term-ansi-current-256-bg-color nil)
(defun term-ansi-256-setup ()
  ;; fix default color
  (when (equal term-default-fg-color "unspecified-fg")
    (set (make-local-variable 'term-default-fg-color)
         (face-foreground 'default)))
  (when (equal term-default-bg-color "unspecified-bg")
    (set (make-local-variable 'term-default-bg-color)
         (face-background 'default)))
  (make-local-variable 'term-ansi-256-reset)
  (make-local-variable 'term-ansi-256-state)
  (make-local-variable 'term-ansi-current-bright)
  (make-local-variable 'term-ansi-current-italic)
  (make-local-variable 'term-ansi-current-strike)
  (make-local-variable 'term-ansi-current-overline)
  (make-local-variable 'term-ansi-current-frame)
  (make-local-variable 'term-ansi-current-256-color)
  (make-local-variable 'term-ansi-current-256-bg-color))
(add-hook 'term-mode-hook #'term-ansi-256-setup)
(defadvice term-ansi-reset (after term-reset-256-color activate)
  "Reset variables for 256 colors."
  (setq term-ansi-256-reset t)
  (setq term-ansi-256-state nil)
  (setq term-ansi-current-bright nil)
  (setq term-ansi-current-italic nil)
  (setq term-ansi-current-strike nil)
  (setq term-ansi-current-overline nil)
  (setq term-ansi-current-frame nil)
  (setq term-ansi-current-256-color nil)
  (setq term-ansi-current-256-bg-color nil))
(defadvice term-reset-terminal (after term-reset-terminal-256 activate)
  "Clear reset flag.
This corresponds to (setq term-ansi-face-already-done nil) for
non-256-color handling."
  (setq term-ansi-256-reset nil))
(defun term-ansi-set-16-color (color &optional background bright)
  (let* ((prop (if background :background :foreground))
         (color (if (and bright (<= 1 color) (<= color 8)) (+ color 8) color))
         (color (elt ansi-term-color-vector color)))
    (setq term-current-face (plist-put term-current-face prop color))))
(defvar term-ansi-256-color-vector [#x00 #x5F #x87 #xAF #xD7 #xFF])
(defun term-ansi-256-color (parameter)
  (cond
   ((and (<= 0 parameter) (< parameter 16))
    ;; system colors
    (elt ansi-term-color-vector (1+ parameter)))
   ((and (<= 16 parameter) (< parameter 232))
    ;; 6x6x6 colors
    (let* ((rgb (- parameter 16))
           (ib (% rgb 6))
           (ig (% (/ rgb 6) 6))
           (ir (/ rgb 36))
           (b (elt term-ansi-256-color-vector ib))
           (g (elt term-ansi-256-color-vector ig))
           (r (elt term-ansi-256-color-vector ir)))
      (format "#%02X%02X%02X" r g b)))
   ((and (<= 232 parameter) (< parameter 256))
    (let* ((step (- parameter 232))
           (g (+ 8 (* 10 step))))
      (format "#%02X%02X%02X" g g g)))
   (t (elt ansi-term-color-vector 0))))
(defun term-ansi-set-256-color (color &optional background)
  (let ((prop (if background :background :foreground))
        (color (term-ansi-256-color color)))
    (setq term-current-face (plist-put term-current-face prop color))))
(defun term-warn-unknown-color (parameter)
  (cond
   ((= parameter 0) t)
   ((= parameter 1) t)
   ((= parameter 4) t)
   ((= parameter 5) t)
   ((= parameter 7) t)
   ((= parameter 8) t)
   ((= parameter 24) t)
   ((= parameter 27) t)
   ((and (<= 30 parameter) (<= parameter 37)) t)
   ((= parameter 39) t)
   ((and (<= 40 parameter) (<= parameter 47)) t)
   ((= parameter 49) t)
   (t (message "Unknown ANSI color sequence: %d" parameter))))
(defsubst term-face (face &rest props) (append props face))
(defadvice term-handle-colors-array
  (around term-256-color (parameter) activate)
  "Handle 256-color parameters."
  (cond
   ((and (eq term-ansi-256-state 'fg) (= 5 parameter))
    (setq term-ansi-256-state 'fg-color))
   ((and (eq term-ansi-256-state 'bg) (= 5 parameter))
    (setq term-ansi-256-state 'bg-color))
   ((and (eq term-ansi-256-state 'fg-color) (<= 0 parameter) (< parameter 256))
    (setq term-ansi-256-state nil)
    (setq term-ansi-current-color 0)
    (setq term-ansi-current-256-color parameter))
   ((and (eq term-ansi-256-state 'bg-color) (<= 0 parameter) (< parameter 256))
    (setq term-ansi-256-state nil)
    (setq term-ansi-current-bg-color 0)
    (setq term-ansi-current-256-bg-color parameter))
   (t
    (setq term-ansi-256-state nil)
    (cond
     ((= 1 parameter) ; bright
      (setq term-ansi-current-bright t)
      ad-do-it) ; bold as well
     ((= 2 parameter) ; faint
      (setq term-ansi-current-bright nil))
     ((= 3 parameter) ; italic
      (setq term-ansi-current-italic t))
     ((= 9 parameter) ; strike out
      (setq term-ansi-current-strike t))
     ((= 22 parameter) ; not bright and not bold
      (setq term-ansi-current-bold nil)
      (setq term-ansi-current-bright nil))
     ((= 23 parameter) ; not italic
      (setq term-ansi-current-italic nil))
     ((= 29 parameter) ; not strike out
      (setq term-ansi-current-strike nil))
     ((= 38 parameter) ; fg color
      (setq term-ansi-256-state 'fg))
     ((= 48 parameter) ; bg color
      (setq term-ansi-256-state 'bg))
     ((= 51 parameter) ; frame
      (setq term-ansi-current-frame t))
     ((= 53 parameter) ; overline
      (setq term-ansi-current-overline t))
     ((= 54 parameter) ; frame off
      (setq term-ansi-current-frame nil))
     ((= 55 parameter) ; overline off
      (setq term-ansi-current-overline nil))
     ((and (<= 90 parameter) (<= parameter 97)) ; bright foreground
      (setq term-ansi-current-color (+ (- parameter 90) 9)))
     ((and (<= 100 parameter) (<= parameter 107)) ; bright background
      (setq term-ansi-current-bg-color (+ (- parameter 100) 9)))
     (t
      (term-warn-unknown-color parameter)
      ad-do-it))))
  (unless term-ansi-256-reset
    (setq term-current-face (if term-ansi-current-reverse
                                (list :background term-default-fg-color
                                      :foreground term-default-bg-color)
                              (list :foreground term-default-fg-color
                                    :background term-default-bg-color)))
    (when (not (= term-ansi-current-color 0))
      (term-ansi-set-16-color term-ansi-current-color
                              term-ansi-current-reverse
                              term-ansi-current-bright)
      (setq term-ansi-current-256-color nil))
    (when term-ansi-current-256-color
      (term-ansi-set-256-color term-ansi-current-256-color
                               term-ansi-current-reverse))
    (when (not (= term-ansi-current-bg-color 0))
      (term-ansi-set-16-color term-ansi-current-bg-color
                              (not term-ansi-current-reverse))
      (setq term-ansi-current-256-bg-color nil))
    (when term-ansi-current-256-bg-color
      (term-ansi-set-256-color term-ansi-current-256-bg-color
                               (not term-ansi-current-reverse)))
    (if term-ansi-current-invisible
        (setq term-current-face
              (plist-put term-current-face :foreground
                         (plist-get term-current-face :background)))
      (when term-ansi-current-bold
        (setq term-current-face
              (append term-bold-attribute term-current-face)))
      (when term-ansi-current-italic
        (setq term-current-face (term-face term-current-face :slant 'italic)))
      (when term-ansi-current-underline
        (setq term-current-face (term-face term-current-face :underline t)))
      (when term-ansi-current-overline
        (setq term-current-face (term-face term-current-face :overline t)))
      (when term-ansi-current-strike
        (setq term-current-face
              (term-face term-current-face :strike-through t)))
      (when term-ansi-current-frame
        (setq term-current-face (term-face term-current-face :box t)))))
  (setq term-ansi-256-reset nil))

;; xterm compatibility
(defun term-need-filling ()
  (not (or (eq term-current-face 'default)
           (eq (plist-get term-current-face :background)
               term-default-bg-color))))
(defun term-fill-char (char count)
  (let ((old-point (point)))
    (insert-char char count)
    (put-text-property old-point (point) 'face term-current-face)))
(defun term-fill-lines (count)
  (dotimes (i count)
    (let ((old-point (point)))
      (insert-char ?  term-width)
      (insert-char ?\n 1)
      (put-text-property old-point (point) 'face term-current-face))))
(defun term-fill-region (start end)
  (while (< start end)
    (goto-char start)
    (let* ((bol (save-excursion (beginning-of-line) (point)))
           (eol (save-excursion (end-of-line) (point)))
           (term-end (+ bol term-width))
           (len (- (if (<= end eol) end term-end) start)))
      (delete-region start (min eol end))
      (goto-char start)
      (term-fill-char ?  len)
      (setq end (- end (- (min eol end) start len))) ;; shrink or expand
      (end-of-line)
      (when (< (point) end) (forward-char))
      (setq start (point)))))
(defun term-erase-to-eol ()
  (let* ((bol (save-excursion (beginning-of-line) (point)))
         (eol (save-excursion (end-of-line) (point)))
         (term-end (+ bol term-width)))
    (when (< (point) eol) (delete-region (point) eol))
    (when (< (point) (+ bol term-width))
      (term-fill-char ?  (- (+ bol term-width) (point))))))
(defun term-erase-in-display (kind)
  "Erase (that is blank out) part of the window.
If KIND is 0, erase from (point) to (point-max); if KIND is 1,
erase from home to point; else erase from home to point-max."
  (term-handle-deferred-scroll)
  (cond
   ((eq kind 1)
    (let ((pos (point)) (start-region term-home-marker)
          (end-region (if (eq kind 1) (1+ (point)) (point-max))))
      (term-fill-region start-region end-region)
      (goto-char pos)))
   ((eq kind 2)
    (term-goto term-height (term-current-column))
    (delete-region (point) (point-max))
    (let ((term-scroll-with-delete nil))
      (term-handle-scroll term-height)))
   ((= (point) term-home-marker)
    (term-erase-in-display 2))
   (t
    (let ((pos (point)))
      (if (term-need-filling)
          (progn (term-fill-region (point) (point-max)) (term-erase-to-eol))
        (delete-region (point) (point-max)))
      (goto-char pos)))))
(defadvice term-erase-in-line
  (around xterm-erase-in-line (kind) activate)
  "With rendition."
  (cond
   ((eq kind 1) ; erase left of point
    (let ((pos (point))
          (cols (term-horizontal-column)) (saved-point (1+ (point))))
      (term-vertical-motion 0)
      (term-fill-region (point) saved-point)
      (goto-char pos)))
   (t ; erase right of point
    (when (eq kind 2) (term-goto (term-current-column) 0)) ; erase whole line
    (let ((pos (point)))
      (if (term-need-filling) (term-erase-to-eol) ad-do-it)
      (goto-char pos)))))
(defadvice term-insert-lines (around xterm-insert-lines activate)
  "Fill lines if the current color is not the default color."
  (if (term-need-filling)
      (flet ((term-insert-char (char count) (term-fill-lines count)))
        ad-do-it)
    ad-do-it))
(defadvice term-delete-chars (around xterm-delete-char activate)
  "Put deleted length of blank at the end of line if the current
color is not the default color."
  (if (term-need-filling)
      (progn
        (let ((term-current-face 'default) (pos (point)))
          (end-of-line)
          (term-erase-to-eol)
          (goto-char pos))
        ad-do-it
        (let ((pos (point)))
          (end-of-line)
          (term-erase-to-eol)
          (goto-char pos)))
    ad-do-it))
(defadvice term-insert-spaces (around xterm-insert-spaces activate)
  "Fill space of the selected color."
  (letf (((symbol-function 'term-insert-char)
          (symbol-function 'term-fill-char)))
    ad-do-it))
(defun term-warn-unknown-sequence (char)
  (cond
   ((eq char ?H) t)
   ((eq char ?A) t)
   ((eq char ?B) t)
   ((eq char ?C) t)
   ((eq char ?D) t)
   ((eq char ?J) t)
   ((eq char ?K) t)
   ((eq char ?L) t)
   ((eq char ?M) t)
   ((eq char ?P) t)
   ((eq char ?@) t)
   ((eq char ?h) t)
   ((eq char ?l) t)
   ((eq char ?m) t)
   ((eq char ?n) t)
   ((eq char ?r) t)
   (t (message "Unknown ANSI escape sequence: %c" char))))
(defvar term-terminal-csi-state nil)
(make-variable-buffer-local 'term-terminal-csi-state)
(defvar term-terminal-pending-output nil)
(make-variable-buffer-local 'term-terminal-pending-output)
(defadvice term-handle-ansi-escape
  (around xterm-handle-ansi-escape-more (proc char) activate)
  "xterm compatibility and bug fix."
  (cond
   ((eq term-terminal-csi-state ?>)
    ;; unsupported
    (message (format "Sequence \033[>...%c is not supported yet" char))
    (setq term-terminal-csi-state nil))
   ((eq char ?>)
    (setq term-terminal-csi-state ?>)
    (when (and (boundp 'str) (boundp 'str-length) (boundp 'i))
      (let ((rest (substring str (1+ i))))
        (setq str (substring str 0 (1+ i))
              str-length i)
        (when (> (length rest) 0) (setq term-terminal-pending-output rest)))))
   ((eq char ?d) ; move to row (terminfo: vpa)
    (term-goto (1- (max term-terminal-parameter 1)) (term-current-column)))
   ((eq char ?f) ; the same as cup (terminfo: cpl)
    (term-handle-ansi-escape proc ?H))
   ((eq char ?B) ; cursor down (terminfo: cud)
    ;; Fix bug: it says that "The `term-scroll-end' line is part of the
    ;; scrolling region", so we have no need to do (1- term-scroll-end)
    (let ((tcr (term-current-row)))
      (unless (= tcr term-scroll-end)
        (term-down
         (if (> (+ tcr term-terminal-parameter) term-scroll-end)
             (- term-scroll-end tcr)
           (max 1 term-terminal-parameter)) t))))
   ((eq char ?G) ; move to column (terminfo: cha)
    (term-goto term-current-row (1- (max term-terminal-parameter 1))))
   ((eq char ?X) ; erase character (with renditions) (terminfo: ech)
    (let* ((pos (point))
           (bol (save-excursion (beginning-of-line) (point)))
           (eol (save-excursion (end-of-line) (point)))
           (end (min (+ (point) (max term-terminal-parameter 1))
                     (+ bol term-width))))
      (if (>= eol end)
          (term-fill-region (point) end)
        (term-fill-region (point) eol)
        (term-fill-char ?  (- end eol)))
      (goto-char pos)))
   ((eq char ?l)
    (cond
     ((or (eq term-terminal-parameter 1047)
          (eq term-terminal-parameter 1049))
      (setq term-scroll-with-delete nil))
     (t ad-do-it)))
   (t
    (term-warn-unknown-sequence char)
    ad-do-it)))
(defadvice term-emulate-terminal
  (around xterm-ignore-some-sequence (proc str) activate)
  "Ignore ISO/IEC 2022 sequence \033(x \033)x \033*x \033+x."
  (setq str (replace-regexp-in-string "\033[()*+]." "" str))
  ad-do-it)
(defadvice term-emulate-terminal
  (after xterm-pending-output (proc str) activate)
  "Handle pending output."
  (with-current-buffer (process-buffer proc)
    (when term-terminal-pending-output
      (cond (term-terminal-csi-state
             (setq term-terminal-state 3)))
      (let ((str term-terminal-pending-output))
        (setq term-terminal-pending-output nil)
        (term-emulate-terminal proc str)))))

;; override the default setting
(setq term-scroll-to-bottom-on-output t)

(provide 'xterm-256color)
;;; xterm-256color ends here
