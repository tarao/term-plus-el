;;; term+file-transfer.el --- term+ file tranferring functionality

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


;;; edit / view files

(defun term+find-file (file &optional other-window)
  (if other-window
      (find-file-other-window file)
    (find-file file)))

(define-minor-mode term+temporal-view-mode
  "View mode which kills the buffer when it quits."
  :group 'term+
  :keymap '(([remap View-quit] . View-kill-and-leave)))

(defun term+view-file (file &optional other-window)
  (let ((buf (get-file-buffer file)))
    (with-current-buffer (term+find-file file other-window)
      (view-mode 1)
      (unless buf (term+temporal-view-mode 1))
      (current-buffer))))

(defun term+resolve-file-name (file)
  (let* ((absolute (file-name-absolute-p file))
         (dir (or (and absolute
                       (fboundp 'term+maybe-remote-directory)
                       (term+maybe-remote-directory "/"))
                  default-directory))
         (relative (let ((default-directory "/")) (file-relative-name file)))
         (file (if absolute relative file)))
    (expand-file-name file dir)))

(defun term+open (files &optional find-file)
  "Open FILES in Emacs.
FILES are \";\" separated list of files.

If `term+open-in-other-window' is non-nil, then the buffer of the
file is opened in the other window.  Otherwise, the buffer is
displayed in the selected window.

FILES can be relative paths.  In this case, `default-directory'
must be set properly.  Especially, when this function is called
from a terminal application, the application should also specify
the current directory of the application and a non-local user and
host name in which the application is running by calling
`term+set-directory', `term+set-user' and `term+set-hostname'."
  (let* ((find-file (or find-file 'term+find-file))
         (files (mapcar #'term+resolve-file-name (split-string files ";")))
         (file (car files)) (files (cdr files))
         (win (selected-window)))
    (funcall find-file file term+open-in-other-window)
    (dolist (file files) (funcall find-file file))
    (when (and (not term+open-in-other-window)
               (fboundp 'set-window-prev-buffers))
      (set-window-prev-buffers win nil))))

(defun term+view (files)
  "Open FILES in `view-mode'.
It behaves the same as `term+open' except that the buffer is in
`view-mode'."
  (term+open files 'term+view-file))

(defun term+copy-files (files target)
  (let ((one-file (and (consp files) (null (cdr files)) (car files)))
        (into-dir (file-directory-p target)))
    (when (not (or one-file into-dir))
      (error "Target must be a directory: %s" target))
    (unless into-dir (setq target (directory-file-name target)))
    (dired-create-files
     'dired-copy-file "Copy" files
     (if into-dir
         (function
          (lambda (from)
            (expand-file-name (file-name-nondirectory from) target)))
       (function (lambda (ignore) target))))))


;;; get / put files

(defun term+read-file-name (files prompt dir default)
  (let ((files (mapcar #'dired-make-relative files)))
    (dired-mark-pop-up " *Files*" 'copy files #'read-file-name
                       prompt dir default)))

(defun term+get (files)
  "Copy FILES to a local directory.
FILES are \";\" separated list of files.

FILES can be relative paths.  See `term+open' for the detail.

The local target directory defaults to `term+download-directory'
if it is non-nil.  The user's home directory (\"~/\") is used
otherwise."
  (interactive "fGet: ")
  (let* ((files (mapcar #'term+resolve-file-name (split-string files ";")))
         (one-file (and (consp files) (null (cdr files)) (car files)))
         (target-dir (or term+download-directory "~/"))
         (default (and one-file (expand-file-name (car files) target-dir)))
         (defaults (dired-dwim-target-defaults files default))
         (prompt-files (if one-file (car files)
                         (format "[%d files]" (length files))))
         (prompt (format "Copy %s to: " prompt-files))
         (target (expand-file-name
                  (minibuffer-with-setup-hook
                      (lambda ()
                        (set (make-local-variable
                              'minibuffer-default-add-function) nil)
                        (setq minibuffer-default defaults))
                    (term+read-file-name files prompt target-dir default)))))
    (term+copy-files files target)))

(defun term+put (&optional arg)
  "Copy a local file(s) to the remote host.

`default-directory' is used as a remote target directory.  See
`term+open' for the detail.

When the local source file is asked, `term+upload-directory' is
used as a default directory.

If ARG equals to \"m\", then after specifying the source
directory, `dired-mode' buffer appears and selecting files and
pressing \"c\" send the files to the remote host.

Otherwise, the local source file is asked and it is sent to the
remote host."
  (interactive)
  (if (string= arg "m")
      (term+put-multi)
    (let ((term+target-directory default-directory)
          (default-directory (or term+upload-directory "~/"))
          (form (get 'term+put-one 'interactive-form)))
      (put 'term+put-one 'interactive-form '(interactive "fPut: "))
      (call-interactively 'term+put-one)
      (put 'term+put-one 'interactive-form form))))

(defun term+put-one (files)
  (let ((files (mapcar #'expand-file-name (split-string files ";"))))
    (term+copy-files files term+target-directory)))

(defun term+put-multi ()
  "Copy a local file(s) to the remote host.
See `term+put' for the detail."
  (interactive)
  (let* ((term+target-directory default-directory)
         (default-directory (or term+upload-directory "~/"))
         (dir (read-directory-name "Directory: " nil nil t))
         (buf (term+put-mode-noselect dir)))
    (switch-to-buffer buf)
    (recursive-edit)
    (when (buffer-live-p buf)
      (when term+target-directory
        (term+copy-files (dired-get-marked-files) term+target-directory))
      (kill-buffer buf))))

(defun term+put-mode-noselect (dir)
  (when (get-buffer term+put-mode-buffer-name)
    (kill-buffer term+put-mode-buffer-name))
  (dired-internal-noselect (file-name-as-directory dir) nil 'term+put-mode))

(define-derived-mode term+put-mode dired-mode
  "C-c C-c:Put,  Q:Abort"
  "Put selected files."
  (rename-buffer term+put-mode-buffer-name t)
  (add-hook 'find-directory-functions #'term+put-mode-noselect nil t)
  (define-key term+put-mode-map (kbd "C-c C-c") #'exit-recursive-edit)
  (define-key term+put-mode-map (kbd "q") #'exit-recursive-edit)
  (define-key term+put-mode-map (kbd "Q") #'term+put-mode-abort)
  (define-key term+put-mode-map (kbd "C-c C-g") #'term+put-mode-abort))

(defun term+put-mode-abort ()
  "Exit `term+put-mode' without putting selected files."
  (interactive)
  (dired-unmark-all-marks)
  (setq term+target-directory nil)
  (exit-recursive-edit))

(provide 'term+file-transfer)
;;; term+file-transfer.el ends here
