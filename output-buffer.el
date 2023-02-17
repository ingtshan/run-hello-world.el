;;; output-buffer.el --- `output-buffer' used by *Output* buffers  -*- lexical-binding: t; -*-

;; Maintainer: ingtshan@qq.com
;; Keywords: run-hello, run-output
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'compile)

;; (defvar output-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "q" 'quit-window)
;;     ;; vim like
;;     (define-key map "j" 'next-line)
;;     (define-key map "l" 'forward-char)
;;     (define-key map "k" 'previous-line)
;;     (define-key map "h" 'backward-char)
;;     (define-key map (kbd "C-d") 'scroll-up)
;;     (define-key map (kbd "C-u") 'scroll-down)
;;     ;; emacs like
;;     (define-key map (kbd "C-f") 'forward-char)
;;     (define-key map (kbd "C-b") 'backward-char)
;;     (define-key map (kbd "C-p") 'previous-line)
;;     (define-key map (kbd "C-n") 'next-line)
;;     map)
;;   "Keymap for Output mode.")

(defvar-local output--subfix-or-buffer nil
  "Subfix for output buffer name or buffer base on weather in compilation mode")

;; (defcustom output-mode-hook nil
;;   "Hook run by `output-mode'."
;;   :type 'hook
;;   :group 'output)

(defcustom output-switch-buffer-function
  #'pop-to-buffer
  "Function called to display the *Output* buffer."
  :type 'function
  :group 'output)

(defun output--buffer-name (&optional name-of-mode)
  "Return a output buffer name"
  (format "*Output: %s*" (or (or output--subfix-or-buffer name-of-mode) "compilation")))

(defun output--switch-to-result-buffer (title command)
  "Compile command in output buffer"
  (setq-local output--subfix-or-buffer title)
  (let* ((origin-buffer (current-buffer))
         (current-directory default-directory)
         (compilation-buffer-name-function #'output--buffer-name))
    (funcall output-switch-buffer-function
             (get-buffer-create (output--buffer-name)))
    (let ((default-directory current-directory)
          (output--subfix-or-buffer title))
      (compile command))
    (setq-local output--subfix-or-buffer origin-buffer)))

(defun output--switch-back-to-origin-buffer ()
  "Switch back to origin when in output buffer"
  (let ((origin-buffer output--subfix-or-buffer))
    (quit-window)
    (switch-to-buffer origin-buffer)))

;;;###autoload
(defun output-switch-to-last-buffer ()
  "Switch to last buffer.
output buffer to origin buffer
origin buffer to output buffer"
  (interactive)
  (if (derived-mode-p 'compilation-mode)
      (output--switch-back-to-origin-buffer)
    (funcall output-switch-buffer-function (get-buffer (output--buffer-name)))))

;;;###autoload
(defun output-shell-command-result (command)
  "Execute shell command result to *Outpu: shell* buffer"
  (interactive
   (list
    (read-shell-command (format-message "Shell command in `%s': "
                                        (abbreviate-file-name
                                         default-directory))
                        nil nil
                        (let ((filename
                               (cond
                                (buffer-file-name)
                                ((eq major-mode 'dired-mode)
                                 (dired-get-filename nil t)))))
                          (and filename (file-relative-name filename))))))
  (output--switch-to-result-buffer "shell" command))

;; (defun output-mode-switch-to-buffer)

;; (defun output-mode-setup ()
;;   "Enter Output mode in the current buffer."
;;   (output-mode)
;;   (setq buffer-read-only nil))

;; (defun output-mode-finish ()
;;   "Finalize Output mode setup in current buffer."
;;   (setq buffer-read-only nil))

;; (define-derived-mode output-mode special-mode "Output"
;;   "Major mode for viewing output text.
;; Entry to this mode runs the normal hook `output-mode-hook'.")

(provide 'output-buffer)
