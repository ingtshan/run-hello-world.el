;;; run-hello-world.el --- `run-hello' commands  -*- lexical-binding: t; -*-

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

(require 'output-buffer)

(defcustom run-hello-workspace "~/run-hello-workspace"
  "Worksapce directory for `run-hello' to put hell world code and executing output"
  :type 'directory
  :group 'run-hello)

(defcustom run-hello-env-dir
  (expand-file-name "env"
                    (cond (load-file-name (file-name-directory load-file-name))
                          ((locate-library "run-hello-world") (file-name-directory (locate-library "run-hello-world")))
                          (t default-directory)))
  "The directory to JSON file containing the configuration for `run-hello' env"
  :type 'directory
  :group 'run-hello)

(defvar run-hello--support-languages nil
  "Alist of languages with env JSON file")

;; (defvar run-output-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map (make-composed-keymap special-mode-map))
;;     map)
;;   "Keymap for Run Output mode.")

(defvar-local run-hello--compile-command-seed nil
  "Format seed for generating complie command")

(defvar-local run-hello--executor nil
  "Executor name for `run-hello'")

(defvar-local run-hello--source-file nil
  "Source file name for `run-hello'")

(defvar-local run-hello--compiled-file nil
  "Compiled file name for `run-hello'")

(defun run-hello--check (executor)
  "check executing status and through error if need"
  (or (executable-find executor)
      (error (format "Run hello needs %s to work. Please, install %s"
                     executor executor))))

(defun run-hello--command (seed &optional arg1 arg2)
  "Return command that compile input to output"
  (declare (side-effect-free t))
    (pcase (save-match-data
             (with-temp-buffer
               (insert seed)
               (goto-char (point-min))
               (count-matches "%s" 1 (point-max))))
      (0 seed)
      (1 (format seed arg1))
      (2 (format seed arg1 arg2))))

(defun run-hello--run-command (executable-file &optional interpreter)
  "Return command that run hello application"
  (if interpreter (format "%s %s" interpreter executable-file)
    (format "./%s" executable-file)))

(defun run-hello--execute ()
  "Execut command in current environment"
  (let ((command (if run-hello--compile-command-seed
                     (format "%s; %s" (run-hello--command
                                       run-hello--compile-command-seed
                                       run-hello--source-file
                                       run-hello--compiled-file)
                             (run-hello--run-command
                              run-hello--compiled-file))
                   (run-hello--run-command run-hello--source-file
                                           run-hello--executor))))
    (output--switch-to-result-buffer run-hello--source-file command)))

;; (defun run-hello--setup (env-file)
;;   "Setup local variables for `run-hello'"
;;   )

(defun run-hello--guess-language ()
  "Guess language base on source file or ask input
Return a language name"
  (let ((guess-list `()))
    (dolist (el run-hello--support-languages)
      (and (string-match-p
            (concat "." (file-name-extension run-hello--source-file) ".json$")
            (cdr el))
           (add-to-list 'guess-list (car el))))
    (cond
     ((= (length guess-list) 1) (car guess-list))
     ((> (length guess-list) 1) (completing-read "Specify a programming language: " guess-list))
     (t (completing-read
         "Select a programming language: "
         (mapcar (lambda (c) (car c)) run-hello--support-languages))))))

(defun run-hello--candidate-list (directory extension)
  "Return all the file name as candidate list"
  (mapcar (lambda (f) `(,(file-name-sans-extension (file-name-base f)) . ,f))
          (directory-files-recursively directory extension)))

(defun run-hello--language-json-other-window (language locate-url &optional base-url)
  "Find lanuage json file other window
If file not exists, init content by given information"
  (let* ((location (url-unhex-string locate-url))
         (file (expand-file-name (concat "languages/" location ".json") run-hello-env-dir)))
    (or (file-directory-p (file-name-directory file)) (make-directory (file-name-directory file) t))
    (or (file-exists-p file)
        (let ((template "{\n\
  \"executor\": \"\",\n\
  \"sourceUrl\": \"%s\",\n\
  \"sourceDir\": \"%s\",\n\
  \"sourceFile\": \"hello.%s\",\n\
  \"compiledFile\": \"hello_%s\",\n\
  \"compileCommandSeed\": \"\"\n}"))
          (append-to-file (format template
                                  (concat base-url locate-url)
                                  (file-name-sans-extension location)
                                  (file-name-extension location)
                                  (file-name-extension location)
                                  ) nil file)))
    (find-file-other-window file)))

;;;###autoload
(defun run-hello-add-support-language ()
  "Workflow to add language for `run-hello'"
  (interactive)
  (let* ((list-text-file (expand-file-name "hello-world.txt" run-hello-env-dir))
         (list-el-dir    (expand-file-name "tmp/hello-world-list" run-hello-env-dir))
         (json-file      (expand-file-name "hello-world.json" run-hello-env-dir))
         (json-object-type 'plist)
         (data (json-read-file json-file)))
    ;; download list
    (or (file-exists-p list-text-file)
        (url-copy-file
         (plist-get data :languagesListUrl) list-text-file))
    ;; generate list
    (or (file-directory-p list-el-dir)
        (with-current-buffer (find-file-noselect
                              (expand-file-name "hello-world.sh" run-hello-env-dir))
          (output-shell-command-result "sh hello-world.sh")))
    ;; interactive
    (let* ((prefix-selection (completing-read
                              "Select prefix: "
                              (run-hello--candidate-list list-el-dir "el")))
           (file-buf (find-file-noselect
                      (expand-file-name
                       (format "%s.el" prefix-selection) list-el-dir)))
           (languages-alist (with-current-buffer file-buf
                              (goto-char (point-max))
                              (elisp--eval-last-sexp nil)))
           (language (completing-read
                      "Select a programming language to add: "
                      (mapcar (lambda (c) (car c)) languages-alist)))
           (location (pcase languages-alist
                       ((map (language location)) location))))
      (kill-buffer file-buf)
      (run-hello--language-json-other-window
       language location (plist-get data :languagesSourceBaseUrl))
      (run-hello-fresh-support-languages))))

;;;###autoload
(defun run-hello-fresh-support-languages ()
  "Research env dir to fresh lanuges list"
  (interactive)
  (setq run-hello--support-languages
        (run-hello--candidate-list
         (expand-file-name "languages" run-hello-env-dir) "json")))

;;;###autoload
(defun run-hello-current-file ()
  "Hello way run current file"
  (interactive)
  (setq-local run-hello--source-file (buffer-name))
  (run-hello-toggle t))

;;;###autoload
(defun run-hello-toggle (&optional skip-find-file)
  "Toggle current buffer to `run-hello'"
  (interactive)
  (unless (and (string-equal run-hello--source-file (buffer-name)) run-hello--executor)
    ;; find out languages
    (or run-hello--support-languages (run-hello-fresh-support-languages))

    ;; setup env
    (let* ((selection (if (string-equal run-hello--source-file (buffer-name))
                          (run-hello--guess-language)
                        (completing-read
                         "Select a programming language: "
                         (mapcar (lambda (c) (car c)) run-hello--support-languages))))

           (json-file (pcase run-hello--support-languages
                        ((map (selection file)) file)))
           (json-object-type 'alist)
           (json-key-type 'symbol)
           (env-data (json-read-file json-file)))

      (let-alist env-data
        (or skip-find-file
            (let* ((dir (concat
                         run-hello-workspace "/"
                         .sourceDir "/"))
                   (file (concat
                          dir
                          .sourceFile)))

              (or (file-directory-p dir) (make-directory dir t))
              (or (file-exists-p file)
                  (url-copy-file
                   .sourceUrl file))
              (find-file file)))

        (or run-hello--source-file
            (setq-local run-hello--source-file .sourceFile))

        (setq-local
         run-hello--executor                   .executor
         run-hello--compiled-file              .compiledFile
         run-hello--compile-command-seed       .compileCommandSeed
         ;; run-hello--source-url source-url
         ;; run-hello--source-dir source-dir
         ))))

  (run-hello--execute))

;;;###autoload
(defun run-hello-other ()
  "Always switch hello source to `run-hello'"
  (interactive)
  (let ((run-hello--source-file nil))
    (run-hello-toggle)))

(provide 'run-hello-world)
