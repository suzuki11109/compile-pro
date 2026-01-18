;;; compile-pro.el --- Per-project compile utitlities for Emacs -*- lexical-binding: t; -*-

;; Author: Aki Suzuki <suzuki11109@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, compile, project
;; URL: https://github.com/suzuki11109/compile-pro

;;; Commentary:

;; This package provides per-project `compile-history` using `project' and `savehist'.
;; It automatically loads project-specific compile history when you run a compilation
;; and saves the history when Emacs exits or compilation finishes.

;;; Code:

(require 'project)

(defgroup compile-pro nil
  "Per-project compile history management."
  :group 'compilation
  :prefix "compile-pro-")

(defvar project-compile-history-alist nil
  "Alist of project roots to their compile histories.")

(add-to-list 'savehist-additional-variables 'project-compile-history-alist)

(defun get-project-compile-history ()
  "Get compile history for current project."
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (history-key (or project-root default-directory)))
    (alist-get history-key project-compile-history-alist nil nil #'string=)))

(defun save-project-compile-history (history)
  "Save compile history for current project."
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (history-key (or project-root default-directory)))
    (setf (alist-get history-key project-compile-history-alist nil nil #'string=)
          history)))

(defun compile-pro-compile-in-dir (dir &optional command)
  "Run `compile' in the selected directory."
  (interactive "DCompile in: ")
  (let ((default-directory dir))
    (if command
        (compile command)
      (call-interactively #'compile))))

;; TODO: M-r to load command from history
(defun compile-pro-compile (&optional command)
  "Run `compile' in the current project's root directory or current working directory."
  (interactive)
  (let* ((project (project-current))
         (project-history (when project (get-project-compile-history)))
         (history (if project (or project-history '()) compile-history))
         (compile-dir (if project (project-root project) default-directory))
         (compilation-buffer-name-function (if project project-compilation-buffer-name-function compilation-buffer-name-function))
         (initial-text (when (use-region-p)
                         (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (command (if command
                      command
                    (read-string (format-message "Compile command in `%s': " (abbreviate-file-name compile-dir))
                                 initial-text))))
    (compile-pro-compile-in-dir compile-dir command)
    (when (and project (and command (not (string-empty-p command))))
      (let ((updated-history (cons command (remove command (or project-history '())))))
        (save-project-compile-history updated-history))))
  )

(defun compile-pro-compile-from-history ()
  "Run `compile' with a choice from project's compile history."
  (interactive)
  (let* ((project (project-current))
         (project-history (when project (get-project-compile-history)))
         (history (if project (or project-history '()) compile-history))
         (compile-dir (if project (project-root project) default-directory))
         (compilation-buffer-name-function (if project project-compilation-buffer-name-function compilation-buffer-name-function))
         (initial-text (when (use-region-p)
                         (string-trim (buffer-substring-no-properties (region-beginning) (region-end)))))
         (command (completing-read
                   (format-message "Compile command in `%s': " (abbreviate-file-name compile-dir))
                   history nil nil initial-text 'compile-history))
         )
    (compile-pro-compile-in-dir compile-dir command)
    (when (and project (and command (not (string-empty-p command))))
      (let ((updated-history (cons command (remove command (or project-history '())))))
        (save-project-compile-history updated-history)))))

(provide 'compile-pro)

;;; compile-pro.el ends here
