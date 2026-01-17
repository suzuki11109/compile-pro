;;; compile-pro.el --- Per-project compile utitlities for Emacs -*- lexical-binding: t; -*-

;; Author: Aki Suzuki <suzuki11109@gmail.com>
;; Version: 0.1.0
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

(defvar compile-pro-project-compile-history-alist nil
  "Alist of project roots to their compile histories.")

(add-to-list 'savehist-additional-variables 'compile-pro-project-compile-history-alist)

(defun compile-pro-get-project-compile-history ()
  "Get compile history for current project."
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (history-key (or project-root default-directory)))
    (alist-get history-key compile-pro-project-compile-history-alist nil nil #'string=)))

(defun compile-pro-save-project-compile-history (history)
  "Save compile history for current project."
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (history-key (or project-root default-directory)))
    (setf (alist-get history-key compile-pro-project-compile-history-alist nil nil #'string=)
          history)))

(defun compile-pro-project-compile-read-command (orig-fun &rest args)
  "Advice to use project-specific compile history."
  (let ((compile-history (get-project-compile-history)))
    (prog1 (apply orig-fun args)
      (compile-pro-save-project-compile-history compile-history))))

(advice-add 'compilation-read-command :around #'compile-pro-project-compile-read-command)

(defun compile-pro--compile (compile-dir command &optional project-history)
  "Internal function to execute compilation in COMPILE-DIR with COMMAND.
Optionally save to PROJECT-HISTORY if provided and command is valid."
  (compile-pro-compile-in-dir compile-dir command)
  (when (and project-history command (not (string-empty-p command)))
    (let ((updated-history (cons command (remove command (or project-history '())))))
      (compile-pro-save-project-compile-history updated-history))))

;;;###autoload
(defun compile-pro-compile-in-dir (dir &optional command)
  "Run `compile' in the selected directory."
  (interactive "DCompile in: ")
  (let ((default-directory dir))
    (if command
        (compile command)
      (call-interactively #'compile))))

;;;###autoload
(defun compile-pro-compile (&optional command)
  "Run `compile' in the current project's root directory or current working directory."
  (interactive)
  (let* ((project-history (compile-pro-get-project-compile-history))
         (project (project-current))
         (compilation-buffer-name-function (if project project-compilation-buffer-name-function compilation-buffer-name-function))
         (compile-dir (if project (project-root project) default-directory)))
    (compile-pro--compile compile-dir command project-history)))

;;;###autoload
(defun compile-pro-compile-from-project-history ()
  "Run `compile' with a choice from project's history.
Fallback to general compile history if not in a project."
  (interactive)
  (let* ((project (project-current))
         (project-history (when project (get-project-compile-history)))
         (compile-dir (if project (project-root project) default-directory))
         (compilation-buffer-name-function
          (if project project-compilation-buffer-name-function
            compilation-buffer-name-function))
         (history-to-use (if project (or project-history '()) compile-history))
         (initial-text (when (use-region-p)
                         (string-trim (buffer-substring-no-properties
                                       (region-beginning) (region-end)))))
         (command (completing-read
                   (format-message "Compile command in `%s': "
                                   (abbreviate-file-name compile-dir))
                   history-to-use nil nil initial-text 'compile-history)))
    (when (and command (not (string-empty-p command)))
      (compile-pro--compile compile-dir command project-history))))

(provide 'compile-pro)

;;; compile-pro.el ends here
