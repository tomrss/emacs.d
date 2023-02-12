;;; mod-defaults.el --- Defaults module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module containing configuration defaults, i. e. Emacs features that
;; are builtin.

;;; Code:

;;;; Persistence across sessions

;; persist selections with builtin savehist mode
(setq history-length 200)
(savehist-mode +1)

;; recent files
(setq recentf-max-menu-items 500)
(setq recentf-max-saved-items 500)
(setq recentf-auto-cleanup 120)
(recentf-mode +1)
(add-to-list 'recentf-exclude +emacs-cache-directory)

;; reopen file at same point
(save-place-mode +1)

;;;; Editing defaults

;; ask y or n instead of yes or no
(fset 'yes-or-no-p 'y-or-n-p)

;; keep all buffers updated if external program change content
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode +1)

;; share system clipboard
(setq save-interprogram-paste-before-kill t)
(setq kill-do-not-save-duplicates t)

;; mute the bell
(setq ring-bell-function 'ignore)

;; remove graphical dialog box and keep it keyboard driven
(setq use-dialog-box nil)

;; silent native compilation warning
(setq native-comp-async-report-warnings-errors 'silent)

;; local variables that are safe to use
(add-to-list 'safe-local-variable-values '(flymake-mode))
(add-to-list 'safe-local-variable-values '(pyvenv-virtualenvwrapper-python . "python3.8"))
(add-to-list 'safe-local-variable-values '(python-interpreter . "python3.8"))
(add-to-list 'safe-local-variable-values '(python-shell-interpreter . "python3.8"))

;; move to thrash instead of delete
(setq delete-by-moving-to-trash t)

;;;; Manage backups

;; disable lockfiles
(setq create-lockfiles nil)

;; change some backup defaults
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 4)
(setq kept-old-versions 2)
(setq version-control t)

;;;; Disable customize

;; disable `customize' for good by setting custom file to random temp file
(setq custom-file
      (expand-file-name
       (format "emacs-custom-%d.el" (random 10000))
       temporary-file-directory))

;;;; Filter logs

;; TODO move this to utils file?
(defun +find-first (predicate list)
  "Find first element in LIST matching PREDICATE."
  (let ((tail list)
        (found nil))
    (while (and (not found) tail)
      (let ((x (car tail)))
        (if (funcall predicate x)
            (setq found x)
          (setq tail (cdr tail)))))
    found))

(defvar filter-logs-patterns
  '("^Cleaning up the recentf list...")
  "Pattern of logs to be filtered out.
It works only on the first parameter of the `message' function.")

(defun filter-logs (log-fun &rest args)
  (unless (or
           (not (and args (car args)))
           (+find-first
             (lambda (log-pattern) (string-match log-pattern (car args)))
             filter-logs-patterns))
          (apply log-fun args)))

(advice-add 'message :around #'filter-logs)

(provide 'mod-defaults)
;;; mod-defaults.el ends here
