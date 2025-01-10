;;; core-defaults.el --- Better defaults -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Module containing configuration defaults, i. e. Emacs features that
;; are builtin.

;;; Code:

(require 'core-init-directory)
(require 'core-utils)

;;;; Persistence across sessions

;; persist selections with builtin savehist mode
(setq history-length 200)
(savehist-mode +1)

;; recent files
(require 'recentf)
(setq recentf-max-menu-items 500)
(setq recentf-max-saved-items 500)
(setq recentf-auto-cleanup 120)
(recentf-mode +1)
(add-to-list 'recentf-exclude u/cache-directory)

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

;; move to thrash instead of delete
(setq delete-by-moving-to-trash t)

;; authinfo crypted with GPG
(setq auth-sources '("~/.authinfo.gpg"))

;;;; Manage backups

;; disable lockfiles
(setq create-lockfiles nil)

;; change some backup defaults
(setq backup-by-copying t)
(setq delete-old-versions t)
(setq kept-new-versions 4)
(setq kept-old-versions 2)
(setq version-control t)

;;;; Spell check

(setq ispell-program-name
      (u/find-first 'executable-find '("aspell" "hunspell" "ispell")))

;;;; Custom file

;; set custom file.  `custom' is not supported by this configuration and is only used
;; for remembering trusted dir-locals, otherwise it should be dropped.
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file t t)

;;;; Filter logs

(defvar u/filter-logs-patterns
  '("^Cleaning up the recentf list...")
  "Pattern of logs to be filtered out.
It works only on the first parameter of the `message' function.")

(defun u/filter-logs (log-fun &rest args)
  "Filter out annoyng logs with predicate LOG-FUN with its ARGS."
  (unless (or
           (not (and args (car args)))
           (u/find-first
            (lambda (log-pattern) (string-match log-pattern (car args)))
            u/filter-logs-patterns))
    (apply log-fun args)))

(advice-add 'message :around #'u/filter-logs)

(provide 'core-defaults)
;;; core-defaults.el ends here
