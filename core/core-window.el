;;; core-window.el --- Window and buffer management -*- lexical-binding: t -*-

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

;; Module for setting up window and buffer management.

;;; Code:

(require 'core-keys)
(require 'core-packaging)

;;;; Stateful window layout

(winner-mode +1)
(u/define-key (kbd "C-c w") 'winner-undo)
(u/define-key (kbd "C-c W") 'winner-redo)

;;;; Window selection and navigation

(use-package ace-window
  :init
  (u/define-key (kbd "M-o") #'ace-window)
  (u/define-key (kbd "M-O") #'ace-delete-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(u/define-key (kbd "M-h") #'windmove-left)
(u/define-key (kbd "M-j") #'windmove-down)
(u/define-key (kbd "M-k") #'windmove-up)
(u/define-key (kbd "M-l") #'windmove-right)

(u/define-key (kbd "C-M--") #'shrink-window-horizontally)
(u/define-key (kbd "C-M-+") #'enlarge-window-horizontally)
(u/define-key (kbd "M--") #'shrink-window)
(u/define-key (kbd "M-+") #'enlarge-window)

(u/define-key (kbd "C-M-S-h") #'windmove-swap-states-left)
(u/define-key (kbd "C-M-S-j") #'windmove-swap-states-down)
(u/define-key (kbd "C-M-S-k") #'windmove-swap-states-up)
(u/define-key (kbd "C-M-S-l") #'windmove-swap-states-right)

(u/define-key (kbd "C-M-l") 'recenter-other-window)

;;;; Window placement and popups

;; define window placement rules
(use-package shackle
  :init
  (shackle-mode +1)
  :config
  (setq shackle-rules
        '((compilation-mode :noselect t)
          (help-mode :popup t :select t :align below :size 0.33)
          (helpful-mode :popup t :select t :align below :size 0.33)
          ("\\*.*-e?shell\\*\\'" :regexp t :popup t :select t :align below :size 0.33)
          ("\\*.*-v?term\\*.*\\'" :regexp t :popup t :select t :align below :size 0.33)
          ("*Async Shell Command*" :ignore t)
          (flycheck-error-list-mode :popup t :select t :align below :size 0.25)
          ("\\*Warnings\\*" :regexp t :noselect t)
          ("\\*eldoc" :regexp t :popup t :noselect t :align right :size 80)
          (kubernetes-overview-mode :select t :align left :size 0.5)
          ("\\*terraform.*\\*" :regexp t :select t :popup t :align right)
          ("\\*latex-comp-.*\\*" :regexp t :ignore t)
          ("*virtualenv*" :ignore t)
          ("*venv*" :ignore t))))

;;;; Workspaces (tab-bar-mode)

;; TODO find out out to use tab-bar-mode
;; (setq tab-bar-show nil)
;; (setq tab-bar-new-tab-choice "*Welcome*")
;; (tab-bar-mode +1)

;;;; Text selection and navigation

;; increases the selected region by semantic units
(use-package expand-region
  :init
  (u/define-key (kbd "C-ò") #'er/expand-region))

;;;; Buffer helpers

;; use `ibuffer' instead of buffer list
(u/define-key (kbd "C-x C-b") 'ibuffer)

(defun u/edit-emacs-config ()
  "Edit the user Emacs init file."
  (interactive)
  (project-switch-project user-emacs-directory))

;; TODO this is probably dangerous
(defun u/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun u/kill-mode-buffers (mode)
  "Kill all buffers with major mode MODE."
  (interactive
   (list
    (intern
     (completing-read
      "Mode: "
      (delete-dups
       (mapcar
		(lambda (buffer)
		  (buffer-local-value 'major-mode buffer))
		(buffer-list)))))))
  (mapc (lambda (buffer)
		  (when (eq mode (buffer-local-value 'major-mode buffer))
			(kill-buffer buffer)))
		(buffer-list))
  (message "Killed buffers with major mode %s" mode))

(defun u/kill-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (u/kill-mode-buffers 'dired-mode))

(defun u/kill-help-buffers ()
  "Kill all help buffers."
  (interactive)
  (u/kill-mode-buffers 'help-mode)
  (u/kill-mode-buffers 'helpful-mode))

(provide 'core-window)
;;; core-window.el ends here
