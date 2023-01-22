;;; mod-window.el --- Window and buffer management module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for setting up window and buffer management.

;;; Code:

;;;; Stateful window layout

(winner-mode +1)
(+define-key (kbd "C-c w") 'winner-undo)
(+define-key (kbd "C-c W") 'winner-redo)

;;;; Window selection and navigation

(+use-package 'ace-window)
(+define-key (kbd "M-o") #'ace-window)
(+define-key (kbd "M-O") #'ace-delete-window)
(with-eval-after-load 'ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(+define-key (kbd "M-h") #'windmove-left)
(+define-key (kbd "M-j") #'windmove-down)
(+define-key (kbd "M-k") #'windmove-up)
(+define-key (kbd "M-l") #'windmove-right)

(+define-key (kbd "C-M--") #'shrink-window-horizontally)
(+define-key (kbd "C-M-+") #'enlarge-window-horizontally)
(+define-key (kbd "M--") #'shrink-window)
(+define-key (kbd "M-+") #'enlarge-window)

(+define-key (kbd "C-M-S-h") #'windmove-swap-states-left)
(+define-key (kbd "C-M-S-j") #'windmove-swap-states-down)
(+define-key (kbd "C-M-S-k") #'windmove-swap-states-up)
(+define-key (kbd "C-M-S-l") #'windmove-swap-states-right)

(+define-key (kbd "C-M-l") 'recenter-other-window)

;;;; Window placement and popups

;; define window placement rules
(+use-package 'shackle)
(shackle-mode +1)
(setq shackle-rules
      '((compilation-mode :noselect t)
		(help-mode :popup t :select t :align below :size 0.33)
		(helpful-mode :popup t :select t :align below :size 0.33)
		("\\*.*-e?shell\\*\\'" :regexp t :popup t :select t :align below :size 0.33)
		("\\*.*-v?term\\*.*\\'" :regexp t :popup t :select t :align below :size 0.33)
		(flycheck-error-list-mode :popup t :select t :align below :size 0.25)
		("\\*Warnings\\*" :regexp t :noselect t)
        ("\\*eldoc" :regexp t :popup t :noselect t :align right :size 80)
        (kubernetes-overview-mode :select t :align left :size 0.5)
        ("\\*terraform.*\\*" :regexp t :select t :popup t :align right)))

;;;; Workspaces (tab-bar-mode)

;; TODO find out out to use tab-bar-mode
;; (setq tab-bar-show nil)
;; (setq tab-bar-new-tab-choice "*Welcome*")
;; (tab-bar-mode +1)

;;;; Buffer helpers

;; use `ibuffer' instead of buffer list
(+define-key (kbd "C-x C-b") 'ibuffer)

(defun +edit-emacs-config ()
  "Edit the user emacs init file."
  (interactive)
  (find-file user-init-file))

;; TODO this is probably dangerous
(defun +kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun +kill-mode-buffers (mode)
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

(defun +kill-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (+kill-mode-buffers 'dired-mode))

(defun +kill-help-buffers ()
  "Kill all help buffers."
  (interactive)
  (+kill-mode-buffers 'help-mode)
  (+kill-mode-buffers 'helpful-mode))

(provide 'mod-window)
;;; mod-window.el ends here
