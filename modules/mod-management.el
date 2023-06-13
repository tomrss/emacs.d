;;; mod-management.el --- Module for setting up desktop env managers -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for customizing dired as a fully fledged file manager

;;; Code:

;;;; File management

(with-eval-after-load 'dired
  ;; dired defaults
  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes (if delete-by-moving-to-trash 'always 'top))
  (setq dired-create-destination-dirs 'ask)
  (setq dired-listing-switches "-agho --group-directories-first")
  (setq dired-use-ls-dired (not (eq system-type 'darwin)))

  ;; enable dired-x for omit mode and enhanced find file commands
  (setq dired-x-hands-off-my-keys nil)
  (setq dired-omit-files "\\`[.]?#\\|\\`[.].*\\'")
  (require 'dired-x)
  (evil-define-key 'normal dired-mode-map (kbd "g h") #'dired-omit-mode)

  ;; open file dwim in dired (supporting both opening in emacs and externally)
  (evil-define-key 'normal dired-mode-map (kbd "RET") #'diredfm-open-dwim)

  ;; easily navigate in dired buffers
  (evil-define-key 'normal dired-mode-map (kbd "l") #'diredfm-open-dwim)
  (evil-define-key 'normal dired-mode-map (kbd "h") #'dired-up-directory)

  ;; echo file size
  (evil-define-key 'normal dired-mode-map (kbd "g s") #'diredfm-file-size))

(u/define-key (kbd "C-x j") #'dired-jump)

;; dired icons
(u/use-package 'all-the-icons-dired)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

;; dired colorization
(u/use-package 'diredfl)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'diredfl-mode))

;;;; Directory tree view

;; TODO move feature flag in separate place
(defvar u/feature-directory-tree-view nil
  "Feature flag for enabling directory tree view.")

(when u/feature-directory-tree-view
  (u/use-package 'treemacs)
  (u/use-package 'treemacs-all-the-icons-autoloads)
  (u/define-key (kbd "C-x c t") #'treemacs-select-window)
  (with-eval-after-load 'treemacs
    (treemacs-project-follow-mode t)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-git-commit-diff-mode t)
    (treemacs-hide-gitignored-files-mode nil)

    (require 'treemacs-all-the-icons)
    (treemacs-load-theme "all-the-icons"))

  (u/use-package 'treemacs-evil)
  (with-eval-after-load 'evil
    (with-eval-after-load 'treemacs
      (require 'treemacs-evil)))

  (u/use-package 'treemacs-magit)
  (with-eval-after-load 'magit
    (with-eval-after-load 'treemacs
      (require 'treemacs-magit))))

;;;; Process management

(with-eval-after-load 'proced
  (setq proced-auto-update-interval 5)
  (proced-toggle-auto-update 1))

(provide 'mod-management)
;;; mod-ui.el ends here
