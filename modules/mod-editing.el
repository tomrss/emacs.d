;;; mod-editing.el --- Editing module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Editing module. TODO refactor in other modules / rename.

;;; Code:

;;;; Text selection and navigation

;; increases the selected region by semantic units
(+use-package 'expand-region)
(+define-key (kbd "C-ò") #'er/expand-region)

;;;; Scratch buffers

(if (eq +packaging-system 'straight)
    (+use-package '(scratch-el :type git
			                   :host github
			                   :repo "tomrss/scratch.el"))
  ;; TODO this is very wrong. if is ugly, and it requires to having used straight
  ;; TODO use package with git integration
  (add-to-list 'load-path "~/.emacs.d/.cache/straight/repos/scratch.el/")
  (require 'scratch))

(with-eval-after-load 'scratch
  (setq scratch-search-fn #'consult-ripgrep)
  (scratch-persist-mode +1))

(+define-key (kbd "C-c s") 'scratch-key-map)

(provide 'mod-editing)
;;; mod-editing.el ends here
