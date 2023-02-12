;;; mod-keys.el --- Key bindings module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for setting up key bindings, including a custom minor mode
;; that holds all keybinding (avoid keys being overwritten by major
;; modes) and vim emulation with EVIL package.

;;; Code:

;;;; Custom keys minor mode

(defvar customized-keys-minor-mode-map (make-sparse-keymap)
  "Keymap for the `customized-keys-minor-mode'.")

(define-minor-mode customized-keys-minor-mode
  "A minor mode so that custom key settings override major modes."
  :init-value t
  :lighter "")

(customized-keys-minor-mode +1)

(defun u/define-key (key def)
  "Define customized KEY with definition DEF."
  (define-key customized-keys-minor-mode-map key def))

;;;; Vim emulation

;; base evil configuration
(u/use-package 'evil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll nil)
(setq evil-want-C-i-jump nil)
(setq evil-respect-visual-line-mode t)
(setq evil-undo-system 'undo-redo)
(evil-mode 1)

;; automatically configure evil for some common modes
(u/use-package 'evil-collection)
(with-eval-after-load 'evil
  (evil-collection-init))

(add-hook 'with-editor-mode-hook 'evil-insert-state)

(provide 'mod-keys)
;;; mod-keys.el ends here
