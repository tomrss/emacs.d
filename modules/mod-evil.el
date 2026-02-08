;;; mod-evil.el --- Vim emulation with Evil mode     -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi@protonmail.com>

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

;;  Setup vim emulation with `evil' mode

;;; Code:

;;;; Base evil configuration
(u/use-package 'evil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll nil)
(setq evil-want-C-i-jump nil)
(setq evil-respect-visual-line-mode t)
(setq evil-undo-system 'undo-redo)
(evil-mode +1)
(add-hook 'with-editor-mode-hook 'evil-insert-state)

;;;; Automatically configure evil for some common modes
(u/use-package 'evil-collection)
(with-eval-after-load 'evil
  (evil-collection-init '(bookmark calendar compile consult corfu custom
                          dired diff-hl eglot embark flymake forge
                          ibuffer info magit org org-roam vertico
                          vterm xref)))

;;;; Evil in dired

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "g h") #'dired-omit-mode)
  (evil-define-key 'normal dired-mode-map (kbd "RET") #'u/dired-open-dwim)
  (evil-define-key 'normal dired-mode-map (kbd "l") #'u/dired-open-dwim)
  (evil-define-key 'normal dired-mode-map (kbd "h") #'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "g s") #'u/dired-file-size))

;;;; Evil in eshell

(with-eval-after-load 'eshell
  (evil-collection-eshell-setup)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-R") #'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-l") #'eshell/clear)
  (evil-define-key '(normal insert) eshell-mode-map (kbd "C-d") #'u/eshell-ctrl-d)
  (evil-define-key 'normal eshell-mode-map (kbd "g .") #'u/eshell-toggle-kube-section)
  (evil-normalize-keymaps))

;;;; Evil in eglot

(evil-define-key 'normal eglot-mode-map (kbd "SPC e") #'eglot)
(evil-define-key 'normal eglot-mode-map (kbd "SPC r") #'eglot-rename)
(evil-define-key 'normal eglot-mode-map (kbd "SPC f") #'u/eglot-format-and-save)

;;;; Evil in treemacs

(with-eval-after-load 'treemacs
  (u/use-package 'treemacs-evil)
  (with-eval-after-load 'evil
    (with-eval-after-load 'treemacs
      (require 'treemacs-evil))))

;;;; Evil in kubernetes buffer

(with-eval-after-load 'kubernetes-overview
  (u/use-package 'kubernetes-evil)
  (require 'kubernetes-evil))

;;;; Evil in welcome screen

(evil-set-initial-state 'welcome-mode 'emacs)

;;;; Forge

;; Setup is automatic, just disable annoying warning.  No eval after
;; load, when forge evil hook is run this variable needs to be already
;; set (we could hook before but its easier like this)
(setq forge-add-default-bindings nil)

(provide 'mod-evil)
;;; mod-evil.el ends here
