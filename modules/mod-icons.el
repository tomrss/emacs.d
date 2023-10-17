;;; mod-icons.el --- Configure icons                 -*- lexical-binding: t; -*-

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

;; Conifgure icons in many Emacs and known packages UI features.

;;; Code:

;;;; Setup icons

(u/use-package 'nerd-icons)
(require 'nerd-icons nil nil)
(when window-system
  (unless (x-list-fonts "Symbols Nerd Font Mono")
    (nerd-icons-install-fonts t)))
(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-extension-icon-alist
               '("go" nerd-icons-mdicon "nf-md-language_go" :face nerd-icons-blue :height 1.2)))

;;;; Icons in dired

(u/use-package 'nerd-icons-dired)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

;;;; Icons in minibuffer completions

(u/use-package 'nerd-icons-completion)
(with-eval-after-load 'marginalia
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(u/use-package 'nerd-icons-ibuffer)
(add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)

;;;; Icons in doom modeline

(setq doom-modeline-icon t)

;;;; Icons in treemacs

(with-eval-after-load 'treemacs
  (u/use-package 'treemacs-nerd-icons)
  (require 'treemacs-nerd-icons)
  (treemacs-load-theme "nerd-icons"))

(provide 'mod-icons)
;;; mod-icons.el ends here
