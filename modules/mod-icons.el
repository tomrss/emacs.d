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

(u/use-package 'all-the-icons)
(when (display-graphic-p)
  (require 'all-the-icons nil nil)
  (unless (x-list-fonts "all-the-icons")
    (all-the-icons-install-fonts t)))

;;;; Icons in dired

(u/use-package 'all-the-icons-dired)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'all-the-icons-dired-mode))

;;;; Icons in minibuffer completions

(u/use-package 'all-the-icons-completion)
(all-the-icons-completion-mode)
(add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)

;;;; Icons in doom modeline

(setq doom-modeline-icon t)

;;;; Icons in treemacs

(with-eval-after-load 'treemacs
  (u/use-package 'treemacs-all-the-icons)
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons"))

(provide 'mod-icons)
;;; mod-icons.el ends here
