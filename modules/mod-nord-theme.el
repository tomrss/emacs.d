;;; mod-nord-theme.el --- Theme Emacs with Nord theme -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Tommaso Rossi

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

;; Theme Emacs with Nord theme, provided by the `doom-themes' package.

;;; Code:

(u/use-package 'doom-themes)

(setq doom-nord-brighter-modeline t)
(load-theme 'doom-nord t)
(set-face-attribute 'query-replace nil :background "#BF616A")
(set-face-attribute 'font-lock-doc-face nil :foreground "#EBCB8B")
(set-face-attribute 'completions-annotations nil :foreground "#EBCB8B")

(provide 'mod-nord-theme)
;;; mod-nord-theme.el ends here
