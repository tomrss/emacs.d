;;; mod-tree-sitter.el --- Use tree-sitter for some prog modes  -*- lexical-binding: t; -*-

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

;; Configure `tree-sitter' for some prog modes.
;; This module is very opinionated, I don't like tree sitter
;; highlighting in all modes.

;;; Code:

;; setup tree sitter

(u/use-package 'tree-sitter)
(u/use-package 'tree-sitter-langs)

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(with-eval-after-load 'tree-sitter
  (require 'tree-sitter-langs))

;; activate tree sitter for some modes

(add-hook 'python-mode-hook #'tree-sitter-mode)
(add-hook 'js-mode-hook #'tree-sitter-mode)
(add-hook 'ts-mode-hook #'tree-sitter-mode)
(add-hook 'terraform-mode-hook #'tree-sitter-mode)

(provide 'mod-tree-sitter)
;;; mod-tree-sitter.el ends here
