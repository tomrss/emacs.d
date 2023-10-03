;;; mod-react.el --- Support for React development   -*- lexical-binding: t; -*-

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

;; Support for React development in JavaScript and TypeScript with
;; `eglot' and `tree-sitter'.

;;; Code:

(require 'mod-node)

;; javascript react
(with-eval-after-load 'js
  (setq js-jsx-indent-level 2)
  (define-derived-mode js-react-mode js-mode
    "JSX")

  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-react-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(js-react-mode . tsx))
  (add-hook 'js-react-mode-hook #'tree-sitter-mode)
  (add-hook 'js-react-mode-hook #'u/eglot-deferred))

(with-eval-after-load 'typescript-mode
  ;; see https://github.com/joaotavora/eglot/issues/624
  ;; see https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode ts-react-mode typescript-mode
    "TSX")

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . ts-react-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(ts-react-mode . tsx))
  (add-hook 'ts-react-mode-hook #'tree-sitter-mode)
  (add-hook 'ts-react-mode-hook #'u/eglot-deferred))

(provide 'mod-react)
;;; mod-react.el ends here
