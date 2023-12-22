;;; mod-node.el --- Support for Node development   -*- lexical-binding: t; -*-

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

;; Support for Node development in JavaScript and TypeScript.

;;; Code:

;; TODO enhance this package
(u/use-package 'nvm)

;; javascript react
(with-eval-after-load 'js
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook #'eglot-ensure))

(u/use-package 'typescript-mode)
(with-eval-after-load 'typescript-mode
  (add-hook 'ts-mode-hook #'eglot-ensure))

(provide 'mod-node)
;;; mod-node.el ends here
