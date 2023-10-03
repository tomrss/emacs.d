;;; mod-scala.el --- Support for Scala development  -*- lexical-binding: t; -*-

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

;; Support for Scala development with `eglot'.

;;; Code:

(u/use-package 'scala-mode)
(add-hook 'scala-mode-hook #'u/eglot-deferred)
(add-to-list 'auto-mode-alist '("\\.sc\(ala\)?\\'" . scala-mode))

(provide 'mod-scala)
;;; mod-scala.el ends here
