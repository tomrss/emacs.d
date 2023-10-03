;;; mod-java.el --- Support for Java development     -*- lexical-binding: t; -*-

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

;; Support for Java development.
;; TODO this module is to be written somehow

;;; Code:

;; TODO after 10 years still not able to work with java in emacs... only lsp worked

;; TODO it doeesnt work
;; (u/use-package 'eglot-java)
;; (add-hook 'java-mode-hook 'eglot-java-mode)

;;;;; ZKoss

(add-to-list 'auto-mode-alist '("\\.zul\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.gsp\\'" . xml-mode))

(provide 'mod-java)
;;; mod-java.el ends here
