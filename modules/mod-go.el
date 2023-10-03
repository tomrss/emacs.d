;;; mod-go.el --- Support for Go development         -*- lexical-binding: t; -*-

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

;; Support for Go development with `eglot'

;;; Code:

(u/use-package 'go-mode)
(add-hook 'go-mode-hook #'u/eglot-deferred)
(add-hook 'go-mode-hook (lambda ()
                          (setq indent-tabs-mode t)
                          (setq outline-regexp "\\(func \\)\\|\\(type \\)")))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(provide 'mod-go)
;;; mod-go.el ends here
