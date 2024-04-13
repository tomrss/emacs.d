;;; mod-scheme.el --- Scheme hacking -*- lexical-binding: t; -*-

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

;; Hacking with scheme and guile with `geiser-guile'

;;; Code:

;; TODO this package declares package as dependency, this creates problem with straight and eglot 
;; in emacs 30 (13-04-2024)
;; https://github.com/radian-software/straight.el/issues/1146
;; (u/use-package 'geiser-guile)

(provide 'mod-scheme)
;;; mod-scheme.el ends here
