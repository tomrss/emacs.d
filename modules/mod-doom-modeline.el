;;; mod-doom-modeline.el --- Fancy modeline with Doom Modeline  -*- lexical-binding: t; -*-

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

;; Setup fancy and modern modeline with `doom-modeline'.  Beware that
;; this module can slow down startup of some tenth of second in most
;; systems.

;;; Code:

(u/use-package 'doom-modeline)
(add-hook 'emacs-startup-hook #'doom-modeline-mode)

(provide 'mod-doom-modeline)
;;; mod-doom-modeline.el ends here
