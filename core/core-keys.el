;;; core-keys.el --- Setup key configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Tommaso Rossi

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

;; Module for setting up key bindings, including a custom minor mode
;; that holds all keybinding (avoid keys being overwritten by major
;; modes) and vim emulation with EVIL package.

;;; Code:

;;;; Custom keys minor mode

(defvar customized-keys-minor-mode-map (make-sparse-keymap)
  "Keymap for the `customized-keys-minor-mode'.")

(define-minor-mode customized-keys-minor-mode
  "A minor mode so that custom key settings override major modes."
  :init-value t
  :lighter "")

(customized-keys-minor-mode +1)

(defun u/define-key (key def)
  "Define customized KEY with definition DEF."
  (define-key customized-keys-minor-mode-map key def))


(provide 'core-keys)
;;; core-keys.el ends here
