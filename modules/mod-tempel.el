;;; mod-tempel.el --- Snippets with tempel  -*- lexical-binding: t; -*-

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

;; Snippets provided by `tempel' package.

;;; Code:

;; Setup completion at point
(defun tempel-setup-capf ()
  "Add the Tempel Capf to `completion-at-point-functions'."
  (setq-local completion-at-point-functions
              (cons #'tempel-complete
                    completion-at-point-functions)))

(use-package tempel
  :hook ((conf-mode prog-mode text-mode) . tempel-setup-capf)
  :config
  (define-key tempel-map (kbd "TAB") #'tempel-next)
  (define-key tempel-map (kbd "<backtab>") #'tempel-previous))

(use-package tempel-collection
  :after tempel)

(provide 'mod-tempel)
;;; mod-tempel.el ends here
