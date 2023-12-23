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

(u/use-package 'tempel)
(u/use-package 'tempel-collection)

;; Setup completion at point
(defun tempel-setup-capf ()
  "Add the Tempel Capf to `completion-at-point-functions'."
  (setq-local completion-at-point-functions
              (cons #'tempel-complete
                    completion-at-point-functions)))

(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

(with-eval-after-load 'tempel
  (define-key tempel-map (kbd "TAB") #'tempel-next)
  (define-key tempel-map (kbd "<backtab>") #'tempel-previous)
  (require 'tempel-collection))

(provide 'mod-tempel)
;;; mod-tempel.el ends here
