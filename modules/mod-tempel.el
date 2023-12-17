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

;; Require trigger prefix before template name when completing.
;; :custom
;; (tempel-trigger-prefix "<")

;; Setup completion at point
(defun tempel-setup-capf ()
  "Add the Tempel Capf to `completion-at-point-functions'.

  `tempel-expand' only triggers on exact matches.  Alternatively use
  `tempel-complete' if you want to see all matches, but then you
  should also configure `tempel-trigger-prefix', such that Tempel
  does not trigger too often when you don't expect it.  NOTE: We add
  `tempel-expand' *before* the main programming mode Capf, such
  that it will be tried first."
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

(with-eval-after-load 'tempel
  (require 'tempel-collection))

(provide 'mod-tempel)
;;; mod-tempel.el ends here
