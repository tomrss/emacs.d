;;; mod-treemacs.el --- Directory tree view with Treemacs -*- lexical-binding: t; -*-

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

;; Setup directory tree view similar to many common IDEs with `treemacs'.

;;; Code:

(use-package treemacs
  :init
  (u/define-key (kbd "C-x c t") #'treemacs-select-window)
  :config
  (treemacs-project-follow-mode t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-git-commit-diff-mode t)
  (treemacs-hide-gitignored-files-mode nil))

(use-package treemacs-magit
  :after (treemacs magit))

(provide 'mod-treemacs)
;;; mod-treemacs.el ends here
