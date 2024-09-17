;;; core-orgmode.el --- Setup Org mode -*- lexical-binding: t -*-

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

;; Module for customizing org-mode.

;;; Code:

(with-eval-after-load 'org
  ;; setup visual fill
  (add-hook 'org-mode-hook (lambda () (u/setup-visual-fill 100)))
  ;; org babel languages
  (require 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
	 (python . t)
	 (shell . t)))

  ;; templates for adding code snippets
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("yml" . "src yaml"))
  (add-to-list 'org-structure-template-alist '("json" . "src json"))
  (add-to-list 'org-structure-template-alist '("go" . "src go"))

  ;; some defaults
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-edit-src-content-indentation 2)
  (setq org-hide-block-startup nil)
  (setq org-src-preserve-indentation nil)
  (setq org-hide-leading-stars t)
  (setq org-adapt-indentation nil)
  (setq org-startup-folded 'content)
  (setq org-cycle-separator-lines 2)
  (setq org-return-follows-link t)
  (setq org-startup-truncated nil)
  (setq org-startup-with-inline-images t)
  (setq org-ellipsis " ▼")

  ;; auto tangle on save
  (defun u/org-auto-tangle ()
	"Set hook for auto tangling org files on save."
	(let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'u/org-auto-tangle 0 t))))

(provide 'core-orgmode)
;;; core-orgmode.el ends here
