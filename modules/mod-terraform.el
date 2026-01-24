;;; mod-terraform.el --- Support for Terraform development  -*- lexical-binding: t; -*-

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

;; Support for Terraform development with `eglot' and some useful
;; helper functions.

;;; Code:

;;;; Setup language

(u/use-package 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'eglot-ensure)
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

;;;; Language server setup

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(terraform-mode "terraform-ls" "serve")))

;;;; Helper functions

(with-eval-after-load 'terraform-mode
  (defun u/terraform-command (command &optional interactive)
    "Execute Terrafom COMMAND.

If INTERACTIVE is non-nil, `comint-mode' will be used."
    (let ((default-directory (project-root (project-current t))))
      (compilation-start
       (concat "terraform " command)
       interactive
       (lambda (_) (format "*terraform: %s @ %s *" command default-directory)))))

  (defun u/terraform-init ()
    "Terraform plan."
    (interactive)
    (u/terraform-command "init"))

  (defun u/terraform-plan ()
    "Terraform plan."
    (interactive)
    (u/terraform-command "plan"))

  (defun u/terraform-apply ()
    "Terraform apply."
    (interactive)
    (u/terraform-command "apply" t))

  (defun u/terraform-apply-auto-approve ()
    "Terraform apply auto approve."
    (interactive)
    (u/terraform-command "apply -auto-approve"))

  (defun u/terraform-destroy ()
    "Terraform destroy."
    (interactive)
    (u/terraform-command "destroy" t)))

(provide 'mod-terraform)
;;; mod-terraform.el ends here
