;;; mod-python.el --- Support for Python development  -*- lexical-binding: t; -*-

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

;; Support for Python development with `eglot' and some features for
;; virtualenv with `pyvenv'.

;;; Code:

;;;; Ensure language server

(defvar u/python-ls-executable "pylsp"
  "Executable of python language server.")

(defvar u/python-ls-pip-package "python-lsp-server"
  "Pip package of python language server.")

(defvar u/python-ls-pip-package-extras '("rope"
                                         "yapf"
                                         "pyflakes"
                                         "pycodestyle")
  "Extras of python language server pip package.")

(defun u/python-install-upgrade-ls ()
  "Install or upgrade python-language-server (pylsp)."
  (interactive)
  (message "Installing python language server...")
  (u/lsp-install-pip-package u/python-ls-pip-package u/python-ls-pip-package-extras)
  (message "Installing python language server...done"))

;;;; Virtualenv integration

(u/use-package 'pyvenv)
(with-eval-after-load 'pyvenv
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

(defun u/python-create-virtualenv (python-interpreter)
  "Create virtualenv with PYTHON-INTERPRETER."
  (interactive (list
                (let ((bin-dir (file-name-directory (executable-find "python3"))))
                  (read-file-name "Python interpreter to use: " bin-dir nil nil "python"))))
  (let ((proj-name (project-name (project-current t))))
    (message "Creating virtualenv for project %s..." proj-name)
    (pyvenv-create proj-name python-interpreter)))

(defun u/python-setup-virtualenv-project (proj)
  "Setup pyvenv in project PROJ."
  (pyvenv-mode +1)
  (let* ((proj-name (project-name proj))
         (venv-directory (expand-file-name proj-name (pyvenv-workon-home))))
    (when (and pyvenv-virtual-env-name
               (not (string-equal proj-name pyvenv-virtual-env-name)))
      (pyvenv-deactivate))
    (unless pyvenv-virtual-env-name
      (if (file-directory-p venv-directory)
          (pyvenv-activate venv-directory)
        (when (y-or-n-p "No virtualenv found.  Create one?")
          (call-interactively #'u/python-create-virtualenv)
          (pyvenv-activate venv-directory))))))

(defun u/python-setup-virtualenv ()
  "Setup virtual environment."
  (when-let* ((proj (project-current)))
    (u/python-setup-virtualenv-project proj)))

;;;; Setup hooks

(add-hook 'python-mode-hook
          (lambda ()
            (u/python-setup-virtualenv)
            (u/eglot-ensure-ls (lambda () (executable-find u/python-ls-executable))
                               #'u/python-install-upgrade-ls)))

(provide 'mod-python)
;;; mod-python.el ends here
