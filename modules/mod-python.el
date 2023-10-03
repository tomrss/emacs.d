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

(u/use-package 'pyvenv)
(with-eval-after-load 'pyvenv
  (setq pyvenv-mode-line-indicator
        '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] "))))

(defun u/setup-virtualenv-project (proj)
  "Setup pyvenv in project PROJ."
  (pyvenv-mode +1)
  (let* ((proj-name (project-name proj))
         (venv-directory (expand-file-name proj-name (pyvenv-workon-home))))
    (when (and pyvenv-virtual-env-name
               (not (string-equal proj-name pyvenv-virtual-env-name)))
      (pyvenv-deactivate))
    (unless pyvenv-virtual-env-name
      (unless (file-directory-p venv-directory)
        (pyvenv-create proj-name python-shell-interpreter))
      (pyvenv-activate venv-directory))))

(defun u/setup-virtualenv ()
  "Setup virtual environment."
  (interactive)
  (u/setup-virtualenv-project (project-current t)))

(defun u/setup-virtualenv-after-dir-locals ()
  "Setup virtual environment after dir locals are set."
  (add-hook 'hack-local-variables-hook #'u/setup-virtualenv nil t))

(add-hook 'python-mode-hook #'u/eglot-deferred)
(add-hook 'python-mode-hook #'u/setup-virtualenv-after-dir-locals)

(provide 'mod-python)
;;; mod-python.el ends here
