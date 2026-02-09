;;; mod-copilot.el --- Setup GitHub copilot in Emacs    -*- lexical-binding: t; -*-

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

;; Setup GitHub copilot Emacs.  Requires a copilot subscription in GitHub.
;; First time usage: run `M-x copilot-install-server` and then
;; `M-x copilot-authenticate` and follow the instructions.

;;; Code:

(use-package copilot
  :straight (copilot :type git
                     :host github
                     :repo "copilot-emacs/copilot.el"
                     :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :if (not (getenv "CI"))
  :config
  (setq copilot-indent-offset-warning-disable t)
  (define-key copilot-completion-map (kbd "C-f") 'copilot-accept-completion))

(provide 'mod-copilot)
;;; mod-copilot.el ends here
