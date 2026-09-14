;;; mod-copilot.el --- Setup GitHub copilot in Emacs    -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Tommaso Rossi

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

;; Claude-Code-like experience for GitHub Copilot, powered by agent-shell (ACP).
;; Requires GitHub Copilot CLI in PATH:  npm install -g @github/copilot
;; First run: log in to Copilot CLI once outside Emacs (`copilot`, then /login).

;;; Code:

(use-package agent-shell
  :straight (agent-shell :type git :host github :repo "xenodium/agent-shell")
  :init
  (with-eval-after-load 'shell-maker
    (setq shell-maker-root-path u/cache-directory))
  :bind-keymap
  ("C-c g" . agent-shell-mode-map)
  :bind
  (:map agent-shell-diff-mode-map
        ("C-c C-c" . agent-shell-diff-accept-all)
        ("C-c C-k" . agent-shell-diff-reject-all)
        ("C-c f"   . agent-shell-diff-open-file))
  :commands (agent-shell agent-shell-github-start-copilot))

(provide 'mod-copilot)
;;; mod-copilot.el ends here
