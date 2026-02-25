;;; mod-vterm.el --- Setup vterm terminal emulator   -*- lexical-binding: t; -*-

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

;; Setup `vterm' terminal emulator, based on C library and very fast.

;;; Code:

(require 'project)

(use-package vterm
  :init
  ;; overrides `project-vc-dir' but I use magit
  (define-key project-prefix-map (kbd "v") #'u/project-vterm)
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000)
  (setq vterm-kill-buffer-on-exit t))

;; add as project popup shell
(defun u/project-vterm (&optional arg)
  "Start vterm in the current project's root directory.

If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.

If a numeric universal argument ARG is passed, get or create a vterm
named after ARG.  That allows multiple vterm project vterms for
the same project."
  (interactive "P")
  (let* ((default-directory (project-root (project-current t)))
         (buf-basename (project-prefixed-buffer-name "vterm"))
         (buf-name (cond
                    ((numberp arg)
                     (format "%s<%d>" buf-basename arg))
                    ((stringp arg)
                     (format "%s--%s" buf-basename arg))
                    (arg
                     (generate-new-buffer-name buf-basename))
                    (t
                     buf-basename))))
    (if-let* ((buf (get-buffer buf-name)))
        (pop-to-buffer buf)
      (vterm-other-window buf-name))))

(provide 'mod-vterm)
;;; mod-vterm.el ends here
