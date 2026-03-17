;;; mod-eat.el --- Setup eat terminal emulator   -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Tommaso Rossi

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

;; Setup `eat' (Emulate A Terminal), a fast terminal emulator written
;; in Emacs Lisp with no native dependencies.

;;; Code:

(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :init
  (define-key project-prefix-map (kbd "t") #'eat-project)
  :config
  (setq eat-kill-buffer-on-exit t)
  (setq eat-query-before-killing-running-terminal 'auto)
  (setq eat-term-scrollback-size nil)
  (setq eat-term-name "xterm-256color")
  (u/eat-ensure-shell-integration)
  :hook
  (eshell-load . eat-eshell-mode)
  (eshell-load . eat-eshell-visual-command-mode))

(defconst u/eat-shell-integration-line
  "[ -n \"$EAT_SHELL_INTEGRATION_DIR\" ] && source \"$EAT_SHELL_INTEGRATION_DIR/%s\""
  "Template for shell integration line, %s is the shell name.")

(defun u/eat-shell-rc-file ()
  "Return the rc file path and shell name for the current shell."
  (let ((shell (file-name-nondirectory
                (or explicit-shell-file-name
                    (getenv "ESHELL")
                    (getenv "SHELL")
                    "/bin/sh"))))
    (pcase shell
      ("bash" (cons (expand-file-name "~/.bashrc") "bash"))
      ("zsh"  (cons (expand-file-name "~/.zshrc") "zsh"))
      (_      (user-error "Shell %s not supported for eat integration" shell)))))

(defun u/eat-ensure-shell-integration ()
  "Ensure eat shell integration is present in the shell rc file.
If not found, ask the user and append it."
  (interactive)
  (pcase-let* ((`(,rc-file . ,shell-name) (u/eat-shell-rc-file))
               (line (format u/eat-shell-integration-line shell-name)))
    (if (not (file-exists-p rc-file))
        (when (y-or-n-p (format "%s does not exist.  Create it with eat integration? " rc-file))
          (with-temp-file rc-file
            (insert line "\n"))
          (message "Created %s with eat shell integration" rc-file))
      (with-temp-buffer
        (insert-file-contents rc-file)
        (unless (string-match-p "EAT_SHELL_INTEGRATION_DIR" (buffer-string))
          (when (y-or-n-p (format "Add eat shell integration to %s? " rc-file))
            (write-region (concat "\n# Added by Emacs eat terminal (eat-ensure-shell-integration)\n" line "\n") nil rc-file t)
            (message "Added eat shell integration to %s" rc-file)))))))

(provide 'mod-eat)
;;; mod-eat.el ends here
