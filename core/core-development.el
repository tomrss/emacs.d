;;; core-development.el --- Development features -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

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

;; Module for development, mainly centered around builtin Eglot mode.

;;; Code:

(require 'project)
(require 'core-completions)
(require 'core-keys)
(require 'core-packaging)
(require 'core-utils)

;;;; Version control

;; magit
(u/use-package 'magit)
(unless (fboundp 'magit-get-current-branch)
  (autoload #'magit-get-current-branch "magit" nil t))
(with-eval-after-load 'magit
  (setq magit-no-message
		'("Turning on magit-auto-revert-mode...")))

;; integrate magit with project
(define-key project-prefix-map (kbd "G") #'magit-status)
(add-to-list 'project-switch-commands '(magit-status "Magit"))

(u/define-key (kbd "C-x g") #'magit-status) ; is the default but it's somehow deleted

;; highlight changes (git gutters)
(u/use-package 'diff-hl)
(autoload #'diff-hl-magit-post-refresh "diff-hl" nil t)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode-unless-remote)
(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
(add-hook 'prog-mode-hook #'diff-hl-mode)
(add-hook 'prog-mode-hook #'diff-hl-margin-mode)
(diff-hl-flydiff-mode t)

;;;; Configure parentheses

;; highlight mathing parentesis
(show-paren-mode +1)
(set-face-attribute 'show-paren-match nil
                    :background "unspecified"
                    :foreground "red"
                    :italic t)

;; close matching parentheses
(electric-pair-mode +1)
(add-to-list 'electric-pair-pairs '(?{ . ?}))

;; highlight matching delimiters with rainbow colors
(u/use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'inferior-emacs-lisp-mode-hook #'rainbow-delimiters-mode)

;;;; Syntax checking

(add-hook 'prog-mode-hook #'flymake-mode)

;;;; Indentation

;; human tab with
(add-hook 'prog-mode-hook (lambda () (setq tab-width 4)))

;; indent with spaces
(setq-default indent-tabs-mode nil)

;; aggressively indent as you type
;; TODO this sometimes interfere with undo
(u/use-package 'aggressive-indent)
(autoload 'aggressive-indent-mode "aggressive-indent")
(with-eval-after-load 'aggressive-indent
  (setq aggressive-indent-comments-too t))
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'lisp-mode-hook #'aggressive-indent-mode)
(add-hook 'scheme-mode-hook #'aggressive-indent-mode)

;;;; Code snippets

;; TODO try tempel

;;;; Debugger

(straight-use-package 'realgud)

;;;; Eglot (Language Server Protocol)

(defun u/eglot-ensure-ls (installedp install-function)
  "Ensure that a language server is installed before starting `eglot'.
If not, prompt the user for installing it.
INSTALLEDP is a predicate checking if language server is installed,
called with no arguments.
INSTALL-FUNCTION is a function that installs the ls."
  (if (funcall installedp)
      (eglot-ensure)
    (when (y-or-n-p "Eglot requires a language server.  Install it now?")
      (funcall install-function)
      (eglot-ensure))))

(defun u/eglot-format-and-save ()
  "Format buffer with eglot and save."
  (interactive)
  (eglot-format)
  (save-buffer))

(with-eval-after-load 'eglot
  (setq eglot-autoshutdown t)
  (setq eglot-autoreconnect 2)
  (setq eglot-confirm-server-initiated-edits nil)
  (define-key eglot-mode-map (kbd "M-RET") #'eglot-code-actions)
  ;; this should enhance performance?
  (fset #'jsonrpc--log-event #'ignore))

;; make eglot completions work with orderless + corfu
(setq completion-category-overrides '((eglot (styles orderless))))

;;;; Treesitter

(u/use-package 'tree-sitter)
(u/use-package 'tree-sitter-langs)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(with-eval-after-load 'tree-sitter
  (require 'tree-sitter-langs))


;;;; LaTeX

;; TODO do something

;;;; Dockerfile

(u/use-package 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))

;;;; Markdown

(u/use-package 'markdown-mode)
(unless
    (fboundp 'gfm-mode)
  (autoload #'gfm-mode "markdown-mode" nil t))
(setq mardown-command "multimarkdown")
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mkd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;;; Yaml

(u/use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-mode))
;; this is for terraform templates
(add-to-list 'auto-mode-alist '("\\.ya?ml\\.tftpl\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (set (make-local-variable 'font-lock-variable-name-face)
                 'font-lock-type-face)))

;;;; Toml

(u/use-package 'toml-mode)
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))

;;;; Csv

(u/use-package 'csv-mode)
(add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-mode))

(provide 'core-development)
;;; core-development.el ends here
