;;; mod-development.el --- Development module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for development, mainly centered around builtin Eglot mode.

;;; Code:

(eval-and-compile
  (require 'project))

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
                    :background nil
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

(defun u/eglot-deferred ()
  "Load eglot deferred excluding consult previews."
  (unless (u/consult-preview-p)
    (eglot-ensure)))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "M-RET") #'eglot-code-actions))

;; make eglot completions work with orderless + corfu
(setq completion-category-overrides '((eglot (styles orderless))))

;;;; Java

;; TODO it doeesnt work
;; (u/use-package 'eglot-java)
;; (add-hook 'java-mode-hook 'eglot-java-mode)

;;;; Groovy

(u/use-package 'groovy-mode)
;; TODO find a groovy ls that works
;; (add-hook 'groovy-mode-hook #'u/eglot-deferred)
(add-to-list 'auto-mode-alist '("\\.groovy\\'" . groovy-mode))

;;;; Kotlin

(u/use-package 'kotlin-mode)
(add-hook 'kotlin-mode-hook #'u/eglot-deferred)
(add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-mode))

;;;; Scala

(u/use-package 'scala-mode)
(add-hook 'scala-mode-hook #'u/eglot-deferred)
(add-to-list 'auto-mode-alist '("\\.sc\(ala\)?\\'" . scala-mode))

;;;; Clojure

(u/use-package 'clojure-mode)
(add-hook 'clojure-mode-hook #'u/eglot-deferred)
(add-to-list 'auto-mode-alist '("\\.clj\\'" . clojure-mode))
(u/use-package 'cider)

;;;; Python

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

;;;; C#

(u/use-package 'csharp-mode)
(add-hook 'csharp-mode-hook #'u/eglot-deferred)
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-mode))

;;;; Go

(u/use-package 'go-mode)
(add-hook 'go-mode-hook #'u/eglot-deferred)
(add-hook 'go-mode-hook (lambda () (setq indent-tabs-mode t)))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

;;;; LaTeX

;; add a preview pane of the current edited LaTeX buffer.
(u/use-package 'latex-preview-pane)
(add-hook 'latex-mode-hook #'latex-preview-pane-mode)

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

;;;; Terraform

(u/use-package 'terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'u/eglot-deferred)
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(terraform-mode "terraform-ls" "serve")))

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
  (u/terraform-command "destroy" t))

;;;; Rest client

(u/use-package 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; TODO write something to persist requests/responses in `scratch' fashion.
;; example: dump request to file on `restclient-http-do-hook',
;; then dump response to same file on `restclient-response-loaded-hook'.
;; just be careful on major mode

;;;; Kubernetes

(u/use-package 'kubernetes)
(u/use-package 'kubernetes-evil)
(with-eval-after-load 'kubernetes-overview
  ;; set very low frequency because it is too dangerous and slow
  (setq kubernetes-poll-frequency 3600)
  (setq kubernetes-redraw-frequency 3600)
  ;; press enter to view kubernetes resource YAML
  (require 'kubernetes-evil))

(provide 'mod-development)
;;; mod-development.el ends here
