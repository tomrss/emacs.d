;;; core-terminals.el --- Configure terminals -*- lexical-binding: t -*-

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

;; Module for configuring and enhancing terminals, deeply focused on Eshell.

;;; Code:

;;;; Compilation and Comint modes

;; colorize compilation buffer
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; follow output with scroll in compilation buffer
(setq compilation-scroll-output t)

;; make prompt readonly
(setq comint-prompt-read-only t)

;; use proper colors in comint mode
(setq comint-terminfo-terminal "term-256color")

;; default compilation command
(setq compile-command "make ")

;;;; Shell

;; note: shell is comint, so comint configuration affects also here

;; properly colorize shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; TODO: i don't want to confirm closing terminals with no real
;; process running when closing emacs.  (get-buffer-process) and
;; than lookup children pid?

;;;; Eshell

(u/define-key (kbd "C-x e") #'eshell)

;; eshell prompt
(defvar u/eshell-prompt-kube-section-enabled nil
  "Whether to show Kubernetes related prompt section.")

(with-eval-after-load 'em-prompt

  (defface u/eshell-whoami-face '((t (:inherit font-lock-builtin-face)))
    "Face for eshell prompt section displaying user and machine.")
  
  (defface u/eshell-venv-face '((t (:inherit default :foreground "grey")))
    "Face for eshell prompt virtualenv section.")

  (defface u/eshell-pwd-face '((t (:inherit font-lock-variable-name-face)))
    "Face for eshell prompt path section.")

  (defface u/eshell-git-face '((t (:inherit font-lock-type-face)))
    "Face for eshell prompt git section.")

  (defface u/eshell-kube-face '((t (:inherit font-lock-doc-face)))
    "Face for eshell prompt kubernetes section.")

  (defface u/eshell-time-face '((t (:inherit font-lock-comment-face)))
    "Face for eshell prompt time section.")

  (defun u/kubectl-config-jsonpath (jsonpath)
    "Get current config property of Kubectl identified by jsonpath."
    (when (executable-find "kubectl")
      (let ((cmd (format "kubectl config view --minify -o jsonpath='{%s}'" jsonpath)))
        (with-temp-buffer
          (shell-command cmd t)
          (buffer-substring-no-properties (point-min) (point-max))))))
  
  (defun u/eshell-prompt-pwd-section ()
    "Eshell prompt section that displays path (pwd)."
    (let ((section (abbreviate-file-name (eshell/pwd))))
      (propertize section 'face 'u/eshell-pwd-face)))

  (defun u/eshell-prompt-venv-section ()
    "Eshell prompt section that displays virtualenv info."
    (when (bound-and-true-p pyvenv-virtual-env-name)
      (let* ((python-version
              (with-temp-buffer
                (shell-command "python --version" t)
                (buffer-substring-no-properties (point-min)
                                                (- (point-max) 1))))
             (section
              (concat "(" pyvenv-virtual-env-name " : " python-version ")")))
        (propertize section 'face 'u/eshell-venv-face))))

  (defun u/eshell-prompt-whoami-section ()
    "Eshell prompt section that displays info about user and system."
    (let ((section (format "%s@%s" user-login-name (system-name))))
      (propertize section 'face 'u/eshell-whoami-face)))

  (defun u/eshell-prompt-git-section ()
    "Eshell prompt section that displays git info."
    (when-let* ((current-branch (when (fboundp 'magit-get-current-branch)
							      (magit-get-current-branch)))
                (section (concat " " current-branch)))
      (propertize section 'face 'u/eshell-git-face)))

  (defun u/eshell-prompt-kube-section ()
    "Eshell prompt section that displays Kubernetes info."
    (when-let* (u/eshell-prompt-kube-section-enabled
                (context (u/kubectl-config-jsonpath ".contexts[0].name"))
                (namespace (u/kubectl-config-jsonpath ".contexts[0].context.namespace"))
                (section
                 (if (or (string-equal namespace "default")
                         (string-blank-p namespace))
                     context
                   (format "%s : %s" context namespace))))
      (propertize section 'face 'u/eshell-kube-face)))

  (defun u/eshell-prompt-time-section ()
    "Eshell prompt section that displays time info."
    (let ((section (format-time-string "%H:%M:%S")))
      (propertize section 'face 'u/eshell-time-face)))

  (defun u/string-join-nonnil (separator &rest strings)
    "Joins STRINGS with SEPARATOR removing nil."
    (string-join (remove nil strings) separator))
  
  (defun u/eshell-prompt ()
    "The eshell prompt."
    (concat
     (if (bobp) "" "\n")
     (u/string-join-nonnil
      (propertize " • " 'face `(:foreground "white"))
      (u/eshell-prompt-venv-section)
      (u/eshell-prompt-whoami-section)
      (u/eshell-prompt-pwd-section)
      (u/eshell-prompt-git-section)
      (u/eshell-prompt-kube-section)
      (u/eshell-prompt-time-section))
     (let ((user-prompt (if (= (user-uid) 0) "\n#" "\nλ")))
	   (propertize user-prompt 'face (if (zerop eshell-last-command-status) 'success 'error)))
     " "))

  (setq eshell-prompt-function #'u/eshell-prompt
		eshell-prompt-regexp "^.*λ "
		eshell-highlight-prompt t))

;; eshell banner
(with-eval-after-load 'em-banner
  (setq eshell-banner-message
        '(concat
          "   __________ ________   __ 
  / __/ __/ // / __/ /  / / 
 / _/_\\ \\/ _  / _// /__/ /__
/___/___/_//_/___/____/____/
"
          (when-let ((proj (project-current)))
            (concat
             "\nin project: "
             (project-name proj)))
          "\nGNU Emacs "
          emacs-version
          "\n\n")))

;; eshell keys and aliases
(with-eval-after-load 'em-alias
  (dolist
      (alias
       '(("q"     "exit")
		 ("f"     "find-file $1")
		 ("ff"    "find-file $1")
		 ("d"     "dired $1")
		 ("pd"    "proced $1")
		 ("l"     "ls -lh $*")
		 ("ll"    "ls -lah $*")
		 ("k"     "kubectl $*")
		 ("h"     "helm $*")
         ("kk"    "kubernetes-overview")
		 ("git"   "git --no-pager $*")
		 ("gg"    "magit-status")
		 ("clear" "clear-scrollback")
         ("less"  "view-file $1")
		 ("u"     "eshell-up $1")))	; see section below for `eshell-up' command and package
    (add-to-list 'eshell-command-aliases-list alias)))

;; eshell history
(with-eval-after-load 'em-hist
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)
  (setq eshell-history-size 10000
		eshell-history-ignoredups t
		eshell-input-filter #'eshell-input-filter-initial-space
		;; don't record command in history if prefixed with whitespace
		eshell-input-filter #'eshell-input-filter-initial-space)
  (eshell-hist-initialize))

;; eshell visual commands
(with-eval-after-load 'em-term
  (dolist (cmd '("htop" "vim" "nvim"))
    (add-to-list 'eshell-visual-commands cmd)))

;; eshell defaults and generic conf
(with-eval-after-load 'eshell
  ;; colors
  (setq eshell-term-name "xterm-256color")
  (setenv "TERM" "xterm-256color")

  ;; disable pager, no need in eshell
  (setenv "PAGER" "cat")
  ;; truncate buffer for performance
  ;; TODO this is giving some problems...
  ;; (add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)

  ;; use TRAMP
  (add-to-list 'eshell-modules-list 'eshell-tramp)

  (setq password-cache t
		password-cache-expiry 3600)

  (setq eshell-buffer-maximum-lines 10000
		eshell-scroll-to-bottom-on-input 'all
		eshell-scroll-to-bottom-on-output 'all
		eshell-kill-processes-on-exit t
		eshell-glob-case-insensitive t
		eshell-error-if-no-glob t)

  (defun u/eshell-ctrl-d ()
    "Like C-d in a shell, it exits eshell and deletes window."
    (interactive)
    (when (and (eolp) (looking-back eshell-prompt-regexp))
      (eshell-life-is-too-much)
      (ignore-errors
        (delete-window))))

  ;; kuberentes section
  (defun u/eshell-toggle-kube-section ()
    "Toggle Kubernetes section in Eshell prompt."
    (interactive)
    (setq u/eshell-prompt-kube-section-enabled
          (not u/eshell-prompt-kube-section-enabled))
    (message "Prompt kubernetes section is now %s"
             (if u/eshell-prompt-kube-section-enabled
                 "enabled"
               "disabled")))
  
  ;; configure keys
  ;; TODO keys not evil!!

  ;; directory navigation
  (defun eshell-up-closest-parent-dir (file)
    "Find the closest parent directory of a file.
Argument FILE the file to find the closest parent directory for."
    (file-name-directory
     (directory-file-name
      (expand-file-name file))))

  (defun eshell-up-find-parent-dir (path &optional match)
    "Find the parent directory based on the user's input.
Argument PATH the source directory to search from.
Argument MATCH a string that identifies the parent directory to search for."
    (let ((closest-parent (eshell-up-closest-parent-dir path)))
      (if match
          (let ((case-fold-search nil))
            (locate-dominating-file
			 closest-parent
             (lambda (parent)
               (let ((dir (file-name-nondirectory
                           (expand-file-name
                            (directory-file-name parent)))))
				 (if (string-match match dir)
                     dir
                   nil)))))
		closest-parent)))

  (defun eshell-up (&optional match)
    "Go to a specific parent directory in eshell.
Argument MATCH a string that identifies the parent directory to go
to."
    (interactive)
    (let* ((path default-directory)
           (parent-dir (eshell-up-find-parent-dir path match)))
      (when parent-dir
        (eshell/cd parent-dir)))))

;; eshell syntax highlighting
(u/use-package 'eshell-syntax-highlighting)
(add-hook 'eshell-mode-hook #'eshell-syntax-highlighting-mode)

(provide 'core-terminals)
;;; core-terminals.el ends here
