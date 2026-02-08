;;; mod-web.el --- Support for web development   -*- lexical-binding: t; -*-

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

;; Support for web development using web-mode and eglot.
;; Handles HTML, JSX, TSX, Vue, Svelte, PHP, and Jinja2 files.

;;; Code:

(require 'mod-node)

;;;; Web-mode setup

(u/use-package 'web-mode)

(with-eval-after-load 'web-mode
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t))

;;;; Derived modes for different file types
;; This allows eglot to use different language servers per file type

(define-derived-mode html-web-mode web-mode "HTML"
  "Web-mode for HTML files.")

(define-derived-mode jsx-web-mode web-mode "JSX"
  "Web-mode for React JSX files.")

(define-derived-mode tsx-web-mode web-mode "TSX"
  "Web-mode for React TSX files.")

(define-derived-mode vue-web-mode web-mode "Vue"
  "Web-mode for Vue files.")

(define-derived-mode svelte-web-mode web-mode "Svelte"
  "Web-mode for Svelte files.")

(define-derived-mode php-web-mode web-mode "PHP"
  "Web-mode for PHP files."
  (setq indent-tabs-mode t))

(define-derived-mode jinja2-web-mode web-mode "Jinja2"
  "Web-mode for Jinja2 template files.")

;;;; File associations

(dolist (entry '(("\\.html?\\'" . html-web-mode)
                 ("\\.jsx\\'" . jsx-web-mode)
                 ("\\.tsx\\'" . tsx-web-mode)
                 ("\\.vue\\'" . vue-web-mode)
                 ("\\.svelte\\'" . svelte-web-mode)
                 ("\\.php\\'" . php-web-mode)
                 ("\\.j2\\'" . jinja2-web-mode)
                 ("\\.jinja2?\\'" . jinja2-web-mode)))
  (add-to-list 'auto-mode-alist entry))

;;;; Language server configuration
;; Centralized configuration: (mode executable npm-package)
;; Uses typescript-language-server from mod-node for JSX/TSX

(defvar u/web-language-servers
  `((html-web-mode "vscode-html-language-server" "vscode-langservers-extracted")
    (jsx-web-mode ,u/node-ls-executable ,u/node-ls-npm-package)
    (tsx-web-mode ,u/node-ls-executable ,u/node-ls-npm-package)
    (vue-web-mode "vue-language-server" "@vue/language-server")
    (svelte-web-mode "svelteserver" "svelte-language-server"))
  "Language server configuration for web modes.
Each entry is (MODE EXECUTABLE NPM-PACKAGE).")

;;;; Eglot ensure

(defun u/web-eglot-ensure ()
  "Ensure eglot is started with the appropriate language server.
Automatically installs the language server if not found."
  (when-let* ((config (assoc major-mode u/web-language-servers))
              (executable (nth 1 config))
              (npm-package (nth 2 config)))
    (u/eglot-ensure-ls
     (lambda () (executable-find executable))
     (lambda ()
       (message "Installing %s..." npm-package)
       (u/lsp-install-npm-package npm-package)
       (message "Installing %s...done" npm-package)))))

;;;; Eglot configuration

(with-eval-after-load 'eglot
  (dolist (config u/web-language-servers)
    (let ((mode (nth 0 config))
          (executable (nth 1 config)))
      (add-to-list 'eglot-server-programs
                   `(,mode . (,executable "--stdio"))))))

;;;; Mode hooks

(dolist (config u/web-language-servers)
  (let ((hook (intern (format "%s-hook" (car config)))))
    (add-hook hook #'u/web-eglot-ensure)))

(provide 'mod-web)
;;; mod-web.el ends here
