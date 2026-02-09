;;; mod-groovy.el --- Support for Groovy development  -*- lexical-binding: t; -*-

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

;; Support for Groovy development with `eglot'.

;;; Code:

;;;; Support sdkman

(add-to-list 'exec-path "~/.sdkman/candidates/groovy/current/bin/")

;;;; Ensure language server

(defvar u/groovy-ls-git-url
  "https://github.com/GroovyLanguageServer/groovy-language-server.git"
  "Directory of groovy language server.")

(defvar u/groovy-ls-directory (u/cache-file "groovy-language-server/")
  "Directory of groovy language server.")

(defvar u/groovy-ls-jar (expand-file-name "build/libs/groovy-language-server-all.jar"
                                          u/groovy-ls-directory)
  "Jar of the groovy language server.")

(defvar u/groovy-ls-build-cmd "./gradlew build -x test"
  "Command for building groovy language server.")

(defun u/groovy-install-ls ()
  "Install groovy language server."
  (unless (file-directory-p u/groovy-ls-directory)
    (message "Cloning groovy language server...")
    (unless (zerop (shell-command (format "git clone %s %s" u/groovy-ls-git-url u/groovy-ls-directory)))
      (error "Unable to clone groovy language server repo from %s" u/groovy-ls-git-url))
    (message "Cloning groovy language server...done"))
  (let* ((default-directory u/groovy-ls-directory)
         (buf (generate-new-buffer "*install-groovy-language-server*")))
    (message "Building groovy language server...")
    (u/shell-command-in-buffer u/groovy-ls-build-cmd buf)
    (message "Building groovy language server...done")))

;;;; Configure groovy mode and eglot

(defun u/groovy-eglot-ensure ()
  "Ensure `eglot' for groovy."
  (u/eglot-ensure-ls (lambda () (file-exists-p u/groovy-ls-jar))
                     #'u/groovy-install-ls))

;; TODO find a groovy ls that works
(use-package groovy-mode
  :mode "\\.groovy\\'"
  :hook (groovy-mode . u/groovy-eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(groovy-mode . ("java" "-jar" ,u/groovy-ls-jar))))

(provide 'mod-groovy)
;;; mod-groovy.el ends here
