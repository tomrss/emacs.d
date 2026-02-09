;;; mod-java.el --- Support for Java development     -*- lexical-binding: t; -*-

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

;; Support for Java development.
;; TODO this module is to be written somehow

;;; Code:

;;;; Support sdkman

(add-to-list 'exec-path "~/.sdkman/candidates/java/current/bin/")
(add-to-list 'exec-path "~/.sdkman/candidates/maven/current/bin/")
(add-to-list 'exec-path "~/.sdkman/candidates/gradle/current/bin/")

;;;; Configure eglot

(use-package eglot-java
  :hook (java-mode . eglot-java-mode)
  :config
  (define-key eglot-java-mode-map (kbd "C-c j n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c j x") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c j t") #'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c j N") #'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c j T") #'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c j R") #'eglot-java-project-build-refresh))

;;;; ZKoss

(add-to-list 'auto-mode-alist '("\\.zul\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.gsp\\'" . xml-mode))

(provide 'mod-java)
;;; mod-java.el ends here
