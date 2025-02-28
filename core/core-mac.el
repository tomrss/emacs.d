;;; core-mac.el --- Configuration for MacOS     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Tommaso Rossi

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

;; This configuration bundle is made for GNU/Linux systems and tested
;; on many distributions.  With this module, it can somehow work in
;; MacOS.

;;; Code:

;; this is necessary in macos, otherwise emacs would not found the correct path
(u/use-package 'exec-path-from-shell)
;; TODO this is really slow, try to use a non-interactive shell
(exec-path-from-shell-initialize)
(setq exec-path-from-shell-arguments '("-l"))

;; fix dired
(with-eval-after-load 'dired
  (if (executable-find "gls")
      ;; use gnu ls from coreutils package
      (progn
        (setq insert-directory-program "gls")
        (setq dired-use-ls-dired t))
    ;; no gnu ls, disable some options that whould not work with mac ls
    (message "GNU ls not found.  For optimal dired experience, install with \"brew install coreutils\"")
    (setq dired-use-ls-dired nil)
    ;; no "group directories first" :(
    (setq dired-listing-switches "-agoahv")))

;; makes the right "option" key behave as in macos and not as emacs meta
(setq ns-right-alternate-modifier nil)

(provide 'core-mac)
;;; core-mac.el ends here
