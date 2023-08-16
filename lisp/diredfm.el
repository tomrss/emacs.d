;;; diredfm.el --- Enhance Dired as file manager -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com>
;; Maintainer: Tommaso Rossi <tommaso.rossi1@protonmail.com>
;; Created: 2023
;; Version: 0.1
;; Keywords: convenience, dired

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

;; Dired is a great file manager.  This package expand furhter its
;; capabilities with some utilities.
;;
;; Features:
;; - open files dwim: open file in Emacs or with external program.
;;    see `diredfm-handlers-alist'.
;; (Currently just this one...)

;;; Code:

(require 'dired)

(defgroup diredfm nil
  "Dired File Manager."
  :group 'dired)

;;;; Custom variables

(defcustom diredfm-handlers-alist
  '(("\\.mkv\\'" . "mpv"))
  "Handlers for opening files in Dired.
Files that do not match any pattern will be opened in Emacs as usual.
Every element of the list can be:
- a cons with filename pattern and handler.
  Handler can be a string shell command or a function.
- a pattern string.  In this case, `diredfm-default-hanlder' will be used."
  :group 'diredfm
  :type 'list)

(defcustom diredfm-default-handler
  (lambda (file)
    (diredfm--open-file-externally "xdg-open" file))
  "Default handler for opening external files."
  :group 'diredfm
  :type 'function)

;;;; Helpers

(defun diredfm--open-file-externally (command file)
  "Open FILE with external COMMAND."
  (async-shell-command (format "%s '%s'" command file)))

(defun diredfm--find-handler (file)
  "Find handler for opening FILE."
  (catch 'handler
    (dolist (handler-spec diredfm-handlers-alist)
      ;; normalize elements in alist
      (let* ((handler-cons
              (cond
               ((stringp handler-spec)
                ;; handler spec is a string with no handler attached, use default handler
                (cons handler-spec diredfm-default-handler))
               ((consp handler-spec)
                ;; use handler in handler spec cons or default if nil
                (cons (car handler-spec) (or (cdr handler-spec) diredfm-default-handler)))
               (t
                (user-error "Elements of handlers alist must be either string or cons"))))
             (pattern (car handler-cons))
             (handler (cdr handler-cons)))
        ;; check if normalized alist element matches pattern
        (when (string-match pattern (file-name-nondirectory file))
          ;; found a match, return attached handler
          (throw 'handler handler))))))

;;;; Commands

;;;###autoload
(defun diredfm-file-size ()
  "Get the file size on disk."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (message
     "Size of %s: %s"
     (file-name-nondirectory file)
     (with-temp-buffer
       (save-match-data
         (shell-command (format "du -hs '%s'" file) t)
         (message "contaent is %s" (buffer-string))
         (goto-char (point-min))
         (when (search-forward-regexp (concat "\\(.*\\).*" file) nil t)
           (match-string 1)))))))

;;;###autoload
(defun diredfm-open-dwim ()
  "Open in file in Emacs or with external program from Dired.
External file handlers are driven by `+dired-open-handlers-alist' and
`+dired-open-default-handler' variables."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (handler (diredfm--find-handler file)))
    (cond
     ((not handler)
      ;; no handler found in `diredfm-handlers-alist', open in Emacs
      (dired-find-file))
     ((functionp handler)
      ;; call function type handler
      (funcall handler file))
     ((stringp handler)
      ;; try to call external executable
      (if-let ((command-handler (executable-find handler)))
          (diredfm--open-file-externally command-handler file)
        (user-error "Handler %s is not found as an executable" handler)))
     (t
      (user-error "Invalid handler type for file %s" file)))))

(provide 'diredfm)
;;; diredfm.el ends here
