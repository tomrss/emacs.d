;;; core-dired.el --- Dired customization -*- lexical-binding: t -*-

;; Copyright (C) 2022-2023 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com>

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

;; Module for customizing `dired' as a fully fledged file manager.

;;; Code:

;;;; File management

(require 'core-keys)
(require 'core-packaging)

(with-eval-after-load 'dired
  (eval-when-compile
    (require 'dired))

;;;; Dired defaults

  (setq dired-auto-revert-buffer t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes (if delete-by-moving-to-trash 'always 'top))
  (setq dired-create-destination-dirs 'ask)
  (setq dired-listing-switches "-agoahv --group-directories-first")
  (setq dired-use-ls-dired (not (eq system-type 'darwin)))

;;;; Enable dired-x for omit mode and enhanced find file commands

  (setq dired-x-hands-off-my-keys nil)
  (setq dired-omit-files "\\`[.]?#\\|\\`[.].*\\'")
  (require 'dired-x)

;;;; Some helpers

  (defvar u/dired-handlers-alist
    '(("\\.mkv\\'" . "mpv")
      ("\\.avi\\'" . "mpv"))
    "Handlers for opening files in Dired.
Files that do not match any pattern will be opened in Emacs as usual.
Every element of the list can be:
- a cons with filename pattern and handler.
  Handler can be a string shell command or a function.
- a pattern string.  In this case, `u/dired-default-hanlder' will be used.")

  (defun u/dired--open-file-externally (command file)
    "Open FILE with external COMMAND."
    (async-shell-command (format "%s '%s'" command file)))

  (defvar u/dired-default-handler
    (lambda (file)
      (u/dired--open-file-externally "xdg-open" file))
    "Default handler for opening external files.")

  (defun u/dired--find-handler (file)
    "Find handler for opening FILE."
    (catch 'handler
      (dolist (handler-spec u/dired-handlers-alist)
        ;; normalize elements in alist
        (let* ((handler-cons
                (cond
                 ((stringp handler-spec)
                  ;; handler spec is a string with no handler attached, use default handler
                  (cons handler-spec u/dired-default-handler))
                 ((consp handler-spec)
                  ;; use handler in handler spec cons or default if nil
                  (cons (car handler-spec) (or (cdr handler-spec) u/dired-default-handler)))
                 (t
                  (user-error "Elements of handlers alist must be either string or cons"))))
               (pattern (car handler-cons))
               (handler (cdr handler-cons)))
          ;; check if normalized alist element matches pattern
          (when (string-match pattern (file-name-nondirectory file))
            ;; found a match, return attached handler
            (throw 'handler handler))))))

  (defun u/dired-file-size ()
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

  (defun u/dired-open-dwim ()
    "Open in file in Emacs or with external program from Dired.
External file handlers are driven by `+dired-open-handlers-alist' and
`+dired-open-default-handler' variables."
    (interactive)
    (let* ((file (dired-get-file-for-visit))
           (handler (u/dired--find-handler file)))
      (cond
       ((not handler)
        ;; no handler found in `u/dired-handlers-alist', open in Emacs
        (dired-find-file))
       ((functionp handler)
        ;; call function type handler
        (funcall handler file))
       ((stringp handler)
        ;; try to call external executable
        (if-let ((command-handler (executable-find handler)))
            (u/dired--open-file-externally command-handler file)
          (user-error "Handler %s is not found as an executable" handler)))
       (t
        (user-error "Invalid handler type for file %s" file))))))

;;;; Set keys

(u/define-key (kbd "C-x j") #'dired-jump)
(define-key dired-mode-map (kbd "h") #'dired-omit-mode)
(define-key dired-mode-map (kbd "RET") #'u/dired-open-dwim)
(define-key dired-mode-map (kbd "f") #'u/dired-open-dwim)
(define-key dired-mode-map (kbd "b") #'dired-up-directory)
(define-key dired-mode-map (kbd "r") #'u/dired-file-size)

;;;; Dired colorization

(u/use-package 'diredfl)
(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'diredfl-mode))

;;;; Process management

(with-eval-after-load 'proced
  (eval-when-compile
    (require 'proced))
  (setq proced-auto-update-interval 5)
  (setq proced-auto-update-flag t))

(provide 'core-dired)
;;; core-dired.el ends here
