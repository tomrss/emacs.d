;;; core-utils.el --- Some Lisp utilities            -*- lexical-binding: t; -*-

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

;; Some Lisp utilities and helpers.

;;; Code:

(defun u/find-first (predicate list)
  "Find first element in LIST matching PREDICATE."
  (let ((tail list)
        (found nil))
    (while (and (not found) tail)
      (let ((x (car tail)))
        (if (funcall predicate x)
            (setq found x)
          (setq tail (cdr tail)))))
    found))

(defun u/shell-command-in-buffer (command &optional buffer)
  "Execute shell command COMMAND in BUFFER.
If BUFFER is nil, new buffer will be generated."
  (let ((buf (or buffer (generate-new-buffer "*shell-command*"))))
    ;; (display-buffer buf)
    (with-current-buffer buf
      (unless (derived-mode-p 'compilation-mode) (compilation-mode))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "$ ")
        (insert command)
        (newline)
        (newline)
        (unless (zerop (call-process-shell-command command nil buf t))
          (error "Error executing command.  See buffer %s for details" (buffer-name buf)))
        (newline)))))

(provide 'core-utils)
;;; core-utils.el ends here
