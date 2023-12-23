;;; core-completions.el --- Setup completion framework -*- lexical-binding: t -*-

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

;; Module for setting up completions, both in minibuffer and at point,
;; setting up candidate filtering and sorting, enhancing
;; self-documentation.

;;; Code:

(require 'core-keys)
(require 'core-packaging)

;;;; Getting help and docs

;; improve self documentation
(u/use-package 'helpful)
(u/define-key [remap describe-command] #'helpful-command)
(u/define-key [remap describe-function] #'helpful-callable)
(u/define-key [remap describe-key] #'helpful-key)
(u/define-key [remap describe-symbol] #'helpful-symbol)
(u/define-key [remap describe-variable] #'helpful-variable)
(u/define-key (kbd "C-h o") #'helpful-symbol)
(u/define-key (kbd "C-h p") #'helpful-at-point)

;; hint keybindings
(u/use-package 'which-key)
(which-key-mode +1)
(with-eval-after-load 'which-key-mode
  (setq which-key-idle-delay 0.5))

;; help and docs in minibuffer
(u/use-package 'marginalia)
(marginalia-mode +1)

;;;; Completion styles and functions

;; completion style (how completion candidates are narrowed)
(u/use-package 'orderless)
(setq orderless-matching-styles
      '(orderless-literal orderless-initialism orderless-regexp))
(setq orderless-component-separator "[ +]+")
(setq completion-styles '(orderless))

;; completing read functions
(u/use-package 'consult)
(with-eval-after-load 'consult
  (setq consult-narrow-key "<"))

(u/define-key (kbd "C-x b")   #'consult-buffer)
(u/define-key (kbd "C-x 4 b") #'consult-buffer-other-window)
(u/define-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
(u/define-key (kbd "M-y")     #'consult-yank-pop)
(u/define-key (kbd "C-x f")   #'consult-recent-file)
(u/define-key (kbd "M-g e")   #'consult-compile-error)
(u/define-key (kbd "M-g w")   #'consult-flymake)
(u/define-key (kbd "M-g g")   #'consult-goto-line)
(u/define-key (kbd "M-g M-g") #'consult-goto-line)
(u/define-key (kbd "M-g o")   #'consult-outline)
(u/define-key (kbd "M-g i")   #'consult-imenu)
(u/define-key (kbd "M-g I")   #'consult-imenu-multi)
(u/define-key (kbd "M-s f")   #'consult-find)
(u/define-key (kbd "M-s L")   #'consult-locate)
(u/define-key (kbd "M-s g")   #'consult-grep)
(u/define-key (kbd "M-s G")   #'consult-git-grep)
(u/define-key (kbd "M-s r")   #'consult-ripgrep)
(u/define-key (kbd "C-s")     #'consult-line)
(u/define-key (kbd "M-s m")   #'consult-multi-occur)
(u/define-key (kbd "M-s k")   #'consult-keep-lines)
(u/define-key (kbd "M-s u")   #'consult-focus-lines)
(define-key minibuffer-local-map (kbd "C-r") #'consult-history)

;; use consult in xref
(setq xref-show-xrefs-function       #'consult-xref)
(setq xref-show-definitions-function #'consult-xref)

(defun u/consult-preview-p ()
  "Helper function to find out if Consult is previewing."
  (when-let (win (active-minibuffer-window))
    (not (eq nil (buffer-local-value
                  'consult--preview-function
                  (window-buffer win))))))

;;;; Minibuffer completions

;; completion UI
(u/use-package 'vertico)
(vertico-mode +1)
(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)

;; enable acting on minibuffer candidates (and much more)
(u/use-package 'embark)
(setq prefix-help-command #'embark-prefix-help-command)
(u/define-key (kbd "C-.") #'embark-act)
(with-eval-after-load 'embark
  (define-key embark-symbol-map (kbd "h") #'helpful-symbol))

(u/use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

;;;; Completions in region

;; completion UI
(u/use-package 'corfu)
(u/use-package 'corfu-terminal)
(setq corfu-auto t)
(setq corfu-auto-delay 0.1)
(setq corfu-cycle t)
(setq corfu-quit-at-boundary t)
(setq corfu-preselect-first t)
(global-corfu-mode 1)
(define-key corfu-map (kbd "C-j") #'corfu-next)
(define-key corfu-map (kbd "C-k") #'corfu-previous)

(setq tab-always-indent 'complete)

;; popup info with docs
(corfu-popupinfo-mode +1)
(setq corfu-popupinfo-delay '(1.5 . 0.))
(set-face-attribute 'corfu-popupinfo nil :height 1.0)

;; dont' remember where I found this piece of code
(add-hook 'eshell-mode-hook #'corfu-mode)

;; work also in terminal
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

;;;; Completion at point functions

(u/use-package 'cape)

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'ispell-completion-at-point :around #'cape-wrap-silent)

(provide 'core-completions)
;;; core-completions.el ends here
