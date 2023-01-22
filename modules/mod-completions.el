;;; mod-completions.el --- Completions module -*- lexical-binding: t -*-

;; Copyright (C) 2022 Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Module for setting up completions, both in minibuffer and at point,
;; setting up candidate filtering and sorting, enhancing
;; self-documentation.

;;; Code:

;;;; Getting help and docs

;; improve self documentation
(+use-package 'helpful)
(+define-key [remap describe-command] #'helpful-command)
(+define-key [remap describe-function] #'helpful-callable)
(+define-key [remap describe-key] #'helpful-key)
(+define-key [remap describe-symbol] #'helpful-symbol)
(+define-key [remap describe-variable] #'helpful-variable)
(+define-key (kbd "C-h o") #'helpful-symbol)
(+define-key (kbd "C-h p") #'helpful-at-point)

;; hint keybindings
(+use-package 'which-key)
(which-key-mode +1)
(with-eval-after-load 'which-key-mode
  (setq which-key-idle-delay 0.5))

;; help and docs in minibuffer
(+use-package 'marginalia)
(marginalia-mode +1)

;;;; Completion styles and functions

;; completion style (how completion candidates are narrowed)
(+use-package 'orderless)
(setq orderless-matching-styles
      '(orderless-literal orderless-initialism orderless-regexp))
(setq orderless-component-separator "[ +]+")
(setq completion-styles '(orderless))

;; completing read functions
(+use-package 'consult)
(with-eval-after-load 'consult
  (setq consult-narrow-key "<"))
(+define-key (kbd "C-x b")   #'consult-buffer)
(+define-key (kbd "C-x 4 b") #'consult-buffer-other-window)
(+define-key (kbd "C-x 5 b") #'consult-buffer-other-frame)
(+define-key (kbd "M-y")     #'consult-yank-pop)
(+define-key (kbd "C-x f")   #'consult-recent-file)
(+define-key (kbd "M-g e")   #'consult-compile-error)
(+define-key (kbd "M-g w")   #'consult-flymake)
(+define-key (kbd "M-g g")   #'consult-goto-line)
(+define-key (kbd "M-g M-g") #'consult-goto-line)
(+define-key (kbd "M-g o")   #'consult-outline)
(+define-key (kbd "M-g i")   #'consult-imenu)
(+define-key (kbd "M-g I")   #'consult-imenu-multi)
(+define-key (kbd "M-s f")   #'consult-find)
(+define-key (kbd "M-s L")   #'consult-locate)
(+define-key (kbd "M-s g")   #'consult-grep)
(+define-key (kbd "M-s G")   #'consult-git-grep)
(+define-key (kbd "M-s r")   #'consult-ripgrep)
(+define-key (kbd "C-s")     #'consult-line)
(+define-key (kbd "M-s m")   #'consult-multi-occur)
(+define-key (kbd "M-s k")   #'consult-keep-lines)
(+define-key (kbd "M-s u")   #'consult-focus-lines)
(define-key minibuffer-local-map (kbd "C-r") #'consult-history)

;; use consult in xref
(setq xref-show-xrefs-function       #'consult-xref)
(setq xref-show-definitions-function #'consult-xref)

(defun +consult-preview-p ()
  "Helper function to find out if Consult is previewing."
  (when-let (win (active-minibuffer-window))
    (not (eq nil (buffer-local-value
                  'consult--preview-function
                  (window-buffer win))))))

;;;; Minibuffer completions

;; completion UI
(+use-package 'vertico)
(vertico-mode +1)
(define-key vertico-map (kbd "C-j") #'vertico-next)
(define-key vertico-map (kbd "C-k") #'vertico-previous)

;; enable acting on minibuffer candidates (and much more)
(+use-package 'embark)
(setq prefix-help-command #'embark-prefix-help-command)
(+define-key (kbd "C-.") #'embark-act)
(with-eval-after-load 'embark
  (define-key embark-symbol-map (kbd "h") #'helpful-symbol))

(+use-package 'embark-consult)
(add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)

;;;; Completions in region

;; completion UI
(+use-package 'corfu)
(+use-package 'corfu-terminal)
(setq corfu-auto t)
(setq corfu-auto-delay 0.1)
(setq corfu-cycle t)
(setq corfu-quit-at-boundary t)
(setq corfu-preselect-first t)
(global-corfu-mode 1)
(define-key corfu-map (kbd "C-j") #'corfu-next)
(define-key corfu-map (kbd "C-k") #'corfu-previous)

;; TODO this would be nice, but straight doesn't add corfu extensions to build
;; (corfu-popupinfo-mode +1)
;; (setq corfu-popupinfo-delay '(2.0 . 0.))
;; (define-key corfu-map (kbd "C-k") #'corfu-previous)
;; (set-face-attribute 'corfu-popupinfo nil :height 1.0)

;; dont' remember where I found this piece of code
(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-quit-at-boundary t
                        corfu-quit-no-match t)
            (corfu-mode +1)))
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(setq tab-always-indent 'complete)

;;;; Completion at point functions

(+use-package 'cape)

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

(provide 'mod-completions)
;;; mod-completions.el ends here
