;;; modern.el --- Replace Emacs's insane default shortcuts with modern ones.

;; Copyright (C) 2015  Jackson Ray Hamilton

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; URL: https://github.com/jacksonrayhamilton/modern.el
;; Keywords: modern standard normal familiar ergonomic settings keybindings
;; Version: 20150404
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; Replace Emacs's insane default shortcuts with modern ones.

;;; Code:

(require 'comint)  ; comint-mode-map
(require 'dired-x) ; dired-jump
(require 'ido)     ; ido-completion-map

(defun modern-unset-keys (&rest keys)
  "Unset KEYS."
  (dolist (key keys)
    (global-unset-key (kbd key))))

;;;###autoload
(defun modern-enable ()
  "Replace Emacs's insane default shortcuts with modern ones.

This function causes irreversible side-effects. To revert them,
restart Emacs."

  ;; Use C-z to undo, C-c to copy, C-x to cut, C-v to paste.  (Also delete text
  ;; as you begin typing or paste.)
  (modern-unset-keys "C-/" "M-w" "C-w" "C-y")
  (cua-mode)

  ;; Use C-h / M-h / C-M-h to move left, C-l / M-l / C-M-l to move right, C-j to
  ;; move down and C-k to move up.  Evilly, these are Vi's navigation keys.  "j"
  ;; looks like a down arrow; "h" is on the left, and "l" is on the right.  By
  ;; elimination, "k" must mean "up".
  (modern-unset-keys "C-M-f" "C-M-b" "C-p" "C-n") ; C-f, M-f, C-b, M-b are used
  (global-set-key (kbd "C-h") 'backward-char)
  (global-set-key (kbd "M-h") 'backward-word)
  (global-set-key (kbd "C-M-h") 'backward-sexp)
  (global-set-key (kbd "C-j") 'next-line)
  (define-key comint-mode-map (kbd "M-j") 'comint-next-input)
  (global-set-key (kbd "C-k") 'previous-line)
  (define-key comint-mode-map (kbd "M-k") 'comint-previous-input)
  (global-set-key (kbd "C-l") 'forward-char)
  (global-set-key (kbd "M-l") 'forward-word)
  (global-set-key (kbd "C-M-l") 'forward-sexp)
  (define-key comint-mode-map (kbd "C-M-l") 'forward-sexp)

  ;; Use C-, to move to the beginning of the line and C-. to move to the end.  ","
  ;; and "." share keys with "<" and ">", which look like left and right arrows,
  ;; which indicate the direction to move.
  (modern-unset-keys "M-a" "M-e") ; C-a and C-e are used
  (global-set-key (kbd "C-,") 'move-beginning-of-line)
  (global-set-key (kbd "C-.") 'move-end-of-line)

  ;; Use C-o to open [files], C-b to switch [buffers], C-s to save, and C-M-s to
  ;; save-as.
  (modern-unset-keys "C-x C-f" "C-x b" "C-x s" "C-x C-w" "C-x C-j")
  (global-set-key (kbd "C-o") 'ido-find-file)
  (define-key dired-mode-map (kbd "C-o") 'ido-find-file)
  (global-set-key (kbd "C-b") 'ido-switch-buffer)
  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "C-M-s") 'ido-write-file)
  (global-set-key (kbd "C-/") 'dired-jump)
  (add-hook
   'ido-setup-hook
   (lambda ()
     (define-key ido-completion-map (kbd "C-x C-f") nil)
     (define-key ido-completion-map (kbd "C-x C-w") nil)
     (define-key ido-completion-map (kbd "C-o") 'ido-fallback-command)
     (define-key ido-completion-map (kbd "C-M-s") 'ido-fallback-command)))

  ;; Use C-w to close, like in a web browser.
  (modern-unset-keys "C-x k")
  (global-set-key (kbd "C-w") 'kill-this-buffer)

  ;; Use C-f to find and C-r to find and replace.  Replace control with meta for
  ;; the regexp versions.
  (modern-unset-keys "M-%" "C-M-%") ; C-s, C-M-s are used
  (global-set-key (kbd "C-f") 'isearch-forward)
  (define-key isearch-mode-map (kbd "C-s") nil)
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (global-set-key (kbd "M-f") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'query-replace)
  (global-set-key (kbd "M-r") 'query-replace-regexp)

  ;; Use C-0, C-1, C-2 and C-3 to manipulate windows.  (Exchange C-x with C.)
  ;; Use M-b to switch between windows; C-b is frequently used to "change
  ;; buffer", so the concept of "changing" is tangled with the the first letter
  ;; of the word "buffer", hence where the "b" in M-b comes from.
  (modern-unset-keys
   "C-x 0" "C-x 1" "C-x 2" "C-x 3"
   "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"
   "C-x o")
  (global-set-key (kbd "C-0") 'delete-window)
  (global-set-key (kbd "C-1") 'delete-other-windows)
  (global-set-key (kbd "C-2") 'split-window-below)
  (global-set-key (kbd "C-3") 'split-window-right)
  (global-set-key (kbd "M-b") 'other-window)

  ;; Use C-a to select all.
  (modern-unset-keys "C-x h")
  (global-set-key (kbd "C-a") 'mark-whole-buffer)

  ;; Eval is fun; use C-e and M-e to evaluate expressions and buffers.
  (modern-unset-keys "C-x C-e")
  (global-set-key (kbd "C-e") 'eval-last-sexp)
  (global-set-key (kbd "M-e") 'eval-buffer)

  ;; Use C-M-d to kill expressions.
  (global-set-key (kbd "C-M-d") 'kill-sexp))

(provide 'modern)

;;; modern.el ends here
