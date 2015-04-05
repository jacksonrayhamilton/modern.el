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

(defun modern-set-key (shortcut command &optional keymap)
  "Setup SHORTCUT for COMMAND, on KEYMAP or globally."
  (cond
   (keymap
    (define-key keymap (kbd shortcut) command))
   (t
    (global-set-key (kbd shortcut) command))))

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
  (modern-set-key "C-h" 'backward-char)
  (modern-set-key "M-h" 'backward-word)
  (modern-set-key "C-M-h" 'backward-sexp)
  (modern-set-key "C-j" 'next-line)
  (modern-set-key "M-j" 'comint-next-input comint-mode-map)
  (modern-set-key "C-k" 'previous-line)
  (modern-set-key "M-k" 'comint-previous-input comint-mode-map)
  (modern-set-key "C-l" 'forward-char)
  (modern-set-key "M-l" 'forward-word)
  (modern-set-key "C-M-l" 'forward-sexp)
  (modern-set-key "C-M-l" 'forward-sexp comint-mode-map)

  ;; Use C-, to move to the beginning of the line and C-. to move to the end.  ","
  ;; and "." share keys with "<" and ">", which look like left and right arrows,
  ;; which indicate the direction to move.
  (modern-unset-keys "M-a" "M-e") ; C-a and C-e are used
  (modern-set-key "C-," 'move-beginning-of-line)
  (modern-set-key "C-." 'move-end-of-line)

  ;; Use C-o to open [files], C-b to switch [buffers], C-s to save, and C-M-s to
  ;; save-as.
  (modern-unset-keys "C-x C-f" "C-x b" "C-x s" "C-x C-w" "C-x C-j")
  (modern-set-key "C-o" 'ido-find-file)
  (modern-set-key "C-o" 'ido-find-file dired-mode-map)
  (modern-set-key "C-b" 'ido-switch-buffer)
  (modern-set-key "C-s" 'save-buffer)
  (modern-set-key "C-M-s" 'ido-write-file)
  (modern-set-key "C-/" 'dired-jump)
  (add-hook
   'ido-setup-hook
   (lambda ()
     (modern-set-key "C-x C-f" nil ido-completion-map)
     (modern-set-key "C-x C-w" nil ido-completion-map)
     (modern-set-key "C-o" 'ido-fallback-command ido-completion-map)
     (modern-set-key "C-M-s" 'ido-fallback-command ido-completion-map)))

  ;; Use C-w to close, like in a web browser.
  (modern-unset-keys "C-x k")
  (modern-set-key "C-w" 'kill-this-buffer)

  ;; Use C-f to find and C-r to find and replace.  Replace control with meta for
  ;; the regexp versions.
  (modern-unset-keys "M-%" "C-M-%") ; C-s, C-M-s are used
  (modern-set-key "C-f" 'isearch-forward)
  (modern-set-key "C-s" nil isearch-mode-map)
  (modern-set-key "C-f" 'isearch-repeat-forward isearch-mode-map)
  (modern-set-key "M-f" 'isearch-forward-regexp)
  (modern-set-key "C-r" 'query-replace)
  (modern-set-key "M-r" 'query-replace-regexp)

  ;; Use C-0, C-1, C-2 and C-3 to manipulate windows.  (Exchange C-x with C.)
  ;; Use M-b to switch between windows; C-b is frequently used to "change
  ;; buffer", so the concept of "changing" is tangled with the the first letter
  ;; of the word "buffer", hence where the "b" in M-b comes from.
  (modern-unset-keys
   "C-x 0" "C-x 1" "C-x 2" "C-x 3"
   "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"
   "C-x o")
  (modern-set-key "C-0" 'delete-window)
  (modern-set-key "C-1" 'delete-other-windows)
  (modern-set-key "C-2" 'split-window-below)
  (modern-set-key "C-3" 'split-window-right)
  (modern-set-key "M-b" 'other-window)

  ;; Use C-a to select all.
  (modern-unset-keys "C-x h")
  (modern-set-key "C-a" 'mark-whole-buffer)

  ;; Eval is fun; use C-e and M-e to evaluate expressions and buffers.
  (modern-unset-keys "C-x C-e")
  (modern-set-key "C-e" 'eval-last-sexp)
  (modern-set-key "M-e" 'eval-buffer)

  ;; Use C-M-d to kill expressions.
  (modern-set-key "C-M-d" 'kill-sexp))

(provide 'modern)

;;; modern.el ends here
