;;; modern.el --- Replace Emacs's insane default shortcuts with modern ones. -*- lexical-binding: t; -*-

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

(defvar modern-saved-keys '())

(defun modern-save-key (keys &optional keymap)
  "Save the command for KEYS, from KEYMAP or global definition."
  (let* ((keymap (or keymap (current-global-map)))
         (command (lookup-key keymap (kbd keys))))
    (setq modern-saved-keys
          (append modern-saved-keys
                  (list (list keymap (kbd keys) command))))))

(defun modern-restore-keys ()
  "Restore all saved keybindings."
  (dolist (keybinding modern-saved-keys)
    (apply 'define-key keybinding))
  (setq modern-saved-keys '()))

(defun modern-set-key (keys command &optional keymap)
  "Setup SHORTCUT for COMMAND, on KEYMAP or globally."
  (let* ((keymap (or keymap (current-global-map))))
    (modern-save-key keys keymap)
    (define-key keymap (kbd keys) command)))

(defun modern-setup-keys ()
  "Setup all keys for `modern-mode'."

  ;; Use C-z to undo, C-c to copy, C-x to cut, C-v to paste.  (Also delete text
  ;; as you begin typing or paste.)

  ;; C-/ is used
  (modern-set-key "M-w" nil)
  (modern-set-key "C-w" nil)
  (modern-set-key "C-y" nil)

  ;; cua-mode will be enabled.

  ;; Use C-h / M-h / C-M-h to move left, C-l / M-l / C-M-l to move right, C-j to
  ;; move down and C-k to move up.  Evilly, these are Vi's navigation keys.  "j"
  ;; looks like a down arrow; "h" is on the left, and "l" is on the right.  By
  ;; elimination, "k" must mean "up".

  ;; C-f, M-f, C-b, M-b are used
  (modern-set-key "C-M-f" nil)
  (modern-set-key "C-M-b" nil)
  (modern-set-key "C-p" nil)
  (modern-set-key "C-n" nil)

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

  ;; Use C-, to move to the beginning of the line and C-. to move to the end.
  ;; "," and "." share keys with "<" and ">", which look like left and right
  ;; arrows, which indicate the direction to move.

  ;; C-a and C-e are used
  (modern-set-key "M-a" nil)
  (modern-set-key "M-e" nil)

  (modern-set-key "C-," 'move-beginning-of-line)
  (modern-set-key "C-." 'move-end-of-line)

  ;; Use C-o to open [files], C-b to switch [buffers], C-s to save, and C-M-s to
  ;; save-as.

  (modern-set-key "C-x C-f" nil)
  (modern-set-key "C-x b" nil)
  (modern-set-key "C-x s" nil)
  (modern-set-key "C-x C-w" nil)
  (modern-set-key "C-x C-j" nil)

  (modern-set-key "C-o" 'find-file)
  (modern-set-key "C-o" 'find-file dired-mode-map)
  (modern-set-key "C-b" 'switch-to-buffer)
  (modern-set-key "C-s" 'save-buffer)
  (modern-set-key "C-M-s" 'write-file)
  (modern-set-key "C-/" 'dired-jump)

  ;; Use C-w to close, like in a web browser.

  (modern-set-key "C-x k" nil)
  (modern-set-key "C-w" 'kill-this-buffer)

  ;; Use C-f to find and C-r to find and replace.  Replace control with meta for
  ;; the regexp versions.

  ;; C-s, C-M-s are used
  (modern-set-key "M-%" nil)
  (modern-set-key "C-M-%" nil)

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

  (modern-set-key "C-x 0" nil)
  (modern-set-key "C-x 1" nil)
  (modern-set-key "C-x 2" nil)
  (modern-set-key "C-x 3" nil)
  (modern-set-key "C-4" nil)
  (modern-set-key "C-5" nil)
  (modern-set-key "C-6" nil)
  (modern-set-key "C-7" nil)
  (modern-set-key "C-8" nil)
  (modern-set-key "C-9" nil)
  (modern-set-key "C-x o" nil)

  (modern-set-key "C-0" 'delete-window)
  (modern-set-key "C-1" 'delete-other-windows)
  (modern-set-key "C-2" 'split-window-below)
  (modern-set-key "C-3" 'split-window-right)
  (modern-set-key "M-b" 'other-window)

  ;; Use C-a to select all.

  (modern-set-key "C-x h" nil)
  (modern-set-key "C-a" 'mark-whole-buffer)

  ;; Eval is fun; use C-e and M-e to evaluate expressions and buffers.

  (modern-set-key "C-x C-e" nil)
  (modern-set-key "C-e" 'eval-last-sexp)
  (modern-set-key "M-e" 'eval-buffer)

  ;; Use C-M-d to kill expressions.

  (modern-set-key "C-M-d" 'kill-sexp))

;;;###autoload
(define-minor-mode modern-mode
  "Replace Emacs's insane default shortcuts with modern ones."
  :global t
  :lighter " Modern"
  (cond
   (modern-mode
    (cua-mode 1)
    (modern-setup-keys)
    (add-hook 'ido-setup-hook 'modern-ido-setup-hook))
   (t
    (cua-mode -1)
    (modern-restore-keys)
    (remove-hook 'ido-setup-hook 'modern-ido-setup-hook))))

(defun modern-ido-setup-hook ()
  "Setup ido keys as soon as `ido-completion-map' is available."
  (when modern-mode
    (modern-set-key "C-x C-f" nil ido-completion-map)
    (modern-set-key "C-x C-w" nil ido-completion-map)
    (modern-set-key "C-o" 'ido-fallback-command ido-completion-map)
    (modern-set-key "C-M-s" 'ido-fallback-command ido-completion-map)))

(provide 'modern)

;;; modern.el ends here
