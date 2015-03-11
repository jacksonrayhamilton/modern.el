;;; modern.el --- Modern settings to make Emacs easier to use.

;; Copyright (C) 2015  Jackson Ray Hamilton

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; URL: https://github.com/jacksonrayhamilton/modern.el
;; Keywords: modern standard normal familiar ergonomic settings keybindings
;; Version: 20150309
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; Emacs can be challenging to use out-of-the-box due to many of its unusual
;; default settings and keyboard shortcuts.  This package enables numerous
;; "standard" text editor features, and rebinds many commonly-used commands to
;; keys that are either familiar or more ergonomic.

;; The goal of this package is make new and experienced Emacs users more
;; productive without imposing too many opinionated features on them.

;; Keys:

;; Basic text manipulation:
;; - Control C: Copy
;; - Control X: Cut
;; - Control V: Paste
;; - Control A: Select all

;; Navigation:
;; - Control H: Back 1 character
;; - Alt H: Back 1 word
;; - Control Alt H: Back 1 expression
;; - Control J: Next line
;; - Control K: Previous line
;; - Control L: Forward 1 character
;; - Alt L: Forward 1 word
;; - Control Alt L: Forward 1 expression
;; - Control ,: Beginning of line
;; - Control .: End of line

;; Files:
;; - Control O: Open a file (or create a new one)
;; - Control B: Switch to another open file
;; - Control S: Save the current file
;; - Control W: Close the current file

;; Searching:
;; - Control F: Find
;; - Alt F: Find by regular expression
;; - Control R: Find and replace
;; - Alt R: Find and replace by regular expression

;; Screens:
;; - Control 0: Hide the focused screen
;; - Control 1: Only show the focused screen
;; - Control 2: Split screen horizontally
;; - Control 3: Split screen vertically

;; Advanced text manipulation:
;; - Control T: Delete line
;; - Alt T: Delete expression

;; Interactive development:
;; - Control E: Evaluate expression
;; - Alt E: Evaluate file

;; The default keys for the above commands are unbound.  All other Emacs keys
;; remain the same.  (If you typically used Control H to get help, try F1
;; instead.)

;;; Code:


;;; Appearance

;; Show the column number in the modeline.
(column-number-mode)

;; Match parentheses.
(require 'paren)
(show-paren-mode)
(setq show-paren-delay 0)

;; Flexibly resize the window.
(setq frame-resize-pixelwise t)


;;; Formatting

;; Default to the commonly-employed "80-column rule."
(setq-default fill-column 80)


;;; Features

;; Quickly switch between open files and navigate through directories.
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Enable C-x C-j for dired-jump.
(require 'dired-x)


;;; Overrides

;; Make prompts less annoying.
(fset 'yes-or-no-p 'y-or-n-p)

;; Ignore duplicates when cutting text.
(setq kill-do-not-save-duplicates t)

;; Just create new files when looking for non-existent ones.
(setq confirm-nonexistent-file-or-buffer nil)


;;; Keybindings

(require 'comint)  ; comint-mode-map
(require 'ido)     ; ido-completion-map

(dolist
    (key
     '(;; Use C-c for copy, C-x for cut, and C-v for paste.
       "M-w" "C-w" "C-y"

       ;; Use C-z for undo.
       "C-/"

       ;; Use h, j, k, l, "," and "." instead of the normal movement keys.
       "C-f" "M-f" "C-M-f" "C-b" "M-b" "C-M-b" "C-p" "C-n"
       "C-a" "C-e" "M-a" "M-e"

       ;; Navigate with the mouse rather than by screenfuls.
       "C-v" "M-v"

       ;; Use C-f for find and C-r for find and replace.  Replace control with
       ;; meta for the regexp version.
       "C-s" "C-M-s" "M-%" "C-M-%"

       ;; Use C-o for opening, C-b for switching, and C-s for saving.
       "C-x C-f" "C-x b" "C-x s"

       ;; Use C-w to close.  Also, C-w is bound to kill-this-buffer, which only
       ;; prompts when killing unsaved buffers.
       "C-x k"

       ;; Use C-0, C-1, C-2 and C-3 for window manipulation.
       "C-x 0" "C-x 1" "C-x 2" "C-x 3"

       ;; Use C-a to select all.
       "C-x h"

       ;; Use C-e to eval the last sexp.
       "C-x C-e"

       ;; Use M-t to kill sexps.
       "C-M-k"))
  (global-unset-key (kbd key)))

;; Unset the number keys.  They are rarely if ever useful, and with these
;; bindings they are more often pressed accidentally.
(mapc
 (lambda (number)
   (global-unset-key (kbd (format "C-%s" number))))
 (number-sequence 0 9))

;; Use C-c for copy, C-x for cut, and C-v for paste.  (Also delete text as you
;; begin typing or paste.)
(cua-mode)

;; Use Vi's navigation keys: h (left), j (down), k (up) and l (right).  "j"
;; looks like a down arrow; "h" is on the left, and "l" is on the right.  By
;; process of elimination, "k" must mean "up".
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

;; "," and "." share keys with "<" and ">", which look like left and right
;; arrows, which indicate which direction to move.
(global-set-key (kbd "C-,") 'move-beginning-of-line)
(global-set-key (kbd "C-.") 'move-end-of-line)

;; Use C-o for opening [files], C-b for switching [buffers], and C-s for saving.
(global-set-key (kbd "C-o") 'ido-find-file)
(define-key dired-mode-map (kbd "C-o") 'ido-find-file)
(add-hook
 'ido-setup-hook
 (lambda ()
   (define-key ido-completion-map (kbd "C-x C-f") nil)
   (define-key ido-completion-map (kbd "C-o") 'ido-fallback-command)))
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-s") 'save-buffer)

;; Use C-w to close, like in a web browser.
(global-set-key (kbd "C-w") 'kill-this-buffer)

;; Use C-f for find and C-r for find and replace.  Replace control with meta for
;; the regexp version.
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-s") nil)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(global-set-key (kbd "M-f") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'query-replace)
(global-set-key (kbd "M-r") 'query-replace-regexp)

;; Use C-0, C-1, C-2 and C-3 for window manipulation.  (Exchange "C-x" for "C".)
(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)

;; Use C-a to select all.
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; Eval is fun; use C-e and M-e to evaluate expressions and buffers.
(global-set-key (kbd "C-e") 'eval-last-sexp)
(global-set-key (kbd "M-e") 'eval-buffer)

;; Use C-t and M-t to kill lines and expressions.  "t" as in "terminate."
(global-set-key (kbd "C-t") 'kill-line)
(global-set-key (kbd "M-t") 'kill-sexp)

(provide 'modern)

;;; modern.el ends here
