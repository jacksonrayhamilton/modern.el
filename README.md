# modern.el

Replace Emacs's default shortcuts with modern ones.

## Installation

Clone this repository:

```bash
cd ~/.emacs.d
mkdir -p lisp
cd lisp
git clone https://github.com/jacksonrayhamilton/modern.el.git
```

Add the following to your init file:

```lisp
(add-to-list 'load-path "~/.emacs.d/lisp/modern.el")
(require 'modern)
(modern-mode)
```

Done!

## Keys

The updated keybindings are as follows.  Their defaults (like <kbd>Alt W</kbd>
for "copy") are disabled.

### Basic text manipulation

- <kbd>Ctrl Z</kbd>: Undo
- <kbd>Ctrl C</kbd>: Copy
- <kbd>Ctrl X</kbd>: Cut
- <kbd>Ctrl V</kbd>: Paste
- <kbd>Ctrl A</kbd>: Select all

### Files

- <kbd>Ctrl O</kbd>: Open / create file
- <kbd>Ctrl B</kbd>: Switch buffer
- <kbd>Ctrl S</kbd>: Save
- <kbd>Ctrl Alt S</kbd>: Save as
- <kbd>Ctrl W</kbd>: Close
- <kbd>Ctrl /</kbd>: View directory

### Searching

- <kbd>Ctrl F</kbd>: Find
- <kbd>Alt F</kbd>: Find by regular expression
- <kbd>Ctrl R</kbd>: Find and replace
- <kbd>Alt R</kbd>: Find and replace by regular expression

### Windows

- <kbd>Ctrl 0</kbd>: Hide the selected window
- <kbd>Ctrl 1</kbd>: Show only the selected window
- <kbd>Ctrl 2</kbd>: Split window horizontally
- <kbd>Ctrl 3</kbd>: Split window vertically

### Text navigation

- <kbd>Ctrl H</kbd>: Back 1 character
- <kbd>Alt H</kbd>: Back 1 word
- <kbd>Ctrl Alt H</kbd>: Back 1 expression
- <kbd>Ctrl L</kbd>: Forward 1 character
- <kbd>Alt L</kbd>: Forward 1 word
- <kbd>Ctrl Alt L</kbd>: Forward 1 expression
- <kbd>Ctrl J</kbd>: Next line
- <kbd>Ctrl K</kbd>: Previous line
- <kbd>Ctrl ,</kbd>: Beginning of line
- <kbd>Ctrl .</kbd>: End of line

### Advanced text manipulation

- <kbd>Ctrl Alt D</kbd>: Delete expression

### Interactive development

- <kbd>Ctrl E</kbd>: Evaluate expression before cursor
- <kbd>Alt E</kbd>: Evaluate file

## Recommendations

### Enable `ido-mode`

`ido-mode` lets you quickly browse open files and folders.  This package's
shortcuts are also supported in `ido-mode`.  Simply add the following to your
init file to enable it:

```lisp
(ido-mode 1)
```
