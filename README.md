# modern.el

Replace Emacs's insane default shortcuts with modern ones.

## Installation

First download and install the latest Emacs for [GNU/Linux][], [Mac OS][] or
[Windows][].

[GNU/Linux]: http://ftp.gnu.org/gnu/emacs/emacs-24.4.tar.gz
[Mac OS]: http://emacsformacosx.com/
[Windows]: http://ftp.gnu.org/gnu/emacs/windows/emacs-24.4-bin-i686-pc-mingw32.zip

Then, from your terminal (*nixes):

```bash
cd ~/.emacs.d
mkdir -p lisp
cd lisp
git clone https://github.com/jacksonrayhamilton/modern.el.git
```

Or from `cmd.exe` (Windows):

```bat
cd %HOMEPATH%\AppData\Roaming\.emacs.d
mkdir lisp
cd lisp
git clone https://github.com/jacksonrayhamilton/modern.el.git
```

And finally in your `~/.emacs` file (*nixes) or
`C:\Users\USER\AppData\Roaming\.emacs` (Windows):

```lisp
(add-to-list 'load-path "~/.emacs.d/lisp/modern.el")
(require 'modern)
(modern-mode)
```

## Keys

Items in italics are default Emacs shortcuts, included here for the sake of
beginners.  All other shortcuts are enabled by this package, and their defaults
(like <kbd>Alt W</kbd> for "copy") are disabled.

### Emacs essentials

- _<kbd>F1</kbd>: Help_
- _<kbd>Ctrl G</kbd>: Cancel_

### Basic text manipulation

- <kbd>Ctrl Z</kbd>: Undo
- <kbd>Ctrl C</kbd>: Copy
- <kbd>Ctrl X</kbd>: Cut
- <kbd>Ctrl V</kbd>: Paste
- <kbd>Ctrl A</kbd>: Select all

### Files

- <kbd>Ctrl O</kbd>: Open / create a file
- <kbd>Ctrl B</kbd>: Switch to another open file
- <kbd>Ctrl S</kbd>: Save
- <kbd>Ctrl Alt S</kbd>: Save as
- <kbd>Ctrl W</kbd>: Close
- <kbd>Ctrl /</kbd>: View folder

### Searching

- <kbd>Ctrl F</kbd>: Find
- <kbd>Alt F</kbd>: Find by regular expression
- <kbd>Ctrl R</kbd>: Find and replace
- <kbd>Alt R</kbd>: Find and replace by regular expression

Tip: Try out regular expressions with <kbd>Alt X re-builder</kbd>.

### Screen sections

- <kbd>Ctrl 0</kbd>: Hide the focused screen section
- <kbd>Ctrl 1</kbd>: Show only the focused screen section
- <kbd>Ctrl 2</kbd>: Split screen horizontally
- <kbd>Ctrl 3</kbd>: Split screen vertically
- <kbd>Alt B</kbd>: Switch to another section

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

(Though the arrow keys and your mouse work too.)

### Advanced text manipulation

- _<kbd>Ctrl Space</kbd>: Start / stop selecting text_
- _<kbd>Ctrl D</kbd>: Delete 1 character ahead_
- _<kbd>Alt D</kbd>: Delete 1 word ahead_
- _<kbd>Alt Backspace</kbd>: Delete 1 word behind_
- <kbd>Ctrl Alt D</kbd>: Delete expression
- _<kbd>Ctrl Shift Backspace</kbd>: Delete line_
- _<kbd>Alt Y</kbd>: Cycle through previous copies (after <kbd>Ctrl V</kbd>)_
- _<kbd>Alt ;</kbd>: Insert a comment or comment-out a selection_
- _<kbd>Alt Q</kbd>: Fit text within the column limit, if possible_

### Useful Emacs utilities

- _<kbd>Alt X</kbd>: Execute a command by name_
  - _<kbd>Alt X customize</kbd>: Customize Emacs behavior_
  - _<kbd>Alt X list-packages</kbd>: Find and install extensions_
- _<kbd>Alt !</kbd>: Execute a shell command_
- _<kbd>Alt :</kbd>: Evaluate an Emacs Lisp expression_

### Interactive development

- <kbd>Ctrl E</kbd>: Evaluate expression before cursor
- <kbd>Alt E</kbd>: Evaluate file

## Recommendations

### Change <kbd>Caps Lock</kbd> to <kbd>Ctrl</kbd>

Remap your <kbd>Caps Lock</kbd> key to <kbd>Ctrl</kbd>.  This may seem like a
weird and hard thing to do, but it isn't, and you will find it is much more
convenient to have 2-3 <kbd>Ctrl</kbd> keys than to have a key exclusively for
shouting at people on the Internet.  On many keyboards <kbd>Caps Lock</kbd> is
also relatively large, making it easier to press.  See [here][MovingTheCtrlKey]
for instructions.

[MovingTheCtrlKey]: http://emacswiki.org/emacs/MovingTheCtrlKey

### Enable `ido-mode`

`ido-mode` lets you quickly browse open files and folders.  This package's
shortcuts are also supported in `ido-mode`.  Simply add the following to your
`~/.emacs` file to enable it:

```lisp
(ido-mode 1)
```
