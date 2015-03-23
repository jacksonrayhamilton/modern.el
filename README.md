# modern.el

Emacs can be challenging to use out-of-the-box due to many of its unusual
default settings and keyboard shortcuts.  This package enables numerous
"standard" text editor features, and rebinds many commonly-used commands to keys
that are either familiar or more ergonomic.

The goal of this package is make new and experienced Emacs users more productive
without imposing too many opinionated features on them.

## Keys:

Items in italics are default Emacs shortcuts, and are mentioned for the sake of
beginners.  All other shortcuts are enabled by this package, and their defaults
(like <kbd>Alt W</kbd> for "copy") are disabled.

### Emacs essentials:
- _<kbd>F1</kbd>: Help_
- _<kbd>Ctrl G</kbd>: Cancel_

### Basic text manipulation:
- <kbd>Ctrl Z</kbd>: Undo
- <kbd>Ctrl C</kbd>: Copy
- <kbd>Ctrl X</kbd>: Cut
- <kbd>Ctrl V</kbd>: Paste
- <kbd>Ctrl A</kbd>: Select all

### Files:
- <kbd>Ctrl O</kbd>: Open / create a file
- <kbd>Ctrl B</kbd>: Switch to another open file
- <kbd>Ctrl S</kbd>: Save
- <kbd>Ctrl Alt S</kbd>: Save as
- <kbd>Ctrl W</kbd>: Close
- <kbd>Ctrl /</kbd>: View directory

### Searching:
- <kbd>Ctrl F</kbd>: Find
- <kbd>Alt F</kbd>: Find by regular expression
- <kbd>Ctrl R</kbd>: Find and replace
- <kbd>Alt R</kbd>: Find and replace by regular expression

Tip: Test out regular expressions with <kbd>Alt X re-builder</kbd>.

### Screen sections:
- <kbd>Ctrl 0</kbd>: Hide the focused screen section
- <kbd>Ctrl 1</kbd>: Show only the focused screen section
- <kbd>Ctrl 2</kbd>: Split screen horizontally
- <kbd>Ctrl 3</kbd>: Split screen vertically
- <kbd>Alt B</kbd>: Switch to another section

### Text navigation:
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

### Advanced text manipulation:
- _<kbd>Ctrl D</kbd>: Delete 1 character ahead_
- _<kbd>Alt D</kbd>: Delete 1 word ahead_
- _<kbd>Alt Backspace</kbd>: Delete 1 word behind_
- <kbd>Ctrl Alt D</kbd>: Delete expression
- _<kbd>Ctrl Shift Backspace</kbd>: Delete line_
- _<kbd>Alt Y</kbd>: Cycle through previous copies and pastes (after pressing
  <kbd>Ctrl V</kbd>)_
- _<kbd>Alt ;</kbd>: Insert a comment or comment-out a selection_
- _<kbd>Alt Q</kbd>: Fit text within the 80-character column mark_

### Useful Emacs utilities:
- _<kbd>Alt X</kbd>: Execute a command by name_
  - _<kbd>Alt X customize</kbd>: Customize Emacs behavior_
  - _<kbd>Alt X list-packages</kbd>: Find and install extensions_
- _<kbd>Alt !</kbd>: Execute a shell command_
- _<kbd>Alt :</kbd>: Evaluate an Emacs Lisp expression_

### Interactive development:
- <kbd>Ctrl E</kbd>: Evaluate expression
- <kbd>Alt E</kbd>: Evaluate file
