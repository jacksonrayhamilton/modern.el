# modern.el

Emacs can be challenging to use out-of-the-box due to many of its unusual
default settings and keyboard shortcuts.  This package enables numerous
"standard" text editor features, and rebinds many commonly-used commands to keys
that are either familiar or more ergonomic.

The goal of this package is make new and experienced Emacs users more productive
without imposing too many opinionated features on them.

## Keys:

### Basic text manipulation:
- <kbd>Control C</kbd>: Copy
- <kbd>Control X</kbd>: Cut
- <kbd>Control V</kbd>: Paste
- <kbd>Control A</kbd>: Select all

### Navigation:
- <kbd>Control H</kbd>: Back 1 character
- <kbd>Alt H</kbd>: Back 1 word
- <kbd>Control Alt H</kbd>: Back 1 expression
- <kbd>Control J</kbd>: Next line
- <kbd>Control K</kbd>: Previous line
- <kbd>Control L</kbd>: Forward 1 character
- <kbd>Alt L</kbd>: Forward 1 word
- <kbd>Control Alt L</kbd>: Forward 1 expression
- <kbd>Control ,</kbd>: Beginning of line
- <kbd>Control .</kbd>: End of line

### Files:
- <kbd>Control O</kbd>: Open a file (or create a new one)
- <kbd>Control B</kbd>: Switch to another open file
- <kbd>Control S</kbd>: Save the current file
- <kbd>Control W</kbd>: Close the current file

### Searching:
- <kbd>Control F</kbd>: Find
- <kbd>Alt F</kbd>: Find by regular expression
- <kbd>Control R</kbd>: Find and replace
- <kbd>Alt R</kbd>: Find and replace by regular expression

### Screens:
- <kbd>Control 0</kbd>: Hide the focused screen
- <kbd>Control 1</kbd>: Only show the focused screen
- <kbd>Control 2</kbd>: Split screen horizontally
- <kbd>Control 3</kbd>: Split screen vertically

### Advanced text manipulation:
- <kbd>Control Alt D</kbd>: Delete expression

### Interactive development:
- <kbd>Control E</kbd>: Evaluate expression
- <kbd>Alt E</kbd>: Evaluate file

The default keys for the above commands are unbound.  All other Emacs keys
remain the same.  (If you typically used <kbd>Control H</kbd> to get help, try
<kbd>F1</kbd> instead.)
