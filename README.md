# modern.el

Emacs can be challenging to use out-of-the-box due to many of its unusual
default settings and keyboard shortcuts.  This package enables numerous
"standard" text editor features, and rebinds many commonly-used commands to
keys that are either familiar or more ergonomic.

The goal of this package is make new and experienced Emacs users more
productive without imposing too many opinionated features on them.

Keys:

Basic text manipulation:
- Control C: Copy
- Control X: Cut
- Control V: Paste
- Control A: Select all

Navigation:
- Control H: Back 1 character
- Alt H: Back 1 word
- Control Alt H: Back 1 expression
- Control J: Next line
- Control K: Previous line
- Control L: Forward 1 character
- Alt L: Forward 1 word
- Control Alt L: Forward 1 expression
- Control ,: Beginning of line
- Control .: End of line

Files:
- Control O: Open a file (or create a new one)
- Control B: Switch to another open file
- Control S: Save the current file
- Control W: Close the current file

Searching:
- Control F: Find
- Alt F: Find by regular expression
- Control R: Find and replace
- Alt R: Find and replace by regular expression

Screens:
- Control 0: Hide the focused screen
- Control 1: Only show the focused screen
- Control 2: Split screen horizontally
- Control 3: Split screen vertically

Advanced text manipulation:
- Control T: Delete line
- Alt T: Delete expression

Interactive development:
- Control E: Evaluate expression
- Alt E: Evaluate file

The default keys for the above commands are unbound.  All other Emacs keys
remain the same.  (If you typically used Control H to get help, try F1
instead.)
