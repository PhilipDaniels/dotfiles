TODO
====
[ ] Ripgrep
[ ] Markdown mode clobbers M-arrow keys.
[ ] Open current file in su mode
[ ] pd-hydras - exists in here, but fonts and themes hydras are commented out.
    The font hydra in particular is nice, and Spacemacs does not support it
    out of the box (though it does support font size adjustments).
[ ] pd-cpp - not up to Spacemacs spec
[ ] pd-font - not ported. Specify more fonts.
[ ] pd-terminal - not ported.
[ ] Keybindings need work - need hyper key on Linux.
[ ] Test on Windows.
[ ] Do not delete emacs.bak - it is the one I want to keep.
[ ] Repeat last command = Ctrl-y.
[ ] GNU Global.
[ ] Multiple cursors.
[ ] Hydra for search and replace operations?
[ ] Writable dired.
[ ] Write an underline function.
[ ] pd-terminal: Make ansi-term update a variable which contains the cwd
    so that it can be smarter about creating new windows. See comment in
    the file.
[ ] pd-sort-paragraph-dwim: make it ignore any comments at the start of
    the paragraph.
[ ] Spell checking.
[ ] Flymake
[ ] Proper bookmarks.
[ ] C/C++/Rust development environment.



Installation
============
To install Spacemacs, first delete or backup your ~/.emacs.d folder and then do

  git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

then run 00install.sh in this folder. Start Emacs normally. Many packages will
be downloaded and compiled.


packages.el
===========
http://spacemacs.org/doc/LAYERS.html#introduction

Loading order is:
  1. layers.el - declare additional layers ???
  2. packages.el - config for packages
  3. funcs.el - all functions in the layer
  4. config.el - layer specific variables
  5. keybindings.el - general key bindings

Delayed loading: (with-eval-after-load 'helm ...code...)
The code does not have to be quoted because with-eval-after-load is a macro.
However, use-package's config block can be used to achieve exactly the same result.

A local package requires a folder <layer>/local/<package>/ containing the source
code for that package. Spacemacs will add that folder to the load-path for you.
See above link for how to declare a local package.

3 possible functions:

  1. <layer>/pre-init-<package>
  2. <layer>/init-<package> - if you "own" the package you need this to get it installed
  3. <layer>/post-init-<package> - normal place to add your customizations if someone else owns the package

In most cases, 2 should just call use-package, which does the work inside its
:config block. Note that 3 will never run if no other layer installs the package!

packages.el may declare variables.


funcs.el
========
Contains all functions. Should be guarded:

(when (configuration-layer/package-usedp 'my-package)
  (defun .... )
)


Use-Package
===========
https://github.com/jwiegley/use-package

:init - runs before loading the package
:config - runs after loading the package
:mode - add mode file extensions


Cheatlist
=========
Exclude a package: http://spacemacs.org/doc/LAYERS.html#packagesel
  works even if it is used by another layer

Require another layer to be included: in layers.el
  (configuration-layer/declare-layer 'git)

Inject code into the initialization of a package that is owned by another layer,
over and above what can be done with pre-init and post-init:
See http://spacemacs.org/doc/LAYERS.html#use-package-hooks

  (spacemacs|use-package-add-hook helm
    :pre-init
    ;; Code
    :post-init
    ;; Code
    :pre-config
    ;; Code
    :post-config
    ;; Code
    )
