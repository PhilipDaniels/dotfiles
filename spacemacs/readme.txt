To install Spacemacs, first delete or backup your ~/.emacs.d folder and then do

  git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

then run 00install.sh in this folder. Start Emacs normally. Many packages will
be downloaded and compiled.

Packages.el
===========
http://spacemacs.org/doc/LAYERS.html#introduction

Loading order is:
  1. layers.el - declare additional layers ???
  2. packages.el - config for packages
  3. funcs.el - all functions in the layer
  4. config.el - layer specific configuration
  5. keybindings.el - general key bindings

Delayed loading: (with-eval-after-load 'helm ...code...)
The code does not have to be quoted because with-eval-after-load is a macro.
However, use-package's config block can be used to achieve exactly the same result.

A local package requires a folder <layer>/local/<package>/ containing the source
code for that package. Spacemacs will add that folder to the load-path for you.


Use-Package
===========
https://github.com/jwiegley/use-package

:init - runs before loading the package
:config - runs after loading the package
:mode - add mode file extensions


TODO
====
Red cursor
Stop prompting to create files
No backups or temp files
