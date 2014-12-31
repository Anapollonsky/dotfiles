Dotfiles
========

This git repo keeps track of customized linux configuration files, to enable me to get set up on a new computer quicker. There could potentially be something worth sharing here, but this is primarily private. I'm writing this just to ensure that markdown is straightforward. Also, there's a separate repo for emacs-related stuff [here](https://github.com/Anapollonsky/emacsd).

**test**
*test*

1. hey
2. ho
3. geronimo

setup.sh does a lot of the platform-agnostic legwork (getting the files to where they need to be via simlinks), though installation, certain directory creation, and some other creative work. Also, note that by default, I assume that this entire directory will be cloned to to ~/.dotfiles, and by default some scripts assume that this entire directory is there (the path to LS_COLORS is hardcoded, for instance). May end up changing some of that.