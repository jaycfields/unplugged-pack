# unplugged-pack

Shared emacs-live settings from David Chelimsky, John Hume, and Jay Fields

## Install/setup

This assumes you don't have emacs-live set up at all. Adjust to taste.

    gem install fuzzy_file_finder
    cd # to home dir
    mv .emacs-live.el .emacs-live.el.bak # for safe keeping if you have it already
    mv .emacs.d       .emacs.d.bak       # for safe keeping if you have it already
    echo "(live-add-packs '(~/.emacs.d/local/unplugged-pack))" > .emacs-live.el
    git clone https://github.com/overtone/emacs-live.git .emacs.d
    cd .emacs.d
    echo "local/*" >> .git/info/exclude
    mkdir local
    cd local
    git clone https://github.com/jaycfields/unplugged-pack.git
    git clone https://github.com/justinweiss/fuzzy-find-in-project.git
    git clone https://github.com/magnars/expand-region.el.git
    git clone https://github.com/gar3thjon3s/expectations-mode.git


# Orig docs

## User Pack Template

This is a template for your own user (or other purpose) pack.

### init.el

Use the file `init.el` for your own configuration elisp. If this starts
getting unweildy then you might want to break out the config into
separate files which you can store in the config directory.

### config

Files placed in the `config` dir may then be referenced and pulled into
your `init.el` via the fn `live-load-config-file`. For example, if you
have the file config/foo.el then you may load it in with:

    (live-load-config-file "foo.el")

### lib

 If you want to pull in external libraries into your pack, then you
 should place the libraries within the lib dir. To add a directory
 within the pack's lib directory to the Emacs load path (so that it's
 contents are available to require) you can use the fn
 `live-add-pack-lib`. For example, if you have the external library bar
 stored in lib which contains the file `baz.el` which you wish to
 require, this may be achieved by:

    (live-add-pack-lib "bar")
    (require 'baz)

 Have fun!
