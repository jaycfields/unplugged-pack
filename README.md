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
    git clone https://github.com/gar3thjon3s/expectations-mode.git
