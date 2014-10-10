(if (eq system-type 'darwin)
    (progn
      (live-set-default-font "Source Code Pro 12")
      (set-variable 'magit-emacsclient-executable "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient")))

(async-shell-command "lein exec ~/.emacs.d/local/unplugged-pack/update_cider_known_hosts.clj")
