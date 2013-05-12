;; User pack init file
;;
;; User this file to initiate the pack configuration.
;; See README for more information.

;; Load bindings config
(live-load-config-file "bindings.el")

(setenv "EXPECTATIONS_COLORIZE" "false")
(setenv "EXPECTATIONS_SHOW_RAW" "false")

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(partial\\)[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "Ƥ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(comp\\)[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∘")
                               nil))))))


(defun fb ()
  "format buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-git-gutter-mode -1)
(global-linum-mode)

(setq-default truncate-lines t)
(setq-default buffer-for-last-expectations-run nil)
(setq-default last-run-grep nil)
(setq-default live-disable-zone t)

(add-to-list 'load-path "~/.emacs.d/local/fuzzy-find-in-project")
(require 'fuzzy-find-in-project)
(defalias 'ffip 'fuzzy-find-in-project)

(dolist (x '(scheme emacs-lisp lisp clojure))
  (remove-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-filter-apply)

;; TODO - assumes install location for unplugged-pack - should figure out where we are instead
(let* ((local-user-settings (concat "~/.emacs.d/local/unplugged-pack/" (user-login-name) ".el")))
  (if (and (file-exists-p local-user-settings) (not live-safe-modep))
      (live-load-config-file local-user-settings)))

(add-to-list 'load-path "~/.emacs.d/local/expectations-mode/")
(add-hook 'nrepl-connected-hook 'bury-buffer)

(require 'expectations-mode)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-M-.") 'er/contract-region)

(defun run-expectations ()
  (interactive)
  (shell-command "EXPECTATIONS_COLORIZE=false lein expectations"))

(defun char-at-point ()
  (interactive)
  (buffer-substring-no-properties (point) (+ 1 (point))))

(defun clj-string-name (s)
  (substring s 1 -1))

(defun clj-keyword-name (s)
  (substring s 1))

(defun delete-and-extract-sexp ()
  (let* ((begin (point)))
    (forward-sexp)
    (let* ((result (buffer-substring-no-properties begin (point))))
      (delete-region begin (point))
      result)))

(defun toggle-clj-keyword-string ()
  (interactive)
  (save-excursion
    (if (equal 1 (point))
        (message "beginning of file reached, this was probably a mistake.")
      (cond ((equal "\"" (char-at-point)) (insert ":" (clj-string-name (delete-and-extract-sexp))))
            ((equal ":" (char-at-point)) (insert "\"" (clj-keyword-name (delete-and-extract-sexp)) "\""))
            (t (progn
                 (backward-char)
                 (toggle-clj-keyword-string)))))))

(global-set-key (kbd "C-:") 'toggle-clj-keyword-string)

(defun find-java-src ()
  (interactive)
  (er/mark-word)
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj"))
         (the-str (buffer-substring-no-properties (region-beginning) (region-end))))
    (if project-root
        (progn
          (grep-string-in the-str
                          (concat project-root "lib/sources"))
          (switch-to-grep)
          (sit-for 0.25)
          (search-forward (concat (expand-file-name project-root) "lib/sources/"))
          (compile-goto-error)
          (let* ((current-point (point)))
            (search-forward-regexp ".*\.jar")
            (switch-to-buffer (buffer-substring-no-properties current-point (point))))
          (search-forward the-str)
          (archive-extract))
      (message (concat "no project.clj found at or below " (buffer-file-name))))))


(global-set-key (kbd "C-c . j") 'find-java-src)

(defun run-expectations-for-source ()
  (interactive)
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj")))
    (if project-root
        (let* ((full-path-with-clojure (test-full-path project-root "test/expectations"))
               (b (current-buffer)))
          (if (file-exists-p full-path-with-clojure)
              (progn
                (switch-to-buffer (find-file-noselect full-path-with-clojure))
                (setq buffer-for-last-expectations-run (current-buffer))
                (nrepl-load-current-buffer)
                (expectations-run-tests)
                (switch-to-buffer b))
            (message (concat "could not find " full-path-with-clojure))))
      (message (concat "no project.clj found at or below " (buffer-file-name))))))

(defun run-expectations-in-buffer (b)
  (setq buffer-for-last-expectations-run b)
  (expectations-run-tests))

(defalias 'll "ls -l $*")

(defun rerun-last-run-expectations ()
  (interactive)
  (let* ((b (current-buffer)))
          (if buffer-for-last-expectations-run
              (progn
                (switch-to-buffer buffer-for-last-expectations-run)
                (nrepl-load-current-buffer)
                (expectations-run-tests)
                (switch-to-buffer b))
            (message (concat "could not find a previous test run")))))

(defun run-expectations-for-file ()
  (interactive)
  (if expectations-mode
      (run-expectations-in-buffer (current-buffer))
    (run-expectations-for-source)))

(global-set-key (kbd "C-c C-,") 'run-expectations-for-file)
(global-set-key (kbd "C-c ,") 'run-expectations-for-file)
(global-set-key (kbd "C-c C-M-,") 'rerun-last-run-expectations)

(defun duplicate-line ()
  (interactive)
  (let* ((cursor-column (current-column)))
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)
    (move-to-column cursor-column)))

(global-set-key (kbd "C-c d") 'duplicate-line)

(defun test-full-path (project-root test-home)
  (concat
   (replace-regexp-in-string
    (concat (expand-file-name project-root) "src/clojure")
    (concat project-root test-home)
    (file-name-sans-extension (buffer-file-name)))
   "_expectations.clj"))

(defun path-to-ns (project-root path)
  (replace-in-string
   (replace-in-string
    (replace-in-string path (concat project-root "test/expectations/") "")
    "_" "-")
   "/" "."))

(defun src-full-path (project-root file-name)
  (concat
   (replace-regexp-in-string
    (concat (expand-file-name project-root) "test/expectations")
    (concat project-root "src/clojure")
    (file-name-directory file-name))
   (replace-regexp-in-string
    "_expectations"
    ""
    (file-name-nondirectory file-name))))

(defun find-expectations ()
  (interactive)
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj")))
    (if project-root
        (let* ((full-path-with-clojure (test-full-path project-root "test/expectations")))
          (if (file-exists-p full-path-with-clojure)
              (set-window-buffer (next-window) (find-file-noselect full-path-with-clojure))
            (when (y-or-n-p (concat "no expectations found. create " full-path-with-clojure "?"))
              (let* ((b (find-file-noselect full-path-with-clojure))
                     (ns (path-to-ns project-root (file-name-sans-extension full-path-with-clojure))))
                (set-window-buffer (next-window) b)
                (switch-to-buffer-other-window b)
                (insert "(ns expectations." ns)
                (reindent-then-newline-and-indent)
                (insert "(:use expectations ")
                (insert (replace-regexp-in-string  "\-expectations$" "" ns))
                (insert "))")
                (expectations-mode)
                (save-buffer)))))
      (message (concat "no project.clj found at or below " (buffer-file-name))))))

(defun find-src ()
  (interactive)
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj")))
    (if project-root
        (let* ((full-path (src-full-path project-root (buffer-file-name))))
          (if (file-exists-p full-path)
              (set-window-buffer (next-window) (find-file-noselect full-path))
            (message (concat "cound not find " full-path))))
      (message (concat "no project.clj found at or below " (buffer-file-name))))))

(defun toggle-expectations-and-src ()
  (interactive)
  (if expectations-mode
      (find-src)
    (find-expectations)))

(global-set-key (kbd "C-c x") 'toggle-expectations-and-src)

(defun clojure-comment-sexp ()
  (interactive)
  (er/mark-clj-word)
  (paredit-wrap-sexp)
  (insert "comment ")
  (fb))

(defun clojure-comment-first-sexp-on-current-line ()
  (interactive)
  (move-beginning-of-line 1)
  (clojure-comment-sexp))

(global-set-key (kbd "C-;") 'clojure-comment-sexp)
(global-set-key (kbd "C-M-;") 'clojure-comment-first-sexp-on-current-line)

(defun switch-project (project-root)
  (interactive (list (read-directory-name "Project Root: " (locate-dominating-file default-directory "project.clj"))))
  (switch-to-buffer "*nrepl-server*")
  (set-buffer-modified-p nil)
  (nrepl-quit)
  (when (equal current-prefix-arg nil)
    (mapc 'kill-buffer (buffer-list)))
  (cd project-root)
  (nrepl-jack-in)
  (switch-to-buffer "*nrepl-server*")
  (clojure-mode)
  (make-directory "~/tmp/emacs" t)
  (let ((fname "~/tmp/emacs/*nrepl-server*"))
    (when (file-exists-p fname)
      (delete-file fname))
    (write-file fname))
  (bury-buffer))

(global-set-key (kbd "C-c s p") 'switch-project)

(defun start-server ()
  (interactive)
  (nrepl-load-current-buffer)
  (nrepl-interactive-eval (nrepl-last-expression))
  (console-layout))

(global-set-key (kbd "C-c C-x C-e") 'start-server)

(defun grep-string-in (s project-root)
  (interactive (list (read-string "string: ")
                     (read-directory-name
                      "Project Root: "
                      (locate-dominating-file default-directory "project.clj"))))
  (message (concat s project-root))
  (let* ((cmd (concat "grep -nH -e \"" s  "\" -R --exclude-dir=\"target\" --exclude-dir=\".git\" " project-root)))
    (setq last-run-grep cmd)
    (grep cmd)))

(defun grep-string-in-project (s)
  (interactive "Mstring: ")
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj")))
    (if project-root
        (grep-string-in s project-root)
      (message (concat "no project.clj found at or below " (buffer-file-name))))))

(defun grep-in (project-root)
  (interactive (list (read-directory-name "Project Root: " (locate-dominating-file default-directory "project.clj"))))
  (er/mark-clj-word)
  (grep-string-in (buffer-substring-no-properties (region-beginning) (region-end)) project-root))

(defun grep-in-project ()
  (interactive)
  (er/mark-clj-word)
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj")))
    (if project-root
        (grep-string-in (buffer-substring-no-properties (region-beginning) (region-end)) project-root)
      (message (concat "no project.clj found at or below " (buffer-file-name))))))

(defun rerun-last-grep ()
  (interactive)
  (if last-run-grep
      (grep last-run-grep)))

(global-set-key (kbd "C-c g p") 'grep-in-project)
(global-set-key (kbd "C-c g i") 'grep-in)
(global-set-key (kbd "C-c g s p") 'grep-string-in-project)
(global-set-key (kbd "C-c g s i") 'grep-string-in)
(global-set-key (kbd "C-c M-g") 'rerun-last-grep)

(defun default-window-layout ()
  (interactive)
  (let* ((w1 (get-buffer-window (current-buffer)))
         (w2 (next-window))
         (b2 (window-buffer w2))
         (s2 (window-start w2)))
    (delete-other-windows)
    (split-window-right)
    (set-window-buffer (second (window-list)) b2)
    (set-window-start (second (window-list)) s2)))

(global-set-key (kbd "C-c w l d") 'default-window-layout)

(defun console-layout ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (win-switch-dispatch)
  (switch-to-buffer "*nrepl-server*")
  (split-window-vertically)
  (win-switch-dispatch)
  (switch-to-buffer "*nrepl*")
  (win-switch-dispatch))

(global-set-key (kbd "C-c w l c") 'console-layout)

(defun clear-nrepl-server-output ()
  (interactive)
  (save-window-excursion
    (switch-to-buffer "*nrepl-server*")
    (mark-whole-buffer)
    (delete-region (region-beginning) (region-end))))

(global-set-key (kbd "C-c w c n s") 'clear-nrepl-server-output)

(global-auto-revert-mode 1)

(defun switch-to-grep ()
  (interactive)
  (switch-to-buffer-other-window "*grep*"))

(defun switch-to-shell ()
  (interactive)
  (if (get-buffer "*eshell*")
      (switch-to-buffer-other-window "*eshell*")
    (eshell)))

(defun switch-to-repl ()
  (interactive)
  (switch-to-buffer-other-window "*nrepl*"))

(global-set-key (kbd "C-c s s") 'switch-to-shell)
(global-set-key (kbd "C-c s g") 'switch-to-grep)
(global-set-key (kbd "C-c s r") 'switch-to-repl)

(defun join-next-line ()
  (interactive)
  (next-line)
  (join-line))

(global-set-key (kbd "C-S-j") 'join-next-line)

(defun create-clj-function ()
  (interactive)
  (when (not (region-active-p))
    (er/mark-clj-word))
  (copy-region-as-kill (region-beginning) (region-end))
  (live-paredit-previous-top-level-form)
  (insert "(defn ")
  (yank)
  (insert " [")
  (save-excursion
    (insert "])\n\n")))

(global-set-key (kbd "C-c r c f") 'create-clj-function)

(defun extract-let (var-name)
  (interactive "Mvar name: ")
  (when (not (region-active-p))
    (er/mark-outside-pairs))
  (kill-region (region-beginning) (region-end))
  (insert var-name)
  (er/mark-outside-pairs)
  (paredit-wrap-sexp)
  (insert "let [" var-name " ")
  (yank)
  (insert "]")
  (reindent-then-newline-and-indent))

(global-set-key (kbd "C-c r e l") 'extract-let)

(defun inline-let-var ()
  (interactive)
  (when (not (region-active-p))
    (er/mark-clj-word))
  (let* ((var-name (buffer-substring-no-properties (region-beginning) (region-end))))
    (forward-sexp)
    (er/mark-outside-pairs)
    (let* ((var-form (buffer-substring-no-properties (region-beginning) (region-end))))
      (kill-region (region-beginning) (region-end))
      (live-paredit-backward-kill-sexp)
      (er/mark-outside-pairs)
      (er/mark-outside-pairs)
      (query-replace var-name var-form))))
