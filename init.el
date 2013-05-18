;; See README for more information.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; override emacs-live defaults ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-linum-mode) ;;; turn on line numbers
(global-git-gutter-mode -1) ;;; turn off git gutter, hides line numbers
(global-auto-revert-mode 1) ;;; allow git pulls/reverts to easily update buffers

(setq-default truncate-lines t) ;;; don't break lines automatically
(setq-default live-disable-zone t) ;;; this does not work well over ssh

(add-hook 'nrepl-connected-hook 'bury-buffer) ;;; don't send me to the repl on connect

(dolist (x '(scheme emacs-lisp lisp clojure)) ;;; disable rainbow-delimiters
  (remove-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

(defun load-current-buffer-to-all-nrepls ()
  (interactive)
  (let ((default-connection (nrepl-current-connection-buffer)))
    (dolist (x nrepl-connection-list)
      (nrepl-make-repl-connection-default x)
      (nrepl-load-current-buffer))
    (nrepl-make-repl-connection-default default-connection)))

(define-key clojure-mode-map (kbd "C-c C-k") 'load-current-buffer-to-all-nrepls)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custom clojure font lock ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load user specific settings ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((local-user-settings (concat "~/.emacs.d/local/unplugged-pack/" (user-login-name) ".el")))
  (if (and (file-exists-p local-user-settings) (not live-safe-modep))
      (live-load-config-file local-user-settings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load commonly used modes ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/local/expectations-mode/")
(require 'expectations-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define environment vars ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "EXPECTATIONS_COLORIZE" "false")
(setenv "EXPECTATIONS_SHOW_RAW" "false")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; define keystrokes for commonly used actions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-M-.") 'er/contract-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expectations common and enhanced tasks ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-expectations ()
  (interactive)
  (shell-command "EXPECTATIONS_COLORIZE=false lein expectations"))

(defun run-expectations-for-source ()
  (interactive)
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj")))
    (if project-root
        (let* ((full-path-with-clojure (test-full-path project-root "test/expectations"))
               (b (current-buffer)))
          (if (file-exists-p full-path-with-clojure)
              (progn
                (switch-to-buffer (find-file-noselect full-path-with-clojure))
                (load-current-buffer-to-all-nrepls)
                (expectations-run-tests)
                (switch-to-buffer b))
            (message (concat "could not find " full-path-with-clojure))))
      (message (concat "no project.clj found at or below " (buffer-file-name))))))

(defun run-expectations-for-file ()
  (interactive)
  (let ((default-connection (nrepl-current-connection-buffer)))
    (when (get-buffer "*nrepl*<2>")
      (nrepl-make-repl-connection-default (get-buffer "*nrepl-connection*<2>")))
    (if expectations-mode
        (expectations-run-tests)
      (run-expectations-for-source))
    (nrepl-make-repl-connection-default default-connection)))

(global-set-key (kbd "C-c C-,") 'run-expectations-for-file)
(global-set-key (kbd "C-c ,") 'run-expectations-for-file)

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

(defun expectations-repl (project-root)
  (interactive (list (read-directory-name "Project Root: " (locate-dominating-file default-directory "project.clj"))))
  (when (get-buffer "*nrepl-connection*<2>")
    (nrepl-close (get-buffer "*nrepl-connection*<2>")))
  (cd project-root)
  (nrepl-jack-in))

;;;;;;;;;;;;;;;;;;;;;;;
;;; java helper fns ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun find-java-src ()
  (interactive)
  (er/mark-word)
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj"))
         (the-str (buffer-substring-no-properties (region-beginning) (region-end))))
    (if project-root
        (progn
          (grep-string-in the-str (concat project-root "lib/sources"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; clojure project fns ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (load-current-buffer-to-all-nrepls)
  (nrepl-interactive-eval (nrepl-last-expression))
  (console-layout))

(global-set-key (kbd "C-c C-x C-e") 'start-server)

;;;;;;;;;;;;;;;;;;;;;;;;
;;; layout shortcuts ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; line manipulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-buffer ()
  "format buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(global-set-key (kbd "C-S-f") 'format-buffer)

(defun join-next-line ()
  (interactive)
  (next-line)
  (join-line))

(global-set-key (kbd "C-S-j") 'join-next-line)

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

(global-set-key (kbd "C-S-d") 'duplicate-line)

(defun clojure-comment-sexp ()
  (interactive)
  (er/mark-clj-word)
  (paredit-wrap-sexp)
  (insert "comment ")
  (fb))

(global-set-key (kbd "C-;") 'clojure-comment-sexp)

(defun clojure-comment-first-sexp-on-current-line ()
  (interactive)
  (move-beginning-of-line 1)
  (clojure-comment-sexp))

(global-set-key (kbd "C-M-;") 'clojure-comment-first-sexp-on-current-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; grep'ing in a project ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default last-run-grep nil)

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

(defun switch-to-grep ()
  (interactive)
  (switch-to-buffer-other-window "*grep*"))

(global-set-key (kbd "C-c g p") 'grep-in-project)
(global-set-key (kbd "C-c g i") 'grep-in)
(global-set-key (kbd "C-c g s p") 'grep-string-in-project)
(global-set-key (kbd "C-c g s i") 'grep-string-in)
(global-set-key (kbd "C-c M-g") 'rerun-last-grep)
(global-set-key (kbd "C-c s g") 'switch-to-grep)
