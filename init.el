;; See README for more information.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; override emacs-live defaults ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-linum-mode) ;;; turn on line numbers
(global-git-gutter-mode -1) ;;; turn off git gutter, hides line numbers
(global-auto-revert-mode 1) ;;; allow git pulls/reverts to easily update buffers
(put 'narrow-to-region 'disabled nil) ;; allow me to narrow-to-region

(setq-default truncate-lines t) ;;; don't break lines automatically
(setq-default live-disable-zone t) ;;; this does not work well over ssh

(setq cider-repl-pop-to-buffer-on-connect nil) ;;; don't send me to the repl on connect
(add-hook 'nrepl-connected-hook 'reset-nrepl-connection-to-default) ;;; always default to first connection
(add-hook 'nrepl-connected-hook 'rename-repl-buffers-connection) ;;; always default to first connection
(add-hook 'clojure-mode-hook 'cider-mode)

(setq-default fill-column 90) ;;; I like my right margin at 90
(setq default-nrepl-server "*default nrepl-server*")
(setq default-nrepl-connection "*default nrepl-connection*")
(setq expectations-nrepl-server "*expectations nrepl-server*")
(setq expectations-nrepl-connection "*expectations nrepl-connection*")

;; Develop in unplugged-pack snippets dir
;; use snippets from emacs-live
(setq yas/root-directory '("~/.emacs.d/local/unplugged-pack/snippets"
                           "~/.emacs.d/etc/snippets"))

;; Map `yas/load-directory' to every element
(mapc 'yas/load-directory yas/root-directory)

(dolist (x '(scheme emacs-lisp lisp clojure)) ;;; disable rainbow-delimiters
  (remove-hook (intern (concat (symbol-name x) "-mode-hook")) 'rainbow-delimiters-mode))

(defun reset-nrepl-connection-to-default ()
  (message "resetting nrepl-connection to %s" default-nrepl-connection)
  (if (get-buffer default-nrepl-connection)
      (nrepl-make-repl-connection-default (get-buffer default-nrepl-connection))
    (message "*** PROBABLE ERROR *** buffer %s could not be found" default-nrepl-connection)))

;; (nrepl-current-connection-buffer)

(defun load-current-buffer-to-all-nrepls ()
  (interactive)
  (dolist (x nrepl-connection-list)
    (message "Loading to conn %s" x)
    (nrepl-make-repl-connection-default (get-buffer x))
    (cider-load-current-buffer))
  (reset-nrepl-connection-to-default))

(define-key cider-mode-map (kbd "C-c C-k") 'load-current-buffer-to-all-nrepls)

(define-clojure-indent
  (cond-> 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; things we used to define ourselves, now in clj-refactor. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key clojure-mode-map (kbd "C-:") 'cljr-cycle-stringlike)
(define-key clojure-mode-map (kbd "C->") 'cljr-cycle-coll)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; custom clojure font lock and key chording ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; key chords
(key-chord-define clojure-mode-map "KL"  "-> ")
(key-chord-define clojure-mode-map "KK"  "{:keys [")
(key-chord-define clojure-mode-map "KP"  "publish-fn")
(key-chord-define clojure-mode-map "PP"  "product")
(key-chord-define clojure-mode-map "SS"  "size")
(key-chord-define clojure-mode-map "Ss"  "side")
(key-chord-define clojure-mode-map "II"  "instrument-id")
(key-chord-define clojure-mode-map "Ii"  "instrument")
(key-chord-define clojure-mode-map "BB"  "bid")
(key-chord-define clojure-mode-map "AA"  "ask")
(key-chord-define clojure-mode-map "TT"  "term")
(key-chord-define clojure-mode-map "CC"  "contract")
(key-chord-define clojure-mode-map "EE"  "exchange")

(key-chord-define clojure-mode-map "ZJ"  "a-fn1")
(key-chord-define clojure-mode-map "ZK"  "a-fn2")
(key-chord-define clojure-mode-map "ZL"  "a-fn3")
(key-chord-define clojure-mode-map "ZI"  "interaction")
(key-chord-define clojure-mode-map "ZN"  "no-op")


;;; font-lock
(dolist (x '((true        т)
             (false       ғ)
             (:keys       ӄ)
             (:strs       ş)
             (nil         Ø)
             (partial     ∂)
             (with-redefs я)
             (comp        º)
             (apply       ζ)
             (a-fn1       α)
             (a-fn2       β)
             (a-fn3       γ)
             (no-op       ε)))
  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `((,(concat "[\[({[:space:]]"
                                "\\(" (symbol-name (first x)) "\\)"
                                "[\])}[:space:]]")
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) ,(symbol-name (second x)))
                                 nil))))))
  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `((,(concat "^"
                                "\\(" (symbol-name (first x)) "\\)"
                                "[\])}[:space:]]")
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) ,(symbol-name (second x)))
                                 nil))))))
  (eval-after-load 'clojure-mode
    '(font-lock-add-keywords
      'clojure-mode `((,(concat "[\[({[:space:]]"
                                "\\(" (symbol-name (first x)) "\\)"
                                "$")
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) ,(symbol-name (second x)))
                                 nil)))))))

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
;; (add-to-list 'load-path "~/.emacs.d/local/Fill-Column-Indicator/")
;; (require 'fill-column-indicator)

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
(global-set-key (kbd "C-S-k") 'live-paredit-forward-kill-sexp)
(global-set-key (kbd "C-c C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; expectations common and enhanced tasks ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-clojure-indent
  (expect 'defun)
  (expect-let 'defun)
  (given 'defun)
  (side-effects 1)
  (context 1)
  (freeze-time 1)
  (redef-state 1)
  (from-each 1))

(defun run-expectations ()
  (interactive)
  (shell-command "EXPECTATIONS_COLORIZE=false lein expectations"))

(defun expectations-run-tests-synch ()
  (interactive)
  (expectations-run-tests t))

(defun run-isolated-expectation ()
  (interactive)
  (save-some-buffers nil (lambda () (equal major-mode 'clojure-mode)))
  (save-window-excursion
    (save-excursion
      (when (clojure-find-ns)
        (goto-char (match-beginning 0))
		(cider-eval-defun-at-point))))
  (save-window-excursion
    (save-excursion
      (save-restriction
        (live-paredit-previous-top-level-form)
        (let (pos1 pos2)
          (setq pos1 (point))
          (end-of-sexp)
          (setq pos2 (point))
          (cua-set-mark)
          (narrow-to-region pos1 pos2)
          (expectations-run-tests t)
		  )))))

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

(defun run-expectations-for-file (&optional prefix)
  (interactive "P")
  (when (get-buffer expectations-nrepl-connection)
    (nrepl-make-repl-connection-default (get-buffer expectations-nrepl-connection)))
  (if expectations-mode
      (if prefix
          (run-isolated-expectation)
        (expectations-run-tests))
    (run-expectations-for-source))
  (reset-nrepl-connection-to-default))

(define-key clojure-mode-map (kbd "C-c C-,") 'run-expectations-for-file)
(define-key clojure-mode-map (kbd "C-c ,") 'run-expectations-for-file)
(define-key expectations-mode-map (kbd "C-c C-,") 'run-expectations-for-file)
(define-key expectations-mode-map (kbd "C-c ,") 'run-expectations-for-file)

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


(defun rename-buffers-on-connected (conn-name server-name)
  (message "renaming %s -> %s, %s -> %s"
           nrepl-server-buffer server-name nrepl-connection-buffer conn-name)
  (switch-to-buffer nrepl-connection-buffer)
  (rename-buffer conn-name)
  (switch-to-buffer nrepl-server-buffer)
  (rename-buffer server-name)
  (clojure-mode)
  (make-directory "~/tmp/emacs/" t)
  (let ((fname (concat "~/tmp/emacs/" server-name)))
    (when (file-exists-p fname)
      (delete-file fname))
    (write-file fname))
  (bury-buffer))

(defun rename-repl-buffers-connection ()
  ;; (message "nrepl-session %s" nrepl-session)
  ;; (message "nrepl-connection-buffer %s" nrepl-connection-buffer)
  ;; (message "nrepl-server-buffer %s" nrepl-server-buffer)
  ;; (message "connection-list length: %s" (length nrepl-connection-list))
  ;; (message "connection-list %s" nrepl-connection-list)
  ;; (message "buffer list %s" (buffer-list))
  ;; (message "current repl buffer %s" (cider-current-repl-buffer))
  (when (eq 0 (length nrepl-connection-list))
    (save-window-excursion
      (rename-buffers-on-connected expectations-nrepl-connection expectations-nrepl-server))
    (message "default repl connected"))
  (when (eq 1 (length nrepl-connection-list))
    (save-window-excursion
      (rename-buffers-on-connected default-nrepl-connection default-nrepl-server))
    (message "expectation repl connected"))
  (reset-nrepl-connection-to-default))

;;;;;;;;;;;;;;;;;;;;;;;
;;; java helper fns ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun find-java-src ()
  (interactive)
  (er/mark-word)
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj"))
         (the-str (concat (buffer-substring-no-properties (region-beginning) (region-end)) ".java")))
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


(defun cider-force-quit ()
  "Quit CIDER without a prompt"
  (interactive)
  (dolist (connection nrepl-connection-list)
  	(when connection
    (nrepl-close connection)))
    (message "All active nREPL connections were closed")
    (cider-close-ancilliary-buffers))

(defun mark-buffer-umodified (buffer)
	(when (get-buffer buffer)
		(switch-to-buffer buffer)
			(set-buffer-modified-p nil)))

(defun switch-repl (project-root)
  (cd project-root)
  (cider-jack-in))

(defun switch-expectations-repl (project-root)
  (message "Starting expectations repl...")
  (remove-hook 'nrepl-connected-hook (first nrepl-connected-hook))
  (switch-repl project-root))

(defun load-project-env (project-root)
  (let ((project-env (format "%s/project.el" project-root)))
    (when (file-exists-p project-env)
      (load-file project-env))))

(defun switch-project (project-root)
  (interactive (list (ido-read-directory-name "Project Root: " (locate-dominating-file default-directory "project.clj"))))
  (let ((project-name (file-name-nondirectory (directory-file-name project-root))))
    (dolist (x (find-buffers "nrepl-server*"))
      (mark-buffer-umodified x))
    (cider-force-quit)
    (load-project-env project-root)
    (when (equal current-prefix-arg nil)
      (mapc 'kill-buffer (buffer-list)))
    (let ((fn (apply-partially #'switch-expectations-repl project-root)))
      (add-hook 'nrepl-connected-hook fn)
      (switch-repl project-root))))

(global-set-key (kbd "C-c s p") 'switch-project)

(defun update-repl-known-hosts ()
  (interactive)
  (setq cider-known-endpoints
        (eval (read (shell-command-to-string
                     "lein exec ~/.emacs.d/local/unplugged-pack/convert_repl_data_to_string.clj")))))

(defun cider-repl-for-connection (conn-buffer)
  (buffer-local-value 'nrepl-repl-buffer
                      (get-buffer conn-buffer)))

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
  (switch-to-buffer default-nrepl-server)
  (split-window-vertically)
  (win-switch-dispatch)
  (switch-to-buffer (cider-current-repl-buffer))
  (win-switch-dispatch))

(global-set-key (kbd "C-c w l c") 'console-layout)

(defun linux-layout ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (win-switch-dispatch)
  (win-switch-dispatch)
  (switch-to-buffer default-nrepl-server)
  (split-window-vertically)
  (win-switch-dispatch)
  (switch-to-buffer (cider-current-repl-buffer))
  (win-switch-dispatch)
  (balance-windows))

(global-set-key (kbd "C-c w l l") 'linux-layout)

(defun visible-window (wlist)
  (if (equal 1 (length wlist))
      nil
    (if (get-buffer-window (first wlist))
        wlist
      (visible-window (cdr wlist)))))

(defun toggle-window-from-list (l)
  (when l
    (select-window (get-buffer-window (first l)))
    (switch-to-buffer (second l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; buffer manipulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun move-buffer-to-other-window ()
  (interactive)
  (let* ((w1 (get-buffer-window (current-buffer)))
         (w2 (next-window)))
    (set-window-buffer w2 (current-buffer))
    (set-window-start w2 (window-start w1)))
  (previous-buffer))

(global-set-key (kbd "C-c w .") 'move-buffer-to-other-window)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun toggle-repl-buffers ()
  (interactive)
  (let* ((w1 (get-buffer-window (current-buffer)))
         (default-repl (cider-repl-for-connection default-nrepl-connection))
         (expectations-repl (cider-repl-for-connection expectations-nrepl-connection)))
    (toggle-window-from-list (visible-window (list default-repl
                                                   expectations-repl
                                                   default-repl)))
    (toggle-window-from-list (visible-window (list default-nrepl-server
                                                   expectations-nrepl-server
                                                   default-nrepl-server)))
    (select-window w1)))

(global-set-key (kbd "C-c w t r") 'toggle-repl-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; line manipulation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun format-buffer ()
  "format buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (window-start) (window-end))
      (beginning-of-buffer)
      (forward-paragraph)
      (narrow-to-region (point) (point-max))
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max) nil)
      (untabify (point-min) (point-max)))))

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

(defun remove-commas ()
  (interactive)
  (save-excursion
    (let (pos1 pos2)
      (if (region-active-p)
          (progn
            (setq pos1 (region-beginning) pos2 (region-end))
            (replace-string "," "" nil pos1 pos2))
        (replace-string "," "" nil (point-min) (point-max))))))

(global-set-key (kbd "C-<") 'remove-commas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; grep'ing in a project ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun grep-string-in (s project-root)
  (interactive (list (read-string "string: ")
                     (ido-read-directory-name
                      "Project Root: "
                      (locate-dominating-file default-directory "project.clj"))))
  (message (concat s project-root))
  (let* ((cmd (concat "grep -nH -e \"" s  "\" -R --exclude-dir=\"target\" --exclude-dir=\".git\" " project-root)))
    (grep cmd)))

(defun grep-string-in-project (s)
  (interactive "Mstring: ")
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj")))
    (if project-root
        (grep-string-in s project-root)
      (message (concat "no project.clj found at or below " (buffer-file-name))))))

(defun grep-in (project-root)
  (interactive (list (ido-read-directory-name "Project Root: " (locate-dominating-file default-directory "project.clj"))))
  (let (pos1 pos2 bds)
    (if (not (region-active-p))
        (er/mark-clj-word))
    (setq pos1 (region-beginning) pos2 (region-end))
    (grep-string-in (buffer-substring-no-properties pos1 pos2) project-root)))

(defun grep-in-project ()
  (interactive)
  (let* ((project-root (locate-dominating-file (file-name-directory (buffer-file-name)) "project.clj")))
    (if project-root
        (grep-in project-root)
      (message (concat "no project.clj found at or below " (buffer-file-name))))))

(defun find-buffer (buffer)
  (first (find-buffers buffer)))

(defun find-buffers (buffer)
	(filter (lambda (b) (string-match buffer b)) (live-list-buffer-names)))

(defun filter (condp lst)
      (delq nil
            (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(global-set-key (kbd "C-c g p") 'grep-in-project)
(global-set-key (kbd "C-c g i") 'grep-in)
(global-set-key (kbd "C-c g s p") 'grep-string-in-project)
(global-set-key (kbd "C-c g s i") 'grep-string-in)

(if (file-exists-p "/tmp/emacs/repl_data.edn")
    (update-repl-known-hosts))
