;;Package -- Summary

;;; Commentary:

;;; Code:
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;;(setq url-proxy-services '(("no_proxy" . "work\\.com")
;;                           ("http" . "global.proxy.lucent.com:8000")))

(setq package-enable-at-startup nil)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Package auto-installation
;; ;(defun ensure-package-installed (&rest packages)
;; ;  "Assure every package is installed, ask for installation if it’s not.
;; ;
;; ;Return a list of installed packages or nil for every skipped package."
;; ;  (mapcar
;; ;   (lambda (package)
;; ;     ;; (package-installed-p 'evil)
;; ;     (if (package-installed-p package)
;; ;         nil
;; ;       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
;; ;           (package-install package)
;; ;         package)))
;; ;   packages))

;; ;(ensure-package-installed  'magit 'save-place 'helm 'projectile 'helm-projectile 'speedbar 'uniquify 'undo-tree 'company 'xcscope 'nlinum 'flycheck 'flycheck-tip 'rainbow-delimiters 'ace-jump-mode 'multi-term 'volatile-highlights 'clean-aindent-mode 'zdiff 'golden-ratio 'discover-my-major) 

;; activate installed packages
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general settings
(setq x-select-enable-clipboard t        ;; copy-paste should work ...
      interprogram-paste-function            ;; ...with...
      'x-cut-buffer-or-selection-value)      ;; ...other X clients

(setq search-highlight t                 ;; highlight when searching...
      query-replace-highlight t)             ;; ...and replacing
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no

(setq completion-ignore-case t           ;; ignore case when completing...
      read-file-name-completion-ignore-case t) ;; ...filenames too

(add-to-list 'same-window-buffer-names '*undo-tree*)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)


(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
    echo area. Has no effect if the character before point is not of
    the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))


(defun goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)))


;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (setq show-paren-style 'parenthesis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; (setcar (cdr (assq 'yas/minor-mode mode-minor-alist)) " yas")
;; (setcar (cdr (assq 'undo-tree-mode minor-mode-alist)) " unt")
;; (setcar (cdr (assq 'golden-ratio-mode minor-mode-alist)) " gdr")
;; (setcar (cdr (assq 'emacs-lisp-mode minor-mode-alist)) " els")
;; (setcar (cdr (assq 'company-mode minor-mode-alist)) " com")
;; (setcar (cdr (assq 'slime-mode minor-mode-alist)) " slm")
;; (setcar (cdr (assq 'flycheck-mode minor-mode-alist)) " fly")
;; (setcar (cdr (assq 'drag-mode minor-mode-alist)) " drg")
;; (setcar (cdr (assq 'helm-mode minor-mode-alist)) " hlm")
;; (setcar (cdr (assq 'whitespace-mode minor-mode-alist)) " wsp")
;; (setcar (cdr (assq 'volatile-highlights-mode minor-mode-alist)) " vhl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key-chord
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay .01)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-Specific packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; haskell
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ghc (haskell)
(require 'ghc)
(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CEDET
;;(require 'CEDET)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME
(require 'slime-autoloads)
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit
(require 'paredit)
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)
(add-hook 'emacs-lisp-mode-hook 'slime-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python Stuff
;(require 'elpy)
;(elpy-enable)
(require 'jedi)
(setq jedi:server-command '("~/.emacs.d/elpa/jedi-20140321.1323/jediepcserver.py"))
(add-hook 'python-mode-hook 'jedi:setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic highlighting
(require 'generic-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired+
;; (setq diredp-hide-details-initially-flag nil)
(require 'dired+)
(setq dired-dwim-target t) ;; Copy to other dired buffer by default

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; imenu
(setq lisp-comment-imenu-generic-expression
      '(;;("Subject"  "^Subject: *\\(.*\\)" 1)
        ("Subsection"     ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n;;\\(.*\\)\n" 1)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq imenu-generic-expression lisp-comment-imenu-generic-expression)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; perspective
(require 'perspective)
(persp-mode 1)
(require 'persp-projectile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; show white space characters
(require 'whitespace)
(setq whitespace-style
      '(face tabs spaces newline space-mark tab-mark newline-mark indentation space-after-tab space-before-tab))
(setq whitespace-display-mappings
 '(
    (space-mark 32 [183] [46]) ; normal space
    (newline-mark 10 [182 10]) ; newlne
    (tab-mark 9 [9655 9] [92 9]) ; tab
))

(global-whitespace-mode)
(tool-bar-mode -1)

;; hl-line: highlight the current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode)
  (winner-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace: save location in file when saving files
(require 'saveplace)                   ;; get the package
(setq save-place-file "~/.emacs.d/cache/saveplace")
(setq-default save-place t)            ;; activate it for all buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigate Emacs windows with shift+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight region after yank
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic expansion
(require 'expand-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regex
;; (require 'visual-regexp)
(require 'visual-regexp-steroids)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth Scrolling
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove whitespace after RETing 2nd time in a row
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; directory comparison and tree-based browser
(require 'ztree-diff)
(require 'ztree-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatic resizing
(require 'golden-ratio)
;;(setq golden-ratio-exclude-modes '("ediff-mode"
;;                                   "gud-mode"
;;                                   "gdb-locals-mode"
;;                                   "gdb-registers-mode"
;;                                   "gdb-breakpoints-mode"
;;                                   "gdb-threads-mode"
;;                                   "gdb-frames-mode"
;;                                   "gdb-inferior-io-mode"
;;                                   "gud-mode"
;;                                   "gdb-inferior-io-mode"
;;                                   "gdb-disassembly-mode"
;;                                   "gdb-memory-mode"
;;                                   "magit-log-mode"
;;                                   "magit-reflog-mode"
;;                                   "magit-status-mode"
;;                                   "IELM"
;;                                   "eshell-mode" "dired-mode"))

;; (golden-ratio-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; templating
(require 'yasnippet)
(yas-global-mode 1)
(add-hook 'shell-mode-hook (lambda()
            (yas-minor-mode -1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm  
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.


(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

;; Specific helm hotkeys

;(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(helm-mode 1)


;; (eval-after-load "helm-gtags"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Navigation
(require 'xcscope)
(add-hook 'c-mode-hook 'cscope-minor-mode)
(add-hook 'c++-mode-hook 'cscope-minor-mode)
(setq cscope-initial-directory "/vobs/")

;; Ggtags
(require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))


(defun create-tags (dir-name)
 "Create tags file."
 (interactive "DDirectory: ")
 (eshell-command 
  (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; Helm gtags
(require 'helm-gtags)
;; ;;Enable helm-gtags-mode
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-hook 'he1lm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)


(setq
helm-gtags-ignore-case t
helm-gtags-auto-update t
helm-gtags-use-input-at-cursor t
helm-gtags-pulse-at-cursor t
;;helm-gtags-prefix-key "\C-cg"
helm-gtags-suggested-key-mapping t
helm-gtatgs-path-style 'relative
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project navigation
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ syntax analyzer
(require 'cc-mode)
(require 'semantic)
(require 'semantic/bovine/gcc)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(global-semantic-idle-breadcrumbs-mode t)
(semantic-mode 1)

(defun semantic-goto-definition (point)
  "Goto definition using semantic-ia-fast-jump
save the pointer marker if tag is found"
  (interactive "d")
  (condition-case err
      (progn
        (ring-insert semantic-tags-location-ring (point-marker))
        (semantic-ia-fast-jump point))
    (error
     ;;if not found remove the tag saved in the ring
     (set-marker (ring-remove semantic-tags-location-ring 0) nil nil)
     (signal (car err) (cdr err)))))

(defun semantic-pop-tag-mark ()
  "popup the tag save by semantic-goto-definition"
  (interactive)
  (if (ring-empty-p semantic-tags-location-ring)
      (message "%s" "No more tags available")
    (let* ((marker (ring-remove semantic-tags-location-ring 0))
              (buff (marker-buffer marker))
                 (pos (marker-position marker)))
      (if (not buff)
            (message "Buffer has been deleted")
        (switch-to-buffer buff)
        (goto-char pos))
      (set-marker marker nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; silver-surfer
(require 'helm-ag)
(setq helm-ag-insert-at-point 'symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocompletion
(require 'company)
;;(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
(add-hook 'after-init-hook 'global-company-mode)
;; (setq company-backends '(company-elisp 
;;                          company-semantic
;;                          company-gtags
;;                          company-dabbrev-code
;;                          company-keywords
;;                          company-files 
;;                          company-dabbrev))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; more autocompletion
(require 'hippie-exp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(require 'org)
(setq org-log-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speedbar
(require 'speedbar)
(require 'semantic/sb)
;;(require 'sr-speedbar)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treat undo as a tree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line numbering
(require 'nlinum)
(global-nlinum-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple terminals
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
(setq term-buffer-maximum-size 0)

;;(remq term-unbind-key-list "C-c")
;;(add-to-list term-bind-key-alist "C-c")
;;(setq term-unbind-key-list '("C-x", "C-h", "M-x", "C-z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rename multiple variables, etc
(require 'iedit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; workspaces
;; (require 'workgroups2)
;; (setq wg-prefix-key (kbd "C-c w"))
;; (workgroups-mode 1)
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace jump mode major function
(require 'ace-jump-mode)

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax checking
(require 'flycheck)
(require 'flycheck-tip)
(add-hook 'after-init-hook 'global-flycheck-mode)
(flycheck-tip-use-timer 'verbose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
(require 'magit)
(autoload 'magit-status "magit" nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching bracket coloring
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ibuffer
(require 'ibuffer)
(setq ibuffer-saved-filter-groups
  (quote (("default"      
            ("Org" ;; all org-related buffers
              (mode . org-mode))  
            ("Mail"
              (or  ;; mail-related buffers
               (mode . message-mode)
               (mode . mail-mode)
               ;; etc.; all your mail related modes
               ))
            ("Vobs"
              (filename . "/vobs/"))
            ("Scripts"
	     (filename . "/home/aapollon/scripts"))
	    ("Manpages"
	     (mode . Man))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline
(require 'powerline)
;; (powerline-vim-theme)
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Diminis- mode names
(require 'diminish)
(eval-after-load "undo-tree"
  '(diminish 'undo-tree-mode))
(eval-after-load "whitespace"
  '(diminish 'global-whitespace-mode))
(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode))
(eval-after-load "company"
  '(diminish 'company-mode))
(eval-after-load "drag-stuff"
  '(diminish 'drag-stuff-mode))
(eval-after-load "helm"
  '(diminish 'helm-mode))
(eval-after-load "volatile-highlights"
  '(diminish 'volatile-highlights-mode ))
(eval-after-load "magit"
  '(diminish 'magit-auto-revert-mode ))
(eval-after-load "flycheck"
  '(diminish 'flycheck-mode ))
(eval-after-load "magit"
  '(diminish 'magit-auto-revert-mode ))
(eval-after-load "slime"
  '(diminish 'slime-mode ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; irc
(require 'erc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drag stuff
(require 'drag-stuff)
(drag-stuff-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings 
(global-unset-key (kbd "C-x c"))
(global-unset-key (kbd "C-c h"))

(key-chord-define-global "jj"				'ace-jump-word-mode)
(key-chord-define-global "jl"				'ace-jump-line-mode)
(key-chord-define-global "jk"				'ace-jump-char-mode)
(key-chord-define-global "zx"				'hippie-expand)
(key-chord-define-global "xx"				'persp-switch)

(global-set-key (kbd "C-c SPC")			'ace-jump-mode)
(global-set-key (kbd "RET")				'newline-and-indent)
(global-set-key (kbd "C-<f4>")				'kill-buffer-and-window)
(global-set-key (kbd "<delete>")			'delete-char)  ; delete == delete
(global-set-key (kbd "M-g")				'goto-line)    ; M-g  'goto-line
(global-set-key (kbd "M-G")				'goto-char)    ; M-g  'goto-char
(global-set-key (kbd "C-x C-r")			'comment-or-uncomment-region) ;;
(global-set-key (kbd "C-c s h")			'prelude-copy-file-name-to-clipboard)
(global-set-key (kbd "C-c C-<left>")			'winner-undo)
(global-set-key (kbd "C-c C-<right>")			'winner-redo)
(global-set-key (kbd "C-.")				'er/expand-region)
(global-set-key (kbd "C-,")				'er/contract-region)
(global-set-key (kbd "C-c r r")			'vr/replace)
(global-set-key (kbd "C-c r q")			'vr/query-replace)
(global-set-key (kbd "C-s")				'vr/isearch-forward)
(global-set-key (kbd "C-r")				'vr/isearch-backward)

;; helm
(global-set-key (kbd "M-x")				'helm-M-x)
(global-set-key (kbd "M-y")				'helm-show-kill-ring)
(global-set-key (kbd "C-x b")				'helm-mini)
(global-set-key (kbd "C-x C-f")			'helm-find-files)
(global-set-key (kbd "C-c m")				'helm-man-woman)
(global-set-key (kbd "C-c f")				'helm-find)
(global-set-key (kbd "C-c l")				'helm-locate)
(global-set-key (kbd "C-c o")				'helm-occur)
(global-set-key (kbd "C-c h t")			'helm-top)
(global-set-key (kbd "C-c h c")			'helm-colors)
;; (global-set-key (kbd "C-c r")			'helm-regexp)
(global-set-key (kbd "C-c e")				'helm-projectile)
(global-set-key (kbd "C-c d")				'helm-semantic-or-imenu)
(global-set-key (kbd "C-c y a")			'helm-ag)
(global-set-key (kbd "C-c y q")			'helm-ag-pop-stack)

;; code navigation
(global-set-key (kbd "C-c c")				'helm-gtags-mode)
(global-set-key (kbd "C-c z")				'cscope-minor-mode)
(global-set-key (kbd "C-c x")				'ggtags-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package-specific

;; xcscope
(define-key cscope-minor-mode-keymap (kbd "C-c s q")	'cscope-pop-mark)

;; ggtags
(define-key ggtags-mode-map (kbd "C-c s g")		'ggtags-find-tag-dwim)
(define-key ggtags-mode-map (kbd "C-c s d")		'ggtags-find-definition)
(define-key ggtags-mode-map (kbd "C-c s f")		'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c s s")		'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c s q")		'ggtags-prev-mark)
(define-key ggtags-mode-map (kbd "C-c s w")		'ggtags-next-mark)
(define-key ggtags-mode-map (kbd "C-c s t")		'ggtags-grep)

;; helm-gtags
(define-key helm-gtags-mode-map (kbd "C-c s v")	'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "C-c s g")	'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "C-c <")		'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >")		'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-c s t")	'helm-gtags-find-tag)
(define-key helm-gtags-mode-map (kbd "C-c s s")	'helm-gtags-find-symbol)
(define-key helm-gtags-mode-map (kbd "C-c s f")	'helm-gtags-find-files)
;;(define-key helm-gtags-mode-map (kbd "C-c s q")	'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c s r")	'helm-gtags-resume)
(define-key helm-gtags-mode-map (kbd "C-c s w")	'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-c s q")	'helm-gtags-previous-history)

;; semantic
(define-key semantic-mode-map (kbd "C-c w d")		'semantic-goto-definition)
(define-key semantic-mode-map (kbd "C-c w q")		'semantic-pop-tag-mark)
(define-key semantic-mode-map (kbd "C-c w e")		'senator-go-up-reference)
(define-key semantic-mode-map (kbd "C-c w s")		'semantic-symref)
(define-key semantic-mode-map (kbd "C-c w z")		'senator-previous-tag)
(define-key semantic-mode-map (kbd "C-c w x")		'senator-next-tag)

;; org
(define-key global-map "\C-cl"				'org-store-link)
(define-key global-map "\C-ca"				'org-agenda)

;; Magit
(define-key global-map (kbd "C-c g")			'magit-status)
(define-key emacs-lisp-mode-map (kbd "C-c j")		'eval-region)

;; ibuffer
(define-key global-map (kbd "C-x C-b")			'ibuffer)

(load-theme 'apples-gray t)
 (provide '.emacs);;; .emacs ends here 
(put 'upcase-region 'disabled nil)
