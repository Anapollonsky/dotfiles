;; Package -- Summary

;;; Commentary:

;;; Code:
(setq package-archives '(("gnu" . "http://elpa.gnu.org/")
		         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; (setq url-proxy-services '(("no_proxy" . "work\\.com")
;;                            ("http" . "global.proxy.lucent.com:8000")))

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
;; global keybindings 
(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line
(global-set-key (kbd "M-G")         'goto-char)    ; M-g  'goto-char
(global-set-key (kbd "C-x C-r")     'comment-or-uncomment-region) ;;

(key-chord-define-global "jj" 'kill-buffer)
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

(global-set-key (kbd "C-c s h") 'prelude-copy-file-name-to-clipboard)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key-chord
(require 'key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay .01)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-Specific packages

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global packages

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

(global-set-key (kbd "C-c C-<left>") 'winner-undo)
(global-set-key (kbd "C-c C-<right>") 'winner-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace: save location in file when saving files
(require 'saveplace)                   ;; get the package
(setq save-place-file "~/.emacs.d/cache/saveplace")
(setq-default save-place t)            ;; activate it for all buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; re-builder
(require 're-builder)
(setq reb-re-syntax 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigate Emacs windows with shift+arrows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Highlight region after yank
(require 'volatile-highlights)
(volatile-highlights-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic expansion
(require 'expand-region)
(global-set-key (kbd "C-.") 'er/expand-region)
(global-set-key (kbd "C-,") 'er/contract-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smooth Scrolling
(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove whitespace after RETing 2nd time in a row
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hotkeys for major modes
;; A quick major mode help with discover-my-major
(require 'discover-my-major)
(global-unset-key (kbd "C-h h"))        ; original "C-h h" displays "hello world" in different languages
;(define-key map (kbd "C-c") nil)
;(define-key 'help-command (kbd "C-c h") 'discover-my-major)

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

(golden-ratio-mode)


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
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

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
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c C-s") 'helm-ff-do-grep)
(global-set-key (kbd "C-c m") 'helm-man-woman)
(global-set-key (kbd "C-c f") 'helm-find)
(global-set-key (kbd "C-c l") 'helm-locate)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c h t") 'helm-top)

;(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

(helm-mode 1)


;; (eval-after-load "helm-gtags"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Navigation
(require 'xcscope)
(global-set-key (kbd "C-c z") 'cscope-minor-mode)
(add-hook 'c-mode-hook 'cscope-minor-mode)
(add-hook 'c++-mode-hook 'cscope-minor-mode)
(setq cscope-initial-directory "/vobs/")
(define-key cscope-minor-mode-keymap (kbd "C-c s q") 'cscope-pop-mark)

;; Ggtags
(require 'ggtags)
(global-set-key (kbd "C-c x") 'ggtags-mode)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c s g") 'ggtags-find-tag-dwim)
(define-key ggtags-mode-map (kbd "C-c s d") 'ggtags-find-definition)
(define-key ggtags-mode-map (kbd "C-c s f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c s s") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c s q") 'ggtags-prev-mark)
(define-key ggtags-mode-map (kbd "C-c s w") 'ggtags-next-mark)

(defun create-tags (dir-name)
 "Create tags file."
 (interactive "DDirectory: ")
 (eshell-command 
  (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; Helm gtags
(require 'helm-gtags)
(global-set-key (kbd "C-c c") 'helm-gtags-mode)
;; ;;Enable helm-gtags-mode
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-hook 'he1lm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c s v") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "C-c s g") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-c s t") 'helm-gtags-find-tag)
(define-key helm-gtags-mode-map (kbd "C-c s s") 'helm-gtags-find-symbol)
(define-key helm-gtags-mode-map (kbd "C-c s f") 'helm-gtags-find-files)
;;(define-key helm-gtags-mode-map (kbd "C-c s q") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c s r") 'helm-gtags-resume)
(define-key helm-gtags-mode-map (kbd "C-c s w") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-c s q") 'helm-gtags-previous-history)

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; C/C++ syntax analyzer
;; ;; (require 'cc-mode)
;; ;; (require 'semantic)
;; ;; (global-semanticdb-minor-mode 1)
;; ;; (global-semantic-idle-scheduler-mode 1)
;; ;; (semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocompletion
(require 'company)
;;(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
(add-hook 'after-init-hook 'global-company-mode)
;; (global-set-key "\t" 'company-complete-common)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; more autocompletion
(require 'hippie-exp)
(global-set-key "\M- " 'hippie-expand)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speedbar
(require 'speedbar)
;;(require 'sr-speedbar)
;;(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

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
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(key-chord-define-global "jj" 'ace-jump-word-mode)
(key-chord-define-global "jl" 'ace-jump-line-mode)
(key-chord-define-global "jk" 'ace-jump-char-mode)

;;(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

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
(define-key global-map (kbd "C-c g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching bracket coloring
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer
(require 'ibuffer)
(define-key global-map (kbd "C-x C-b") 'ibuffer)
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
              (filename . "/home/aapollon/scripts"))))))

(add-hook 'ibuffer-mode-hook
  (lambda ()
    (ibuffer-switch-to-saved-filter-groups "default")))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Move line/regions up or down
;; (require 'move-text)
;; (global-set-key [M-S-up] 'move-text-up)
;; (global-set-key [M-S-down] 'move-text-down)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drag stuff
(require 'drag-stuff)
(drag-stuff-global-mode t)
;; ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ;; e-mail
;; ;; ;; got this line from one of the tutorials. Seemed interesting enough
;; ;; (setq gnus-invalid-group-regexp "[:`'\"]\\|^$")

;; ;; ;; standard way of getting imap going
;; ;; (setq gnus-select-method 
;; ;;          '(nnimap "gmail"
;; ;;           (nnimap-address "imap.gmail.com")
;; ;;           (nnimap-server-port 993)
;; ;;           (nnimap-stream ssl)))

;; ;; ;; set up smtp so we can send from gmail too:
;; ;; (setq message-send-mail-function 'smtpmail-send-it
;; ;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;; ;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587 "anapollonsky@gmail.com" nil))
;; ;;       smtpmail-default-smtp-server "smtp.gmail.com"
;; ;;       smtpmail-smtp-server "smtp.gmail.com"
;; ;;       smtpmail-smtp-service 587)

;; ;; ;;http://www.emacswiki.org/cgi-bin/wiki/GnusGmail
;; ;; ;;http://linil.wordpress.com/2008/01/18/gnus-gmail/

;; ;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; ;; ;; Threads are nice!
;; ;; (setq gnus-summary-thread-gathering-function
;; ;;       'gnus-gather-threads-by-subject)

;; ;; (setq user-full-name "Andrew Apollonsky")
;; ;; (setq user-mail-address "anapollonsky@gmail.com")
;; ;; (setq send-mail-function 'smtpmail-send-it)

(load-theme 'apples-gray t)
