;; Package -- Summary

;;; Commentary:

;;; Code:
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))


(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package auto-installation
;(defun ensure-package-installed (&rest packages)
;  "Assure every package is installed, ask for installation if it’s not.
;
;Return a list of installed packages or nil for every skipped package."
;  (mapcar
;   (lambda (package)
;     ;; (package-installed-p 'evil)
;     (if (package-installed-p package)
;         nil
;       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
;           (package-install package)
;         package)))
;   packages))


;(ensure-package-installed  'magit 'save-place 'helm 'projectile 'helm-projectile 'speedbar 'uniquify 'undo-tree 'company 'xcscope 'nlinum 'flycheck 'rainbow-delimiters 'ace-jump-mode 'multi-term) 

;; activate installed packages
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global keybindings 
(global-set-key (kbd "RET")         'newline-and-indent)
(global-set-key (kbd "C-<f4>")      'kill-buffer-and-window)
(global-set-key (kbd "<delete>")    'delete-char)  ; delete == delete
(global-set-key (kbd "M-g")         'goto-line)    ; M-g  'goto-line
(global-set-key (kbd "M-G")         'goto-char)    ; M-g  'goto-char
(global-set-key (kbd "C-c C-r")     'comment-or-uncomment-region) ;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; package-specific


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; show white space characters
;; (require 'whitespace)
;; ;; make whitespace-mode use just basic coloring
;; ; make whitespace-mode use just basic coloring
;; (setq whitespace-style (quote
;;   ( spaces tabs newline space-mark tab-mark newline-mark)))
;; (setq whitespace-display-mappings
;;  '(
;;    (space-mark 32 [183] [46]) ; normal space
;;    (newline-mark 10 [182 10]) ; newlne
;;    (tab-mark 9 [9655 9] [92 9]) ; tab
;; ))

;; (global-whitespace-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; slick-copy: make copy-past a bit more intelligent
;; from: http://www.emacswiki.org/emacs/SlickCopy
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

;; Makes yank not work
;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single
;; line instead".
;; (interactive
;;  (if mark-active (list (region-beginning) (region-end))
;;    (list (line-beginning-position)
;; 	 (line-beginning-position 2)))))

(tool-bar-mode -1)

; http://www.emacswiki.org/emacs/WinnerMode
(when (fboundp 'winner-mode)
  (winner-mode 1))

(global-set-key (kbd "C-c C-<left>") 'winner-undo)
(global-set-key (kbd "C-c C-<right>") 'winner-redo)

;; hl-line: highlight the current line
(when (fboundp 'global-hl-line-mode)
  (global-hl-line-mode t)) ;; turn it on for all modes by default

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; saveplace: save location in file when saving files
(require 'saveplace)                   ;; get the package
(setq save-place-file "~/.emacs.d/cache/saveplace")
(setq-default save-place t)            ;; activate it for all buffers

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; minibuffer autocompletion
;(require 'icicles)
;(icy-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
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

;; Helm gtags
(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 helm-gtatgs-path-style 'relative
 )
;; (eval-after-load "helm-gtags"

(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c s v") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "C-c s d") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
(define-key helm-gtags-mode-map (kbd "C-c s t") 'helm-gtags-find-tag)
(define-key helm-gtags-mode-map (kbd "C-c s s") 'helm-gtags-find-symbol)
(define-key helm-gtags-mode-map (kbd "C-c s f") 'helm-gtags-find-files)
(define-key helm-gtags-mode-map (kbd "C-c s q") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c s r") 'helm-gtags-resume)

(helm-gtags-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; project navigation
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++ syntax analyzer
;; (require 'cc-mode)
;; (require 'semantic)

;; (global-semanticdb-minor-mode 1)
;; (global-semantic-idle-scheduler-mode 1)

;; (semantic-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autocompletion
(require 'company)
;;(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speedbar
(require 'speedbar)
 ;;(require 'sr-speedbar)
 ;;(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treat undo as a tree
(require 'undo-tree)
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":"
  uniquify-after-kill-buffer-p t
  uniquify-ignore-buffers-re "^\\*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'xcscope)
(setq cscope-initial-directory "/vobs/")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'ggtags)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
;;               (ggtags-mode 1))))

;; (setq path-to-ctags "~/.emacs.d/.ctags_lib") ;; <- your ctags path here
;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (eshell-command 
;;    (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; line numbering
(require 'nlinum)
(global-nlinum-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; multiple terminals
(require 'multi-term)
(setq multi-term-program "/bin/zsh")
;;(remq term-unbind-key-list "C-c")
;;(add-to-list term-bind-key-alist "C-c")
;;(setq term-unbind-key-list '("C-x", "C-h", "M-x", "C-z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; workspaces
;; (require 'workgroups2)
;; (setq wg-prefix-key (kbd "C-c w"))
;; (workgroups-mode 1)
;; (setq wg-session-file "~/.emacs.d/.emacs_workgroups")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax checking
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
(require 'magit)
(autoload 'magit-status "magit" nil t)
(define-key global-map (kbd "C-c g") 'magit-status)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching bracket coloring
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("085b401decc10018d8ed2572f65c5ba96864486062c0a2391372223294f89460" "2affb26fb9a1b9325f05f4233d08ccbba7ec6e0c99c64681895219f964aac7af" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "1ba463f6ac329a56b38ae6ac8ca67c8684c060e9a6ba05584c90c4bffc8046c3" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(delete-selection-mode t)
 '(org-CUA-compatible nil)
 '(org-replace-disputed-keys nil)
 '(recentf-mode t)
 '(shift-select-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
