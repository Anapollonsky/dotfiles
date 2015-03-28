;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '(colors fasd git perspectives slime smex python c-c++ cscope regex extra-langs)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(evil-search-highlight-persist)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (progn
    (add-to-list 'custom-theme-load-path "~/.emacs.d/private/themes")
    (setq-default
     ;; Specify the startup banner. Default value is `official', it displays
     ;; the official spacemacs logo. An integer value is the index of text
     ;; banner, `random' chooses a random text banner in `core/banners'
     ;; directory. A string value must be a path to a .PNG file.
     ;; If the value is nil then no banner is displayed.
     ;; dotspacemacs-startup-banner 'official
     dotspacemacs-startup-banner 'official
     ;; List of themes, the first of the list is loaded when spacemacs starts.
     ;; Press <SPC> T n to cycle to the next theme in the list (works great
     ;; with 2 themes variants, one dark and one light)
     dotspacemacs-themes '(apples
                           leuven
                           monokai
                           solarized-dark
                           zenburn)
     ;; If non nil the cursor color matches the state color.
     dotspacemacs-colorize-cursor-according-to-state t
     ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
     ;; size to make separators look not too crappy.
     dotspacemacs-default-font '("DejaVu Sans Mono"
                                 :size 15
                                 :weight normal
                                 :width normal
                                 :powerline-scale 1.1)
     ;; The leader key
     dotspacemacs-leader-key "SPC"
     ;; Major mode leader key is a shortcut key which is the equivalent of
     ;; pressing `<leader> m`. Set it to `nil` to disable it.
     dotspacemacs-major-mode-leader-key ","
     ;; The command key used for Evil commands (ex-commands) and
     ;; Emacs commands (M-x).
     ;; By default the command key is `:' so ex-commands are executed like in Vim
     ;; with `:' and Emacs commands are executed with `<leader> :'.
     dotspacemacs-command-key ":"
     ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
     ;; several times cycle between the kill ring content.
     dotspacemacs-enable-paste-micro-state t
     ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
     ;; the commands bound to the current keystrokes.
     dotspacemacs-guide-key-delay 0.4
     ;; If non nil a progress bar is displayed when spacemacs is loading. This
     ;; may increase the boot time on some systems and emacs builds, set it to
     ;; nil ;; to boost the loading time.
     dotspacemacs-loading-progress-bar t
     ;; If non nil the frame is fullscreen when Emacs starts up.
     ;; (Emacs 24.4+ only)
     dotspacemacs-fullscreen-at-startup nil
     ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
     ;; Use to disable fullscreen animations in OSX."
     dotspacemacs-fullscreen-use-non-native nil
     ;; If non nil the frame is maximized when Emacs starts up.
     ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
     ;; (Emacs 24.4+ only)
     dotspacemacs-maximized-at-startup nil
     ;; A value from the range (0..100), in increasing opacity, which describes
     ;; the transparency level of a frame when it's active or selected.
     ;; Transparency can be toggled through `toggle-transparency'.
     dotspacemacs-active-transparency 90
     ;; A value from the range (0..100), in increasing opacity, which describes
     ;; the transparency level of a frame when it's inactive or deselected.
     ;; Transparency can be toggled through `toggle-transparency'.
     dotspacemacs-inactive-transparency 90
     ;; If non nil unicode symbols are displayed in the mode line.
     dotspacemacs-mode-line-unicode-symbols t
     ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
     ;; scrolling overrides the default behavior of Emacs which recenters the
     ;; point when it reaches the top or bottom of the screen.
     dotspacemacs-smooth-scrolling t
     ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
     dotspacemacs-smartparens-strict-mode nil
     ;; If non nil advises quit functions to keep server open when quitting.
     dotspacemacs-persistent-server nil
     ;; The default package repository used if no explicit repository has been
     ;; specified with an installed package.
     ;; Not used for now.
     dotspacemacs-default-package-repository nil)
    ;; User initialization goes here
    )
  )

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (progn
    ;; identification
    (setf user-full-name "Andrew Apollonsky")
    (setf user-mail-address "Anapollonsky@gmail.com")

    ;; browser
    (setq browse-url-generic-program (or (executable-find "google-chrome-stable") (executable-find "chromium")))
    (setq browse-url-browser-function 'browse-url-generic)

    ;; no dialog boxes
    (setq use-dialog-box nil)

    ;; Backups
    (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
    (setq delete-old-versions -1)
    (setq version-control t)
    (setq vc-make-backup-files t)
    (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

    ;; History
    (setq savehist-file "~/.emacs.d/savehist")
    (savehist-mode 1)
    (setq history-length t)
    (setq history-delete-duplicates t)
    (setq savehist-save-minibuffer-history 1)
    (setq savehist-additional-variables
          '(kill-ring
            search-ring
            regexp-search-ring))

    ;; sudo-open current file
    (evil-leader/set-key "ofs" 'sudo-edit)

    ;; org-babel
    (org-babel-do-load-languages ;; Parse babel blocks for these languages
     'org-babel-load-languages
     '((C . t)
       (python . t)
       (lisp . t)
       (latex . t)
       (sh . t)
       ))
    (setq org-src-fontify-natively t)
    (add-to-list 'org-latex-packages-alist '("" "minted")) ;; Add minted to the defaults packages to include when exporting.
    (setq org-latex-listings 'minted)  ;; Tell the latex export to use the minted package for source code coloration.
    (setq org-latex-pdf-process ;; Let the exporter use the -shell-escape option to let latex execute external programs.
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

    ;; org-capture
    (setq org-directory "~/org")
    (setq org-default-notes-file (concat org-directory "/notes.org"))
    (evil-leader/set-key "ooc" 'org-capture)
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline (concat org-directory "/notes.org") "Tasks")
             "** TODO %?\n %i\n")
            ("l" "Link" plain (file+headline (concat org-directory "/notes.org") "Links")
             "- %?\n %x\n")
            ("q" "Quick Note" plain (file+headline (concat org-directory "/notes.org") "Quick Notes")
             "+ %?\n %i\n")))
    (setq org-agenda-files '("~/org/agenda.org" "~/org/notes.org"))

    ;; helm
    
    ;; whitespace
    (evil-leader/set-key "ofw" 'fixup-whitespace)
    (evil-leader/set-key "ofc" 'whitespace-cleanup)
    (evil-leader/set-key "ofl" 'delete-blank-lines)
    (setq whitespace-style
          '(face tabs spaces newline space-mark tab-mark newline-mark indentation space-after-tab space-before-tab))
    (setq whitespace-display-mappings
          '(
            (space-mark 32 [183] [46]) ; normal space
            (newline-mark 10 [182 10]) ; newlne
            (tab-mark 9 [9655 9] [92 9]) ; tab
            ))
    (when (display-graphic-p) (global-whitespace-mode))

    ;; highlighting
    (evil-leader/set-key "ohs" 'hlt-highlight-symbol)
    (evil-leader/set-key "ohc" 'hlt-unhighlight-all-prop)
    ))

