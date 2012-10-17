(add-to-list 'load-path "~/.emacs.d/plugins/single-files")
(require 'eval-after-load)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)

;; (load-file "~/.emacs.d/plugins/cedet-1.1/common/cedet.el")
;; (global-ede-mode 1)                      ; Enable the Project management system
;; (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;; (global-srecode-minor-mode 1)            ; Enable template insertion menu

(global-unset-key [(f10)])
(global-set-key [(f10)]
		(lambda() (interactive) (find-file "~/.emacs.d/my_key_settings.el")))
(global-unset-key [(f11)])
(global-set-key [(f11)]
		(lambda() (interactive) (find-file "~/.emacs.d/.emacs")))
(global-set-key [(f12)] (lambda()
		(interactive)(save-some-buffers (buffer-file-name)) (eval-buffer))) ;; evaluate buffer
;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 80) (height . 20)))
;; ==================== Modes ====================
(global-linum-mode t)
(ido-mode t)
(delete-selection-mode t)
(visual-line-mode 1)
(setq transient-mark-mode t)
(setq which-function-mode t)
(global-font-lock-mode t)        ;Syntax highlight
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(icomplete-mode t);; icomplete mode in minibuffer
(auto-fill-mode 1)
(iswitchb-mode t)
(normal-erase-is-backspace-mode t)
(file-name-shadow-mode t)
(global-auto-revert-mode -1)
;; (auto-show-make-point-visible)
;; (blink-cursor-mode t)
;; (pc-selection-mode t)
;; (shift-select-mode t)
;; ==================== Varibles ====================
(setq resize-mini-windows nil)
(setq compilation-ask-about-save -1)
(setq after-find-file-from-revert-buffer)
(setq c-default-style "linux" c-basic-offset 4)
(setq vc-handled-backends nil)
(setq ido-save-directory-list-file "~/.emacs.d/desktop-save/ido-last.txt")
(setq ido-save-history nil )
(setq ido-enable-flex-matching t)
(custom-set-variables
 '(ido-enable-last-directory-history -1)
 '(ido-record-commands nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0))
(setq split-height-threshold nil)(setq split-width-threshold 0)
(setq split-height-threshold 0)(setq split-width-threshold nil)
(setq search-highlight t)
(setq require-final-newline t)
(setq kill-ring-max 2000);; Set delete record
(setq minibuffer-message-timeout 1)
(setq default-tab-width 2)
(setq truncat-lines -1)
(setq-default truncate-partial-width-windows -1)
(add-hook 'before-save-hook
          '(lambda () ;;create directory before saving
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))
(setq hippie-expand-verbose t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq visual-bell -1)  ;;; Disable system beep
(auto-image-file-mode t)
(setq enable-recursive-minibuffers t)
(setq kill-whole-line t) ;; C-k kill whole line including lind end
(require 'tramp)
(setq tramp-default-method "ssh")
;; (setq initial-major-mode 'text-mode)
(setq column-number-mode t)   ;; Display Cursor Location
(setq line-number-mode t)
(setq display-time-12hr-format t);;;; Display Time
(setq display-time-day-and-date t)
(display-time)
(setq inhibit-startup-message t)  ;; Disable Startup Message
(mouse-wheel-mode t)    ;; Response to mouse scrolling
(setq-default make-backup-files t)
(setq-default make-temp-files t)
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening
(setq auto-save-default t)  ; disable # files%p
(setq make-backup-files nil) ;; do not make backup files
(auto-save-mode t)
(setq kept-new-versions 10 ;; Enable versioning with modified values
      kept-old-versions 5
      version-control t
      delete-old-versions t
      backup-by-copying t)
(setq backup-directory-alist `((".*" . "~/.emacs.d/auto-save-list/")))
(setq auto-save-file-name-transforms `((".*" ,
																				"~/.emacs.d/auto-save-list" t)))
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

(setq temporary-file-directory  "~/.emacs.d/temp-list/")
(setq-default fill-column 72)    ;; Set Fill Column and auto fill
(setq scroll-margin 3  scroll-conservatively 10000)
(fset 'yes-or-no-p 'y-or-n-p)  ;; ask by y or n
(setq frame-title-format (list "%b %p  [%f] " (getenv "USERNAME") " %s %Z   " emacs-version))
(setq standard-indent 2)

(setq-default indent-tabs-mode -1)
(setq-default tab-always-indent -1)
(setq message-log-max 512)
;; (setq c-auto-newline 1)
(remove-hook 'coding-hook 'turn-on-hl-line-mode)
(setq comint-buffer-maximum-size 10240)
(setq font-lock-maximum-decoration t)
;;================================================================================
;; (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
;; (setq yas-snippet-dirs '("~/.emacs.d/plugins/yasnippet" "~/.emacs.d/plugins/yasnippet/snippets"))
;; (require 'yasnippet)
;; ;; (setq yas-snippet-dirs '("~/.emacs.d/plugins/yasnippet" "~/.emacs.d/plugins/yasnippet/snippets"))
;; (yas-global-mode 1)

;; (setq yas/prompt-functions '(yas/x-prompt yas/dropdown-prompt))
;; (require 'dropdown-list)
;; (setq yas/prompt-functions '(yas/dropdown-prompt
;;                              yas/ido-prompt
;;                              yas/completing-prompt))
;;================================================================================
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete-131")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete-131/ac-dict")
(ac-config-default)
(require 'auto-complete-extension)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; (defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
;;   (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
;;   (add-to-list 'ac-sources 'ac-source-yasnippet))

;;================================================================================
;; (add-to-list 'load-path "~/.emacs.d/plugins/color-theme-660")
;; (add-to-list 'load-path "~/.emacs.d/plugins/color-theme-660/themes")
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-arjen)))
;; (color-theme-sons-of-obsidian)
;;================================================================================
(require 'cursor-chg)  ; Load the library
(toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
(change-cursor-mode 1) ; Turn on change for overwrite, read-only, and input mode
(curchg-change-cursor-when-idle-interval 10) ; change the idle timer
;; ==================== org mode ====================
(when (< emacs-major-version 24)
	(setq load-path (cons "~/.emacs.d/plugins/org-7.9.1/lisp" load-path))
	(setq load-path (cons "~/.emacs.d/plugins/org-7.9.1/contrib/lisp" load-path))
	(require 'org-install)
	(require 'org-special-blocks)
	;set  initial langauges we want org-babel to support
	(org-babel-do-load-languages
	 'org-babel-load-languages
	 '((sh . t)  (python . t)   (R . t)   (ruby . t)   (ditaa . t)   (dot . t)
		 (octave . t)   (sqlite . t)   (perl . t)   ))
	)
;; ; Add short cut keys for the org-agenda
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-ca" 'org-agenda)
(setq org-support-shift-select t)
(global-set-key "\C-c \C-b" 'org-export-as-html-and-open)
(add-hook 'org-mode-hook  (lambda () (setq truncate-lines t)))
;; ==================== Circuits ====================
(add-to-list 'load-path "~/.emacs.d/plugins/netlist-modes")
;; (require 'spectre-mode)
;; (add-to-list 'auto-mode-alist '("\\.scs$" . spectre-mode))
;; (require 'spice-mode)
;; (add-to-list 'auto-mode-alist '("\\.sp$" . spice-mode))
;; (add-to-list 'auto-mode-alist '("\\.cir$" . spice-mode))
;; ==================== icicles ====================
;; (add-to-list 'load-path "~/.emacs.d/plugins/icicles")(require 'icicles)
;; ==================================================================
;; (require 'auto-complete-yasnippet)
(require 'unicad)
(require 'hungry-delete) (turn-on-hungry-delete-mode)
(require 'dired-lis)
(require 'smart-compile)
(require 'compile-dwim)
(require 'smart-operator)
(require 'autopair) (autopair-global-mode t) ;; to enable in all buffers
(require 'auto-pair+) (global-unset-key "\C-m")
(require 'highlight-sexp)
(require 'icomplete+)
(require 'iswitchb-fc)
(require 'ebs)(ebs-initialize)(global-set-key [(control tab)] 'ebs-switch-buffer)
(require 'color-theme-single) (color-theme-arjen)
;(require 'kill-ring-ido)
(require 'browse-kill-ring+)
(require 'auto-show)(auto-show-mode 1)(setq-default auto-show-mode t)
(require 'backup-each-save) (add-hook 'after-save-hook 'backup-each-save)
(require 'saveplace) (setq save-place-file "~/.emacs.d/desktop-save/saveplace.txt")(setq-default save-place t)
(load-file "~/.emacs.d/my_key_settings.el")
(require 'maxframe) (maximize-frame)

;; (define-key 'c-mode-map  "\C-c \C-c" 'Compile)
;; (if (eq window-system 'w32) (emacs-maximize) )
(global-unset-key "\C-c \C-c")
(custom-set-variables
 '(ac-trigger-key "TAB")
 '(ac-auto-start nil)
 '(ac-use-menu-map t))
(message " Loading Acommplished ")
