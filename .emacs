(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/single-files")
(require 'eval-after-load)
(load-file "~/.emacs.d/my_key_settings.el")
;; ========= Modes ==========
(global-linum-mode t)
(ido-mode t)
(delete-selection-mode t)
(setq transient-mark-mode t)
(setq which-function-mode t)
(global-font-lock-mode t)        ;Syntax highlight
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(icomplete-mode t);; icomplete mode in minibuffer
;; (blink-cursor-mode t)
;;(pc-selection-mode t)
;(shift-select-mode t)
;; ========= Varibles ==========
(setq kill-ring-max 2000);; Set delete record
(add-hook 'before-save-hook
          '(lambda () ;;create directory before saving
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-blank-lines)
(setq visual-bell -1)  ;;; Disable system beep
(auto-image-file-mode t)
(setq enable-recursive-minibuffers t)
(setq kill-whole-line t) ;; C-k kill whole line including lind end
(require 'tramp)
(setq tramp-default-method "ssh")
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)
(setq column-number-mode t)   ;; Display Cursor Location
(setq line-number-mode t)
(setq display-time-12hr-format t);;;; Display Time
(setq display-time-day-and-date t)
(display-time)
(setq inhibit-startup-message t)  ;; Disable Startup Message
(mouse-wheel-mode t)    ;; Response to mouse scrolling
(setq-default make-backup-files t)
(setq-default make-temp-files t)
(setq auto-save-default t)  ; disable # files%p
(setq kept-new-versions 10 ;; Enable versioning with modified values
      kept-old-versions 5
      version-control t
      delete-old-versions t
      backup-by-copying t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/auto-save-list"))))  ;; Save all backup file in this directory.
(global-set-key "\C-k" 'kill-whole-line)
(setq temporary-file-directory  "~/.emacs.d/temp-list")
(setq-default fill-column 72)    ;; Set Fill Column and auto fill
(setq auto-fill-mode 1)
(setq scroll-margin 3  scroll-conservatively 10000)
(fset 'yes-or-no-p 'y-or-n-p)  ;; ask by y or n
(setq frame-title-format (list "%b %p  [%f] " (getenv "USERNAME") " %s %Z   " emacs-version))
(setq standard-indent 2)
(remove-hook 'coding-hook 'turn-on-hl-line-mode)
(if (eq window-system 'w32) (set-frame-font "Consolas 11") )


;;----------------------------------------------------------------------
;; (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
;; (require 'yas-jit)
;; (setq yas/root-directory "~/.emacs.d/plugins/yasnippet")
;; (yas/jit-load)

;;----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete-131")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete-131/ac-dict")
(ac-config-default)

;;----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/plugins/color-theme-660")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))
;;----------------------------------------------------------------------
(require 'cursor-chg)  ; Load the library
(toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
(change-cursor-mode 1) ; Turn on change for overwrite, read-only, and input mode
(curchg-change-cursor-when-idle-interval 10) ; change the idle timer
;;----------------------------------------------------------------------
(require 'dired-lis)
(require 'smart-compile)
(require 'compile-dwim)
(require 'smart-operator)
(require 'autopair) (autopair-global-mode t) ;; to enable in all buffers
(require 'auto-pair+)
(require 'highlight-sexp)
(require 'icomplete+)
;(require 'kill-ring-ido)
(require 'browse-kill-ring+)
