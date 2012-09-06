(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/single-files")
(require 'eval-after-load)

;; ========= Modes ==========
(global-linum-mode t)
(ido-mode t)
(delete-selection-mode t)
(setq transient-mark-mode t)
(setq which-function-mode t)
(global-font-lock-mode t)        ;Syntax highlight

(menu-bar-mode nil)
(tool-bar-mode nil)
;(pc-selection-mode t)
;(shift-select-mode t)

;; ========= Varibles ==========
(setq kill-ring-max 2000);; Set delete record
(add-hook 'before-save-hook
          '(lambda () ;;create directory before saving
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))
(setq visual-bell nil)  ;;; Disable system beep
(set-scroll-bar-mode t)   ;; no scroll bar, even in x-window system (recommended)
(auto-image-file-mode t)
(setq require-final-newline t)
(setq enable-recursive-minibuffers t)
(icomplete-mode t);; icomplete mode in minibuffer
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
(setq auto-save-default nil)  ; disable # files%p
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
(setq frame-title-format (list "%b %p  " (getenv "USERNAME") "   [ %f ]  " emacs-version))
(blink-cursor-mode nil)
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(remove-hook 'coding-hook 'turn-on-hl-line-mode)
(if (eq window-system 'w32) (set-frame-font "Consolas 11") )
; ========= Keyboard Definition ==========
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-x\C-j" 'dired-jump)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key "\C-x\C-k" 'kill-this-buffer)
(global-unset-key "\C-o")
(global-set-key (kbd "<C-tab>") 'other-window)
(global-unset-key "\C-z")
(global-set-key "\C-z" 'undo)
(global-unset-key [insert])
(global-unset-key [backspace] )
(global-set-key [backspace] 'delete-backward-char)
(global-unset-key [delete] )
(global-set-key [delete] 'delete-char)
(global-set-key [C-delete] 'kill-word)
(define-key global-map [home] `beginning-of-line)
(define-key global-map [end] `end-of-line)
(define-key global-map (kbd "RET") 'newline-and-indent)
(setq standard-indent 2)
(normal-erase-is-backspace-mode 1)
(global-set-key [(f2)] 'set-mark-command)    ;set F2 as set mark
(define-key isearch-mode-map '[backspace] 'isearch-delete-char)
;; ==================== Coding ====================
(setq buffer-file-coding-system 'utf-8-unix)
(setq default-file-name-coding-system 'utf-8-unix)
(setq default-keyboard-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq default-sendmail-coding-system 'utf-8-unix)
(setq default-terminal-coding-system 'utf-8-unix)

;;----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yas-jit)
(setq yas/root-directory "~/.emacs.d/plugins/yasnippet")
(yas/jit-load)

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

;;----------------------------------------------------------------------
(require 'dired-lis)
(require 'smart-compile)
(require 'compile-dwim)
(require 'smart-operator)
(require 'autopair) (autopair-global-mode) ;; to enable in all buffers
(require 'auto-pair+)
;(require 'kill-ring-ido)
(require 'browse-kill-ring+)
(load-file "~/.emacs.d/my_key_settings.el")


