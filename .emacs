;; ==================== Init ====================
(add-to-list 'load-path "~/.emacs.d/plugins/single-files")
(require 'eval-after-load)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)  ;; Disable Startup Message
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
(global-visual-line-mode t)
(global-linum-mode t)
(delete-selection-mode t)
(visual-line-mode 1)
(ido-mode t)
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
(display-time)
(mouse-wheel-mode t)    ;; Response to mouse scrolling
(setq-default indent-tabs-mode -1)
(setq-default tab-always-indent -1)

;; (auto-show-make-point-visible)
;; (blink-cursor-mode t)
;; (pc-selection-mode t)
;; (shift-select-mode t)

;; ==================== Varibles ====================
(setq transient-mark-mode t)
(setq which-function-mode t)
(setq x-select-enable-clipboard t)
(setq resize-mini-windows nil)
(setq compilation-ask-about-save -1)
(setq after-find-file-from-revert-buffer)
(setq c-default-style "linux" c-basic-offset 2)
(setq vc-handled-backends nil)
(setq ido-save-directory-list-file "~/.emacs.d/desktop-save/ido-last.txt")
(setq ido-save-history nil )
(setq ido-enable-flex-matching t)
(setq split-height-threshold nil)(setq split-width-threshold 0)
(setq split-height-threshold 0)(setq split-width-threshold nil)
(setq search-highlight t)
(setq require-final-newline t)
(setq kill-ring-max 2000);; Set delete record
(setq minibuffer-message-timeout 3)
(setq default-tab-width 2)
(setq truncat-lines -1)
(setq hippie-expand-verbose t)
(setq visual-bell -1)  ;;; Disable system beep
(setq enable-recursive-minibuffers t)
(setq kill-whole-line t) ;; C-k kill whole line including lind end
(setq column-number-mode t)   ;; Display Cursor Location
(setq line-number-mode t)
(setq display-time-12hr-format t);;;; Display Time
(setq display-time-day-and-date t)
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening
(setq scroll-margin 3  scroll-conservatively 10000)
(setq frame-title-format (list "%b %p  [%f] " (getenv "USERNAME") " %s %Z   " emacs-version))
(setq standard-indent 2)
(setq message-log-max 512)
(setq comint-buffer-maximum-size 10240)
(setq font-lock-maximum-decoration t)

(setq-default truncate-partial-width-windows -1)
(setq-default fill-column 72)    ;; Set Fill Column and auto fill
(fset 'yes-or-no-p 'y-or-n-p)  ;; ask by y or n
(custom-set-variables
 '(ido-enable-last-directory-history -1)
 '(ido-record-commands nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0))

;; ==================== Large Packages ====================
;; ==================== Auto complete ====================
 (add-to-list 'load-path "~/.emacs.d/plugins/auto-complete-131")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete-131/ac-dict")
(ac-config-default)
(require 'auto-complete-extension)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
(custom-set-variables
 '(ac-trigger-key "TAB")
 '(ac-auto-start t)
 '(ac-use-menu-map t))



;; ==================== Simple Packages ====================
(require 'tramp)  (setq tramp-default-method "ssh")
(require 'color-theme-single) (color-theme-arjen)
(require 'unicad)
(require 'autopair) (autopair-global-mode t) ;; to enable in all buffers
(require 'auto-pair+) (global-unset-key "\C-m")
(require 'highlight-sexp)
(require 'icomplete+)
(require 'iswitchb-fc)
(require 'isearch+)
(require 'browse-kill-ring+)
(require 'auto-show)(auto-show-mode 1)(setq-default auto-show-mode t)
(require 'redo+)(global-set-key (kbd "C-S-z") 'redo)
(require 'buffcycle) 
(require 'ebs)(ebs-initialize)(global-set-key [(control tab)] 'ebs-switch-buffer)
;; ==================== Change Cursor ====================
(require 'cursor-chg)  ; Load the library
(toggle-cursor-type-when-idle 1) ; Turn on cursor change when Emacs is idle
(change-cursor-mode 1) ; Turn on change for overwrite, read-only, and input mode
(curchg-change-cursor-when-idle-interval 5) ; change the idle timer
;;================================================================================

;; ==================== show paren ====================
(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-mismatch-face "red")
(set-face-foreground 'show-paren-mismatch-face nil)
(set-face-background 'show-paren-match-face nil)
(set-face-foreground 'show-paren-match-face "green")
(set-face-attribute 'show-paren-match-face nil
        :weight 'extra-bold :underline t :overline nil :slant 'oblique)
(set-face-attribute 'show-paren-mismatch-face nil
	:strike-through t
        :weight 'black :underline nil :overline nil :slant 'oblique)
(defadvice show-paren-function
      (after show-matching-paren-offscreen activate)
      "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
      (interactive)
      (if (not (minibuffer-prompt))
          (let ((matching-text nil))
            ;; Only call `blink-matching-open' if the character before point
            ;; is a close parentheses type character. Otherwise, there's not
            ;; really any point, and `blink-matching-open' would just echo
            ;; "Mismatched parentheses", which gets really annoying.
            (if (char-equal (char-syntax (char-before (point))) ?\))
                (setq matching-text (blink-matching-open)))
            (if (not (null matching-text))
                (message matching-text)))))		 
;;================================================================================


;; ==================== Yasnippet ====================
;; (add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
;; (setq yas-snippet-dirs '("~/.emacs.d/plugins/yasnippet" "~/.emacs.d/plugins/yasnippet/snippets"))
;; (require 'yasnippet)
;; ;; (setq yas-snippet-dirs '("~/.emacs.d/plugins/yasnippet" "~/.emacs.d/plugins/yasnippet/snippets"))
;; (yas-global-mode 1)
;; (defadvice ac-common-setup (after give-yasnippet-highest-priority activate)
;;   (setq ac-sources (delq 'ac-source-yasnippet ac-sources))
;;   (add-to-list 'ac-sources 'ac-source-yasnippet))
;;================================================================================

;; ==================== color-theme-660 ====================
;; (add-to-list 'load-path "~/.emacs.d/plugins/color-theme-660")
;; (add-to-list 'load-path "~/.emacs.d/plugins/color-theme-660/themes")
;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)
;;      (color-theme-arjen)))
;;================================================================================


;; ==================== Not Used ====================
;; (add-to-list 'load-path "~/.emacs.d/plugins/netlist-modes")
;; (require 'spectre-mode)
;; (add-to-list 'auto-mode-alist '("\\.scs$" . spectre-mode))
;; (require 'spice-mode)
;; (add-to-list 'auto-mode-alist '("\\.sp$" . spice-mode))
;; (add-to-list 'auto-mode-alist '("\\.cir$" . spice-mode))
;; ==================== icicles ====================
;; (add-to-list 'load-path "~/.emacs.d/plugins/icicles")(require 'icicles)
;; ==================================================================
;; (require 'auto-complete-yasnippet)
;; (require 'hungry-delete) (turn-on-hungry-delete-mode)
;; (require 'dired-lis)
;; (require 'smart-compile)
;; (require 'compile-dwim)
;; (require 'smart-operator)x
;; (require 'kill-ring-ido)

(load-file "~/.emacs.d/my_key_settings.el")

;; ==================== Autosave and backup ====================
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/\\1" t)))
  '(backup-directory-alist '((".*" . "~/.emacs.d/auto-save-list/"))))
;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/auto-save-list/" t)
(setq-default make-backup-files)
(setq-default make-backup-files nil)
(setq-default make-temp-files nil)
(setq auto-save-default nil)  ; disable # files%p
(setq make-backup-files nil) ;; do not make backup files
(auto-save-mode nil)
(setq kept-new-versions 10 ;; Enable versioning with modified values
      kept-old-versions 5
			version-control t
      delete-old-versions t
      backup-by-copying nil)
(setq backup-directory-alist `((".*" . "~/.emacs.d/auto-save-list/")))
(setq auto-save-file-name-transforms `((".*" , "~/.emacs.d/auto-save-list" t)))
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving
(setq temporary-file-directory  "~/.emacs.d/auto-save-list/")
;; ==================== Hooks ====================
(add-hook 'before-save-hook
          '(lambda () ;;create directory before saving
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory buffer-file-name) t))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'emacs-lisp-mode-hook (lambda () (define-key emacs-lisp-mode-map "\C-\\" 'eval-last-sexp)))
(remove-hook 'coding-hook 'turn-on-hl-line-mode)				 
(message "Loading Acommplished")
