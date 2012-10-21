;; ================== Some functions==============
(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding
point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))
(defun quote-word ()
  "add double quotes to the current word"
  (interactive)
  (let (p1 p2 bds)
    (setq bds (bounds-of-thing-at-point 'word))
    (setq p1 (car bds) p2 (cdr bds))
    (goto-char p1)
    (insert "\"")
    (goto-char (+ 1 p2))
    (insert "\"")))
;; set new method of kill a whole line
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
(defun open-eshell-other-buffer ()
   "Open eshell in other buffer"
   (interactive)
   (split-window-horizontally)
   (other-window 1)
   (eshell)
   )
;; Page down/up move the point, not the screen.
(global-set-key [next]
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key [prior]
  (lambda () (interactive)
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

(defun onekey-compile ()
   "Compile current buffer"
  (save-some-buffers (buffer-file-name))
  (interactive)
  (let (filename suffix progname compiler)
    (setq filename (file-name-nondirectory buffer-file-name))
    (setq progname (file-name-sans-extension filename))
    (setq suffix (file-name-extension filename))
    (if (string= suffix "c") (setq compiler (concat "gcc -std=c99 -g -Wall -o " progname " ")))
    (if (or (string= suffix "cc") (string= suffix "cpp"))
		(setq compiler (concat "g++ -g -Wall -std=c99 -o " progname " ")))
    (if (string= suffix "tex") (setq compiler "pdflatex "))
    (if (string= suffix "py") (setq compiler "python "))
    (compile (concat compiler filename))))


;; my modified dwim, comment current line no matter where the cursor is; the commented part is original
(defun comment-dwim-line (&optional arg)
        "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank
        and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim,
        when it inserts comment at the end of the line."
          (interactive "*P")
          (comment-normalize-vars)
					;; (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
          ;;     (comment-or-uncomment-region (line-beginning-position) (line-end-position))
          ;;   (comment-dwim arg))
					(if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position))
            (comment-dwim arg))
         ;; (comment-or-uncomment-region (line-beginning-position) (line-end-position))
					)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

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

(defun kill-whitespace ()
   "Kill the whitespace between two non-whitespace characters"
   (interactive "*")
     (save-excursion
       (save-restriction
         (save-match-data
                  (progn
                  (re-search-backward "[^ \t\r\n]" nil t)
                  (re-search-forward "[ \t\r\n]+" nil t)
                  (replace-match " " nil nil))))))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun delete-backward-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))
;; ==================== iswitch-buffer settings ====================
(defun iswitchb-local-keys ()
      "Using the arrow keys to select a buffer"
      (mapc (lambda (K)
	      (let* ((key (car K)) (fun (cdr K)))
    	        (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	    '(("<right>" . iswitchb-next-match)
	      ("<left>"  . iswitchb-prev-match)
	      ("<up>"    . ignore             )
	      ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)
(setq iswitchb-buffer-ignore '("^ "  "*Compile-log*" "*Help*" "*Ibuffer" "*Completion*"))

;; ==================== Recent File ====================
;(require 'recentf)
;(setq recentf-max-menu-items 5) (setq recentf-max-saved-items 5)
;(setq recentf-save-file "~/.emacs.d/desktop-save/recentf-list.txt")
;(require 'recentf-ext)(recentf-mode t)
;(defun ido-recentf-open ()
;  "Use `ido-completing-read' to \\[find-file] a recent file"
;  (interactive)
;  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;      (message "Opening file...")
;    (message "Aborting")))
;(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

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
;; ==================== System Coding ====================
(setq buffer-file-coding-system 'utf-8-unix)
(setq default-file-name-coding-system 'utf-8-unix)
(setq default-keyboard-coding-system 'utf-8-unix)
(setq default-sendmail-coding-system 'utf-8-unix)
(setq default-terminal-coding-system 'utf-8-unix)
;; ==================== Add hooks ====================
(add-hook 'emacs-lisp-mode-hook (lambda () (define-key emacs-lisp-mode-map "\C-\\" 'eval-last-sexp)))
;; ==================== Keyboard Definition ====================
(global-set-key "\C-g" 'keyboard-escape-quit)
(global-set-key "\C-x\C-b" 'ibuffer)
(global-set-key "\C-xk" 'kill-this-buffer)
(global-set-key (kbd "C-S-k") 'kill-line)
(global-set-key (kbd "M-\'") 'split-window-horizontally)
(global-set-key (kbd "C-\"") 'delete-windows-on)

(global-set-key (kbd "C-\'") 'delete-other-windows)
(global-set-key "\M-p" 'scroll-down-line)
(global-set-key "\M-n" 'scroll-up-line)
(global-set-key (kbd "C-\;") 'recenter-top-bottom)
(global-set-key "\M-z" 'repeat-complex-command)
(global-set-key "\M-c" 'eval-region)
;;==================== The following messes up with original settings
(global-set-key "\C-o" 'other-window)
(global-set-key "\C-z" 'undo)
(global-set-key "\M-z" 'repeat-complex-command)
(require 'redo+)(global-set-key (kbd "C-S-z") 'redo)
(require 'buffcycle) (global-set-key "\C-x\C-k" 'kill-this-buffer-if-not-scratch)
(global-set-key "\C-q" '(lambda() (interactive) (switch-to-buffer (other-buffer))))
(global-set-key (kbd "C-S-b") 'kill-line)
;; (global-set-key "\C-t" 'comment-dwim-line)
(global-set-key "\C-t" 'comment-or-uncomment-region-or-line)
(global-set-key "\M-b" 'delete-blank-lines)
(global-set-key (kbd "C-v") 'yank)
(global-set-key (kbd "C-S-v") 'yank-pop)
(global-set-key (kbd "M-d") 'kill-whitespace)
;; (global-set-key (kbd "C-f") 'kill-ring-save)
(global-set-key (kbd "C-b") 'backward-delete-char)
(global-set-key (kbd "M-q") 'compile)
(global-set-key (kbd "C-S-q") 'delete-backward-word)
(global-set-key (kbd "C-S-d") 'kill-word)

(global-set-key "\C-j" 'backward-char)
(global-set-key "\C-k" 'forward-char)
(global-set-key (kbd "C-S-j") 'backward-word)
(global-set-key (kbd "C-S-k") 'forward-word)

;; (add-hook 'org-mode-hook (lambda () (define-key org-mode-map "\C-k" 'forward-char)))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\C-k" 'forward-char)
						(local-set-key (kbd "C-S-k") 'forward-word)
						(local-set-key "\C-j" 'backward-char)
						(local-set-key (kbd "C-S-j") 'backward-word)
            ;; yasnippet (allow yasnippet to do its thing in org files)
            ;; (org-set-local 'yas/trigger-key [tab])
            ;; (define-key yas/keymap [tab] 'yas/next-field-group)
						(global-visual-line-mode t)
						))

(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key "\C-k" 'forward-char)
						(local-set-key (kbd "C-S-k") 'forward-word)
						(local-set-key "\C-j" 'backward-char)
						(local-set-key (kbd "C-S-j") 'backward-word)
            ;; yasnippet (allow yasnippet to do its thing in org files)
            ;; (org-set-local 'yas/trigger-key [tab])
            ;; (define-key yas/keymap [tab] 'yas/next-field-group)
						))

(global-set-key (kbd "C-f") 'delete-backward-char)
(global-set-key (kbd "C-S-f") 'delete-backward-word)

(global-set-key (kbd "M-n")     ; page down
  (lambda () (interactive)
    (condition-case nil (scroll-up)
      (end-of-buffer (goto-char (point-max))))))

(global-set-key (kbd "M-p")
  (lambda () (interactive) ; page up
    (condition-case nil (scroll-down)
      (beginning-of-buffer (goto-char (point-min))))))

(global-set-key (kbd "C-,")  '(lambda() (interactive)(forward-line -1)))
(global-set-key  (kbd "C-.") '(lambda() (interactive)(forward-line 1)))
;; ========================= Function Keys ========================
(global-unset-key [(f1)])
(global-unset-key [(f2)])
(global-unset-key [(f9)])
(global-unset-key [(f8)])
(global-unset-key [(f10)])
(global-unset-key [(f11)])
(global-unset-key [(f12)])
(global-unset-key [backspace] )
(global-unset-key [insert])
(global-unset-key [delete] )

(global-set-key [(f1)] (lambda() (interactive)  (save-some-buffers (buffer-file-name)) (recompile)))
(global-set-key [(f2)] 'set-mark-command)    ;set F2 as set mark
(global-set-key [(f8)] 'open-eshell-other-buffer)
(global-set-key [(f9)]	(lambda()(interactive) (switch-to-buffer "*scratch*")))
(global-set-key [(f10)]	(lambda() (interactive) (find-file "~/.emacs.d/my_key_settings.el")))
(global-set-key [(f11)] 	(lambda() (interactive) (find-file "~/.emacs.d/.emacs")))
(global-set-key [(f12)] 	(lambda() (interactive)(save-some-buffers (buffer-file-name))(eval-buffer)))

(global-set-key [backspace] 'delete-backward-char)
(global-set-key [delete] 'delete-char)
(global-set-key [C-delete] 'kill-word)
(define-key global-map [home] `beginning-of-line)
(define-key global-map [end] `end-of-line)
(global-set-key [insert] 'onekey-compile)
(define-key isearch-mode-map '[backspace] 'isearch-delete-char)
(require 'ebs)(ebs-initialize)(global-set-key [(control tab)] 'ebs-switch-buffer)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; ======================= Windows Fonts =======================
(defun qiang-font-existsp (font)
  (if (null (x-list-fonts font))
      nil t))
(defvar font-list '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))
(require 'cl) ;; find-if is in common list package
(find-if #'qiang-font-existsp font-list)

(defun qiang-make-font-string (font-name font-size)
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
    (format "%s %s" font-name font-size)))

(defun qiang-set-font (english-fonts
                       english-font-size
                       chinese-fonts
                       &optional chinese-font-size)
  "english-font-size could be set to \":pixelsize=18\" or a integer.
If set/leave chinese-font-size to nil, it will follow english-font-size"
  (require 'cl)                         ; for find if
  (let ((en-font (qiang-make-font-string
                  (find-if #'qiang-font-existsp english-fonts)
                  english-font-size))
        (zh-font (font-spec :family (find-if #'qiang-font-existsp chinese-fonts)
                            :size chinese-font-size)))

    ;; Set the default English font
    ;;
    ;; The following 2 method cannot make the font settig work in new frames.
    ;; (set-default-font "Consolas:pixelsize=18")
    ;; (add-to-list 'default-frame-alist '(font . "Consolas:pixelsize=18"))
    ;; We have to use set-face-attribute
    (message "Set English Font to %s" en-font)
    (set-face-attribute
     'default nil :font en-font)

    ;; Set Chinese font
    ;; Do not use 'unicode charset, it will cause the english font setting invalid
    (message "Set Chinese Font to %s" zh-font)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        zh-font))))
;; (qiang-set-font
;;  '( "Monaco" "Consolas" "DejaVu Sans Mono" "Monospace" "Courier New") ":pixelsize=18"
;;  '("Microsoft Yahei" "文泉驿等宽微米黑" "黑体" "新宋体" "宋体"))

(if (eq window-system 'w32)
		(set-frame-font "Monaco 12")
	(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
	)  ;good


;; ======================= Windows Fonts =======================
;; (if (eq window-system 'w32) (set-frame-font "Bitstream Vera Sans 14") )
;; (if (eq window-system 'w32) (set-frame-font "Inconsolata 14") )
;; (if (eq window-system 'w32) (set-frame-font "Lucida Sans Typewriter 14") )
;; (if (eq window-system 'w32) (set-frame-font "Lucida Console 14") )
;; (if (eq window-system 'w32) (set-frame-font "Monaco 14") )  ;good
;; (if (eq window-system 'w32) (set-frame-font "Anonymous 12") ) ;good
;; (if (eq window-system 'w32) (set-frame-font "DejaVu Sans Mono 14") )
;; (if (eq window-system 'w32) (set-frame-font "Consolas 11") ) ;good

;; Font in linux
(if (eq window-system 'x) (set-frame-font "Monospace 14") )  ;good

;; Font in window


;; End of my keyboard and function settings

(message "End of key settings")
