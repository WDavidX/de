

;; ================== Some functions==============
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
;; In practice, this means that they can move the
;; point to the beginning or end of the buffer.
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
    (if (string= suffix "c") (setq compiler (concat "gcc -g -Wall -o " progname " ")))
    (if (or (string= suffix "cc") (string= suffix "cpp")) (setq compiler (concat "g++ -g -Wall -o " progname " ")))
    (if (string= suffix "tex") (setq compiler "pdflatex "))
    (if (string= suffix "py") (setq compiler "python "))
    (compile (concat compiler filename))))

(defun comment-dwim-line (&optional arg)
        "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank
        and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim,
        when it inserts comment at the end of the line."
          (interactive "*P")
          (comment-normalize-vars)
          (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
              (comment-or-uncomment-region (line-beginning-position) (line-end-position))
            (comment-dwim arg)))

(defadvice show-paren-function
      (after show-matching-paren-offscreen activate)     "If the matching paren is offscreen, show the matching line in the
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

;; ============ iswitch-buffer settings ==============
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
(setq iswitchb-buffer-ignore '("^ " "*Message*" "*Compile-log*" "*Help*" "*Ibuffer"))
;; ========= Keyboard Definition ==========
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
(normal-erase-is-backspace-mode 1)
(global-set-key [(f2)] 'set-mark-command)    ;set F2 as set mark
(define-key isearch-mode-map '[backspace] 'isearch-delete-char)

;; ==================== Screen Settings========================
(defun window-half-height ()
     (max 1 (/ (1- (window-height (selected-window))) 2)))

(defun scroll-up-half ()
     (interactive)
     (scroll-up (window-half-height)))

(defun scroll-down-half ()
     (interactive)
     (scroll-down (window-half-height)))

(defun emacs-maximize ()
  "Maximize emacs window in windows os"
  (interactive)
  (w32-send-sys-command 61488))        ; WM_SYSCOMMAND #xf030 maximize
(defun emacs-minimize ()
  "Minimize emacs window in windows os"
  (interactive)
  (w32-send-sys-command #xf020))    ; #xf020 minimize
(defun emacs-normal ()
  "Normal emacs window in windows os"
  (interactive)
  (w32-send-sys-command #xf120))    ; #xf120 normalimize

;; (defun set-frame-size-according-to-resolution ()
;;   (interactive)
;;   (if window-system
;;   (progn
;;     ;; use 120 char wide window for largeish displays
;;     ;; and smaller 80 column windows for smaller displays
;;     ;; pick whatever numbers make sense for you
;;     (if (> (x-display-pixel-width) 1280)
;;            (add-to-list 'default-frame-alist (cons 'width 120))
;;            (add-to-list 'default-frame-alist (cons 'width 80)))
;;     ;; for the height, subtract a couple hundred pixels
;;     ;; from the screen height (for panels, menubars and
;;     ;; whatnot), then divide by the height of a char to
;;     ;; get the height we want
;;     (add-to-list 'default-frame-alist
;;          (cons 'height (/ (- (x-display-pixel-height) 200)
;;                              (frame-char-height)))))))
;(set-frame-position current-frame 0 0)
;(set-frame-size-according-to-resolution)

;(defun toggle-fullscreen ()
 ; (interactive)
;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
;  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
;	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
;)
;(toggle-fullscreen)
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
;; ==================== Coding ====================
(setq buffer-file-coding-system 'utf-8-unix)
(setq default-file-name-coding-system 'utf-8-unix)
(setq default-keyboard-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
(setq default-sendmail-coding-system 'utf-8-unix)
(setq default-terminal-coding-system 'utf-8-unix)
;;--------------------------------------------------------------------
;; Function Keys
(global-set-key "\C-q" 'comment-dwim-line)
(global-set-key "\C-o" '(lambda() (interactive) (switch-to-buffer (other-buffer))))
(global-set-key [insert] 'onekey-compile)
(global-set-key "\C-\\" 'eval-last-sexp)
(global-set-key [(f8)] 'open-eshell-other-buffer)
(global-unset-key [(f9)]) (global-set-key [(f9)] (lambda()(interactive) (switch-to-buffer "*scratch*")))
(global-unset-key [(f10)]) (global-set-key [(f10)] (lambda() (interactive) (find-file "~/.emacs.d/my_key_settings.el")))
(global-unset-key [(f11)]) (global-set-key [(f11)] (lambda() (interactive) (find-file "~/.emacs.d/.emacs")))
(global-set-key [(f12)] (lambda() (interactive)(save-some-buffers (buffer-file-name))(eval-buffer))) ;; evaluate buffer
(global-unset-key [(f1)])
(global-set-key [(f1)] 'onekey-compile)

;;======== The following messes up with original settings
;; ;(global-set-key "\C-l" 'forward-char)
;; (global-set-key "\C-k" 'backward-char)
;; (global-set-key "\C-l" 'forward-char)
;; (global-set-key "\M-k" 'backward-word)
;; (global-set-key "\M-l" 'forward-word)
;; ;(global-set-key "\C-u" 'scroll-up)
;; ;(global-set-key "\C-i" 'scroll-down)
;; ;; (global-set-key "\C-u" '(lambda() (forward-line 1))
;; (global-set-key "\M-i"
;;   (lambda () (interactive)
;;     (condition-case nil (scroll-up)
;;       (end-of-buffer (goto-char (point-max))))))
;; (global-set-key "\M-j"
;;   (lambda () (interactive)
;;     (condition-case nil (scroll-down)
;;       (beginning-of-buffer (goto-char (point-min))))))

;; (global-set-key "\C-i" '(lambda() (interactive)(forward-line -1)))
;; (global-set-key "\C-j" '(lambda() (interactive)(forward-line 1)))

;; (global-set-key "\M-p" 'beginning-of-buffer)
;; (global-set-key "\M-n" 'end-of-buffer)
;; (global-set-key "\C-p" 'previous-line)
;; (global-set-key "\C-n" 'next-line)

;; (global-set-key "\C-f" 'delete-backward-char)
;; (global-set-key "\M-f" 'backward-kill-word)
;; (global-set-key "\M-w" 'kill-ring-save)
;; (global-set-key "\C-w" 'kill-region)
;; (global-set-key "\C-v" 'yank)
;; (global-set-key "\M-v" 'yank-pop)
;; (global-set-key "\C-y" 'yank)
;; (global-set-key "\C-b" 'kill-line)

;; ======================= Windows Fonts =======================
;; (if (eq window-system 'w32) (set-frame-font "Bitstream Vera Sans 14") )
;; (if (eq window-system 'w32) (set-frame-font "Inconsolata 14") )
;; (if (eq window-system 'w32) (set-frame-font "Lucida Sans Typewriter 14") )
;; (if (eq window-system 'w32) (set-frame-font "Lucida Console 14") )
;; (if (eq window-system 'w32) (set-frame-font "Monaco 14") )  ;good
(if (eq window-system 'w32) (set-frame-font "Monaco 12") )  ;good
;; (if (eq window-system 'w32) (set-frame-font "Anonymous 12") ) ;good
;; (if (eq window-system 'w32) (set-frame-font "DejaVu Sans Mono 14") )
;; (if (eq window-system 'w32) (set-frame-font "Consolas 11") ) ;good
