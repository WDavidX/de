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

;; ==================== show paren ====================
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
(global-set-key [insert] 'onekey-compile)
(global-set-key "\C-\\" 'onekey-compile)
(global-set-key [(f8)] 'open-eshell-other-buffer)
(global-unset-key [(f9)]) (global-set-key [(f9)] (lambda()(interactive) (switch-to-buffer "*scratch*")))
(global-unset-key [(f10)]) (global-set-key [(f10)] (lambda() (interactive) (find-file "~/.emacs.d/my_key_settings.el")))
(global-unset-key [(f11)]) (global-set-key [(f11)] (lambda() (interactive) (find-file "~/.emacs.d/.emacs")))
(global-set-key [(f12)] (lambda() (interactive)(save-some-buffers (buffer-file-name)) (eval-buffer))) ;; evaluate buffer
(global-unset-key [(f1)])
(global-set-key [(f1)] 'onekey-compile)