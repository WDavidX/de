;http://xenon.stanford.edu/~manku/emacs.html

; Author: Gurmeet Singh Manku (manku_AT_stanford_DOT_edu)
; Last edited on 23 Sep 2001

;; Set fonts
;(set-default-font "adobe-courier-medium-r-*--*-120-*-*-m-*-iso8859-1")

;; add my directory to load-path
(setq load-path (cons "~/emacs" load-path))
;; common-lisp
(require 'cl)
;; AucTex stuff
(require 'tex-site)
;; better highlighting in c-mode
(require 'ctypes)

;; inhibit startup message
(setq inhibit-startup-message t)

;; dont want too many messages
(setq message-log-max 512)

;; makes ctrl, shift, alt sticky in xemacs
(setq modifier-keys-are-sticky t)

;; last lines should end in a carriage return
(setq require-final-newline t)

;; display date and time always
(setq display-time-day-and-date t)
(display-time)

;; highlight matching parentheses next to cursor
(require 'paren)
(show-paren-mode t)

;; type "y"/"n" instead of "yes"/"no"
(fset 'yes-or-no-p 'y-or-n-p)

;; do not add new lines with arrow down at end of buffer
(setq next-line-add-newlines nil)

;; C-k kills whole line and newline if at beginning of line
(setq kill-whole-line t)

;; in C mode, delete hungrily
(setq c-hungry-delete-key t)

;; spaces instead of tabs by default
(setq-default indent-tabs-mode nil)

; let emacs put in a return for you after left curly braces,
; right curly braces, and semi-colons.
(setq c-auto-newline 1)

;; long lines and horizontal scrolling stuff:
;;
;; truncate lines if they are too long
(setq-default truncate-lines t)
;; trucate even even when screen is split into multiple windows
(setq-default truncate-partial-width-windows nil)
;; load auto-show (shows lines when cursor moves to right of long line)
(require 'auto-show)
(auto-show-mode 1)
(setq-default auto-show-mode t)
;; position cursor to end of output in shell mode
(auto-show-make-point-visible)

; highlight region between point and mark
(transient-mark-mode t)
; highlight during query
(setq query-replace-highlight t)
; highlight incremental search
(setq search-highlight t)

;; set maximum-buffer size for shell-mode
(setq comint-buffer-maximum-size 10240)
;; truncate shell buffer to comint-buffer-maximum-size
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)
;; dont show passwords in clear text
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)
;; remove ctrl-m from shell output
(add-hook 'comint-output-filter-functions
          'comint-strip-ctrl-m)

;; make text-mode default
(setq default-major-mode 'text-mode)

;; we want fontification in all modes
(global-font-lock-mode t t)
;; maximum possible fontification
(setq font-lock-maximum-decoration t)

;; .tex files should be handled by latex-mode
(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (modify-syntax-entry ?- "w")       ; now '-' is not considered a word-delimiter
            ))

(add-hook 'c-mode-common-hook
          (lambda ()
            (turn-on-auto-fill)
            (setq fill-column 80)
            (setq comment-column 60)
            (modify-syntax-entry ?_ "w")       ; now '_' is not considered a word-delimiter
            (c-set-style "whitesmith")         ; set indentation style
            (local-set-key [(control tab)]     ; move to next tempo mark
                           'tempo-forward-mark)
            (local-set-key "\M-u"              ; capitalize current word (c constants)
                           '(lambda () (interactive) (backward-word 1) (upcase-word 1)))
            ))

(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (auto-fill-mode 1)
             ))

(add-hook 'shell-mode-hook
	  '(lambda ()
             (local-set-key [home]        ; move to beginning of line, after prompt
                            'comint-bol)
	     (local-set-key [up]          ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
	     (local-set-key [down]        ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))
             ))

(add-hook 'gud-mode-hook
	  '(lambda ()
             (local-set-key [home]        ; move to beginning of line, after prompt
                            'comint-bol)
	     (local-set-key [up]          ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
	     (local-set-key [down]        ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))
             ))

(setq tex-mode-hook
   '(lambda ()
      (auto-fill-mode 1)
      ))

(setq latex-mode-hook
   '(lambda ()
      (auto-fill-mode 1)
      ))

(setq my-key-pairs
      '((?! ?1) (?@ ?2) (?# ?3) (?$ ?4) (?% ?5)
        (?^ ?6) (?& ?7) (?* ?8) (?( ?9) (?) ?0)
        (?- ?_) (?\" ?') (?{ ?[) (?} ?])         ; (?| ?\\)
        ))

(defun my-key-swap (key-pairs)
  (if (eq key-pairs nil)
      (message "Keyboard Zapped!!")
      (progn
        (keyboard-translate (caar key-pairs)  (cadar key-pairs))
        (keyboard-translate (cadar key-pairs) (caar key-pairs))
        (my-key-swap (cdr key-pairs))
        )
    ))

(defun my-key-restore (key-pairs)
  (if (eq key-pairs nil)
      (message "Keyboard Restored!!")
      (progn
        (keyboard-translate (caar key-pairs)  (caar key-pairs))
        (keyboard-translate (cadar key-pairs) (cadar key-pairs))
        (my-key-restore (cdr key-pairs))
        )
    ))

;; global key bindings
(global-set-key [C-delete]    'kill-word)
(global-set-key [C-backspace] 'backward-kill-word)
(global-set-key [home]        'beginning-of-line)
(global-set-key [end]         'end-of-line)
(global-set-key [C-home]      'beginning-of-buffer)
(global-set-key [C-end]       'end-of-buffer)
(global-set-key [f1]          'find-file)
(global-set-key [f3]          'manual-entry)
(global-set-key [f4]          'shell)
(global-set-key [f5]          '(lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key [S-f7]        'compile)
(global-set-key [f7]          'next-error)
(global-set-key [C-f7]        'kill-compilation)
(global-set-key [f8]          'other-window)
(global-set-key [f9]          'save-buffer)
(global-set-key [f10]         '(lambda () (interactive) (my-key-swap    my-key-pairs)))
(global-set-key [S-f10]       '(lambda () (interactive) (my-key-restore my-key-pairs)))
(global-set-key [C-f12]       'save-buffers-kill-emacs)
(global-set-key "\C-x\C-b"    'electric-buffer-list)
; Make Emacs use "newline-and-indent" when you hit the Enter key so
; that you don't need to keep using TAB to align yourself when coding.
(global-set-key "\C-m"        'newline-and-indent)

(defun my-editing-function (first last len)
  (interactive)
  (if (and (boundp 'major-mode)
           (member major-mode (list 'c-mode 'c++-mode 'gud-mode 'fundamental-mode))
           (= len 0)
           (> (point) 4)
           (= first (- (point) 1)))
      (cond
       ((and (string-equal (buffer-substring (point) (- (point) 2)) "__")
             (not (string-equal (buffer-substring (point) (- (point) 3)) "___")))
        (progn (delete-backward-char 2) (insert-char ?- 1) (insert-char ?> 1)))

       ((string-equal (buffer-substring (point) (- (point) 3)) "->_")
        (progn (delete-backward-char 3) (insert-char ?_ 3)))

       ((and (string-equal (buffer-substring (point) (- (point) 2)) "..")
             (not (string-equal (buffer-substring (point) (- (point) 3)) "...")))
        (progn (delete-backward-char 2) (insert-char ?[ 1) (insert-char ?] 1) (backward-char 1)))

       ((and (> (point-max) (point))
             (string-equal (buffer-substring (+ (point) 1) (- (point) 2)) "[.]"))
        (progn (forward-char 1) (delete-backward-char 3) (insert-char ?. 1) (insert-char ?. 1) ))
       )
    nil))

(add-hook 'after-change-functions 'my-editing-function)

;; This is a way to hook tempo into cc-mode
(defvar c-tempo-tags nil
  "Tempo tags for C mode")
(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

;;; C-Mode Templates and C++-Mode Templates (uses C-Mode Templates also)
(require 'tempo)
(setq tempo-interactive t)

(add-hook 'c-mode-hook '(lambda ()
                                (local-set-key [f11] 'tempo-complete-tag)))
(add-hook 'c-mode-hook '(lambda ()
			  (tempo-use-tag-list 'c-tempo-tags)
			  ))
(add-hook 'c++-mode-hook '(lambda ()
			    (tempo-use-tag-list 'c-tempo-tags)
			    (tempo-use-tag-list 'c++-tempo-tags)
			    ))

;;; Preprocessor Templates (appended to c-tempo-tags)

(tempo-define-template "c-include"
		       '("include <" r ".h>" > n
			 )
		       "include"
		       "Insert a #include <> statement"
		       'c-tempo-tags)

(tempo-define-template "c-ifdef"
		       '("ifdef " (p "ifdef-clause: " clause) > n> p n
			 "#else /* !(" (s clause) ") */" n> p n
			 "#endif /* " (s clause)" */" n>
			 )
		       "ifdef"
		       "Insert a #ifdef #else #endif statement"
		       'c-tempo-tags)

(tempo-define-template "c-ifndef"
		       '("ifndef " (p "ifndef-clause: " clause) > n
			 "#define " (s clause) n> p n
			 "#endif /* " (s clause)" */" n>
			 )
		       "ifndef"
		       "Insert a #ifndef #define #endif statement"
		       'c-tempo-tags)
;;; C-Mode Templates

(tempo-define-template "c-if"
		       '(> "if (" (p "if-clause: " clause) ")" n>
                           "{" > n>
                           > r n
                           "}" > n>
                           )
		       "if"
		       "Insert a C if statement"
		       'c-tempo-tags)

(tempo-define-template "c-else"
		       '(> "else" n>
                           "{" > n>
                           > r n
                           "}" > n>
                           )
		       "else"
		       "Insert a C else statement"
		       'c-tempo-tags)

(tempo-define-template "c-if-else"
		       '(> "if (" (p "if-clause: " clause) ")"  n>
                           "{" > n
                           > r n
                           "}" > n
                           "else" > n
                           "{" > n>
                           > r n
                           "}" > n>
                           )
		       "ifelse"
		       "Insert a C if else statement"
		       'c-tempo-tags)

(tempo-define-template "c-while"
		       '(> "while (" (p "while-clause: " clause) ")" >  n>
                           "{" > n
                           > r n
                           "}" > n>
                           )
		       "while"
		       "Insert a C while statement"
		       'c-tempo-tags)

(tempo-define-template "c-for"
		       '(> "for (" (p "for-clause: " clause) ")" >  n>
                           "{" > n
                           > r n
                           "}" > n>
                           )
		       "for"
		       "Insert a C for statement"
		       'c-tempo-tags)

(tempo-define-template "c-for-i"
		       '(> "for (" (p "variable: " var) " = 0; " (s var)
                           " < "(p "upper bound: " ub)"; " (s var) "++)" >  n>
                           "{" > n
                           > r n
                           "}" > n>
                           )
		       "fori"
		       "Insert a C for loop: for(x = 0; x < ..; x++)"
		       'c-tempo-tags)

(tempo-define-template "c-main"
		       '(> "int main(int argc, char *argv[])" >  n>
                           "{" > n>
                           > r n
                           > "return 0 ;" n>
                           > "}" > n>
                           )
		       "main"
		       "Insert a C main statement"
		       'c-tempo-tags)

(tempo-define-template "c-if-malloc"
		       '(> (p "variable: " var) " = ("
                           (p "type: " type) " *) malloc (sizeof(" (s type)
                           ") * " (p "nitems: " nitems) ") ;" n>
                           > "if (" (s var) " == NULL)" n>
                           > "error_exit (\"" (buffer-name) ": " r ": Failed to malloc() " (s var) " \") ;" n>
                           )
		       "ifmalloc"
		       "Insert a C if (malloc...) statement"
		       'c-tempo-tags)

(tempo-define-template "c-if-calloc"
		       '(> (p "variable: " var) " = ("
                           (p "type: " type) " *) calloc (sizeof(" (s type)
                           "), " (p "nitems: " nitems) ") ;" n>
                           > "if (" (s var) " == NULL)" n>
                           > "error_exit (\"" (buffer-name) ": " r ": Failed to calloc() " (s var) " \") ;" n>
                           )
		       "ifcalloc"
		       "Insert a C if (calloc...) statement"
		       'c-tempo-tags)

(tempo-define-template "c-switch"
		       '(> "switch (" (p "switch-condition: " clause) ")" n>
                           "{" >  n>
                           "case " (p "first value: ") ":" > n> p n
                           "break;" > n> p n
                           "default:" > n> p n
                           "break;" > n
                           "}" > n>
                           )
		       "switch"
		       "Insert a C switch statement"
		       'c-tempo-tags)

(tempo-define-template "c-case"
		       '(n "case " (p "value: ") ":" > n> p n
			   "break;" > n> p
			   )
		       "case"
		       "Insert a C case statement"
		       'c-tempo-tags)

; ----- VM --------
(autoload 'vm "~/emacs/vm" "Start VM on your primary inbox." t)
(autoload 'vm-visit-folder "~/emacs/vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-mail "~/emacs/vm" "Send a mail message using VM." t)
(autoload 'vm-submit-bug-report "~/emacs/vm" "Send a bug report about VM." t)

;(setq rebox-default-style '233)
;(add-hook 'c-mode-hook 'fp-c-mode-routine)
;(defun fp-c-mode-routine ()
;  (local-set-key "\M-q" 'rebox-comment))
;(load "~/emacs/rebox")
;;(autoload 'rebox-comment "~/emacs/rebox" nil t)
;;(autoload 'rebox-region "~/emacs/rebox" nil t)

;(cond (window-system
;           (setq hilit-mode-enable-list  '(not text-mode)
;                 hilit-background-mode   'dark
;                 hilit-inhibit-hooks     nil
;                 hilit-inhibit-rebinding nil)
;           (require 'hilit19)))

;(require 'hl319)

;(load "~/comint-smart.el")

;(setq-default abbrev-mode t)
;((if (file-exists-p  "~/.abbrev_defs") (read-abbrev-file "~/.abbrev_defs"))
;(setq save-abbrevs t)

;; table mode for easier creation of tables
;; Uncomment the following if u want to use tables
;; (require 'table)
;; uncomment the following if u desire automatic detection of tables
;; if the file is huge, it delays loading file.. so turn it off in general
;; (add-hook 'text-mode-hook 'table-recognize)

(autoload 'reftex-mode    "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" t)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
;  (add-hook 'latex-mode-hook 'turn-on-reftex)) ; with Emacs latex mode

(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-use-multiple-selection-buffers t)

;; To integrate with AUCTeX, use
(setq reftex-plug-into-AUCTeX t)

;; get intermittent messages to stop typing
(type-break-mode)
;; allow recursive editing in minibuffer
(setq enable-recursive-minibuffers t)
;; minibuffer gets resized if it becomes too big
(resize-minibuffer-mode 1)
;; follow-mode allows easier editing of long files
(follow-mode t)
;; want two windows at startup
(split-window-horizontally)
;; move to other window
(other-window 1)
;; start a shell
(shell)
;; move back to first window
(other-window 1)

; Author: Gurmeet Singh Manku (manku_AT_stanford_DOT_edu)
; Last edited on 23 Sep 2001
