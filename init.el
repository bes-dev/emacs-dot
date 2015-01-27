(require 'cl-lib)
(require 'dired-x)

;; lose UI early
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; no splash screen
(setq inhibit-startup-message t)
(setq mac-command-modifier 'meta)
(setq initial-scratch-message nil)

;; ------------------------------------------------------------
;; Set Dark GUI Theme
;; ------------------------------------------------------------
(load-theme 'tango-dark)
(enable-theme 'tango-dark)
;; make background a little darker
(set-background-color "#1d1f21")

;; ------------------------------------------------------------
;; EXTERNAL PACKAGES
;; initialization
;; ------------------------------------------------------------

(setq package-archives '(("org-mode" . "http://orgmode.org/elpa/")
			 ("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ))
(setq package-enable-at-startup nil)
(package-initialize)
;; check if the required packages are installed; suggest installing if not
(map-y-or-n-p
 "Package %s is missing. Install? "
 '(lambda (package)
    ;; for some reason, package-install doesn't work well if you
    ;; won't call package-refresh-contents beforehand
    (unless (boundp '--package-contents-refreshed-on-init)
      (package-refresh-contents)
      (setq --package-contents-refreshed-on-init 1))
    (package-install package))
 (cl-remove-if 'package-installed-p
	       '(
		 dired-details
		 window-numbering
		 revive
		 cmake-mode
		 auto-complete-clang
		 yasnippet
		 auto-complete
		 auto-complete-c-headers
		 magit
		 multiple-cursors
		 ))
 '("package" "packages" "install"))

;; define translations
(define-key key-translation-map [?\C-h] [?\C-?]) ;; translate C-h to DEL

;; ------------------------------------------------------------
;; built-in
;; ------------------------------------------------------------
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'ibuffer nil t)

;; ibuffer groups
(setq-default ibuffer-saved-filter-groups
	      (quote (("default"
		       ("org"  (mode . org-mode))
		       ("dired" (mode . dired-mode))
		       ("D" (mode . d-mode))
		       ("C/C++" (or
				 (mode . cc-mode)
				 (mode . c-mode)
				 (mode . c++-mode)))
		       ("magit" (name . "^\\*magit"))
		       ("Markdown" (mode . markdown-mode))
		       ("emacs" (name . "^\\*Messages\\*$"))
		       ("shell commands" (name . "^\\*.*Shell Command\\*"))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(global-set-key (kbd "\C-x \C-b") 'ibuffer)

;; ------------------------------------------------------------
;; Save backup files into specialize directory
;; ------------------------------------------------------------
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

;;----------------------------------------------------------------------------
;; Enable automatic spell checking
;;----------------------------------------------------------------------------
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-mode)

;; dired-details
(eval-after-load "dired-details-autoloads"
  '(progn
     (when (require 'dired-details nil t)
       (add-hook 'dired-mode-hook
		 '(lambda ()
		    (dired-details-install)
		    (setq dired-details-nhidden-string "--- ")
		    (define-key dired-mode-map (kbd "h") 'dired-details-toggle))))))

(eval-after-load "window-numbering-autoloads"
  '(progn
     (if (require 'window-numbering nil t)
	 (window-numbering-mode 1)
       (warn "window-numbering-mode not found"))))

;; ------------------------------------------------------------
;; org-mode
;; ------------------------------------------------------------
(add-hook 'org-mode-hook (lambda ()
                           (toggle-truncate-lines -1)))

;; ------------------------------------------------------------
;; multiple-cursor
;; ------------------------------------------------------------
(eval-after-load "multiple-cursors-autoloads"
  '(progn
     (when (require 'multiple-cursors nil t)
       (defun mc/mark-all-dispatch ()
         "- add a fake cursor at current position

- call mc/edit-lines if multiple lines are marked

- call mc/mark-all-like-this if marked region is on a single line"
         (interactive)
         (cond
          ((not (region-active-p))
           (mc/create-fake-cursor-at-point)
           (mc/maybe-multiple-cursors-mode))
          ((> (- (line-number-at-pos (region-end))
                 (line-number-at-pos (region-beginning))) 0)
           (mc/edit-lines))
          (t
           (mc/mark-all-like-this))))

       (defun mc/align ()
         "Aligns all the cursor vertically."
         (interactive)
         (let ((max-column 0)
               (cursors-column '()))
           (mc/for-each-cursor-ordered
            (mc/save-excursion
             (goto-char (overlay-start cursor))
             (let ((cur (current-column)))
               (setq cursors-column (append cursors-column (list cur)))
               (setq max-column (if (< max-column cur) cur max-column)))))

           (defun mc--align-insert-times ()
             (interactive)
             (dotimes (_ times)
               (insert " ")))

           (mc/for-each-cursor-ordered
            (let ((times (- max-column (car cursors-column))))
              (mc/execute-command-for-fake-cursor 'mc--align-insert-times cursor))
            (setq cursors-column (cdr cursors-column)))))

       (setq mc/list-file "~/.mc-lists.el")
       (load mc/list-file t) ;; load, but no errors if it does not exist yet please

       (global-set-key (kbd "C->")  'mc/mark-next-like-this)
       (global-set-key (kbd "C-<")  'mc/mark-previous-like-this)

       (global-set-key (kbd "M-@") 'mc/mark-all-dispatch)
       (global-set-key (kbd "M-#") 'mc/insert-numbers)
       (global-set-key (kbd "M-'") 'mc/align))))

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; navigation between buffers
(global-set-key "\C-x\C-p" 'previous-buffer)
(global-set-key "\C-x\C-n" 'next-buffer)
(global-set-key "\C-x\C-\\" 'other-window)

(require 'cc-mode)
(global-set-key (kbd "C-x C-i") 'linum-mode)

(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "~/CMake/Auxiliary/cmake-mode.el" t)

;;;; tab as 4 spaces
(setq tab-width 4)

;;;; spaces instead tabs
(setq c-basic-indent 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
