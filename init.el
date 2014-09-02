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
		 sr-speedbar
		 revive
		 google-translate
		 cmake-mode
		 cedit
		 ecb
		 auto-complete-clang
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

;; ------------------------------------------------------------
;; sr-speedbar
;; ------------------------------------------------------------
(require 'sr-speedbar)
(custom-set-variables '(speedbar-show-unknown-files t))
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)

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
		    (setq dired-details-hidden-string "--- ")
		    (define-key dired-mode-map (kbd "h") 'dired-details-toggle))))))

(eval-after-load "window-numbering-autoloads"
  '(progn
     (if (require 'window-numbering nil t)
	 (window-numbering-mode 1)
       (warn "window-numbering-mode not found"))))


;; ------------------------------------------------------------
;; ADVICES
(defadvice insert-for-yank-1 (after indent-region activate)
  "Indent yanked region in certain modes, C-u prefix to disable"
  (if (and (not current-prefix-arg)
	   (member major-mode '(sh-mode
				emacs-lisp-mode lisp-mode
				c-mode c++-mode objc-mode d-mode java-mode cuda-mode
				LaTeX-mode TeX-mode
				xml-mode html-mode css-mode)))
      (indent-region (region-beginning) (region-end) nil)))

(eval-after-load "revive-autoloads"
  '(progn
     (when (require 'revive nil t)
       (defun revive-save-window-configuration ()
         (interactive)
         (save-window-excursion
           (let ((config (prin1-to-string (current-window-configuration-printable))))
             (find-file "~/.revive-windows.el")
             (erase-buffer)
             (insert config)
             (save-buffer))))
       (defun revive-restore-window-configuration ()
         (interactive)
         (let ((config))
           (save-window-excursion
             (find-file "~/.revive-windows.el")
             (beginning-of-buffer)
             (setq config (read (current-buffer)))
             (kill-buffer))
           (restore-window-configuration config)))
       (define-key ctl-x-map "S" 'revive-save-window-configuration)
       (define-key ctl-x-map "R" 'revive-restore-window-configuration)
       (revive-restore-window-configuration))))

;; google translate
(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key "\C-ct" 'google-translate-smooth-translate)


;; ------------------------------------------------------------
;; IDE for C/C++
;; ------------------------------------------------------------

(require 'cc-mode)

(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;;; activate ecb
(require 'ecb)
(require 'ecb-autoloads)

;;; configure ecb
(custom-set-variables '(ecb-options-version "2.40"))
(custom-set-faces)
(setq ecb-examples-bufferinfo-buffer-name nil)
(setq ecb-tip-of-the-day nil)
;;; window style
(setq ecb-layout-name "left3")
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-compile-window-height 12)
;;; key bindings
;;; activate and deactivate ecb
(global-set-key (kbd "C-x C-;") 'ecb-activate)
(global-set-key (kbd "C-x C-'") 'ecb-deactivate)
;;; show/hide ecb window
(global-set-key (kbd "C-;") 'ecb-show-ecb-windows)
(global-set-key (kbd "C-'") 'ecb-hide-ecb-windows)
;;; quick navigation between ecb windows
(global-set-key (kbd "C-)") 'ecb-goto-window-edit1)
(global-set-key (kbd "C-!") 'ecb-goto-window-directories)
(global-set-key (kbd "C-@") 'ecb-goto-window-sources)
(global-set-key (kbd "C-#") 'ecb-goto-window-methods)
(global-set-key (kbd "C-$") 'ecb-goto-window-compilation)

;; auto-complete
(require 'auto-complete-clang)
(define-key c++-mode-map (kbd "C-M-<return>") 'ac-complete-clang) ;; replace C-S-<return> with a key binding that you want

; Add cmake listfile names to the mode list.
(require 'cmake-mode)
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "~/CMake/Auxiliary/cmake-mode.el" t)

(defun parent-directory (dir)
  "Returns parent directory of dir"
  (when dir
	(file-name-directory (directory-file-name (expand-file-name dir)))))
(defun search-file-up (name &optional path)
  "Searches for file `name' in parent directories recursively"
  (let* ((file-name (concat path name))
		 (parent (parent-directory path))
		 (path (or path default-directory)))
	(cond
	 ((file-exists-p file-name) file-name)
	 ((string= parent path) nil)
	 (t (search-file-up name parent)))))

(defun update-tags-file (arg)
  "Suggests options to update the TAGS file via ctags.
With prefix arg - makes a call as sudo. Works for remote hosts
also (>=23.4)"
  (interactive "P")
  (let ((tags-file-name
		 (read-file-name
		  "TAGS file: " (let ((fn (search-file-up "TAGS" default-directory)))
						  (if fn
							  (parent-directory fn)
							default-directory))
		  nil nil "TAGS"))
		(ctags-command "")
		(languages (case major-mode
					 ((cc-mode c++-mode c-mode) "--languages=C,C++")
					 ((d-mode) "--languages=D")
					 (t ""))))
	(when tags-file-name
	  (setq ctags-command (concat ctags-command "cd " (replace-regexp-in-string ".*:" "" (file-name-directory tags-file-name)) " && ")))
	(setq ctags-command (concat ctags-command "ctags -e " languages " -R . "))
	(with-temp-buffer
	  (when arg
		(cd (add-sudo-to-filename (expand-file-name default-directory))))
	  (shell-command (read-from-minibuffer "ctags command: " ctags-command)))
	(visit-tags-table tags-file-name)))

(global-set-key "\C-x\C-u"          'update-tags-file)
(global-set-key "\C-x\C-v"          'visit-tags-table)
(global-set-key "\C-x\C-t"          'tags-reset-tags-tables)
(global-set-key "\C-x\C-l"          'tags-apropos)
