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
                 yasnippet
                 magit
                 multiple-cursors
                 cmake-mode
                 auto-complete
                 auto-yasnippet
                 ess
                 ))
 '("package" "packages" "install"))

;; change window size
(defun my-enlarge-vert ()
  (interactive)
  (enlarge-window 2))

(defun my-shrink-vert ()
  (interactive)
  (enlarge-window -2))

(defun my-enlarge-horz ()
  (interactive)
  (enlarge-window-horizontally 2))

(defun my-shrink-horz ()
  (interactive)
  (enlarge-window-horizontally -2))

(global-set-key (kbd "C-9") 'my-shrink-horz)
(global-set-key (kbd "C-)") 'my-enlarge-vert)
(global-set-key (kbd "C-(") 'my-shrink-vert)
(global-set-key (kbd "C-0") 'my-enlarge-horz)


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
            (ibuffer-switch-to-saved-filter-groups "default")
            (linum-mode 0)))

(global-set-key (kbd "\C-x \C-b") 'ibuffer)

;; ------------------------------------------------------------
;; Dired
;; ------------------------------------------------------------
(defun dired-open-native ()
  "Open marked files (or the file the cursor is on) from dired."
  (interactive)
  (let* ((files (dired-get-marked-files t current-prefix-arg))
         (n (length files)))
    (when (or (<= n 3)
              (y-or-n-p (format "Open %d files?" n)))
      (dolist (file files)
        (call-process "gnome-open"
                      nil 0 nil file)))))

(define-key dired-mode-map (kbd "C-c o") 'dired-open-native)

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

(add-hook 'dired-mode-hook
          '(lambda()
             (visual-line-mode 0) ;; unwrap lines.
             (linum-mode 0) ;; turn off line numbers.
             (auto-revert-mode) ;; auto-refresh dired
             ))

(provide 'init-dired)

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

(global-set-key (kbd "C-x w") 'aya-create)
(global-set-key (kbd "C-x y") 'aya-expand)
(global-set-key (kbd "C-o") 'aya-open-line)

(eval-after-load "yasnippet-autoloads"
  '(progn
     (if (require 'yasnippet nil t)
         (progn
           (let ((yas-dir "/home/sergei/.emacs.d/emacs-dot/.yasnippets"))
             (when (file-exists-p yas-dir)
               (setq yas-snippet-dirs (list yas-dir))))
           (setq-default yas-prompt-functions (quote
                                               (yas-dropdown-prompt
                                                yas-ido-prompt
                                                yas-completing-prompt
                                                yas-x-prompt
                                                yas-no-prompt)))
           (yas-global-mode 1)

           (add-hook 'term-mode-hook
                     '(lambda ()
                        (yas-minor-mode -1))))
       (message "WARNING: yasnippet not found"))))

;; navigation between buffers
(global-set-key "\C-x\C-p" 'previous-buffer)
(global-set-key "\C-x\C-n" 'next-buffer)
(global-set-key "\C-x\C-\\" 'other-window)

(require 'cc-mode)

(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

(c-add-style "mycodingstyle"
             '((c-comment-only-line-offset . 0)
               (c-hanging-braces-alist . ((substatement-open before after)))
               (c-offsets-alist . ((innamespace          . 0)))))

;; c/c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-hook 'c-mode-common-hook
          '(lambda()
             (c-set-style "mycodingstyle")))

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;;;; spaces instead tabs
(setq c-basic-indent 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;;;; hide toolbar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq x-select-enable-clipboard t)

;; Delete selection
(delete-selection-mode t)

(eval-after-load "cmake-mode-autoloads"
  '(progn
     (when (require 'cmake-mode nil t)
       (setq auto-mode-alist
             (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                       ("CMakeCache\\.txt\\'" . cmake-mode)
                       ("\\.cmake\\'" . cmake-mode))
                     auto-mode-alist)))))

;; Linum plugin
(require 'linum) ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d") ;; задаем формат нумерации строк

(global-set-key (kbd "C-x t") 'term)

(require 'grep)

(grep-apply-setting 'grep-find-command
                    (quote ("find . -type f -exec grep -nIHRi -e  {} +" . 37)))

(global-set-key (kbd "C-c f") 'find-grep)

(require 'auto-complete)

(require 'magit)
(add-hook 'magit-mode-hook
          '(lambda()
             (linum-mode 0)))

(global-set-key (kbd "C-c m") 'magit-status)

(defun kill-whitespace ()
          "Kill the whitespace between two non-whitespace characters"
          (interactive "*")
          (save-excursion
            (save-restriction
              (save-match-data
                (progn
                  (re-search-backward "[^ \t\r\n]" nil t)
                  (re-search-forward "[ \t\r\n]+" nil t)
                  (replace-match "" nil nil))))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

(defun only-current-buffer ()
  (interactive)
    (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(global-set-key (kbd "C-x q") 'only-current-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

(global-set-key (kbd "C-z") 'undo)

(setq-default truncate-lines t)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

(setq make-pointer-invisible t)

(global-set-key (kbd "C-c c") 'compile)

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-unset-key (kbd "<C-left>"))
(global-unset-key (kbd "<C-right>"))
(global-unset-key (kbd "<C-up>"))
(global-unset-key (kbd "<C-down>"))
(global-unset-key (kbd "<M-left>"))
(global-unset-key (kbd "<M-right>"))
(global-unset-key (kbd "<M-up>"))
(global-unset-key (kbd "<M-down>"))

(when (require 'auto-complete nil t)
  (require 'auto-complete-config)
  (ac-flyspell-workaround)
  (setq-default ac-auto-show-menu 0)
  (global-auto-complete-mode 1))

(add-hook 'c-mode-common-hook
'(lambda ()
   (add-to-list 'ac-sources 'ac-source-yasnippet)
))

(defun add-sudo-to-filename (filename)
  "Adds sudo proxy to filename for use with TRAMP.

Works for both local and remote hosts (>=23.4). The syntax used
for remote hosts follows the pattern
'/ssh:you@remotehost|sudo:remotehost:/path/to/file'. Some people
say, that you may need to call smth like
`(set-default 'tramp-default-proxies-alist (quote ((\".*\"
\"\\`root\\'\" \"/ssh:%u@%h:\"))))', but it works for me just fine
without it. "
  (with-temp-buffer
    (insert filename)
    (goto-char (point-max))
    (if (re-search-backward "@\\(.*\\):" nil t)
        (let ((remote-name (buffer-substring (match-beginning 1) (match-end 1))))
          (goto-char (match-end 1))
          (insert (concat "|sudo:" remote-name))
          (goto-char (point-min))
          (forward-char)
          (when (looking-at "scp")
            (delete-char 3)
            (when (looking-at "c")
              (delete-char 1))
            (insert "ssh"))
          (buffer-string))
      (concat "/sudo::" filename))))

(define-key global-map (kbd "\C-x!")
  (defun sudo-edit-current-file (&optional arg)
    "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
        (find-file (concat "/sudo:root@localhost:"
                           (ido-read-file-name "Find file(as root): ")))
      (let ((position (point)))
        (find-alternate-file (add-sudo-to-filename buffer-file-name))
        (goto-char position)))))

(require 'compile)
(make-variable-buffer-local 'compile-command)
(global-set-key "\C-c\C-c" 'compile)

;; auto-scroll until first error
(setq-default compilation-scroll-output (quote first-error))

;; those patterns are used by dmd compiler
(setq-default compilation-error-regexp-alist
              (append '(("^\\(.*?\\)(\\([0-9]+\\)): Warning:" 1 2 nil 1)
                        ("^\\(.*?\\)(\\([0-9]+\\)): Error:" 1 2 nil 2))
                      compilation-error-regexp-alist))

(defun translate-google (arg)
  (interactive (list (read-string "Language (default en:ru):")))
  (if (equal arg "")
      (setq arg "en:ru"))
  (with-output-to-temp-buffer "*translate*"
    (async-shell-command
     (concat "trans " arg " \"" (buffer-substring (mark) (point)) "\"") "*translate*")))

(global-set-key (kbd "C-x c") 'calculator)

(global-set-key (kbd "C-c t") 'translate-google)

(defun compile-run (arg)
  (interactive (list (read-string "path:")))
  (with-output-to-temp-buffer "*compile-run*"
    (async-shell-command arg "*compile-run*")))

(global-set-key (kbd "C-c r") 'compile-run)

(setq ediff-split-window-function 'split-window-horizontally)
(defvar my-ediff-awin-config nil "Window configuration after ediff.")
