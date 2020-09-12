;; ------------------------------------------------------------
;; maximize window
;; ------------------------------------------------------------
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ------------------------------------------------------------
;; lose UI early
;; ------------------------------------------------------------
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; ------------------------------------------------------------
;; no splash screen
;; ------------------------------------------------------------
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
(set-face-attribute 'default nil :family "DejaVu Sans Mono")

;; ------------------------------------------------------------
;; Delete selection
;; ------------------------------------------------------------
(delete-selection-mode 1)

;; ------------------------------------------------------------
;; change window size
;; ------------------------------------------------------------
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

;; ------------------------------------------------------------
;; change font (for retina display)
;; ------------------------------------------------------------
;;(set-frame-font "Menlo 14" nil t)

;; ------------------------------------------------------------
;; define translations
;; ------------------------------------------------------------
(define-key key-translation-map [?\C-h] [?\C-?]) ;; translate C-h to DEL

;; ------------------------------------------------------------
;; ibuffer groups
;; ------------------------------------------------------------
(require 'ibuffer nil t)
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
;; Input methods
;; ------------------------------------------------------------
(setq alternative-input-methods
      '(("russian-computer" . [?\C-\\])))

(setq default-input-method
      (caar alternative-input-methods))

(defun toggle-alternative-input-method (method &optional arg interactive)
  (if arg
      (toggle-input-method arg interactive)
    (let ((previous-input-method current-input-method))
      (when current-input-method
        (deactivate-input-method))
      (unless (and previous-input-method
                   (string= previous-input-method method))
        (activate-input-method method)))))

(defun reload-alternative-input-methods ()
  (dolist (config alternative-input-methods)
    (let ((method (car config)))
      (global-set-key (cdr config)
                      `(lambda (&optional arg interactive)
                         ,(concat "Behaves similar to `toggle-input-method', but uses \""
                                  method "\" instead of `default-input-method'")
                         (interactive "P\np")
                         (toggle-alternative-input-method ,method arg interactive))))))

(reload-alternative-input-methods)

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

;; ------------------------------------------------------------
;; navigation between buffers
;; ------------------------------------------------------------
(global-set-key "\C-x\C-p" 'previous-buffer)
(global-set-key "\C-x\C-n" 'next-buffer)
(global-set-key "\C-x\C-\\" 'other-window)

;; ------------------------------------------------------------
;; spaces instead tabs
;; ------------------------------------------------------------
(setq c-basic-indent 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; ------------------------------------------------------------
;; hide toolbar
;; ------------------------------------------------------------
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq x-select-enable-clipboard t)

;; ------------------------------------------------------------
;; Linum plugin
;; ------------------------------------------------------------
(require 'linum) ;; вызвать Linum
(line-number-mode   t) ;; показать номер строки в mode-line
(global-linum-mode  t) ;; показывать номера строк во всех буферах
(column-number-mode t) ;; показать номер столбца в mode-line
(setq linum-format " %d") ;; задаем формат нумерации строк

;; ------------------------------------------------------------
;; terminal
;; ------------------------------------------------------------
(global-set-key (kbd "C-x t") 'term)

;; ------------------------------------------------------------
;; Kill whitespace
;; ------------------------------------------------------------
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

;; ------------------------------------------------------------
;; Comment/uncomment region
;; ------------------------------------------------------------
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(global-set-key (kbd "C-/") 'comment-or-uncomment-region-or-line)

;; ------------------------------------------------------------
;; yes/no -> y/n
;; ------------------------------------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)

;; ------------------------------------------------------------
;; grep
;; ------------------------------------------------------------
(require 'grep)
(grep-apply-setting 'grep-find-command
                    (quote ("find . -type f -exec grep -nIHRi -e  {} +" . 37)))
(global-set-key (kbd "C-c f") 'find-grep)

;; ------------------------------------------------------------
;; Undo via C-z
;; ------------------------------------------------------------
(global-set-key (kbd "C-z") 'undo)

;; ------------------------------------------------------------
;; Truncate lines via C-x t
;; ------------------------------------------------------------
(setq-default truncate-lines t)
(global-set-key (kbd "C-x t") 'toggle-truncate-lines)

;; ------------------------------------------------------------
;; Make pointer invisible
;; ------------------------------------------------------------
(setq make-pointer-invisible t)

;; ------------------------------------------------------------
;; Disable arrow buttons
;; ------------------------------------------------------------
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

;; ------------------------------------------------------------
;; Edit current file as superuser
;; ------------------------------------------------------------
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


;; ------------------------------------------------------------
;; built-in
;; ------------------------------------------------------------
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;; ------------------------------------------------------------
;; Close all buffers exclude current
;; ------------------------------------------------------------
(defun only-current-buffer ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))
(global-set-key (kbd "C-x q") 'only-current-buffer)

;; ------------------------------------------------------------
;; Calculator
;; ------------------------------------------------------------
(global-set-key (kbd "C-x c") 'calculator)

;; ------------------------------------------------------------
;; Toggle fullscrean
;; ------------------------------------------------------------
(defun toggle-fullscreen ()
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth) ;tests if already fullscreened
      (set-frame-parameter nil 'fullscreen 'maximized)
    (set-frame-parameter nil 'fullscreen 'fullboth)))
(global-set-key (kbd "<f11>") 'toggle-fullscreen)

;; ------------------------------------------------------------
;; Banish mouse position
;; ------------------------------------------------------------
(defun mouse-avoidance-banish-destination ()
  "The position to which Mouse-Avoidance mode `banish' moves the mouse.
You can redefine this if you want the mouse banished to a different corner."
  (let* ((pos (window-edges)))
    (cons (- (nth 1 pos) 2)
          (+ (nth 1 pos) 100))))
(setq mouse-avoidance-banish-position 'mouse-avoidance-banish-destination)

;; ------------------------------------------------------------
;; Windows autonumbering mode
;; ------------------------------------------------------------
(eval-after-load "window-numbering-autoloads"
  '(progn
     (if (require 'window-numbering nil t)
         (window-numbering-mode 1)
       (message "WARNING: window-numbering-mode not found"))))

;; ------------------------------------------------------------
;; Global autocomplete
;; ------------------------------------------------------------
(require 'auto-complete)
(global-auto-complete-mode t)

;; ------------------------------------------------------------
;; Global company mode
;; ------------------------------------------------------------
(add-hook 'after-init-hook 'global-company-mode)

;; ------------------------------------------------------------
;; Yasnippet
;; ------------------------------------------------------------
(require 'yasnippet)
(yas-global-mode 1)

;; ------------------------------------------------------------
;; Pretty buffer names
;; ------------------------------------------------------------
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

;; ------------------------------------------------------------
;; For remote files, opened with TRAMP, it makes sense to append
;; the hostname to the buffer name.
;; ------------------------------------------------------------
(require 'tramp)
(defun append-tramp-host ()
  "Appends host name to the current buffer name for remote
files"
  (interactive)
  (when (tramp-tramp-file-p default-directory)
    (rename-buffer
     (concat
      (replace-regexp-in-string " <.*>$" "" (or (uniquify-buffer-base-name) (buffer-name)))
      " <"
      (tramp-file-name-host
       (tramp-dissect-file-name default-directory)) ">")
     t)))

(add-hook 'find-file-hook 'append-tramp-host)
(add-hook 'dired-mode-hook 'append-tramp-host)

;; ------------------------------------------------------------
;; shell
;; ------------------------------------------------------------
(global-set-key (kbd "C-c e") 'eshell)

;; ------------------------------------------------------------
;; turn off sounds
;; ------------------------------------------------------------
(setq ring-bell-function 'ignore)

;; ------------------------------------------------------------
;; show time in progress bar
;; ------------------------------------------------------------
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
                     'face 'egoge-display-time)))
(display-time-mode 1)

(provide 'emacs-generic)
