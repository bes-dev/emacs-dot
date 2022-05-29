;; ------------------------------------------------------------
;; Compile
;; ------------------------------------------------------------
(require 'compile)
(make-variable-buffer-local 'compile-command)
(global-set-key "\C-c\C-c" 'compile)

;; auto-scroll until first error
(setq-default compilation-scroll-output (quote first-error))

;; fontify last line in comint-mode
(add-to-list 'compilation-mode-font-lock-keywords
             '("^Comint \\(finished\\).*"
               (1 compilation-info-face)))
(add-to-list 'compilation-mode-font-lock-keywords
             '("^Comint \\(exited abnormally\\|interrupt\\|killed\\|terminated\\|segmentation fault\\)\\(?:.*with code \\([0-9]+\\)\\)?.*"
               (1 compilation-error-face)
               (2 compilation-error-face nil t)))

;; bind common keys to behave similar to plain compile-mode
(define-key comint-mode-map "\C-c\C-k" 'kill-compilation)
(define-key comint-mode-map "g"
  (defun comint-g-or-recompile (N)
    (interactive "p")
    (if (or
         (comint-check-proc (current-buffer))
         ()
         )
        (self-insert-command N)
      (recompile))))

;; ------------------------------------------------------------
;; Markdown
;; ------------------------------------------------------------
(eval-after-load "markdown-mode-autoloads"
  '(progn
     (if (require 'markdown-mode nil t)
         (progn
           (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))

           (define-key markdown-mode-map (kbd "M-p") nil)
           (define-key markdown-mode-map (kbd "M-n") nil)
           (define-key markdown-mode-map (kbd "\C-c\C-c") nil)
           (define-key markdown-mode-map (kbd "\C-c\C-e") 'markdown-export))
       (message "WARNING: markdown-mode not found"))))

;; ------------------------------------------------------------
;; C/C++
;; ------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.c\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh\\'" . c++-mode))
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key c-mode-base-map "\C-c\C-o"
               'ff-find-other-file)
             (define-key c-mode-base-map (kbd "C-c .")
               'c-guess-buffer)

             (define-key c-mode-base-map "\C-c\C-c"    nil)
             (define-key c-mode-base-map (kbd "C-M-h") nil)
             (define-key c-mode-base-map (kbd "M-j")   nil)

             ;; set //-style comments for c-mode
             (setq comment-start "//" comment-end "")))

(setq-default c-basic-offset 4)
(setq-default c-default-style (quote ((c-mode . "bsd") (c++-mode . "bsd") (d-mode . "bsd") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))

;; ------------------------------------------------------------
;; Emacs Lisp
;; ------------------------------------------------------------
(require 'ert)
(define-key ert-results-mode-map "g"
  'ert-results-rerun-all-tests)

(require 'etags)
(define-key emacs-lisp-mode-map (kbd "M-.")
  (defun find-function-push-tag (function)
    "This function is meant as a drop-in replacement for find-tag
  in emacs-lisp-mode. It calls find-function and inserts current
  position into find-tag-marker-ring."
    (interactive (find-function-read))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-function function)))

;; ------------------------------------------------------------
;; Log files
;; Update log file as they appear.
;; ------------------------------------------------------------
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

;; ------------------------------------------------------------
;; YAML
;; ------------------------------------------------------------
(eval-after-load "yaml-mode-autoloads"
  '(progn
     (if (require 'yaml-mode nil t)
         (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
       (message "WARNING: yaml-mode not found"))))


;; ------------------------------------------------------------
;; CMake
;; ------------------------------------------------------------
(eval-after-load "cmake-mode-autoloads"
  '(progn
     (when (require 'cmake-mode nil t)
       (setq auto-mode-alist
             (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                       ("CMakeCache\\.txt\\'" . cmake-mode)
                       ("\\.cmake\\'" . cmake-mode))
                     auto-mode-alist)))))


;; ------------------------------------------------------------
;; magit
;; ------------------------------------------------------------
(eval-after-load "magit-autoloads"
  '(progn
     (if (require 'magit nil t)
         (progn
           (require 'gitignore-mode nil t)
           (require 'gitconfig-mode nil t)
           (require 'gitattributes-mode nil t)

           (setq magit-last-seen-setup-instructions "1.4.0")

           (setq
            magit-revert-item-confirm nil
            magit-diff-refine-hunk t
            magit-push-always-verify nil)

           (define-key magit-mode-map [C-tab] nil)
           (define-key magit-mode-map (kbd "M-w") nil)

           ;; push stashes to the bottom of the status buffer
           (delete 'magit-insert-stashes magit-status-sections-hook)
           (add-to-list 'magit-status-sections-hook 'magit-insert-stashes t)

           (global-set-key (kbd "\C-c m")   'magit-status)
           (global-set-key (kbd "\C-c RET") 'magit-status)
           (global-set-key (kbd "\C-x v b") 'magit-blame))
       (message "WARNING: magit not found"))))

;; ------------------------------------------------------------
;; Python
;; ------------------------------------------------------------
;; (require 'elpy)
;; (elpy-enable)
;; (setq elpy-rpc-python-command "python3")
;; (add-hook 'elpy-mode-hook
;;           (lambda()
;;             (define-key elpy-mode-map (kbd "C-c C-n") 'elpy-goto-definition-other-window)
;;             (define-key elpy-mode-map (kbd "C-c n") 'elpy-goto-definition)))


;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

(provide 'program-languages)
