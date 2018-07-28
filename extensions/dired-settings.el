(require 'dired-details (concat emacs-d "modules/dired-details.el"))
(dired-details-install)

(require 'dired-x)
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
(define-key dired-mode-map (kbd "C-c r") 'dired-do-delete)
(define-key dired-mode-map (kbd "C-c c") 'dired-do-copy)

;; ------------------------------------------------------------
;; dired-details
;; ------------------------------------------------------------
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

;; (provide 'init-dired)
(setq dired-dwim-target t)

(provide 'dired-settings)
