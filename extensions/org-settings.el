(require 'org)
(require 'ox-reveal)
(require 'org-superstar)
(require 'org-pretty-table (concat emacs-d "modules/org-pretty-table.el"))
(require 'org-fragtog (concat emacs-d "modules/org-fragtog.el"))
(require 'stripe-buffer (concat emacs-d "modules/stripe-buffer.el"))

;; (eval-after-load "ox-reveal-autoloads"
;;   '(progn
;;      (when (and (require 'org nil t) (require 'ox-reveal nil t))
;;        (defun conditional-org-reveal-export-to-html ()
;;          (save-excursion
;;            (beginning-of-buffer)
;;            (when (search-forward "#+REVEAL" nil nil)
;;              (org-reveal-export-to-html))))

;;        (add-hook 'org-ctrl-c-ctrl-c-final-hook
;;                  'conditional-org-reveal-export-to-html)

;;        (defcustom org-reveal-repo "https://github.com/hakimel/reveal.js.git"
;;          "Destination of the reveal.js repo. You can point it to a local clone."
;;          :group 'ox-reveal)
;;        (defun org-reveal-init ()
;;          (interactive)
;;          (async-shell-command (concat "git clone --depth 1 " org-reveal-repo))
;;          (find-file "template.org")
;;          (insert "revealjs")
;;          (yas-expand)
;;          (goto-char (point-min))
;;          (search-forward "(")
;;          (backward-char)
;;          (forward-sexp)
;;          (eval-and-replace)
;;          (backward-delete-char 1)
;;          (search-backward "\"")
;;          (delete-char 1)
;;          (save-buffer)))))

;; show inline images
(setq org-startup-with-inline-images t)
;; add pretty view of list
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
;; from snosov1 config
(setq-default org-confirm-babel-evaluate nil)
(setq-default org-hide-leading-stars t)
(setq-default org-modules (quote (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-habit org-irc org-mew org-mhe org-rmail org-vm org-wl org-w3m)))
(setq-default org-src-fontify-natively t)
(setq-default org-startup-indented t)
(setq-default org-support-shift-select (quote always))
;; pretty table view
(add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))
;; highlight active row of the table
;; (add-hook 'org-mode-hook (lambda () (hl-line-mode 1)))
(add-hook 'org-mode-hook 'turn-on-stripe-table-mode)
;; configure TODO keywords
(setq org-todo-keywords
      '((sequence "TODO" "ACTIVE" "DONE" "CANCELED")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "red" :weight bold))
        ("ACTIVE" . (:foreground "yellow" :weight bold))
        ("DONE" . (:foreground "green" :weight bold))
        ("CANCELED" . (:foreground "pink" :weight bold))))

;; inline preview latex formulas
(add-hook 'org-mode-hook 'org-fragtog-mode)
;; change inline latex font size
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.7))
;; ;; prettify symbols
;; (add-hook 'org-mode-hook (lambda ()
;;    "Beautify Org Checkbox Symbol"
;;    (push '("[ ]" . "☐") prettify-symbols-alist)
;;    (push '("[X]" . "☑" ) prettify-symbols-alist)
;;    (push '("[-]" . "❍" ) prettify-symbols-alist)
;;    (prettify-symbols-mode)))
;; open hyperlink in the same frame
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus . org-gnus-no-new-news)
        (file . find-file)
        (wl . wl-other-frame)))
;; ;; long lines. TODO: doesn't work well with tables
;; (add-hook 'org-mode-hook 'visual-line-mode)

(provide 'org-settings)
