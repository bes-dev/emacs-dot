(require 'org)
(require 'ox-reveal)

(eval-after-load "ox-reveal-autoloads"
  '(progn
     (when (and (require 'org nil t) (require 'ox-reveal nil t))
       (defun conditional-org-reveal-export-to-html ()
         (save-excursion
           (beginning-of-buffer)
           (when (search-forward "#+REVEAL" nil nil)
             (org-reveal-export-to-html))))

       (add-hook 'org-ctrl-c-ctrl-c-final-hook
                 'conditional-org-reveal-export-to-html)

       (defcustom org-reveal-repo "https://github.com/hakimel/reveal.js.git"
         "Destination of the reveal.js repo. You can point it to a local clone."
         :group 'ox-reveal)
       (defun org-reveal-init ()
         (interactive)
         (async-shell-command (concat "git clone --depth 1 " org-reveal-repo))
         (find-file "template.org")
         (insert "revealjs")
         (yas-expand)
         (goto-char (point-min))
         (search-forward "(")
         (backward-char)
         (forward-sexp)
         (eval-and-replace)
         (backward-delete-char 1)
         (search-backward "\"")
         (delete-char 1)
         (save-buffer)))))


(provide 'org-settings)
