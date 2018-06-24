(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(defcustom ispell-common-dictionaries
  '("en" "ru")
  "List of dictionaries for common use"
  :group 'ispell)

(setq-default ispell-dictionary (car ispell-common-dictionaries))

(define-key flyspell-mode-map (kbd "C-c M-$")
  (defun ispell-next-dictionary()
    "Cycle through dictionaries in `ispell-common-dictionaries'"
    (interactive)
    (let* ((dic ispell-current-dictionary)
           (next (cadr (member dic ispell-common-dictionaries)))
           (change (if next next (car ispell-common-dictionaries))))
      (ispell-change-dictionary change))))

(define-key flyspell-mode-map (kbd "C-x M-$")
  (defun flyspell-buffer-or-region ()
    (interactive)
    (if (region-active-p)
        (flyspell-region (region-beginning) (region-end))
      (flyspell-buffer))))

(provide 'spell-check)
