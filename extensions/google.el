;; ------------------------------------------------------------
;; Provide functionality for web search engines
;; ------------------------------------------------------------

(defmacro url-do-it (backend-name query-beginning docstring)
  `(defun ,(intern (format "%s-it" (mapconcat 'identity (split-string (downcase backend-name)) "-"))) ()
     ,(format "%s the selected region if any, display a query prompt otherwise" docstring)
     (interactive)
     (browse-url
      (concat
       ,query-beginning
       (url-hexify-string (if mark-active
                              (buffer-substring (region-beginning) (region-end))
                            (read-string (concat ,backend-name ": "))))))))

(global-set-key (kbd "\C-cg") (url-do-it "Google" "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" "Google"))
(global-set-key (kbd "\C-cl") (url-do-it "Lingvo" "http://lingvopro.abbyyonline.com/en/Translate/en-ru/" "Translate (using Lingvo)"))
(global-set-key (kbd "\C-cu") (url-do-it "Urban Dictionary" "http://www.urbandictionary.com/define.php?term=" "Find a definition in Urban Dictionary for"))

;; (defun google-translate (arg)
;;   (interactive (list (read-string "Language (default en:ru):")))
;;   (if (equal arg "")
;;       (setq arg "en:ru"))
;;   (with-output-to-temp-buffer "*translate*"
;;     (async-shell-command
;;      (concat "trans " arg " \"" (buffer-substring (mark) (point)) "\"") "*translate*")))
;; (global-set-key (kbd "C-c t") 'google-translate)

(provide 'google)
