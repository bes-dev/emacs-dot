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
                 window-numbering
                 multiple-cursors
                 company
                 auto-complete
                 flycheck
                 cmake-mode
                 magit
                 org
                 yasnippet
                 elpy
                 ;; auto-yasnippet
                 ;; ox-reveal
                 ))
 '("package" "packages" "install"))


;; ------------------------------------------------------------
;; LOCAL PACKAGES
;; initialization
;; ------------------------------------------------------------
(defconst emacs-d
  (file-name-directory (or load-file-name
                           (when (boundp 'bytecomp-filename) bytecomp-filename)
                           buffer-file-name))
  "Installation directory of emacs e.g as .emacs.d"
)

;; Adding paths to the variable load-path
(dolist (relpath '(""
                   "extensions/"
                   "modules/"
                   )
                 )
  (add-to-list 'load-path (concat emacs-d relpath)))

(require 'emacs-generic (concat emacs-d "extensions/emacs-generic.el"))
(require 'multiple-cursors-autoloads (concat emacs-d "extensions/multiple-cursors-autoloads.el"))
(require 'dired-settings (concat emacs-d "extensions/dired-settings.el"))
(require 'google (concat emacs-d "extensions/google.el"))
(require 'spell-check (concat emacs-d "extensions/spell-check.el"))
(require 'program-languages (concat emacs-d "extensions/program-languages.el"))
