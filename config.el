;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Variable to determine how many modules are loaded, for debugging
;; Any number is number of modules
;; nil means all
(setq debug-my-config nil)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Egidius Mysliwietz"
      user-mail-address "egidius@mysliwietz.de"
      auth-sources '("~/.authinfo")
      auth-source-cache-expiry nil)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)
(custom-set-faces!
  '(doom-modeline-buffer-modified :foreground "orange")
  '(org-cite :foreground "purple")
  '(org-cite-key :foreground "MediumPurple1" :slant italic))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
; they are implemented.

(add-to-list 'load-path "~/.doom.d/modules")

(defmacro -- (var)
  `(when ,var
    (setq ,var (- ,var 1))))

(defmacro ++ (var)
  `(when ,var
    (setq ,var (+ ,var 1))))


(setq use-package-verbose t)
(async-bytecomp-package-mode 1)

(defmacro after-startup (&rest func)
  `(unless debug-my-config (add-hook! 'after-startup-hook
                             '(lambda () ,@func))))

(mkdir (concat doom-private-dir "modules") t)

(defmacro execution-time (func)
  `(let ((time (current-time)))
     ,func
     (float-time (time-since time))))

(defmacro execution-time-format (func)
  `(let ((time (current-time)))
     ,func
     (message (format "Loaded %s in %.06f." ',func (float-time (time-since time))))))

(defmacro load-module (module)
   `(when
        (or (not debug-my-config) (> debug-my-config 0))
      (-- debug-my-config)
      (make-thread
       (let* ((benchmark (benchmark-run (require ,module)))
             (time (car benchmark))
             (gbc (nth 1 benchmark))
             (gbt (nth 2 benchmark)))
         (message "%s - Loaded %s in %.06fs, using %s garbage collections in %.06fs."
                  ,debug-my-config ,module time gbc gbt)))))

;; Load a module only if dependency could successfully be loaded
(defmacro load-module-if (dependency module)
`(when (require ,dependency nil 'noerror)
   (load-module ,module)))

(load-module 'cl) ; Still a requirement for ivy
(load-module 'private-config)
;(load-module '(require 'bible))
(when (require 'mu4e nil 'noerror)
  (load-module 'email)
  (load-module 'email-config)
  (load-module 'email-accounts))
; Unsorted
(setq doom-modeline-enable-word-count t)
;(doom/quickload-session)

(defun doom--get-modules (file)
  (unless (file-exists-p file)
    (user-error "%s does not exist" file))
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward "(doom! " nil t)
      (goto-char (match-beginning 0))
      (cdr (sexp-at-point)))))

(defun doom--put-modules (tmpfile modules)
  (with-temp-file tmpfile
    (delay-mode-hooks (emacs-lisp-mode))
    (insert (replace-regexp-in-string " " "\n" (prin1-to-string modules)))
    (indent-region (point-min) (point-max))))

(defun indent-buffer ()
  "Indent each nonblank line in the buffer"
  (interactive)
  (indent-region (point-min) (point-max)))

;;;###autoload
(defun doom/what-has-changed ()
  "Open an ediff session to compare the module list in
~/.emacs.d/init.example.el and ~/.doom.d/init.el."
  (interactive)
  (let ((old-modules (doom--get-modules (expand-file-name (concat doom-emacs-dir "templates/init.example.el"))))
        (new-modules (doom--get-modules (expand-file-name "init.el" doom-private-dir)))
        (example-init-el "/tmp/doom-init.example.el")
        (private-init-el "/tmp/doom-private-init.el"))
    (doom--put-modules example-init-el old-modules)
    (doom--put-modules private-init-el new-modules)
    (ediff private-init-el example-init-el)))

(load-module 'workarounds)

(load-module 'utility-functions)

(load-module 'auto-correct)

(load-module 'beancount-tweaks)

(load-module 'dired-inline-images)

(load-module 'dired-tweaks)

(load-module 'tab-to-expand-subfiles)

(load-module 'narrow-by-regex-dired)

(load-module 'ebook-tweaks)

(load-module 'editing)

(load-module 'elfeed-tweaks)

(load-module 'interface)

(load-module 'exwm-tweaks)

(load-module 'exwm-buffer-movements)

(load-module 'exwm-gaps)

(load-module 'general)

(load-module 'navigation)

(load-module 'shortcuts)

(load-module 'config-visit)

(load-module 'search-ivy)

(after! vertico
   (load-module 'search-with-vertico)
)

(load-module 'read-aloud)

(load-module 'speed-read)

(load-module 'spaced-repetition)

(load-module 'accounting)

(when home?
  (load-module 'popes))

(load-module 'keycast-tweaks)

(load-module 'weather)

(mkdir "~/.local/share/applications/" t)

(mkdir (concat doom-private-dir "ext/org-protocol/") t)

(require 'org-protocol)
(add-to-list 'org-protocol-protocol-alist
             '("Download like with youtube-dl"
               :protocol "youtube-dl"
               :function youtube-dl-protocol-handler))

(defun youtube-dl-protocol-handler (data)
  "Add url to youtube-dl download queue."
  (let ((url (plist-get data :url))
        (title (plist-get data :title)))
    (unless (string= title "about:blank")
      (youtube-dl
       (plist-get data :url)
       :title (plist-get data :title))))
  nil)

(load-module 'org-roam-tweaks)

(load-module 'org-tweaks)

(load-module 'org-functions)

(load-module 'org-citations)

(load-module 'org-links)

(load-module 'languages)

(load-module-if 'mu4e 'email)

(load-module-if 'mu4e 'email-config)

(load-module-if 'mu4e 'email-accounts)

(load-module 'latex-tweaks)

(load-module 'pdf-and-annotation-tweaks)
