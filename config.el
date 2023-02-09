;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

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
;; they are implemented.

(add-to-list 'load-path "~/.doom.d/modules")

(defmacro dec (var)
  `(when ,var
    (setq ,var (- ,var 1))))

(defmacro inc (var)
  `(when ,var
    (setq ,var (+ ,var 1))))


(when (or (not debug-my-config) (> debug-my-config 0)) (dec debug-my-config) (message "a"))

(setq use-package-verbose t)
(async-bytecomp-package-mode 1)

(defun after-startup (func)
  (unless debug-my-config (add-hook! after-startup-hook
                            (make-thread
                             (load-module func)))))

;; Variable to determine how many modules are loaded, for debugging
;; Any number is number of modules
;; -1 means all
(setq debug-my-config nil)
(defmacro load-module (module)
   `(when (or (not debug-my-config) (> debug-my-config 0)) (dec debug-my-config) (make-thread
   (let ((time (current-time)))
     ;(message (format "Loading %s." func-name))
     (require ,module)
     (message (format "Loaded %s in %.06f." ,module (float-time (time-since time))))))))

(load-module 'cl) ; Still a requirement for ivy
(load-module 'exwm-tweaks)
(load-module 'general)
(load-module 'interface)
(load-module 'navigation)
(load-module 'shortcuts)
(load-module 'private-config)
(load-module 'search)
(load-module 'read-aloud)
(load-module 'read-single)
(load-module 'pamparam)
(load-module 'beancount-tweaks)
(load-module 'popes)
;(load-module '(require 'bible))
(load-module 'keycast-tweaks)
(load-module 'wttrin)
(load-module 'org-tweaks)
(load-module 'languages)
(when (require 'mu4e nil 'noerror)
  (load-module 'email)
  (load-module 'email-config)
  (load-module 'email-accounts))
(load-module 'latex-tweaks)
; Unsorted
(setq doom-modeline-enable-word-count t)

(load-module 'utility-functions)

(load-module 'auto-correct)

(load-module 'beancount-tweaks)

(load-module 'dired-inline-images)

(load-module 'dired-tweaks)

(load-module 'ebook-tweaks)

(load-module 'org-roam-tweaks)

(load-module 'editing)

(load-module 'elfeed-tweaks)
