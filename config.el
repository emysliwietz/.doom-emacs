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
(custom-set-faces! '(doom-modeline-buffer-modified :foreground "orange"))

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
(setq debug-my-config 0)
(setq use-package-verbose t)
(async-bytecomp-package-mode 1)

(defun after-startup (func)
  (unless debug-my-config (add-hook! after-startup-hook
                            (make-thread
                             (load-module func)))))

(defun load-module (func)
  (make-thread
   (let ((func-name (substring (format "%s" func) 22 -2))
         (time (current-time)))
     ;(message (format "Loading %s." func-name))
     (funcall func)
     (message (format "Loaded %s in %.06f." func-name (float-time (time-since time)))))))

(load-module '(lambda () (require 'cl))) ; Still a requirement for ivy
(load-module '(lambda () (require 'exwm-tweaks)))
(load-module '(lambda () (require 'general)))
(load-module '(lambda () (require 'interface)))
(load-module '(lambda () (require 'navigation)))
(load-module '(lambda () (require 'shortcuts)))
(load-module '(lambda () (require 'private-config)))
(load-module '(lambda () (require 'search)))
(load-module '(lambda () (require 'auto-correct)))
(load-module '(lambda () (require 'read-aloud)))
(load-module '(lambda () (require 'read-single)))
(load-module '(lambda () (require 'dired-tweaks)))
(load-module '(lambda () (require 'dired-inline-images)))
(load-module '(lambda () (require 'editing)))
(load-module '(lambda () (require 'pamparam)))
(load-module '(lambda () (require 'beancount-tweaks)))
(load-module '(lambda () (require 'popes)))
;(load-module '(require 'bible))
(load-module '(lambda () (require 'keycast-tweaks)))
(load-module '(lambda () (require 'wttrin)))
(load-module '(lambda () (require 'ebook-tweaks)))
(load-module '(lambda () (require 'elfeed-tweaks)))
(load-module '(lambda () (require 'org-tweaks)))
(load-module '(lambda () (require 'languages)))
(when (require 'mu4e nil 'noerror)
  (load-module '(lambda () (require 'email)))
  (load-module '(lambda () (require 'email-config)))
  (load-module '(lambda () (require 'email-accounts))))
(load-module '(lambda () (require 'latex-tweaks)))
; Unsorted
