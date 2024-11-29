;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! xelb)
(package! exwm)
(package! winum)
(package! try)
(package! counsel)

(package! diredfl)
(package! all-the-icons-dired)
(package! diredful)
(package! dired-git-info)
(package! async)
(package! dired-quick-sort)
(package! openwith)
(package! lispy)
(package! hydra)
(package! ace-link)
(package! sudo-edit)
(package! rotate)
(package! vlf)
(package! evil-escape :disable t)

; Auto Activating Snippets
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets"))

(package! string-inflection)
(package! info-colors)
(package! modus-themes)
;(package! theme-magic)
(package! keycast)
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))
(package! spray)
;(package! authinfo-color-mode
;  :recipe (:repo "lisp/authinfo-color-mode"))
(package! systemd)
; Ebooks
(package! calibredb)
(package! nov)
; Email
(package! org-mime)
(package! org-auto-tangle)
(package! mu4e-alert)
(package! outline-minor-faces)
(package! mu4e-conversation)
(package! elfeed-summary)
(package! org-modern)
(package! good-scroll)
(package! desktop-environment)
(package! emms)
(package! elfeed-tube)
(package! mpv)
(package! elfeed-tube-mpv)
(package! spell-fu)
(package! wallpaper)





(package! casual-dired)
(package! dired-open-with)

(package! dired-subtree)

(package! dired-narrow)

(package! treesit-auto)
(package! avy)
(package! casual-avy :recipe (:host github :repo "kickingvegas/casual-avy"))
(package! yasnippet-snippets)

(package! indent-bars :recipe (:host github :repo "jdtsmith/indent-bars"))



(package! xclip)





(package! buffer-move)

(package! exwm-outer-gaps
  :recipe
  (:host github
   :repo "lucasgruss/exwm-outer-gaps"))



(package! switch-window)







(package! swiper)
(package! vertico-posframe)
;; (package! vertico)
;; (package! vertico :recipe (:files (:defaults "extensions/*") ; Special recipe to load extensions conveniently
;;                              :includes (vertico-indexed
;;                                         vertico-flat
;;                                         vertico-grid
;;                                         vertico-mouse
;;                                         vertico-quick
;;                                         vertico-buffer
;;                                         vertico-repeat
;;                                         vertico-reverse
;;                                         vertico-directory
;;                                         vertico-multiform
;;                                         vertico-unobtrusive
;;                                         )))















(package! org-roam)
(package! org-roam-ui)
(package! org-roam-timestamps)
(package! org-roam-bibtex)
(package! citar-org-roam)

(package! engrave-faces)



;(package! citar-capf :recipe (:host github :repo "mclear-tools/citar-capf"))
(package! zotra)





(package! org-appear :recipe (:host github :repo "awth13/org-appear"))















(package! org-noter)





(package! gptel :recipe (:host github :repo "karthink/gptel"))

(package! async-await)
(package! aichat :recipe (:host github :repo "xhcoding/emacs-aichat"))



(package! org-gcal)
(package! request)
(package! alert)
(package! persist)
(package! aio)
(package! oauth2-auto)



(package! nano :recipe (:host github :repo "rougier/nano-emacs"))
;(package! nano-theme :recipe (:host github :repo "rougier/nano-theme"))
(package! mu4e-dashboard :recipe (:host github :repo "rougier/mu4e-dashboard"))
(package! svg-tag-mode)
(package! ts)
(package! nano-sidebar :recipe (:host github :repo "rougier/nano-sidebar"))

(package! eee
      :recipe
      (:host github
       :repo "eval-exec/eee.el"
       :files (:defaults "*.el" "*.sh")))

; Temporary workaround, remove at emacs 29
(package! transient
      :recipe (:host github :repo "magit/transient"))

(package! with-editor
          :recipe (:host github :repo "magit/with-editor"))

(unpin! magit)

(package! diff-hl :disable t)
