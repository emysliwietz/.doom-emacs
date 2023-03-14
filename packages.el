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
(package! all-the-icons-completion)
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
(package! aas :recipe (:host github :repo "ymarco/auto-activating-snippets")
  :pin "1699bec4d244a1f62af29fe4eb8b79b6d2fccf7d")

(package! string-inflection :pin "fd7926ac17293e9124b31f706a4e8f38f6a9b855")

(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")
(package! modus-themes :pin "392ebb115b07f8052d512ec847619387d109edd6")
(package! theme-magic :pin "844c4311bd26ebafd4b6a1d72ddcc65d87f074e3")
(package! keycast :pin "04ba7519f34421c235bac458f0192c130f732f12")
(package! page-break-lines :recipe (:host github :repo "purcell/page-break-lines"))
(package! spray :pin "74d9dcfa2e8b38f96a43de9ab0eb13364300cb46")
;(package! authinfo-color-mode
;  :recipe (:repo "lisp/authinfo-color-mode"))
(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
; Ebooks
(package! calibredb :pin "cb93563d0ec9e0c653210bc574f9546d1e7db437")
(package! nov :pin "b3c7cc28e95fe25ce7b443e5f49e2e45360944a3")
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

; Temporary workaround, remove at emacs 29
(package! transient
      :pin "c2bdf7e12c530eb85476d3aef317eb2941ab9440"
      :recipe (:host github :repo "magit/transient"))

(package! with-editor
          :pin "bbc60f68ac190f02da8a100b6fb67cf1c27c53ab"
          :recipe (:host github :repo "magit/with-editor"))

(unpin! magit)

(package! dired-subtree)

(package! dired-narrow)









(package! buffer-move)

(package! exwm-outer-gaps
  :recipe
  (:host github
   :repo "lucasgruss/exwm-outer-gaps"))



(package! switch-window)







(package! swiper)
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



(package! org-appear :recipe (:host github :repo "awth13/org-appear"))











(package! org-noter)



(package! gptel :recipe (:host github :repo "karthink/gptel"))
