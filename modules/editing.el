;;; editing.el -*- lexical-binding: t; -*-

(defun which-active-modes ()
  "Return which minor modes are enabled in the current buffer."
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                        (if (and (symbolp mode) (symbol-value mode))
                            (add-to-list 'active-modes mode))
                      (error nil) ))
          minor-mode-list)
    (format "%s" active-modes)))

(defun replace-regexp-entire-buffer (pattern replacement)
  "Perform regular-expression replacement throughout buffer."
  (interactive
   (let ((args (query-replace-read-args "Replace" t)))
     (setcdr (cdr args) nil)    ; remove third value returned from query---args
     args))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward pattern nil t)
      (replace-match replacement))))


(move-text-default-bindings)

; Casual Avy as menu for avy jumps
(setq relative nil) ; workaround for casual-avy bug, needed as long as not in melpa
(keymap-global-set "M-s" #'casual-avy-tmenu)

(setq toggle-auto-fill-boolean nil
      which-key-idle-delay 0.5
      which-key-allow-multiple-replacements t)

(after! which-key
  (pushnew!
   which-key-replacement-alist
   '(("" . "\\`+?evil[-:]?\\(?:a-\\)?\\(.*\\)") . (nil . "◂\\1"))
   '(("\\`g s" . "\\`evilem--?motion-\\(.*\\)") . (nil . "◃\\1"))
   ))

(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort))

(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-ispell
    company-files
    company-yasnippet))


(set-company-backend! 'ess-r-mode
  '(company-R-args company-R-objects company-dabbrev-code :separate))

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

(setq eros-eval-result-prefix "⟹ ")

(defun toggle-auto-fill-on ()
  (set-fill-column 100) ;80
  (auto-fill-mode t)
  (setq toggle-auto-fill-boolean t)
  ;(string-match-p "auto-fill-function" (which-active-modes))
  (message "auto-fill-mode on"))


(defun toggle-auto-fill-off ()
  (replace-regexp-entire-buffer "\n" " ")
  (auto-fill-mode nil)
  (setq toggle-auto-fill-boolean nil)
  (message "auto-fill-mode off")
  )

(defun toggle-auto-fill ()
  "Toggle auto fill mode and reset buffer to non-auto-fill."
  (interactive)
  (if toggle-auto-fill-boolean
      (toggle-auto-fill-off)
    (toggle-auto-fill-on)
    ))



(global-set-key (kbd "M-q") 'evil-mc-make-cursor-move-next-line)

(use-package! aas
  :commands aas-mode)

(setq yas-triggers-in-field t)

(use-package! string-inflection
  :commands (string-inflection-all-cycle
             string-inflection-toggle
             string-inflection-camelcase
             string-inflection-lower-camelcase
             string-inflection-kebab-case
             string-inflection-underscore
             string-inflection-capital-underscore
             string-inflection-upcase)
  :init
  (map! :leader :prefix ("c~" . "naming convention")
        :desc "cycle" "~" #'string-inflection-all-cycle
        :desc "toggle" "t" #'string-inflection-toggle
        :desc "CamelCase" "c" #'string-inflection-camelcase
        :desc "downCase" "d" #'string-inflection-lower-camelcase
        :desc "kebab-case" "k" #'string-inflection-kebab-case
        :desc "under_score" "_" #'string-inflection-underscore
        :desc "Upper_Score" "u" #'string-inflection-capital-underscore
        :desc "UP_CASE" "U" #'string-inflection-upcase)
  (after! evil
    (evil-define-operator evil-operator-string-inflection (beg end _type)
      "Define a new evil operator that cycles symbol casing."
      :move-point nil
      (interactive "<R>")
      (string-inflection-all-cycle)
      (setq evil-repeat-info '([?g ?~])))
    (define-key evil-normal-state-map (kbd "g~") 'evil-operator-string-inflection)
    (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
    (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
    ))

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(use-package! authinfo-color-mode
  :mode ("authinfo.gpg\\'" . authinfo-color-mode)
  :init (advice-add 'authinfo-mode :override #'authinfo-color-mode))

(use-package! systemd
  :defer t)

(setq global-visual-line-mode t
 evil-respect-visual-line-mode t)

(use-package treesit-auto
  :demand t
  :config
  (setq treesit-auto-install 'prompt)
  (setq treesit-font-lock-level 4) ; all the fontlock details
  (global-treesit-auto-mode))

(map! :map evil-normal-state-map "t t" 'transpose-chars)

(provide 'editing)
