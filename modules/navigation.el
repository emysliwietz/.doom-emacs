;;; navigation.el -*- lexical-binding: t; -*-

;; Kill minibuffer when loosing focus
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")

(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'consult-recent-file
      :ne "p" #'doom/open-private-config
      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :ne "." (cmd! (doom-project-find-file "~/.config/")) ; . for dotfiles
      :ne "b" #'+vertico/switch-workspace-buffer
      :ne "B" #'consult-buffer
      :ne "q" #'save-buffers-kill-terminal)

(map! :n [mouse-8] #'better-jumper-jump-backward
      :n [mouse-9] #'better-jumper-jump-forward)

;(setq org-roam-directory "") ;; Temporary workaroundA
;(setq frame-title-format
;      '(""
;        (:eval
;         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
;             (replace-regexp-in-string
;              ".*/[0-9]*-?" "☰ "
;              (subst-char-in-string ?_ ?  buffer-file-name))
;           "%b"))
;        (:eval
;         (let ((project-name (projectile-project-name)))
;           (unless (string= "-" project-name)
;             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))

(setq display-line-numbers-type 'relative)



;;; Switch window
(use-package! switch-window
  :config
  (setq switch-window-multiple-frames nil)
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts
	'("j" "k" "l" "a" "s" "d" "f")) ; ö does not work without pressing RET
  :bind
  ([remap other-window] . switch-window))

;;; Temporarily maximize current buffer
(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
	   (jump-to-register '_)
	 (progn
	   (window-configuration-to-register '_)
	   (delete-other-windows))))


(defun transparent-buffer-advice
  (orig-fun &rest args)
  (shell-command "transset -p 1") ; 0.3
  (let
      ((res
	(apply orig-fun args)))
    (shell-command "transset -p 1")
    res))


;;; kill current buffer
(defun kill-curr-buffer ()
  (interactive)
  (if (not (string-equal (buffer-name (current-buffer)) "*scratch*"))
      (kill-buffer (current-buffer))
    (bury-buffer)
  ))

;;; move to start and end of buffer
(global-set-key (kbd "M-n") 'end-of-buffer)
(global-set-key (kbd "M-p") 'beginning-of-buffer)

;; Kill all buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-x C-k k") 'close-all-buffers)

(setq org-agenda-files ())
;; Kill unwanted buffers
(defun kill-if-unwanted (buffer)
  (let ((b (buffer-name buffer))
	(bfn (buffer-file-name buffer))
	(unwanted-buffers '(
			    "*Messages*"
			    "*Backtrace*"
			    "*Help*"
			    "*Warnings*"
			    "*Compile-Log*"
			    "*elfeed-log*"
			    "*system-packages*"
			    "*Async Shell Command*"
			    "*Flycheck errors*"
			    "*Flycheck error messages*"
			    "*Flymake log*"
			    "*Calendar*"
			    "*XELB-DEBUG*"
			    "*Read-Aloud Log*"
			    "*elfeed-search*"
			    "elfeed.org"
			    )))
    (cond ((member b unwanted-buffers) (kill-buffer buffer))
	  ((member bfn (mapcar 'expand-file-name org-agenda-files)) (kill-buffer buffer))
	  ((string-match "^\*tramp.*\*$" b) (kill-buffer buffer))
	  ((string-match "\.png$" b) (kill-buffer buffer))
	  ((string-match "\.jpg$" b) (kill-buffer buffer))
	  ((string-match "\.jpeg$" b) (kill-buffer buffer))
	  ((string-match "\.gif$" b) (kill-buffer buffer))
	  ((string-match "\.log$" b) (kill-buffer buffer))
	  ((string-match "^_region_.tex$" b) (kill-buffer buffer))
	  ((string-match "^\*helpful .*\*" b) (kill-buffer buffer))
	  ((string-match "^magit" b) (kill-buffer buffer))
	  ((string-match "^\*.*\*$" b) (kill-buffer buffer))
	  )))

(defun kill-unwanted-buffers ()
  (interactive)
  (mapc 'kill-if-unwanted (buffer-list)))

(global-set-key (kbd "C-x k") 'kill-unwanted-buffers)

;;; Window splitting
(defun split-and-follow-horizontally ()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(defun kill-and-balance ()
  (interactive)
  (delete-window)
  (balance-windows))
(global-set-key (kbd "C-x 0") 'kill-and-balance)



;;; Subword moving
(global-subword-mode 1)

;;; Cycle though tabs
;(global-set-key (kbd "<C-tab>") 'next-buffer)
;(global-set-key (kbd "<C-iso-lefttab>") 'previous-buffer)

;;; Winum mode for easy moving through windows
(use-package! winum
  :config
  (setq winum-auto-setup-mode-line nil)
  (winum-mode t)
  :bind (
	 ("s-=" . winum-select-window-0)
	 ("s-!" . winum-select-window-1)
	 ("s-\"" . winum-select-window-2)
	 ("s-§" . winum-select-window-3)
	 ("s-$" . winum-select-window-4)
	 ("s-%" . winum-select-window-5)
	 ("s-&" . winum-select-window-6)
	 ("s-/" . winum-select-window-7)
	 ("s-(" . winum-select-window-8)
	 ("s-)" . winum-select-window-9)
	 ("s-°" . winum-select-window-by-number))
  )

(provide 'navigation)
