;;; shortcuts.el -*- lexical-binding: t; -*-

;;;; Copy-whole-line
(defun copy-whole-line ()
  (interactive)
  (save-excursion
    (kill-ring-save (point-at-bol) (point-at-eol))))

(global-set-key (kbd "C-c w l") 'copy-whole-line)

;;;; Copy-line-above and copy-line-below (and paste)
(defun copy-line-above ()
  (interactive)
  (save-excursion
    (evil-previous-visual-line)
    (copy-whole-line)
    (evil-next-visual-line)
    (evil-paste-after 1)))

(global-set-key (kbd "C-c l a") 'copy-line-above)

(defun copy-line-below ()
  (interactive)
  (save-excursion
    (evil-next-visual-line)
    (copy-whole-line)
    (evil-previous-visual-line)
    (evil-paste-after 1)))

(global-set-key (kbd "C-c l b") 'copy-line-below)

;;;; Duplicate line
(defun duplicate-line ()
  (interactive)
  (save-excursion
    (evil-open-below 1)
    (copy-line-above))
  (evil-next-visual-line)
  (evil-normal-state)
  (evil-forward-char))

(global-set-key (kbd "C-c l l") 'duplicate-line)

;;; Kill word improved
;;; normal kill-word kills forward, but not whole word. This fixes that
(defun kill-whole-word ()
  (interactive)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c k w") 'kill-whole-word)

;;; File shortcuts
;; Note taken on [2018-08-03 Fri 18:19]
(global-unset-key (kbd "C-c z"))

(defadvice goto-line (after unfold-tree activate)
  (when (outline-invisible-p)
    (save-excursion
      (outline-previous-visible-heading 1)
      (org-fold-show-subtree))))


(defun agenda-today ()
  (interactive)
  (goto-line (string-to-number (shell-command-to-string "~/.scripts/agendatoday")))
  (org-reveal 1))

(defun dailyplan()
  (interactive)
  (find-file (shell-command-to-string "date +'/home/user/dp/dailyplan/%Y/%Y-%m/%Y-%m-%d.org' | tr -d '\n'"))
  (end-of-buffer))

                                        ;(add-hook 'find-file-hook 'dailyplan-hook)
                                        ;(defun dailyplan-hook ()
                                        ;  (when (string= (buffer-file-name) "dailyplan.org")
                                        ;    (agenda-today)))

(defun books()
  (interactive)
  (find-file "~/pCloudDrive/agenda/books.org"))

(defun thesis()
  (interactive)
  (find-file "~/nextcloud/bachelor/thesis/structure.tex"))

(defun projects()
  (interactive)
  (find-file "~/pCloudDrive/agenda/currprojects.org"))

(defun movies()
  (interactive)
  (find-file "~/pCloudDrive/agenda/movies.org"))

(defun reviews()
  (interactive)
  (find-file "~/pCloudDrive/agenda/reviews/2018.org")
  (split-and-follow-vertically)
  (find-file "~/pCloudDrive/agenda/reviews/template.org"))

(defun ceres()
  (interactive)
  (find-file "/ssh:user@sermak.xyz:~"))

(defun ceres-root()
  (interactive)
  (find-file "/ssh:user@sermak.xyz|sudo:root@sermak.xyz:/"))

(defun jarvis()
  (interactive)
  (find-file "/ssh:user@sermak.xyz|sudo:root@jarvis:/"))

(defun jarvis-root()
  (interactive)
  (find-file "/ssh:user@sermak.xyz|ssh:user@jarvis:/"))

(global-set-key (kbd "C-c z d") 'dailyplan)
(global-set-key (kbd "C-c z b") 'books)
(global-set-key (kbd "C-c z m") 'movies)
(global-set-key (kbd "C-c z r") 'reviews)
(global-set-key (kbd "C-c z p") 'projects)
(global-set-key (kbd "C-c z t") 'thesis)
(global-set-key (kbd "C-c z e") 'mu4e)
(global-set-key (kbd "C-c z s c") 'ceres)
(global-set-key (kbd "C-c z s r") 'ceres-root)

;; University
(setq uni-base-folder "/mnt/server-de/mnt/backup/backups/pre_master/Uni")

(defun open-uni-folder (folder)
  "Mount/Open university folder specified as FOLDER."
  (when (not (file-exists-p uni-base-folder))
    (shell-command "sshfs -p2222 sermak.xyz:/ /mnt/server-de"))
  (let ((dir (f-join uni-base-folder folder)))
    (when (not (file-exists-p dir))
      (mkdir dir t))
    (find-file (f-join uni-base-folder folder))))

(defmacro uni-folder-shortcut (shortcut folder funcname)
  `(progn
     (defun ,funcname ()
       ,(format "Open Uni/%s" folder)
       (interactive)
       (open-uni-folder ,folder))
     (global-set-key (kbd (concat "C-c u " ,shortcut)) ',funcname)))

(uni-folder-shortcut "u" "" uni)
(uni-folder-shortcut "6" "6" uni6)
(uni-folder-shortcut "l 1" "6/Orthodox Liturgy I" orthodox-liturgy-1)
(uni-folder-shortcut "l 2" "6/Orthodox Liturgy II" orthodox-liturgy-2)
(uni-folder-shortcut "h 1" "6/Orthodox History I" orthodox-history-1)
(uni-folder-shortcut "h 2" "6/Orthodox History II" orthodox-history-2)
(uni-folder-shortcut "t 1" "6/Orthodox Theology I" orthodox-theology-1)
(uni-folder-shortcut "t 2" "6/Orthodox Theology II" orthodox-theology-2)
(uni-folder-shortcut "s" "6/Orthodox Scripture" orthodox-scripture)
(uni-folder-shortcut "a" "6/Orthodox Anthropology" orthodox-anthropology)
(uni-folder-shortcut "w" "6/War and Statesbuilding in Afghanistan" war-and-statesbuilding)
(uni-folder-shortcut "e" "6/Exegesis of the Old and New Testament" exegesis)
(uni-folder-shortcut "m" "6/Monte Carlo Techniques" monte-carlo)

;; Tones
(global-set-key (kbd "C-c -") (lambda () (interactive) (insert "̄")))
(global-set-key (kbd "C-c ^") (lambda () (interactive) (insert "̂")))
;;; Chinese tones
(global-set-key (kbd "C-c 1") (lambda () (interactive) (insert "̄")))
(global-set-key (kbd "C-c 2") (lambda () (interactive) (insert "́")))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (insert "̌")))
(global-set-key (kbd "C-c 4") (lambda () (interactive) (insert "̀")))
;;; Devanagari anusvara
(global-set-key (kbd "C-c 5") (lambda () (interactive) (insert "̇")))
(global-set-key (kbd "C-c 6") (lambda () (interactive) (insert "̣")))

;;; Rectangle mark mode
(global-set-key (kbd "C-ü") (lambda () (interactive) (rectangle-mark-mode)))
;;; Sudo-edit
(use-package! sudo-edit
  :bind ("C-c s" . sudo-edit))

(defun rededicate-window ()
  "Toggles window dedication in the selected window."
  (interactive)
  (let ((dedication (not (window-dedicated-p (selected-window)))))
    (message (format "%s" dedication))
    (set-window-dedicated-p (selected-window) dedication)))

(global-set-key (kbd "s-S-<return>") (lambda () (interactive) (+vterm/toggle nil)))

                                        ; Org agenda

(defun agenda-folder ()
  (interactive)
  (find-file "/home/user/sync/agenda/"))

(defun agenda-daily ()
  (find-file "/home/user/sync/agenda/daily.org"))

(defun agenda-show-daily-habits ()
  "Open daily habits document as org column"
  (interactive)
  (agenda-daily)
  (org-columns)
  (org-modern-mode -1))

(defun agenda-uni ()
  (interactive)
  (find-file "/home/user/sync/agenda/uni.org"))

(defun agenda-personal ()
  (interactive)
  (find-file "/home/user/sync/agenda/personal.org"))

(global-set-key (kbd "C-c a a") 'agenda-folder)
(global-set-key (kbd "C-c a u") 'agenda-uni)
(global-set-key (kbd "C-c a p") 'agenda-personal)
(global-set-key (kbd "s-t") 'agenda-show-daily-habits)

                                        ; Books

(defun books ()
  (interactive)
  (find-file "/home/user/dox/books/"))

(global-set-key (kbd "C-c b") 'books)

(provide 'shortcuts)
