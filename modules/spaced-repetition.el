(defun pamparam-latin ()
  (interactive)
  (find-file "~/dox/pamparam/latin/Latin.org"))

(defun pamparam-tagalog ()
  (interactive)
  (find-file "~/dox/pamparam/tagalog/Tagalog.org"))

(defun pamparam-drill-latin ()
  (interactive)
  (find-file "~/dox/pamparam/latin/Latin.pam")
  (pamparam-drill))

(defun pamparam-drill-tagalog ()
  (interactive)
  (find-file "~/dox/pamparam/tagalog/Tagalog.pam")
  (pamparam-drill))

(defun pamparam-magit-commit ()
  (interactive)
  (find-file "~/dox/pamparam/")
  (magit)
  )

(defun pamparam-push ()
  (interactive)
  (async-shell-command "cd ~/dox/pamparam/ && ~/dox/pamparam/update.sh")
  )

(add-to-list 'recentf-exclude "pamparam.*/cards")
(add-to-list 'recentf-exclude "/pamparam/")

;;* `hydra-pamparam'
(defhydra hydra-pamparam (:exit t)
  "pam"
  ("t" pamparam-drill-tagalog "tagalog")
  ("l" pamparam-drill-latin "latin")
  ("d" pamparam-drill "drill")
  ("s" pamparam-sync "sync")
  ("m" pamparam-pull "more cards")
  ("p" pamparam-push "push")
  ("gl" pamparam-latin "goto latin")
  ("gt" pamparam-tagalog "goto tagalog")
  ("q" nil "quit"))
(hydra-set-property 'hydra-pamparam :verbosity 1)

(global-set-key (kbd "C-c v") 'hydra-pamparam/body)

(setq pamparam-path "/home/user/dox/pamparam/pamparam.pam")

(provide 'spaced-repetition)
