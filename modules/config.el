;;; config.el -*- lexical-binding: t; -*-

(setq module-dir (concat doom-private-dir "modules/"))

(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(defun config-visit ()
  (interactive)
  (find-file (concat doom-private-dir "config.el")))

(defun init-visit ()
  (interactive)
  (find-file (concat doom-private-dir "init.el")))

(defun packages-visit ()
  (interactive)
  (find-file (concat doom-private-dir "packages.el")))

(defun module-visit ()
  (interactive)
  (find-file module-dir))

(defun recompile-modules ()
  (interactive)
  (digit-argument nil)
  (byte-recompile-directory module-dir 0))

(defun config-reload ()
  (interactive)
  ;;(recompile-modules)
  (load-file (expand-file-name (concat doom-private-dir "config.el"))))

(global-set-key (kbd "C-c e c") 'config-visit)
(global-set-key (kbd "C-c e p") 'packages-visit)
(global-set-key (kbd "C-c e i") 'init-visit)
(global-set-key (kbd "C-c e m") 'module-visit)
(global-set-key (kbd "C-c r") 'config-reload)

(provide 'config)
