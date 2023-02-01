;;; latex.el -*- lexical-binding: t; -*-

(setq TeX-command-extra-options "--shell-escape")
(setq TeX-engine 'luatex)
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "<f6>")
                           (lambda () (interactive) (org-latex-export-to-pdf t)))))
(defun force-compile ()
  "Set the file modification times on the current file, then call
TeX-command-sequence.
This forces a complete recompilation of the document, even if the source
(.tex) is older than any existing outputs (.pdf etc)."
  (interactive)
  (set-buffer-modified-p t) ;; sets mod time to current time
  (save-buffer)
  (TeX-command-sequence t t))

(defun auto-async-export ()
  (when (and (eq major-mode 'org-mode)
             (string-equal "t" (pop (cdr (car (org-collect-keywords '("auto_async_export")))))))
    (message "Exporting to pdf...")
    (org-latex-export-to-pdf t)
    (when (eq major-mode 'latex-mode)
      (force-compile))
    )
  )

(defun org-after-save-cmd ()
  (interactive)
  (when (eq major-mode 'org-mode)
  (let ((cmd (cdr (car (org-collect-keywords '("on_save_cmd"))))))
        (when cmd
  (async-shell-command-no-window (pop cmd))))))
(setq password-cache t ; enable password caching
      password-cache-expiry 36000) ; for ten hours (time in secs)

(add-hook! 'after-save-hook 'auto-async-export)
(add-hook! 'after-save-hook 'org-after-save-cmd)
                                        ;(global-set-key [f6] (lambda () (interactive) (org-latex-export-to-pdf t)))
(setq org-latex-compiler "lualatex")
(setq-default TeX-master nil)
(add-to-list 'org-latex-packages-alist
             '("AUTO" "polyglossia" t ("xelatex" "lualatex")))

(defun latex-word-count ()
  "Return latex word count using texlive"
  (interactive)
  (let ((file-name (if (eq major-mode 'latex-mode)
                       (buffer-file-name)
                     (if (eq major-mode 'org-mode)
                         (file-name-with-extension (buffer-file-name) "tex")
                       (buffer-file-name)))))
  (message (shell-command-to-string (format "texcount -1 -merge -template={1} %s" file-name)))))

(setq org-export-headline-levels 5)
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))
(setq org-preview-latex-process-alist
   '((dvipng :programs
      ("dvilualatex" "dvipng")
      :description "dvi > png" :message "you need to install the programs: dvilualatex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
      (1.0 . 1.0)
      :latex-compiler
      ("dvilualatex -interaction nonstopmode -output-directory %o %f")
      :image-converter
      ("dvipng -D %D -T tight -o %O %f")
      :transparent-image-converter
      ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
     (dvisvgm :programs
      ("lualatex" "dvisvgm")
      :description "dvi > svg" :message "you need to install the programs: lualatex and dvisvgm." :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
      (1.7 . 1.5)
      :latex-compiler
      ("lualatex -interaction nonstopmode -output-directory %o %f")
      :image-converter
      ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs
      ("lualatex" "convert")
      :description "pdf > png" :message "you need to install the programs: lualatex and imagemagick." :image-input-type "pdf" :image-output-type "png" :image-size-adjust
      (1.0 . 1.0)
      :latex-compiler
      ("lualatex -interaction nonstopmode -output-directory %o %f")
      :image-converter
      ("convert -density %D -trim -antialias %f -quality 100 %O"))))

(provide 'latex-tweaks)
