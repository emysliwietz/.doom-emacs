(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(citar-org-styles-format 'short)
 '(custom-safe-themes
   '("88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-preview-latex-process-alist
   '((dvipng :programs ("dvilualatex" "dvipng") :description "dvi > png" :message
      "you need to install the programs: dvilualatex and dvipng."
      :image-input-type "dvi" :image-output-type "png" :image-size-adjust
      (1.0 . 1.0) :latex-compiler
      ("dvilualatex -interaction nonstopmode -output-directory %o %f")
      :image-converter ("dvipng -D %D -T tight -o %O %f")
      :transparent-image-converter
      ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
     (dvisvgm :programs ("lualatex" "dvisvgm") :description "dvi > svg" :message
      "you need to install the programs: lualatex and dvisvgm."
      :image-input-type "dvi" :image-output-type "svg" :image-size-adjust
      (1.7 . 1.5) :latex-compiler
      ("lualatex -interaction nonstopmode -output-directory %o %f")
      :image-converter ("dvisvgm %f -n -b min -c %S -o %O"))
     (imagemagick :programs ("lualatex" "convert") :description "pdf > png"
      :message "you need to install the programs: lualatex and imagemagick."
      :image-input-type "pdf" :image-output-type "png" :image-size-adjust
      (1.0 . 1.0) :latex-compiler
      ("lualatex -interaction nonstopmode -output-directory %o %f")
      :image-converter
      ("convert -density %D -trim -antialias %f -quality 100 %O"))))
 '(package-selected-packages
   '(all-the-icons-ivy async-await cmake-mode exwm ivy-youtube notmuch spell-fu
     transient vertico))
 '(safe-local-variable-values '((TeX-master . resume.tex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "dark violet" :weight bold :height 4.0))))
 '(doom-modeline-buffer-modified ((t (:foreground "orange"))))
 '(org-cite ((t (:foreground "purple"))))
 '(org-cite-key ((t (:foreground "MediumPurple1" :slant italic)))))
(put 'secrets-mode 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'customize-set-variable 'disabled nil)
