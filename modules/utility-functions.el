(defun string-inflection-title-to-lisp-case-function (title-str)
  "Title String for Something => title-string-for-something"
  (string-inflection-kebab-case-function (s-replace-all '((" " . "-")) title-str)))
(provide 'utility-functions)
