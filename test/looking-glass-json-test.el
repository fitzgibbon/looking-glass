;;; looking-glass-json-test.el --- JSON package tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass)
(require 'looking-glass-json)

(ert-deftest lg-json-key-index-and-text-prism ()
  (let ((doc '(("name" . "Ada") ("age" . 10) ("tags" . ["elisp" "optics"]))))
    (should (equal (lg-preview (lg-json-object-key "name") doc) (lg-just "Ada")))
    (should (equal (lg-over (lg-json-object-key "age") (lambda (x) (+ x 1)) doc)
                   '(("name" . "Ada") ("age" . 11) ("tags" . ["elisp" "optics"]))))
    (should (equal (lg-preview (lg-compose (lg-json-array-index 1)
                                           (lg-json-object-key "tags"))
                               doc)
                   (lg-just "optics"))))

  (let ((roundtrip (lg-preview lg-json-text-prism "{\"name\":\"Ada\",\"age\":10}")))
    (should (lg-just-p roundtrip))
    (should (stringp (lg-review lg-json-text-prism (cdr roundtrip)))))

  (should (equal (lg-preview lg-json-bool t) (lg-just t)))
  (should (equal (lg-preview lg-json-bool :false) (lg-just nil)))
  (should (equal (lg-review lg-json-bool nil) :false))
  (should (equal (lg-preview lg-json-null-prism lg-json-null) (lg-just lg-json-null))))

(ert-deftest lg-json-values-and-members ()
  (let ((doc '(("name" . "Ada") ("age" . 10))))
    (should (equal (lg-to-list-of lg-json-values doc) '("Ada" 10)))
    (should (equal (lg-over lg-json-values (lambda (value)
                                             (if (numberp value) (+ value 1) value))
                           doc)
                   '(("name" . "Ada") ("age" . 11))))
    (should (equal (lg-ito-list-of lg-json-members doc)
                   '(("name" . "Ada") ("age" . 10))))))

(provide 'looking-glass-json-test)

;;; looking-glass-json-test.el ends here
