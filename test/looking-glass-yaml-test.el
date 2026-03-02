;;; looking-glass-yaml-test.el --- YAML package tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass)
(require 'looking-glass-yaml)

(ert-deftest lg-yaml-basic-optics ()
  (let ((yaml '((name . "Ada") (age . 10) (langs . ["elisp" "haskell"]))))
    (should (equal (lg-preview (lg-yaml-key 'name) yaml) (lg-just "Ada")))
    (should (equal (lg-preview (lg-compose (lg-yaml-index 0)
                                           (lg-yaml-key 'langs))
                               yaml)
                   (lg-just "elisp")))
    (should (equal (lg-set (lg-yaml-key-at 'city) (lg-just "London") yaml)
                   '((name . "Ada") (age . 10) (langs . ["elisp" "haskell"]) (city . "London"))))))

(provide 'looking-glass-yaml-test)

;;; looking-glass-yaml-test.el ends here
