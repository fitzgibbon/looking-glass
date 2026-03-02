;;; looking-glass-toml-test.el --- TOML package tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass)
(require 'looking-glass-toml)

(ert-deftest lg-toml-basic-optics ()
  (let ((toml '("title" "Example" "port" 8080 "features" ["x" "y"])))
    (should (equal (lg-preview (lg-toml-key "title") toml) (lg-just "Example")))
    (should (equal (lg-preview (lg-compose (lg-toml-array-index 1)
                                           (lg-toml-key "features"))
                               toml)
                   (lg-just "y")))
    (should (equal (lg-set (lg-toml-key-at "port") (lg-just 8081) toml)
                   '("title" "Example" "port" 8081 "features" ["x" "y"])))))

(provide 'looking-glass-toml-test)

;;; looking-glass-toml-test.el ends here
