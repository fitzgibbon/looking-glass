;;; looking-glass-json-test.el --- Tests for looking-glass-json -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'looking-glass)
(require 'looking-glass-json)

(ert-deftest lg-json-string-boundary-prism ()
  (let* ((optic (lg-json-string-alist))
         (source "{\"user\":{\"name\":\"Ada\",\"age\":20}}"))
    (should (equal (lg-view (lg-compose optic (lg-jkey "user") (lg-jkey "name"))
                            source)
                   "Ada"))
    (let* ((updated (lg-over (lg-compose optic
                                         (lg-jpath "user" "age"))
                             (lambda (v) (+ v 1))
                             source))
           (decoded (lg-view optic updated)))
      (should (= (lg-view (lg-compose (lg-jkey "user") (lg-jkey "age"))
                          decoded)
                 21)))))

(ert-deftest lg-jkey-and-jindex-over-nested-values ()
  (let ((data '(("users" . [(("name" . "Ada")) (("name" . "Bob"))]))))
    (should (equal (lg-view (lg-compose (lg-jkey "users")
                                        (lg-jindex 1)
                                        (lg-jkey "name"))
                            data)
                   "Bob"))
    (should (equal (lg-over (lg-compose (lg-jkey "users")
                                        (lg-jindex 0)
                                        (lg-jkey "name"))
                            #'upcase
                            data)
                   '(("users" . [(("name" . "ADA")) (("name" . "Bob"))]))))))

(ert-deftest lg-ijkey-and-ijindex-report-indices ()
  (should (equal (lg-ito-list-of (lg-ijkey "x") '(("x" . 10)))
                 '(("x" . 10))))
  (should (equal (lg-ito-list-of (lg-ijindex 2) [4 5 6 7])
                 '((2 . 6)))))

(ert-deftest lg-jvalues-and-ijvalues-on-arrays ()
  (should (equal (lg-to-list-of (lg-jvalues) [1 2 3])
                 '(1 2 3)))
  (should (equal (lg-over (lg-jvalues) (lambda (v) (* 2 v)) '(1 2 3))
                 '(2 4 6)))
  (should (equal (lg-ito-list-of (lg-ijvalues) [7 8])
                 '((0 . 7) (1 . 8)))))

(ert-deftest lg-json-macro-sugar-forms ()
  (let ((source '(("user" . (("name" . "ada") ("age" . 20))))))
    (should (equal (lg^ source (lg/path-json "user" "name"))
                   "ada"))
    (should (equal (lg= source (lg/path-json "user" "age") 21)
                   '(("user" . (("name" . "ada") ("age" . 21))))))
    (should (equal (lg~ source (lg/path-json "user" "name") #'upcase)
                   '(("user" . (("name" . "ADA") ("age" . 20))))))))

(ert-deftest lg-json-macro-sugar-indexed-and-threading ()
  (should (equal (lg~i '(10 20 30)
                       (lg-ieach-list)
                       (idx value)
                       (+ value idx))
                 '(10 21 32)))
  (should (equal (lg->> '(1 2 3)
                        (lg-over (lg-each-list) (lambda (v) (* 2 v)))
                        (lg-over (lg-each-list) (lambda (v) (+ v 1))))
                 '(3 5 7)))
  (should (equal (lg-> '(1 2 3)
                       (append '(0))
                       (append '(9)))
                 '(1 2 3 0 9))))

(provide 'looking-glass-json-test)

;;; looking-glass-json-test.el ends here
