;;; looking-glass-json-schema-test.el --- JSON Schema converter tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass-json-schema)

(defun lg-json-schema-test--hash (&rest pairs)
  "Return an equal-tested hash table populated from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(defun lg-json-schema-test--date (year month day)
  "Return a semantic date with YEAR, MONTH, and DAY."
  (make-lg-json-schema-date :year year :month month :day day))

(ert-deftest lg-json-schema-converts-complete-nested-document ()
  (let* ((schema
          '(:properties
            (:created (:type "string" :format "date")
             :events
             (:items
              (:properties
               (:when (:type "string" :format "date")))))))
         (set (lg-json-schema-compile schema :object-type 'plist))
         (converter (lg-json-schema-converter set))
         (native '(:created "2025-01-02"
                   :events [(:when "2025-02-03") (:when "2025-03-04")]))
         (semantic (lg-view converter native)))
    (should (equal semantic
                   `(:created ,(lg-json-schema-test--date 2025 1 2)
                     :events
                     [(:when ,(lg-json-schema-test--date 2025 2 3))
                      (:when ,(lg-json-schema-test--date 2025 3 4))])))
    (should (equal (lg-review converter semantic) native))))

(ert-deftest lg-json-schema-converter-composes-with-ordinary-optics ()
  (let* ((set (lg-json-schema-compile
               '(:properties (:born (:format "date")))
               :object-type 'plist))
         (born (lg-compose (lg-json-schema-converter set)
                           (lg-plist-key :born)))
         (replacement (lg-json-schema-test--date 2000 2 29)))
    (should (equal (lg-view born '(:born "1999-12-31"))
                   (lg-json-schema-test--date 1999 12 31)))
    (should (equal (lg-set born replacement '(:born "1999-12-31"))
                   '(:born "2000-02-29")))))

(ert-deftest lg-json-schema-leaves-json-text-to-existing-parser-optics ()
  (let* ((set (lg-json-schema-compile
               "{\"properties\":{\"born\":{\"format\":\"date\"}}}"))
         (converter (lg-json-schema-converter set))
         (json-converter (lg-compose lg-json-parse converter))
         (semantic (lg-view json-converter "{\"born\":\"2025-01-02\"}")))
    (should (equal (gethash "born" semantic)
                   (lg-json-schema-test--date 2025 1 2)))
    (should (equal (lg-review json-converter semantic)
                   "{\"born\":\"2025-01-02\"}"))))

(ert-deftest lg-json-schema-supports-native-object-and-array-representations ()
  (let ((schema
         '(:properties (:dates (:items (:format "date"))))))
    (dolist (configuration
             '((plist array (:dates ["2025-01-01"]) :dates)
               (plist list (:dates ("2025-01-01")) :dates)
               (alist array ((dates . ["2025-01-01"])) dates)))
      (pcase-let ((`(,object-type ,array-type ,source ,key) configuration))
        (let* ((set (lg-json-schema-compile
                     schema :object-type object-type :array-type array-type))
               (semantic (lg-view (lg-json-schema-converter set) source))
               (dates (if (eq object-type 'plist)
                          (plist-get semantic key)
                        (cdr (assq key semantic)))))
          (should (equal (elt dates 0)
                         (lg-json-schema-test--date 2025 1 1))))))
    (let* ((source (lg-json-schema-test--hash "dates" '("2025-01-01")))
           (set (lg-json-schema-compile
                 schema :object-type 'hash-table :array-type 'list))
           (semantic (lg-view (lg-json-schema-converter set) source)))
      (should (equal (car (gethash "dates" semantic))
                     (lg-json-schema-test--date 2025 1 1))))))

(ert-deftest lg-json-schema-whole-conversion-is-pure ()
  (let* ((schema
          (lg-json-schema-test--hash
           "properties"
           (lg-json-schema-test--hash
            "date" (lg-json-schema-test--hash "format" "date"))))
         (set (lg-json-schema-compile schema))
         (source (lg-json-schema-test--hash "date" "2025-01-01"))
         (semantic (lg-view (lg-json-schema-converter set) source)))
    (should (equal (gethash "date" source) "2025-01-01"))
    (should (lg-json-schema-date-p (gethash "date" semantic)))
    (should-not (eq source semantic))))

(ert-deftest lg-json-schema-items-start-after-prefix-items ()
  (let* ((uppercase (lg-iso #'upcase #'downcase))
         (schema
          '(:prefixItems [(:format "code")]
            :items (:format "date")))
         (set (lg-json-schema-compile
               schema
               :formats `(("code" . ,uppercase))))
         (converter (lg-json-schema-converter set))
         (semantic (lg-view converter ["abc" "2025-01-01"])))
    (should (equal (aref semantic 0) "ABC"))
    (should (equal (aref semantic 1)
                   (lg-json-schema-test--date 2025 1 1)))
    (should (equal (lg-review converter semantic)
                   ["abc" "2025-01-01"]))))

(ert-deftest lg-json-schema-converts-tuples-by-position ()
  (let* ((schema
          '(:properties
            (:period
             (:prefixItems [(:format "date") (:format "date")]))))
         (set (lg-json-schema-compile schema :object-type 'plist))
         (converter (lg-json-schema-converter set))
         (semantic
          (lg-view converter '(:period ["2025-01-01" "2025-12-31"]))))
    (should (equal semantic
                   `(:period [,(lg-json-schema-test--date 2025 1 1)
                              ,(lg-json-schema-test--date 2025 12 31)])))
    (should (equal (lg-review converter semantic)
                   '(:period ["2025-01-01" "2025-12-31"])))))

(ert-deftest lg-json-schema-reuses-definitions-and-references ()
  (let* ((schema
          '(:$defs
            (:event (:properties (:on (:format "date"))))
            :properties
            (:event (:$ref "#/$defs/event"))))
         (set (lg-json-schema-compile schema :object-type 'plist))
         (root (lg-json-schema-converter set))
         (event (lg-json-schema-converter set "event")))
    (should (equal (lg-json-schema-definitions set) '("event")))
    (should (equal
             (lg-view root '(:event (:on "2025-05-06")))
             `(:event (:on ,(lg-json-schema-test--date 2025 5 6)))))
    (should (equal
             (lg-view event '(:on "2025-05-06"))
             `(:on ,(lg-json-schema-test--date 2025 5 6))))))

(ert-deftest lg-json-schema-supports-finite-values-with-recursive-references ()
  (let* ((schema
          '(:properties
            (:on (:format "date")
             :next (:$ref "#"))))
         (set (lg-json-schema-compile schema :object-type 'plist))
         (converter (lg-json-schema-converter set))
         (native '(:on "2025-01-01"
                   :next (:on "2025-01-02"
                          :next (:on "2025-01-03"))))
         (semantic (lg-view converter native)))
    (should (lg-json-schema-date-p
             (plist-get (plist-get (plist-get semantic :next) :next) :on)))
    (should (equal (lg-review converter semantic) native))))

(ert-deftest lg-json-schema-dispatches-tagged-one-of-converters ()
  (let* ((schema
          '(:oneOf
            [(:properties
              (:kind (:const "started")
               :at (:format "date-time")))
             (:properties
              (:kind (:const "scheduled")
               :on (:format "date"))) ]))
         (set (lg-json-schema-compile schema :object-type 'plist))
         (converter (lg-json-schema-converter set))
         (started (lg-view converter
                           '(:kind "started" :at "2025-01-02T03:04:05Z")))
         (scheduled (lg-view converter
                             '(:kind "scheduled" :on "2025-02-03"))))
    (should (integerp (time-convert (plist-get started :at) 'integer)))
    (should (equal (plist-get scheduled :on)
                   (lg-json-schema-test--date 2025 2 3)))
    (should (equal (lg-review converter started)
                   '(:kind "started" :at "2025-01-02T03:04:05Z")))
    (should (equal (lg-review converter scheduled)
                   '(:kind "scheduled" :on "2025-02-03")))))

(ert-deftest lg-json-schema-composes-all-of-converters ()
  (let* ((schema
          '(:allOf
            [(:properties (:start (:format "date")))
             (:properties (:end (:format "date"))) ]))
         (set (lg-json-schema-compile schema :object-type 'plist))
         (semantic
          (lg-view (lg-json-schema-converter set)
                   '(:start "2025-01-01" :end "2025-01-31"))))
    (should (lg-json-schema-date-p (plist-get semantic :start)))
    (should (lg-json-schema-date-p (plist-get semantic :end)))))

(ert-deftest lg-json-schema-custom-formats-override-defaults ()
  (let* ((uppercase (lg-iso #'upcase #'downcase))
         (set (lg-json-schema-compile
               '(:properties
                 (:code (:format "code")
                  :date (:format "date")))
               :object-type 'plist
               :formats `(("code" . ,uppercase)
                          ("date" . ,uppercase))))
         (converter (lg-json-schema-converter set))
         (semantic (lg-view converter
                            '(:code "abc" :date "mixed-case"))))
    (should (equal semantic '(:code "ABC" :date "MIXED-CASE")))
    (should (equal (lg-review converter semantic)
                   '(:code "abc" :date "mixed-case")))))

(ert-deftest lg-json-schema-date-and-date-time-converters-are-canonical ()
  (let* ((date (lg-view lg-json-schema-date-iso "2024-02-29"))
         (instant
          (lg-view lg-json-schema-date-time-iso
                   "2025-01-02T03:04:05.125Z"))
         (precise
          (lg-view lg-json-schema-date-time-iso
                   "2025-01-02T03:04:05.123456789012Z")))
    (should (equal (lg-review lg-json-schema-date-iso date) "2024-02-29"))
    (should (equal (lg-review lg-json-schema-date-time-iso instant)
                   "2025-01-02T03:04:05.125Z"))
    (should (equal (lg-review lg-json-schema-date-time-iso precise)
                   "2025-01-02T03:04:05.123456789012Z"))
    (should-error (lg-view lg-json-schema-date-iso "2025-02-29"))
    (should-error
     (lg-view lg-json-schema-date-time-iso "2025-02-30T03:04:05Z"))
    (should-error
     (lg-view lg-json-schema-date-time-iso "2025-01-01T25:00:00Z"))))

(ert-deftest lg-json-schema-time-and-duration-codecs ()
  (let* ((time
          (lg-view lg-json-schema-time-iso "12:34:56.125+01:30"))
         (duration
          (lg-view lg-json-schema-duration-iso "P1Y2M3DT4H5M6.125S")))
    (should (equal (lg-review lg-json-schema-time-iso time)
                   "12:34:56.125+01:30"))
    (should (equal (lg-review lg-json-schema-duration-iso duration)
                   "P1Y2M3DT4H5M6.125S"))
    (should-error (lg-view lg-json-schema-time-iso "25:00:00Z"))
    (should-error (lg-view lg-json-schema-duration-iso "P"))))

(ert-deftest lg-json-schema-network-address-codecs ()
  (let ((ipv4 (lg-view lg-json-schema-ipv4-iso "192.0.2.1"))
        (ipv6 (lg-view lg-json-schema-ipv6-iso "2001:db8::1")))
    (should (equal ipv4 [192 0 2 1]))
    (should (equal ipv6 [8193 3512 0 0 0 0 0 1]))
    (should (equal (lg-review lg-json-schema-ipv4-iso ipv4) "192.0.2.1"))
    (should (equal (lg-review lg-json-schema-ipv6-iso ipv6)
                   "2001:db8:0:0:0:0:0:1"))
    (should-error (lg-view lg-json-schema-ipv4-iso "999.0.0.1"))
    (should-error (lg-view lg-json-schema-ipv6-iso "2001::db8::1"))))

(ert-deftest lg-json-schema-uri-and-reference-codecs ()
  (let ((uri (lg-view lg-json-schema-uri-iso
                      "https://example.com/a?x=1#part"))
        (reference (lg-view lg-json-schema-uri-reference-iso "../a?x=1")))
    (should (url-p uri))
    (should (equal (lg-review lg-json-schema-uri-iso uri)
                   "https://example.com/a?x=1#part"))
    (should (equal (lg-review lg-json-schema-uri-reference-iso reference)
                   "../a?x=1"))
    (should-error (lg-view lg-json-schema-uri-iso "../relative"))))

(ert-deftest lg-json-schema-email-and-hostname-codecs ()
  (let ((email (lg-view lg-json-schema-email-iso "alice@Example.COM"))
        (idn-email
         (lg-view lg-json-schema-idn-email-iso
                  "user@xn--r8jz45g.xn--zckzah"))
        (hostname (lg-view lg-json-schema-hostname-iso "Example.COM"))
        (idn-hostname
         (lg-view lg-json-schema-idn-hostname-iso
                  "xn--r8jz45g.xn--zckzah")))
    (should (equal (lg-json-schema-email-local email) "alice"))
    (should (equal (lg-review lg-json-schema-email-iso email)
                   "alice@example.com"))
    (should (equal (lg-json-schema-email-domain idn-email) "例え.テスト"))
    (should (equal (lg-review lg-json-schema-idn-email-iso idn-email)
                   "user@xn--r8jz45g.xn--zckzah"))
    (should (equal hostname ["example" "com"]))
    (should (equal idn-hostname ["例え" "テスト"]))))

(ert-deftest lg-json-schema-uuid-and-base64-codecs ()
  (let* ((uuid-text "550e8400-e29b-41d4-a716-446655440000")
         (uuid (lg-view lg-json-schema-uuid-iso uuid-text))
         (bytes (lg-view lg-json-schema-base64-iso "aGVsbG8=")))
    (should (= (length uuid) 16))
    (should-not (multibyte-string-p uuid))
    (should (equal (lg-review lg-json-schema-uuid-iso uuid) uuid-text))
    (should (equal bytes "hello"))
    (should-not (multibyte-string-p bytes))
    (should (equal (lg-review lg-json-schema-base64-iso bytes) "aGVsbG8="))))

(ert-deftest lg-json-schema-regex-codec-produces-matchable-emacs-regex ()
  (let* ((source "^(cat|dog)+\\d{2}$")
         (regex (lg-view lg-json-schema-regex-iso source)))
    (should (lg-json-schema-regex-match-p regex "catdog42"))
    (should-not (lg-json-schema-regex-match-p regex "catbird42"))
    (should (equal (lg-review lg-json-schema-regex-iso regex) source))
    (should-error (lg-view lg-json-schema-regex-iso "(?<=id=)\\d+"))
    (should-error (lg-view lg-json-schema-regex-iso "\\p{Letter}+"))))

(ert-deftest lg-json-schema-pointer-and-template-codecs ()
  (let ((pointer
         (lg-view lg-json-schema-json-pointer-iso "/a~1b/~0name"))
        (relative
         (lg-view lg-json-schema-relative-json-pointer-iso "2/items/0"))
        (template
         (lg-view lg-json-schema-uri-template-iso
                  "https://example.com/{user}/{id}")))
    (should (equal (lg-json-schema-pointer-tokens pointer)
                   '("a/b" "~name")))
    (should (equal (lg-review lg-json-schema-json-pointer-iso pointer)
                   "/a~1b/~0name"))
    (should (= (lg-json-schema-relative-pointer-up relative) 2))
    (should (equal
             (lg-review lg-json-schema-relative-json-pointer-iso relative)
             "2/items/0"))
    (should (equal (lg-json-schema-uri-template-variables template)
                   '("user" "id")))
    (should (equal (lg-review lg-json-schema-uri-template-iso template)
                   "https://example.com/{user}/{id}"))))

(ert-deftest lg-json-schema-compiler-applies-formats-and-content-encoding ()
  (let* ((schema
          '(:properties
            (:address (:format "ipv4")
             :pattern (:format "regex")
             :payload (:contentEncoding "base64"))))
         (set (lg-json-schema-compile schema :object-type 'plist))
         (converter (lg-json-schema-converter set))
         (native '(:address "192.0.2.1"
                   :pattern "^id=\\d+$"
                   :payload "aGVsbG8="))
         (semantic (lg-view converter native)))
    (should (equal (plist-get semantic :address) [192 0 2 1]))
    (should (lg-json-schema-regex-match-p
             (plist-get semantic :pattern) "id=42"))
    (should (equal (plist-get semantic :payload) "hello"))
    (should (equal (lg-review converter semantic) native))))

(ert-deftest lg-json-schema-does-not-duplicate-json-primitive-conversion ()
  (should (equal (json-parse-string "[true,false,null]" :array-type 'list)
                 '(t :false :null)))
  (let* ((schema
          '(:prefixItems
            [(:type "boolean") (:type "boolean") (:type "null")]))
         (set (lg-json-schema-compile schema :array-type 'list
                                     :null-object :null
                                     :false-object :false))
         (value '(t :false :null)))
    (should (equal (lg-view (lg-json-schema-converter set) value) value))))

(ert-deftest lg-json-schema-identity-and-errors ()
  (let* ((set (lg-json-schema-compile '(:properties (:name (:type "string")))
                                      :object-type 'plist))
         (value '(:name "Ada")))
    (should (equal (lg-view (lg-json-schema-converter set) value) value))
    (should-error (lg-json-schema-converter set "missing")))
  (should-error (lg-json-schema-compile '(:$ref "https://example.com/s")))
  (should-error (lg-json-schema-compile "not json"))
  (should-error (lg-json-schema-compile '(:properties ())
                                        :object-type 'vector)))

(provide 'looking-glass-json-schema-test)

;;; looking-glass-json-schema-test.el ends here
