;;; looking-glass-json-schema-test.el --- JSON Schema optic tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'looking-glass-json-schema)

(defun lg-json-schema-test--hash (&rest pairs)
  "Return an equal-tested hash table populated from PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(ert-deftest lg-json-schema-generates-nested-text-optics ()
  (let* ((schema
          "{\"type\":\"object\",\"properties\":{\"user\":{\"type\":\"object\",\"properties\":{\"name\":{\"type\":\"string\"}}},\"enabled\":{\"type\":\"boolean\"}}}")
         (optics (lg-json-schema-generate schema))
         (name (lg-json-schema-optic optics "/user/name" 'text)))
    (should (equal (lg-json-schema-paths optics)
                   '("" "/enabled" "/user" "/user/name")))
    (should (equal (lg-over name #'upcase
                            "{\"user\":{\"name\":\"Ada\"},\"enabled\":true}")
                   "{\"user\":{\"name\":\"ADA\"},\"enabled\":true}"))))

(ert-deftest lg-json-schema-generated-parsed-optics-are-pure ()
  (let* ((schema (lg-json-schema-test--hash
                  "properties"
                  (lg-json-schema-test--hash "name" (make-hash-table))))
         (optics (lg-json-schema-generate schema))
         (name (lg-json-schema-optic optics "/name"))
         (source (lg-json-schema-test--hash "name" "Ada" "count" 1))
         (updated (lg-over name #'upcase source)))
    (should (equal (gethash "name" source) "Ada"))
    (should (equal (gethash "name" updated) "ADA"))
    (should-not (eq source updated))))

(ert-deftest lg-json-schema-optional-property-has-no-focus-when-absent ()
  (let* ((optics (lg-json-schema-generate
                  '(:properties (:nickname (:type "string")))
                  :object-type 'plist
                  :null-object :null))
         (nickname (lg-json-schema-optic optics "/nickname"))
         (source '(:name "Ada")))
    (should-not (lg-has nickname source))
    (should (eq source (lg-over nickname #'upcase source)))))

(ert-deftest lg-json-schema-supports-native-schema-and-instance-object-types ()
  (let* ((alist-schema
          '((properties
             (profile
              (properties
               (age (type . "number")))))))
         (alist-set (lg-json-schema-generate
                     alist-schema :object-type 'alist :null-object :null))
         (alist-optic (lg-json-schema-optic alist-set "/profile/age"))
         (plist-schema
          '(:properties (:profile (:properties (:age (:type "number"))))))
         (plist-set (lg-json-schema-generate
                     plist-schema :object-type 'plist :null-object :null))
         (plist-optic (lg-json-schema-optic plist-set "/profile/age")))
    (should (equal (lg-over alist-optic #'1+ '((profile . ((age . 41)))))
                   '((profile . ((age . 42))))))
    (should (equal (lg-over plist-optic #'1+ '(:profile (:age 41)))
                   '(:profile (:age 42))))))

(ert-deftest lg-json-schema-generates-homogeneous-array-traversals ()
  (let* ((schema
          '(:properties (:scores (:type "array" :items (:type "number")))))
         (vector-set (lg-json-schema-generate
                      schema :object-type 'plist :null-object :null))
         (vector-optic (lg-json-schema-optic vector-set "/scores/*"))
         (list-set (lg-json-schema-generate
                    schema :object-type 'hash-table :array-type 'list
                    :null-object :null))
         (list-optic (lg-json-schema-optic list-set "/scores/*"))
         (table (lg-json-schema-test--hash "scores" '(1 2 3))))
    (should (equal (lg-to-list-of vector-optic '(:scores [1 2 3])) '(1 2 3)))
    (should (equal (lg-over vector-optic #'1+ '(:scores [1 2 3]))
                   '(:scores [2 3 4])))
    (should (equal (gethash "scores" (lg-over list-optic #'1+ table))
                   '(2 3 4)))))

(ert-deftest lg-json-schema-generates-tuple-index-optics ()
  (let* ((schema
          '(:properties
            (:point (:type "array"
                     :prefixItems [(:type "number") (:type "string")]))))
         (optics (lg-json-schema-generate
                  schema :object-type 'plist :null-object :null))
         (second (lg-json-schema-optic optics "/point/1"))
         (list-parsed-schema
          '(:properties
            (:legacy (:items ((:type "number") (:type "string"))))))
         (list-parsed-set (lg-json-schema-generate
                           list-parsed-schema
                           :object-type 'plist :null-object :null)))
    (should (member "/point/0" (lg-json-schema-paths optics)))
    (should (member "/legacy/1" (lg-json-schema-paths list-parsed-set)))
    (should (equal (lg-over second #'upcase '(:point [10 "north"]))
                   '(:point [10 "NORTH"])))))

(ert-deftest lg-json-schema-expands-combiners-and-local-references ()
  (let* ((schema
          "{\"$defs\":{\"person\":{\"properties\":{\"name\":{\"type\":\"string\"}}}},\"allOf\":[{\"properties\":{\"id\":{\"type\":\"number\"}}},{\"properties\":{\"owner\":{\"$ref\":\"#/$defs/person\"}}}]}")
         (optics (lg-json-schema-generate schema)))
    (should (equal (lg-json-schema-paths optics)
                   '("" "/id" "/owner" "/owner/name")))
    (should (equal
             (lg-over (lg-json-schema-optic optics "/owner/name" 'text)
                      #'upcase
                      "{\"id\":1,\"owner\":{\"name\":\"Ada\"}}")
             "{\"id\":1,\"owner\":{\"name\":\"ADA\"}}"))))

(ert-deftest lg-json-schema-bounds-recursive-reference-expansion ()
  (let* ((schema
          "{\"properties\":{\"value\":{},\"next\":{\"$ref\":\"#\"}}}")
         (paths (lg-json-schema-paths (lg-json-schema-generate schema))))
    (should (equal paths
                   '("" "/next" "/next/next" "/next/value" "/value")))))

(ert-deftest lg-json-schema-uses-escaped-json-pointer-paths ()
  (let* ((schema
          '(:properties (:a/b (:properties (:x~y (:type "string"))))))
         (optics (lg-json-schema-generate
                  schema :object-type 'plist :null-object :null)))
    (should (member "/a~1b/x~0y" (lg-json-schema-paths optics)))
    (should (equal
             (lg-over (lg-json-schema-optic optics "/a~1b/x~0y")
                      #'upcase
                      '(:a/b (:x~y "ok")))
             '(:a/b (:x~y "OK"))))))

(ert-deftest lg-json-schema-validates-input-and-lookups ()
  (should-error (lg-json-schema-generate "not json"))
  (should (equal (lg-json-schema-paths
                  (lg-json-schema-generate (json-parse-string "false")))
                 '("")))
  (should-error (lg-json-schema-generate '(:$ref "https://example.com/s")))
  (should-error (lg-json-schema-generate
                 '(:properties ()) :object-type 'alist))
  (let ((optics (lg-json-schema-generate '(:properties ()))))
    (should-error (lg-json-schema-optic optics "/missing"))
    (should-error (lg-json-schema-optic optics "" 'other))))

(provide 'looking-glass-json-schema-test)

;;; looking-glass-json-schema-test.el ends here
