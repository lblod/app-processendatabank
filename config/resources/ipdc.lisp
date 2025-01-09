(in-package :mu-cl-resources)

(define-resource ipdcInstance ()
  :class (s-prefix "ipdc:InstancePublicService")
  :properties `((:name :language-string-set ,(s-prefix "dct:title"))
                (:product-number :string ,(s-prefix "schema:productID")))
  :resource-base (s-url "http://data.lblod.info/ipdc-instances/")
  :on-path "ipdc-instances")