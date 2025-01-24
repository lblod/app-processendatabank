(in-package :mu-cl-resources)

(define-resource ipdcProduct ()
  :properties `((:name :language-string-set ,(s-prefix "dct:title"))
                (:product-number :string ,(s-prefix "schema:productID")))
  :on-path "ipdc-products")

(define-resource ipdcInstance (ipdcProduct)
  :class (s-prefix "ipdc:InstancePublicService")
  :resource-base (s-url "http://data.lblod.info/ipdc-instances/")
  :on-path "ipdc-instances")

(define-resource ipdcConcept (ipdcProduct)
  :class (s-prefix "ipdc:ConceptualPublicService")
  :resource-base (s-url "http://data.lblod.info/ipdc-concepts/")
  :on-path "ipdc-concepts")
