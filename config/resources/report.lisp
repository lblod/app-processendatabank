(in-package :mu-cl-resources)

(define-resource report()
  :class (s-prefix "reporting:Report")
  :properties `((:title :string ,(s-prefix "dct:title"))
                (:description :string ,(s-prefix "dct:description"))
                (:created :datetime ,(s-prefix "dct:created")))
  :has-one `((file :via ,(s-prefix "prov:generated")
                   :as "file"))
  :resource-base (s-url "http://data.lblod.info/reports/")
  :on-path "reports")