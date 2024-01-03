(in-package :mu-cl-resources)

(setf *include-count-in-paginated-responses* t)
(setf *supply-cache-headers-p* t)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(setf *cache-model-properties-p* t)

;;;;
;; NOTE
;; docker-compose stop; docker-compose rm; docker-compose up
;; after altering this file.

;; BPMN Based Ontology (BBO) (See https://www.irit.fr/recherches/MELODI/ontologies/BBO)

(define-resource thing ()
  :properties `((:name :string ,(s-prefix "bbo:name"))
                (:created-on :string ,(s-prefix "bbo:createdOn"))
                (:max-value :string ,(s-prefix "bbo:maxValue"))
                (:id :string ,(s-prefix "bbo:id"))
                (:value :string ,(s-prefix "bbo:value"))
                (:min-value :string ,(s-prefix "bbo:minValue"))
                (:process-type :string ,(s-prefix "bbo:processType"))
                (:type :string ,(s-prefix "bbo:type")))
  :on-path "things")

;; (define-resource activity (thing flowNode)
;;   :class (s-prefix "bbo:Activity")
;;   :properties `((:completion-quantity :integer ,(s-prefix "bbo:completionQuantity"))
;;                 (:is-for-compensation :boolean ,(s-prefix "bbo:isForCompensation"))
;;                 (:start-quantity :integer ,(s-prefix "bbo:startQuantity")))
;;   :resource-base (s-url "https://example.org/")
;;   :on-path "activities")

;; (define-resource flowElement (thing)
;;   :class (s-prefix "bbo:FlowElement")
;;   :resource-base (s-url "https://example.org/")
;;   :on-path "flow-elements")

;; (define-resource flowNode (thing flowElement)
;;   :class (s-prefix "bbo:FlowNode")
;;   :resource-base (s-url "https://example.org/")
;;   :on-path "flow-nodes")

;; (define-resource task (thing activity)
;;   :class (s-prefix "bbo:Task")
;;   :resource-base (s-url "https://example.org/")
;;   :on-path "tasks")

(define-resource task (thing)
  :class (s-prefix "bbo:Task")
  :resource-base (s-url "https://example.org/")
  :on-path "tasks")

;; File resources (https://github.com/mu-semtech/file-service)

(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:name :string ,(s-prefix "nfo:fileName"))
                (:format :string ,(s-prefix "dct:format"))
                (:size :number ,(s-prefix "nfo:fileSize"))
                (:extension :string ,(s-prefix "dbpedia:fileExtension"))
                (:created :datetime ,(s-prefix "dct:created")))
  :has-one `((file :via ,(s-prefix "nie:dataSource")
                   :inverse t
                   :as "download"))
  :resource-base (s-url "http://data.example.com/files/")
  :features `(include-uri)
  :on-path "files")
