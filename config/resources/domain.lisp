(in-package :mu-cl-resources)

(setf *include-count-in-paginated-responses* t)
(setf *supply-cache-headers-p* t)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(setf *cache-model-properties-p* t)

;;;;
;; NOTE
;; docker-compose stop; docker-compose rm; docker-compose up
;; after altering this file.

(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:name :string ,(s-prefix "nfo:fileName")))
  :resource-base (s-url "https://example.org/services/bpmn-file-service/files/")
  :on-path "files")

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

(define-resource process (thing)
  :class (s-prefix "bbo:Process")
  :has-many `((file :via ,(s-prefix "prov:wasDerivedFrom")
                    :as "derivations"))
  :resource-base (s-url "https://example.org/")
  :on-path "processes")

(define-resource task (thing)
  :class (s-prefix "bbo:Task")
  :has-many `((process :via ,(s-prefix "teamingAI:belongsToProcess")
                       :as "processes"))
  :resource-base (s-url "https://example.org/")
  :on-path "tasks")

(define-resource sendTask (task)
  :class (s-prefix "bbo:SendTask")
  :resource-base (s-url "https://example.org/")
  :on-path "send-tasks")

(define-resource manualTask (task)
  :class (s-prefix "bbo:ManualTask")
  :resource-base (s-url "https://example.org/")
  :on-path "manual-tasks")

(define-resource businessRuleTask (task)
  :class (s-prefix "bbo:BusinessRuleTask")
  :resource-base (s-url "https://example.org/")
  :on-path "business-rule-tasks")

(define-resource userTask (task)
  :class (s-prefix "bbo:UserTask")
  :resource-base (s-url "https://example.org/")
  :on-path "user-tasks")

(define-resource receiveTask (task)
  :class (s-prefix "bbo:ReceiveTask")
  :resource-base (s-url "https://example.org/")
  :on-path "receive-tasks")

(define-resource scriptTask (task)
  :class (s-prefix "bbo:ScriptTask")
  :resource-base (s-url "https://example.org/")
  :on-path "script-tasks")