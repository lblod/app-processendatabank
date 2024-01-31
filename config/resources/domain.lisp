;;;;
;; NOTE
;; docker-compose stop; docker-compose rm; docker-compose up
;; after altering this file.

(in-package :mu-cl-resources)

(setf *include-count-in-paginated-responses* t)
(setf *supply-cache-headers-p* t)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)
(setf *cache-model-properties-p* t)

(defparameter *default-page-size* 20)

;; -------------------------------------------------------------------------------------

(define-resource bpmnFile ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:name :string ,(s-prefix "nfo:fileName"))
                (:format :string ,(s-prefix "dct:format"))
                (:size :number ,(s-prefix "nfo:fileSize"))
                (:extension :string ,(s-prefix "dbpedia:fileExtension"))
                (:created :datetime ,(s-prefix "dct:created"))
                (:modified :datetime ,(s-prefix "dct:modified")))
  :resource-base (s-url "https://example.org/services/bpmn-file-service/files/")
  :on-path "bpmn-files")

(define-resource bpmnElement ()
  :properties `((:name :string ,(s-prefix "bbo:name")))
  :has-many `((process :via ,(s-prefix "teamingAI:belongsToProcess")
                       :as "processes"))
  :on-path "bpmn-elements")

;; -------------------------------------------------------------------------------------
;; BPMN Based Ontology (BBO) (See https://www.irit.fr/recherches/MELODI/ontologies/BBO)
;; -------------------------------------------------------------------------------------

(define-resource process ()
  :class (s-prefix "bbo:Process")
  :has-many `((bpmnFile :via ,(s-prefix "prov:wasDerivedFrom")
                    :as "derivations"))
  :resource-base (s-url "https://example.org/")
  :on-path "processes")

;; Tasks

(define-resource task (bpmnElement)
  :class (s-prefix "bbo:Task")
  :resource-base (s-url "https://example.org/")
  :on-path "tasks")

(define-resource businessRuleTask (task)
  :class (s-prefix "bbo:BusinessRuleTask")
  :resource-base (s-url "https://example.org/")
  :on-path "business-rule-tasks")

(define-resource manualTask (task)
  :class (s-prefix "bbo:ManualTask")
  :resource-base (s-url "https://example.org/")
  :on-path "manual-tasks")

(define-resource receiveTask (task)
  :class (s-prefix "bbo:ReceiveTask")
  :resource-base (s-url "https://example.org/")
  :on-path "receive-tasks")

(define-resource scriptTask (task)
  :class (s-prefix "bbo:ScriptTask")
  :resource-base (s-url "https://example.org/")
  :on-path "script-tasks")

(define-resource sendTask (task)
  :class (s-prefix "bbo:SendTask")
  :resource-base (s-url "https://example.org/")
  :on-path "send-tasks")

(define-resource serviceTask (task)
  :class (s-prefix "bbo:ServiceTask")
  :resource-base (s-url "https://example.org/")
  :on-path "service-tasks")

(define-resource userTask (task)
  :class (s-prefix "bbo:UserTask")
  :resource-base (s-url "https://example.org/")
  :on-path "user-tasks")