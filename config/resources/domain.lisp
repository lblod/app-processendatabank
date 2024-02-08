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

(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:name :string ,(s-prefix "nfo:fileName"))
                (:format :string ,(s-prefix "dct:format"))
                (:size :number ,(s-prefix "nfo:fileSize"))
                (:extension :string ,(s-prefix "dbpedia:fileExtension"))
                (:created :datetime ,(s-prefix "dct:created"))
                (:modified :datetime ,(s-prefix "dct:modified")))
  :has-one `((file :via ,(s-prefix "nie:dataSource")
                   :inverse t
                   :as "download"))
  :resource-base (s-url "http://data.lblod.info/files/")
  :features `(include-uri)
  :on-path "files")

(define-resource bpmnElement ()
  :properties `((:name :string ,(s-prefix "bbo:name"))
                (:classification :uri ,(s-prefix "rdf:type")))
  :has-many `((process :via ,(s-prefix "teamingAI:belongsToProcess")
                       :as "processes"))
  :resource-base (s-url "http://data.lblod.info/bpmn-elements/")

  :on-path "bpmn-elements")

;; -------------------------------------------------------------------------------------
;; BPMN Based Ontology (BBO) (See https://www.irit.fr/recherches/MELODI/ontologies/BBO)
;; -------------------------------------------------------------------------------------

(define-resource boundaryEvent (bpmnElement)
  :class (s-prefix "bbo:BoundaryEvent")
  :properties `((:reference :string ,(s-prefix "bbo:attachedToRef"))
                (:outgoing :string ,(s-prefix "bbo:has_outgoing"))
                (:eventDefinition :string ,(s-prefix "bbo:has_eventDefinition")))
  :resource-base (s-url "http://data.lblod.info/boundary-events/")
  :on-path "boundary-events")

(define-resource businessRuleTask (task)
  :class (s-prefix "bbo:BusinessRuleTask")
  :resource-base (s-url "http://data.lblod.info/business-rule-tasks/")
  :on-path "business-rule-tasks")

(define-resource endEvent (bpmnElement)
  :class (s-prefix "bbo:EndEvent")
  :properties `((:incoming :string ,(s-prefix "bbo:has_incoming")))
  :resource-base (s-url "http://data.lblod.info/end-events/")
  :on-path "end-events")

(define-resource errorEventDefinition (bpmnElement)
  :class (s-prefix "bbo:ErrorEventDefinition")
  :resource-base (s-url "http://data.lblod.info/error-event-defintions/")
  :on-path "error-event-defintions")

(define-resource error (bpmnElement)
  :class (s-prefix "bbo:Error")
  :resource-base (s-url "http://data.lblod.info/errors/")
  :on-path "errors")

(define-resource manualTask (task)
  :class (s-prefix "bbo:ManualTask")
  :resource-base (s-url "http://data.lblod.info/manual-tasks/")
  :on-path "manual-tasks")

(define-resource process ()
  :class (s-prefix "bbo:Process")
  :has-many `((file :via ,(s-prefix "prov:wasDerivedFrom")
                    :as "derivations"))
  :resource-base (s-url "http://data.lblod.info/processes/")
  :on-path "processes")

(define-resource receiveTask (task)
  :class (s-prefix "bbo:ReceiveTask")
  :resource-base (s-url "http://data.lblod.info/receive-tasks/")
  :on-path "receive-tasks")

(define-resource scriptTask (task)
  :class (s-prefix "bbo:ScriptTask")
  :resource-base (s-url "http://data.lblod.info/script-tasks/")
  :on-path "script-tasks")

(define-resource sendTask (task)
  :class (s-prefix "bbo:SendTask")
  :resource-base (s-url "http://data.lblod.info/send-tasks/")
  :on-path "send-tasks")

(define-resource serviceTask (task)
  :class (s-prefix "bbo:ServiceTask")
  :resource-base (s-url "http://data.lblod.info/service-tasks/")
  :on-path "service-tasks")

(define-resource task (bpmnElement)
  :class (s-prefix "bbo:Task")
  :resource-base (s-url "http://data.lblod.info/tasks/")
  :on-path "tasks")

(define-resource userTask (task)
  :class (s-prefix "bbo:UserTask")
  :resource-base (s-url "http://data.lblod.info/user-tasks/")
  :on-path "user-tasks")