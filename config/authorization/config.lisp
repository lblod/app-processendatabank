;;;;;;;;;;;;;;;;;;;
;;; delta messenger
(in-package :delta-messenger)

;; (push (make-instance 'delta-logging-handler) *delta-handlers*) ;; enable if delta messages should be logged on terminal
(add-delta-messenger "http://deltanotifier/")
(setf *log-delta-messenger-message-bus-processing* nil) ;; set to t for extra messages for debugging delta messenger

;;;;;;;;;;;;;;;;;
;;; configuration
(in-package :client)
(setf *log-sparql-query-roundtrip* nil) ; change nil to t for logging requests to virtuoso (and the response)
(setf *backend* "http://virtuoso:8890/sparql")

(in-package :server)
(setf *log-incoming-requests-p* nil) ; change nil to t for logging all incoming requests

;;;;;;;;;;;;;;;;
;;; prefix types
(in-package :type-cache)

(add-type-for-prefix "http://mu.semte.ch/sessions/" "http://mu.semte.ch/vocabularies/session/Session") ; each session URI will be handled for updates as if it had this mussession:Session type

;;;;;;;;;;;;;;;;;
;;; access rights

(in-package :acl)

;; these three reset the configuration, they are likely not necessary
(defparameter *access-specifications* nil)
(defparameter *graphs* nil)
(defparameter *rights* nil)

;; Prefixes used in the constraints below (not in the SPARQL queries)
(define-prefixes
  ;; Core
  :mu "http://mu.semte.ch/vocabularies/core/"
  :session "http://mu.semte.ch/vocabularies/session/"
  :ext "http://mu.semte.ch/vocabularies/ext/"
  ;; Custom prefix URIs here, prefix casing is ignored
  :besluit "http://data.vlaanderen.be/ns/besluit#"
  :organisatie "http://lblod.data.gift/vocabularies/organisatie/"
  :euvoc "http://publications.europa.eu/ontology/euvoc#"
  :schema "http://schema.org/"
  :cogs "http://vocab.deri.ie/cogs#"
  :nfo "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#"
  :skos "http://www.w3.org/2004/02/skos/core#"
  :org "http://www.w3.org/ns/org#"
  :prov "http://www.w3.org/ns/prov#"
  :foaf "http://xmlns.com/foaf/0.1/"
  :proces "https://data.vlaanderen.be/ns/proces#"
  :bbo "https://www.irit.fr/recherches/MELODI/ontologies/BBO#"
  :bboext "https://www.teamingai-project.eg/BBOExtension#")


;;;;;;;;;;;;;
;; User roles

(supply-allowed-group "public")

(supply-allowed-group "org"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                         ext:sessionRole \"LoketLB-OpenProcesHuisGebruiker\".
          }"
  :parameters ("session_group"))

(supply-allowed-group "authenticated"
  :query "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
          PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
          SELECT DISTINCT ?session_group ?session_role WHERE {
            <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                         ext:sessionRole ?session_role.
          }")

(grant (read)
       :to-graph public ;; see define-graph below
       :for-allowed-group "public")
(grant (read)
       :to-graph shared
       :for-allowed-group "public")
(grant (read)
       :to-graph job
       :for-allowed-group "public")

(grant (read write)
       :to organizations
       :for "org")

(grant (read write)
       :to shared
       :for "authenticated")


;;;;;;;;;
;; Graphs
;;
;; These are the graph specifications known in the system.  No
;; guarantees are given as to what content is readable from a graph.  If
;; two graphs are nearly identitacl and have the same name, perhaps the
;; specifications can be folded too.  This could help when building
;; indexes.

(define-graph shared ("http://mu.semte.ch/graphs/shared")
  ;; bpmn-element-type
  ("bbo:Activity" -> _)
  ("bbo:BoundaryEvent" -> _)
  ("bbo:BusinessRuleTask" -> _)
  ("bbo:CallableElement" -> _)
  ("bbo:CatchEvent" -> _)
  ("bbo:EndEvent" -> _)
  ("bbo:Error" -> _)
  ("bbo:ErrorEventDefinition" -> _)
  ("bbo:Event" -> _)
  ("bbo:EventDefinition" -> _)
  ("bbo:ExclusiveGateway" -> _)
  ("bbo:FlowElement" -> _)
  ("bbo:FlowElementsContainer" -> _)
  ("bbo:FlowNode" -> _)
  ("bbo:Gateway" -> _)
  ("bbo:InclusiveGateway" -> _)
  ("bbo:IntermediateThrowEvent" -> _)
  ("bbo:ManualTask" -> _)
  ("bbo:MessageEventDefinition" -> _)
  ("bbo:ParallelGateway" -> _)
  ("bbo:Process" -> _)
  ("bbo:Property" -> _)
  ("bbo:ReceiveTask" -> _)
  ("bbo:RootElement" -> _)
  ("bbo:ScriptTask" -> _)
  ("bbo:SendTask" -> _)
  ("bbo:SequenceFlow" -> _)
  ("bbo:ServiceTask" -> _)
  ("bbo:StartEvent" -> _)
  ("bbo:SubProcess" -> _)
  ("bbo:Task" -> _)
  ("bbo:ThrowEvent" -> _)
  ("bbo:UserTask" -> _)
  ("bboext:Collaboration" -> _)
  ("bboext:DataObject" -> _)
  ("bboext:DataObjectReference" -> _)
  ("bboext:Lane" -> _)
  ("bboext:LaneSet" -> _)
  ("bboext:Participant" -> _)
  ;; process-type
  ("proces:Proces" -> _)
  ("nfo:FileDataObject" -> _))

(define-graph organizations ("http://mu.semte.ch/graphs/organizations/")
  ;; bpmn-element-type
  ("bbo:Activity" -> _)
  ("bbo:BoundaryEvent" -> _)
  ("bbo:BusinessRuleTask" -> _)
  ("bbo:CallableElement" -> _)
  ("bbo:CatchEvent" -> _)
  ("bbo:EndEvent" -> _)
  ("bbo:Error" -> _)
  ("bbo:ErrorEventDefinition" -> _)
  ("bbo:Event" -> _)
  ("bbo:EventDefinition" -> _)
  ("bbo:ExclusiveGateway" -> _)
  ("bbo:FlowElement" -> _)
  ("bbo:FlowElementsContainer" -> _)
  ("bbo:FlowNode" -> _)
  ("bbo:Gateway" -> _)
  ("bbo:InclusiveGateway" -> _)
  ("bbo:IntermediateThrowEvent" -> _)
  ("bbo:ManualTask" -> _)
  ("bbo:MessageEventDefinition" -> _)
  ("bbo:ParallelGateway" -> _)
  ("bbo:Process" -> _)
  ("bbo:Property" -> _)
  ("bbo:ReceiveTask" -> _)
  ("bbo:RootElement" -> _)
  ("bbo:ScriptTask" -> _)
  ("bbo:SendTask" -> _)
  ("bbo:SequenceFlow" -> _)
  ("bbo:ServiceTask" -> _)
  ("bbo:StartEvent" -> _)
  ("bbo:SubProcess" -> _)
  ("bbo:Task" -> _)
  ("bbo:ThrowEvent" -> _)
  ("bbo:UserTask" -> _)
  ("bboext:Collaboration" -> _)
  ("bboext:DataObject" -> _)
  ("bboext:DataObjectReference" -> _)
  ("bboext:Lane" -> _)
  ("bboext:LaneSet" -> _)
  ("bboext:Participant" -> _)
  ;; process-type
  ("proces:Proces" -> _)
  ("nfo:FileDataObject" -> _))

(define-graph public ("http://mu.semte.ch/graphs/public")
  ;; bpmn-element-type
  ("bbo:Activity" -> _)
  ("bbo:BoundaryEvent" -> _)
  ("bbo:BusinessRuleTask" -> _)
  ("bbo:CallableElement" -> _)
  ("bbo:CatchEvent" -> _)
  ("bbo:EndEvent" -> _)
  ("bbo:Error" -> _)
  ("bbo:ErrorEventDefinition" -> _)
  ("bbo:Event" -> _)
  ("bbo:EventDefinition" -> _)
  ("bbo:ExclusiveGateway" -> _)
  ("bbo:FlowElement" -> _)
  ("bbo:FlowElementsContainer" -> _)
  ("bbo:FlowNode" -> _)
  ("bbo:Gateway" -> _)
  ("bbo:InclusiveGateway" -> _)
  ("bbo:IntermediateThrowEvent" -> _)
  ("bbo:ManualTask" -> _)
  ("bbo:MessageEventDefinition" -> _)
  ("bbo:ParallelGateway" -> _)
  ("bbo:Process" -> _)
  ("bbo:Property" -> _)
  ("bbo:ReceiveTask" -> _)
  ("bbo:RootElement" -> _)
  ("bbo:ScriptTask" -> _)
  ("bbo:SendTask" -> _)
  ("bbo:SequenceFlow" -> _)
  ("bbo:ServiceTask" -> _)
  ("bbo:StartEvent" -> _)
  ("bbo:SubProcess" -> _)
  ("bbo:Task" -> _)
  ("bbo:ThrowEvent" -> _)
  ("bbo:UserTask" -> _)
  ("bboext:Collaboration" -> _)
  ("bboext:DataObject" -> _)
  ("bboext:DataObjectReference" -> _)
  ("bboext:Lane" -> _)
  ("bboext:LaneSet" -> _)
  ("bboext:Participant" -> _)
  ;; process-type
  ("proces:Proces" -> _)
  ("nfo:FileDataObject" -> _)
  ;; public-type
  ("org:Role" -> _)
  ("besluit:Bestuurseenheid" -> _)
  ("foaf:Person" -> _)
  ("foaf:OnlineAccount" -> _)
  ("skos:Concept" -> _)
  ("org:Organization" -> _)
  ("org:Site" -> _)
  ("schema:ContactPoint" -> _)
  ("organisatie:TypeVestiging" -> _)
  ("organisatie:BestuurseenheidClassificatieCode" -> _)
  ("organisatie:OrganisatieStatusCode" -> _)
  ("skos:ConceptScheme" -> _)
  ("euvoc:Country" -> _)
  ("prov:Location" -> _))

(define-graph job ("http://mu.semte.ch/graphs/bpmn-job")
  ("cogs:Job" -> _))

;; Example:
;; (define-graph company ("http://mu.semte.ch/graphs/companies/")
;;   ("foaf:OnlineAccount"
;;    -> "foaf:accountName"
;;    -> "foaf:accountServiceHomepage")
;;   ("foaf:Group"
;;    -> "foaf:name"
;;    -> "foaf:member"))


