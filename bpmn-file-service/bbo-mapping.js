export const mapping = (uploadResourceUri) => `
@prefix bbo: <https://www.irit.fr/recherches/MELODI/ontologies/BBO#> .
@prefix bboExtension: <https://www.teamingai-project.eg/BBOExtension#> .
@prefix ql: <http://semweb.mmlab.be/ns/ql#> .
@prefix rami: <https://w3id.org/i40/rami#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix rml: <http://semweb.mmlab.be/ns/rml#> .
@prefix rr: <http://www.w3.org/ns/r2rml#> .
@prefix teamingAI: <https://www.teamingai-project.eu/> .
@prefix fnml:   <http://semweb.mmlab.be/ns/fnml#> .
@prefix fno: 	<https://w3id.org/function/ontology#> .
@prefix idlab-fn: <http://example.com/idlab/function/> .
@prefix muCore: <http://mu.semte.ch/vocabularies/core/>.
@prefix prov: <http://www.w3.org/ns/prov#>.

<#UuidMapping>
    rr:predicate muCore:uuid;
    rr:objectMap [
        fnml:functionValue [
            rr:predicateObjectMap [
                rr:predicate fno:executes ;
                rr:objectMap [ rr:constant idlab-fn:random ]
            ] ;
        ]
    ].

teamingAI:AssociationMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:association']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:Association
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:has_sourceRef;
        rr:objectMap [ rr:template "{@sourceRef}" ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{@targetRef}" ]
    ].

teamingAI:BoundaryEventMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:boundaryEvent']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:BoundaryEvent
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:attachedToRef;
        rr:objectMap [ rr:template "{@attachedToRef}" ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:outgoing']}" ]
    ],
    [
        rr:predicate bbo:has_eventDefinition;
        rr:objectMap [ rr:template "{./*[name()='bpmn:messageEventDefinition']/@id}" ]
    ],
    [
        rr:predicate bbo:has_eventDefinition;
        rr:objectMap [ rr:template "{./*[name()='bpmn:errorEventDefinition']/@id}" ]
    ].

teamingAI:BusinessRuleTaskMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:businessRuleTask']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:BusinessRuleTask
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:CollaborationMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']//*[name()='bpmn:collaboration']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:Collaboration
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:DataInputAssociationMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']//*[name()='bpmn:dataInputAssociation']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:DataInputAssociation
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:has_sourceRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:sourceRef']}" ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:targetRef']}" ]
    ],
    [
        rr:predicate bboExtension:is_dataInputFor;
        rr:objectMap [ rr:template "{../@id}" ]
    ].

teamingAI:DataObjectMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']//*[name()='bpmn:dataObject']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:DataObject
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:DataObjectReferenceMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']//*[name()='bpmn:dataObjectReference']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:DataObjectReference
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bboExtension:dataObjectRef;
        rr:objectMap [ rr:template "{@dataObjectRef}" ]
    ].

teamingAI:DataOutputAssociationMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']//*[name()='bpmn:dataOutputAssociation']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:DataOutputAssociation
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:targetRef']}" ]
    ],
    [
        rr:predicate bboExtension:is_dataOutputFrom;
        rr:objectMap [ rr:template "{../@id}" ]
    ].

teamingAI:DataStoreReferenceMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']//*[name()='bpmn:dataStoreReference']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:DataStoreReference
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ].

teamingAI:EndEventMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:endEvent']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:EndEvent
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate bbo:has_sourceRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:incoming']}" ]
    ].

teamingAI:ErrorEventDefinitionMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "//*[name()='bpmn:errorEventDefinition']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:ErrorEventDefinition
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:ErrorMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:error']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:Error
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ].

teamingAI:ExlusiveGatewayMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']//*[name()='bpmn:exclusiveGateway']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:ExclusiveGateway
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:DefaultSequenceFlow;
        rr:objectMap [ rr:template "{@default}" ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:outgoing']}" ]
    ],
    [
        rr:predicate bbo:has_sourceRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:incoming']}" ]
    ].

teamingAI:InclusiveGatewayMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']//*[name()='bpmn:inclusiveGateway']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:InclusiveGateway
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:DefaultSequenceFlow;
        rr:objectMap [ rr:template "{@default}" ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:outgoing']}" ]
    ],
    [
        rr:predicate bbo:has_sourceRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:incoming']}" ]
    ].

teamingAI:IntermediateThrowMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']//*[name()='bpmn:intermediateThrowEvent']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:IntermediateThrowEvent
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:outgoing']}" ]
    ],
    [
        rr:predicate bbo:has_sourceRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:incoming']}" ]
    ],
    [
        rr:predicate bbo:has_eventDefinition;
        rr:objectMap [ rr:template "{./*[name()='bpmn:messageEventDefinition']}" ]
    ].

teamingAI:LaneMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:laneSet']/*[name()='bpmn:lane']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:Lane
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:has_activity;
        rr:objectMap [ rr:template "{*[text()]}" ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ].

teamingAI:LaneSetMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:laneSet']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:LaneSet
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:ManualTask
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:manualTask']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:ManualTask
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:MessageEventDefinitionMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "//*[name()='bpmn:messageEventDefinition']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:MessageEventDefinition
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:MessageFlowMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:collaboration']/*[name()='bpmn:messageFlow']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:MessageFlow
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate bbo:has_sourceRef;
        rr:objectMap [ rr:template "{@sourceRef}" ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{@targetRef}" ]
    ].

teamingAI:ParallelGatewayMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:parallelGateway']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:ParallelGateway
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:DefaultSequenceFlow;
        rr:objectMap [ rr:template "{@default}" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:outgoing']}" ]
    ],
    [
        rr:predicate bbo:has_sourceRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:incoming']}" ]
    ].

teamingAI:ParticipantMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:collaboration']/*[name()='bpmn:participant']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:Participant
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate bboExtension:processRef;
        rr:objectMap [ rr:template "{@processRef}" ]
    ].

teamingAI:ProcessMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:Process
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate prov:wasDerivedFrom;
        rr:objectMap [ rr:template "${uploadResourceUri}" ]
    ].

teamingAI:PropertyMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']//*[name()='bpmn:property']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:Property
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate bbo:has_property_element;
        rr:objectMap [ rr:template "{../@id}" ]
    ].

teamingAI:ReceiveTaskMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:receiveTask']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:ReceiveTask
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:ScriptTaskMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:scriptTask']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:ScriptTask
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:SendTaskMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:sendTask']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:SendTask
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:SequenceFlowMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:sequenceFlow']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:SequenceFlow
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:has_sourceRef;
        rr:objectMap [ rr:template "{@sourceRef}" ]
    ],
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{@targetRef}" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ].

teamingAI:ServiceTaskMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:serviceTask']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:ServiceTask
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:StartEventMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:startEvent']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:StartEvent
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:has_targetRef;
        rr:objectMap [ rr:template "{./*[name()='bpmn:outgoing']}" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ].

teamingAI:SubProcessMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:subProcess']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:SubProcess
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToRAMILayer;
        rr:objectMap [ rr:constant rami:Business ]
    ],
    [
        rr:predicate teamingAI:belongsToView;
        rr:objectMap [ rr:constant teamingAI:BusinessProcessManagementView ]
    ].

teamingAI:TaskMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:task']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:Task
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

teamingAI:TextAnnotationMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:textAnnotation']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bboExtension:TextAnnotation
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ],
    [
        rr:predicate rdfs:comment;
        rr:objectMap [ rr:template "{*[text()]}" ]
    ].

teamingAI:UserTaskMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:userTask']"
    ];

    rr:subjectMap [
        rr:template "{@id}";
        rr:class bbo:UserTask
    ];

    rr:predicateObjectMap <#UuidMapping>,
    [
        rr:predicate bbo:name;
        rr:objectMap [ rml:reference "@name" ]
    ],
    [
        rr:predicate teamingAI:belongsToProcess;
        rr:objectMap [ rr:parentTriplesMap teamingAI:ProcessMapping ]
    ].

`;
