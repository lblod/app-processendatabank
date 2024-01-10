export const mapping = `
@prefix ex: <https://example.org/> .
@prefix ql: <http://semweb.mmlab.be/ns/ql#> .
@prefix rml: <http://semweb.mmlab.be/ns/rml#> .
@prefix rr: <http://www.w3.org/ns/r2rml#> .
@prefix fnml:   <http://semweb.mmlab.be/ns/fnml#> .
@prefix fno: 	<https://w3id.org/function/ontology#> .
@prefix idlab-fn: <http://example.com/idlab/function/> .
@prefix muCore: <http://mu.semte.ch/vocabularies/core/>.

ex:UuidMapping
    a rr:TriplesMap;

    rml:logicalSource [
        rml:source "input.bpmn";
        rml:referenceFormulation ql:XPath;
        rml:iterator "/*[name()='bpmn:definitions']/*[name()='bpmn:process']/*[name()='bpmn:task']"
    ];

    rr:subjectMap [
        rr:template "{@id}"
    ];

    rr:predicateObjectMap [
        rr:predicate muCore:uuid;
        rr:objectMap [
            fnml:functionValue [
                rr:predicateObjectMap [
                    rr:predicate fno:executes ;
                    rr:objectMap [ rr:constant idlab-fn:random ]
                ] ;
            ]
        ]
    ].
`
