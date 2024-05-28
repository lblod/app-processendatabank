alias Acl.Accessibility.Always, as: AlwaysAccessible
alias Acl.Accessibility.ByQuery, as: AccessByQuery
alias Acl.GraphSpec.Constraint.Resource.AllPredicates, as: AllPredicates
alias Acl.GraphSpec.Constraint.Resource.NoPredicates, as: NoPredicates
alias Acl.GraphSpec.Constraint.ResourceFormat, as: ResourceFormatConstraint
alias Acl.GraphSpec.Constraint.Resource, as: ResourceConstraint
alias Acl.GraphSpec, as: GraphSpec
alias Acl.GroupSpec, as: GroupSpec
alias Acl.GroupSpec.GraphCleanup, as: GraphCleanup

defmodule Acl.UserGroups.Config do
  @public_type [
    "http://www.w3.org/ns/org#Role",
    "http://data.vlaanderen.be/ns/besluit#Bestuurseenheid",
    "http://xmlns.com/foaf/0.1/Person",
    "http://xmlns.com/foaf/0.1/OnlineAccount",
    "http://www.w3.org/2004/02/skos/core#Concept",
    "http://www.w3.org/ns/org#Organization",
    "http://lblod.data.gift/vocabularies/organisatie/TypeVestiging",
    "http://lblod.data.gift/vocabularies/organisatie/BestuurseenheidClassificatieCode",
    "http://lblod.data.gift/vocabularies/organisatie/OrganisatieStatusCode",
    "http://www.w3.org/2004/02/skos/core#ConceptScheme",
    "http://publications.europa.eu/ontology/euvoc#Country",
    "http://www.w3.org/ns/prov#Location",
  ]

  @bpmn_element_type [
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#Activity",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#BoundaryEvent",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#BusinessRuleTask",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#CallableElement",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#CatchEvent",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#EndEvent",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#Error",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#ErrorEventDefinition",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#Event",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#EventDefinition",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#ExclusiveGateway",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#FlowElement",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#FlowElementsContainer",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#FlowNode",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#Gateway",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#InclusiveGateway",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#IntermediateThrowEvent",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#ManualTask",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#MessageEventDefinition",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#ParallelGateway",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#Process",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#Property",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#ReceiveTask",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#RootElement",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#ScriptTask",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#SendTask",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#SequenceFlow",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#ServiceTask",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#StartEvent",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#SubProcess",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#Task",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#ThrowEvent",
    "https://www.irit.fr/recherches/MELODI/ontologies/BBO#UserTask",
    "https://www.teamingai-project.eg/BBOExtension#Collaboration",
    "https://www.teamingai-project.eg/BBOExtension#DataObject",
    "https://www.teamingai-project.eg/BBOExtension#DataObjectReference",
    "https://www.teamingai-project.eg/BBOExtension#Lane",
    "https://www.teamingai-project.eg/BBOExtension#LaneSet",
    "https://www.teamingai-project.eg/BBOExtension#Participant"
  ]

  @file_type [
    "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject"
  ]
    defp is_authenticated() do
    %AccessByQuery{
      vars: [],
      query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
        PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
        SELECT DISTINCT ?session_group ?session_role WHERE {
          <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                       ext:sessionRole ?session_role.
        }"
      }
  end
  def user_groups do
    [

      # shared data
      %GroupSpec{
        name: "shared",
        useage: [:write, :read_for_write],
        access: is_authenticated(),
        graphs: [
                  %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/shared",
                    constraint: %ResourceConstraint{
                      resource_types: @bpmn_element_type ++ @file_type } },
                      ] },
        # // ORGANIZATION DATA
      %GroupSpec{
        name: "org",
        useage: [:read, :write, :read_for_write],
        access: %AccessByQuery{
          vars: ["session_group"],
          query: "PREFIX ext: <http://mu.semte.ch/vocabularies/ext/>
                  PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
                  SELECT DISTINCT ?session_group WHERE {
                    <SESSION_ID> ext:sessionGroup/mu:uuid ?session_group;
                                 ext:sessionRole \"LoketLB-OpenProcesHuisGebruiker\".
                    }" },
        graphs: [ %GraphSpec{
                    graph: "http://mu.semte.ch/graphs/organizations/",
                    constraint: %ResourceConstraint{
                      resource_types: @bpmn_element_type ++ @file_type } },
                      ] },


      # // PUBLIC
      %GroupSpec {
        name: "public",
        useage: [:read],
        access: %AlwaysAccessible{},
        graphs: [
          %GraphSpec {
            graph: "http://mu.semte.ch/graphs/public",
            constraint: %ResourceConstraint {
              resource_types: @public_type ++ @bpmn_element_type ++ @file_type
            }
          },

          %GraphSpec {
            graph: "http://mu.semte.ch/graphs/shared",
            constraint: %ResourceConstraint {
              resource_types: @bpmn_element_type ++ @file_type
            }
          }
        ]
      },

      # // CLEANUP
      %GraphCleanup {
        originating_graph: "http://mu.semte.ch/application",
        useage: [:write],
        name: "clean"
      }
    ]
  end
end
