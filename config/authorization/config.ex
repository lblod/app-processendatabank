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
  def user_groups do
    # These elements are walked from top to bottom.  Each of them may
    # alter the quads to which the current query applies.  Quads are
    # represented in three sections: current_source_quads,
    # removed_source_quads, new_quads.  The quads may be calculated in
    # many ways.  The useage of a GroupSpec and GraphCleanup are
    # common.
    [
      # // PUBLIC
      %GroupSpec {
        name: "public",
        useage: [:read, :write],
        access: %AlwaysAccessible{},
        graphs: [
                  %GraphSpec {
                    graph: "http://mu.semte.ch/graphs/public",
                    constraint: %ResourceConstraint {
                      resource_types: [
                        "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#FileDataObject",
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
                        "https://www.teamingai-project.eg/BBOExtension#Participant",
                      ]
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
