import { generateReportFromData } from "../helpers.js";
import { querySudo } from "@lblod/mu-auth-sudo";

const reportName = "Procesaantallen";

export default {
  cronPattern: "0 3 * * *",
  name: reportName,
  execute: async () => {
    const reportInfo = {
      title: reportName,
      description: "Aantal processen per bestuur",
      filePrefix: "report-process-counts",
    };

    const queryString = `
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX proces: <https://data.vlaanderen.be/ns/proces#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT *
      WHERE {
        {
          SELECT ?groupName
                 (COUNT(DISTINCT ?unarchivedProcess) AS ?unarchivedProcesses)
                 (COUNT(DISTINCT ?archivedProcess) AS ?archivedProcesses)
                 (COUNT(DISTINCT ?process) AS ?totalProcesses)
          WHERE {
            ?group a besluit:Bestuurseenheid ;
                   skos:prefLabel ?groupName .

            ?process a proces:Proces ;
                     dct:publisher ?group .

            OPTIONAL { ?process adms:status ?status }

            BIND(IF(?status = <http://lblod.data.gift/concepts/concept-status/gearchiveerd>, STR(?process), UNDEF) AS ?archivedProcess)
            BIND(IF(!BOUND(?status) || ?status != <http://lblod.data.gift/concepts/concept-status/gearchiveerd>, STR(?process), UNDEF) AS ?unarchivedProcess)
          }
          GROUP BY ?group ?groupName
          ORDER BY LCASE(?groupName)
        }
        UNION
        {
          SELECT ("" AS ?groupName)
                 (COUNT(DISTINCT ?unarchivedProcess) AS ?unarchivedProcesses)
                 (COUNT(DISTINCT ?archivedProcess) AS ?archivedProcesses)
                 (COUNT(DISTINCT ?process) AS ?totalProcesses)
          WHERE {
            ?process a proces:Proces .

            OPTIONAL { ?process adms:status ?status }

            BIND(IF(?status = <http://lblod.data.gift/concepts/concept-status/gearchiveerd>, STR(?process), UNDEF) AS ?archivedProcess)
            BIND(IF(!BOUND(?status) || ?status != <http://lblod.data.gift/concepts/concept-status/gearchiveerd>, STR(?process), UNDEF) AS ?unarchivedProcess)
          }
        }
      }
      `;
    const queryResponse = await querySudo(queryString);

    const data = queryResponse.results.bindings.map((group) => ({
      Bestuur: group.groupName.value,
      "Aantal zichtbare processen": group.unarchivedProcesses.value,
      "Aantal gearchiveerde processen": group.archivedProcesses.value,
      "Totaal aantal processen": group.totalProcesses.value,
    }));

    await generateReportFromData(
      data,
      [
        "Bestuur",
        "Aantal zichtbare processen",
        "Aantal gearchiveerde processen",
        "Totaal aantal processen",
      ],
      reportInfo
    );
  },
};
