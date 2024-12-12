import { generateReportFromData } from "../helpers.js";
import { querySudo } from "@lblod/mu-auth-sudo";

export default {
  cronPattern: "0 3 * * *",
  name: "processesReport",
  execute: async () => {
    const reportInfo = {
      title: "Processenrapport",
      description: "Lijst van alle processen en hun besturen",
      filePrefix: "processen",
    };

    const queryString = `
      PREFIX besluit: <http://data.vlaanderen.be/ns/besluit#>
      PREFIX skos: <http://www.w3.org/2004/02/skos/core#>
      PREFIX proces: <https://data.vlaanderen.be/ns/proces#>
      PREFIX dct: <http://purl.org/dc/terms/>
      PREFIX adms: <http://www.w3.org/ns/adms#>

      SELECT DISTINCT *
      WHERE {
        ?group a besluit:Bestuurseenheid ;
               skos:prefLabel ?groupName .

        ?process a proces:Proces ;
                 dct:publisher ?group ;
                 dct:title ?title ;
                 dct:created ?created ;
                 dct:modified ?modified .

        OPTIONAL { ?process adms:status ?status }
      }
      ORDER BY LCASE(?groupName), LCASE(?title), ?created, ?modified, ?status
      `;
    const queryResponse = await querySudo(queryString);

    const data = queryResponse.results.bindings.map((process) => ({
      bestuur: process.groupName.value,
      proces: process.title.value,
      aangemaakt: formatDate(process.created.value),
      aangepast: formatDate(process.modified.value),
      gearchiveerd:
        process.status?.value ===
        "http://lblod.data.gift/concepts/concept-status/gearchiveerd"
          ? "yes"
          : "no",
    }));

    await generateReportFromData(
      data,
      ["bestuur", "proces", "aangemaakt", "aangepast", "gearchiveerd"],
      reportInfo
    );
  },
};

function formatDate(isoString) {
  const date = new Date(isoString);

  const year = date.getFullYear();
  const month = String(date.getMonth() + 1).padStart(2, "0"); // Months are 0-based
  const day = String(date.getDate()).padStart(2, "0");
  const hours = String(date.getHours()).padStart(2, "0");
  const minutes = String(date.getMinutes()).padStart(2, "0");

  return `${year}-${month}-${day} ${hours}:${minutes}`;
}
