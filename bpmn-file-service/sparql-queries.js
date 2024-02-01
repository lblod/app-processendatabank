import {
  sparqlEscapeUri,
  sparqlEscapeString,
  sparqlEscapeInt,
  sparqlEscapeDateTime,
} from "mu";

const PREFIXES = `
   PREFIX mu: <http://mu.semte.ch/vocabularies/core/>
   PREFIX nfo: <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#>
   PREFIX dct: <http://purl.org/dc/terms/>
   PREFIX dbpedia: <http://dbpedia.org/ontology/>
   PREFIX nie: <http://www.semanticdesktop.org/ontologies/2007/01/19/nie#>
`;

export function generateUpdateQuery(triples) {
  return `INSERT DATA { ${triples} }`;
}

export function generateFileUpdateQuery(
  uploadResourceUri,
  uploadResourceName,
  uploadResourceUuid,
  fileFormat,
  fileSize,
  fileExtension,
  fileResourceName,
  fileResourceUuid,
) {
  const fileResourceUri = `share://${fileResourceName}`;
  const now = new Date();

  // prettier-ignore
  return `
    ${PREFIXES}
    INSERT DATA {
      ${sparqlEscapeUri(uploadResourceUri)} a nfo:FileDataObject ;
          nfo:fileName ${sparqlEscapeString(uploadResourceName)} ;
          mu:uuid ${sparqlEscapeString(uploadResourceUuid)} ;
          dct:format ${sparqlEscapeString(fileFormat)} ;
          nfo:fileSize ${sparqlEscapeInt(fileSize)} ;
          dbpedia:fileExtension ${sparqlEscapeString(fileExtension)} ;
          dct:created ${sparqlEscapeDateTime(now)} ;
          dct:modified ${sparqlEscapeDateTime(now)} .
      ${sparqlEscapeUri(fileResourceUri)} a nfo:FileDataObject ;
          nie:dataSource ${sparqlEscapeUri(uploadResourceUri)} ;
          nfo:fileName ${sparqlEscapeString(fileResourceName)} ;
          muCore:uuid ${sparqlEscapeString(fileResourceUuid)} ;
          dct:format ${sparqlEscapeString(fileFormat)} ;
          nfo:fileSize ${sparqlEscapeInt(fileSize)} ;
          dbpedia:fileExtension ${sparqlEscapeString(fileExtension)} ;
          dct:created ${sparqlEscapeDateTime(now)} ;
          dct:modified ${sparqlEscapeDateTime(now)} .
    }`;
}

export function generateFilesSelectQuery(nameFilter) {
  // prettier-ignore
  let query = `
    ${PREFIXES}
    SELECT * WHERE {
        ?uri mu:uuid ?uuid ;
             nfo:fileName ?name ;
             dct:format ?format ;
             dbpedia:fileExtension ?extension ;
             nfo:fileSize ?size .
        FILTER NOT EXISTS { ?uri nie:dataSource ?source  }
    `;

  if (nameFilter) {
    query += `FILTER(CONTAINS(LCASE(?name), ${sparqlEscapeString(
      nameFilter.toLowerCase(),
    )}))\n`;
  }

  query += `}`;

  return query;
}

export function generateFileSelectQuery(uploadResourceUuid) {
  // prettier-ignore
   return `
      SELECT * WHERE {
          ?uri mu:uuid ${sparqlEscapeString(uploadResourceUuid)} ;
               nfo:fileName ?name ;
               dct:format ?format ;
               dbpedia:fileExtension ?extension ;
               nfo:fileSize ?size ;
               dct:created ?created ;
               dct:modified ?modified .
      }`;

}

export function generateUploadResourceUriSelectQuery(uploadResourceUuid) {
  // prettier-ignore
  return `
    ${PREFIXES}
    SELECT * WHERE {
        ?uri mu:uuid ${sparqlEscapeString(uploadResourceUuid)};
             nfo:fileName ?fileName ;
             dbpedia:fileExtension ?extension .
        ?fileUrl nie:dataSource ?uri .
    }`;

}
