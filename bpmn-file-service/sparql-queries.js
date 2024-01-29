import {
  sparqlEscapeUri,
  sparqlEscapeString,
  sparqlEscapeInt,
  sparqlEscapeDateTime,
} from "mu";

const muCore = "http://mu.semte.ch/vocabularies/core/";
const nfo = "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#";
const dct = "http://purl.org/dc/terms/";
const dbpedia = "http://dbpedia.org/ontology/";
const nie = "http://www.semanticdesktop.org/ontologies/2007/01/19/nie#";

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
  fileResourceUuid
) {
  const fileResourceUri = `share://${fileResourceName}`;
  const now = new Date();

  let triples = `${sparqlEscapeUri(
    uploadResourceUri
  )} a <${nfo}FileDataObject> ;\n`;
  triples += `<${nfo}fileName> ${sparqlEscapeString(uploadResourceName)} ;\n`;
  triples += `<${muCore}uuid> ${sparqlEscapeString(uploadResourceUuid)} ;\n`;
  triples += `<${dct}format> ${sparqlEscapeString(fileFormat)} ;\n`;
  triples += `<${nfo}fileSize> ${sparqlEscapeInt(fileSize)} ;\n`;
  triples += `<${dbpedia}fileExtension> ${sparqlEscapeString(
    fileExtension
  )} ;\n`;
  triples += `<${dct}created> ${sparqlEscapeDateTime(now)} ;\n`;
  triples += `<${dct}modified> ${sparqlEscapeDateTime(now)} .\n`;

  triples += `${sparqlEscapeUri(fileResourceUri)} a <${nfo}FileDataObject> ;\n`;
  triples += `<${nie}dataSource> ${sparqlEscapeUri(uploadResourceUri)} ;\n`;
  triples += `<${nfo}fileName> ${sparqlEscapeString(fileResourceName)} ;\n`;
  triples += `<${muCore}uuid> ${sparqlEscapeString(fileResourceUuid)} ;\n`;
  triples += `<${dct}format> ${sparqlEscapeString(fileFormat)} ;\n`;
  triples += `<${nfo}fileSize> ${sparqlEscapeInt(fileSize)} ;\n`;
  triples += `<${dbpedia}fileExtension> ${sparqlEscapeString(
    fileExtension
  )} ;\n`;
  triples += `<${dct}created> ${sparqlEscapeDateTime(now)} ;\n`;
  triples += `<${dct}modified> ${sparqlEscapeDateTime(now)} .\n`;

  return generateUpdateQuery(triples);
}

export function generateFilesSelectQuery(nameFilter) {
  let query = `SELECT * WHERE {\n`;
  query += `?uri <${muCore}uuid> ?uuid ;\n`;
  query += `<${nfo}fileName> ?name ;\n`;
  query += `<${dct}format> ?format ;\n`;
  query += `<${dbpedia}fileExtension> ?extension ;\n`;
  query += `<${nfo}fileSize> ?size .\n`;
  query += `FILTER NOT EXISTS { ?uri <${nie}dataSource> ?source  }\n`;

  if (nameFilter) {
    query += `FILTER(CONTAINS(LCASE(?name), ${sparqlEscapeString(
      nameFilter.toLowerCase()
    )}))\n`;
  }

  query += `}`;

  return query;
}

export function generateFileSelectQuery(uploadResourceUuid) {
  let query = `SELECT * WHERE {\n`;
  query += `?uri <${muCore}uuid> ${sparqlEscapeString(uploadResourceUuid)} ;\n`;
  query += `<${nfo}fileName> ?name ;\n`;
  query += `<${dct}format> ?format ;\n`;
  query += `<${dbpedia}fileExtension> ?extension ;\n`;
  query += `<${nfo}fileSize> ?size ;\n`;
  query += `<${dct}created> ?created ;\n`;
  query += `<${dct}modified> ?modified .\n`;
  query += `}`;

  return query;
}

export function generateUploadResourceUriSelectQuery(uploadResourceUuid) {
  let query = `SELECT * WHERE {\n`;
  query += `?uri <${muCore}uuid> ${sparqlEscapeString(uploadResourceUuid)} ;\n`;
  query += `<${nfo}fileName> ?fileName ;\n`;
  query += `<${dbpedia}fileExtension> ?extension .\n`;
  query += `?fileUrl <${nie}dataSource> ?uri .\n`;
  query += `}`;

  return query;
}
