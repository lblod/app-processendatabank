import {
  app,
  update,
  query,
  uuid,
  sparqlEscapeUri,
  sparqlEscapeString,
  sparqlEscapeInt,
  sparqlEscapeDateTime,
} from "mu";
import multer from "multer";
import { readFile, unlink, rename } from "fs/promises";
import * as RmlMapper from "@comake/rmlmapper-js";
import { mapping as bboMapping } from "./bbo-mapping.js";
import { mapping as uuidMapping } from "./uuid-mapping.js";
import path from "path";
import { existsSync, mkdirSync } from "fs";

const muCore = "http://mu.semte.ch/vocabularies/core/";
const nfo = "http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#";
const dct = "http://purl.org/dc/terms/";
const dbpedia = "http://dbpedia.org/ontology/";
const nie = "http://www.semanticdesktop.org/ontologies/2007/01/19/nie#";

const storageFolderPath = "/share/";
const allowedFileExtensions = [".bpmn", ".xml"];
const upload = multer({ dest: "temp/" });

app.post("/", upload.single("file"), async (req, res) => {
  console.log("hallo")
  const rewriteUrl = req.get("x-rewrite-url");
  if (!rewriteUrl) {
    return res.status(400).send("X-Rewrite-URL header is missing.");
  }

  const tempFilePath = req.file.path;
  const uploadResourceName = req.file.originalname;
  const fileExtension = path.extname(uploadResourceName);

  if (!allowedFileExtensions.includes(fileExtension.toLowerCase())) {
    await unlink(tempFilePath);
    return res
      .status(400)
      .send(
        `Invalid file extension: The file extension '${fileExtension}' is not allowed. Only ${allowedFileExtensions.join(
          ", "
        )} are.`
      );
  }

  const bpmn = await readFile(tempFilePath, "utf-8");
  let triples;
  try {
    triples = await translateToRdf(bpmn);
  } catch (error) {
    await unlink(tempFilePath);
    return res.status(error.statusCode || 500).send(error.message);
  }

  const bboQuery = generateUpdateQuery(triples);
  await update(bboQuery);

  const uploadResourceUuid = uuid();
  const fileFormat = req.file.mimetype;
  const fileSize = req.file.size;
  const fileResourceUuid = uuid();
  const fileResourceName = fileResourceUuid + fileExtension;

  const fileQuery = generateFileUpdateQuery(
    uploadResourceName,
    uploadResourceUuid,
    fileFormat,
    fileSize,
    fileExtension,
    fileResourceName,
    fileResourceUuid
  );
  await update(fileQuery);

  if (!existsSync(storageFolderPath)) {
    mkdirSync(storageFolderPath);
  }
  await rename(tempFilePath, storageFolderPath + fileResourceName);

  return res
    .status(201)
    .contentType("application/vnd.api+json")
    .json({
      data: {
        type: "bpmn-files",
        id: uploadResourceUuid,
        attributes: {
          name: uploadResourceName,
          format: fileFormat,
          size: fileSize,
          extension: fileExtension,
        },
      },
      links: {
        self: `${req.protocol}://${req.hostname}${rewriteUrl}/${uploadResourceUuid}`,
      },
    });
});

app.get("/:id", async (req, res) => {
  const rewriteUrl = req.get("x-rewrite-url");
  if (!rewriteUrl) {
    return res.status(400).send("X-Rewrite-URL header is missing.");
  }

  const uploadResourceUuid = req.params.id;
  const selectQuery = generateFileSelectQuery(uploadResourceUuid);
  const result = await query(selectQuery);
  const bindings = result.results.bindings;
  if (bindings.length === 0) {
    return res.status(404).send("Not Found");
  }
  const firstBinding = bindings[0];

  return res
    .status(200)
    .contentType("application/vnd.api+json")
    .json({
      data: {
        type: "bpmn-files",
        id: uploadResourceUuid,
        attributes: {
          name: firstBinding.name.value,
          format: firstBinding.format.value,
          size: firstBinding.size.value,
          extension: firstBinding.extension.value,
        },
      },
      links: {
        self: `${req.protocol}://${req.hostname}${rewriteUrl}`,
      },
    });
});

app.get("/:id/download", async (req, res) => {
  const uploadResourceUuid = req.params.id;
  const selectQuery = generateUploadResourceUriSelectQuery(uploadResourceUuid);
  const result = await query(selectQuery);
  const bindings = result.results.bindings;
  if (bindings.length === 0) {
    return res.status(404).send("Not Found");
  }
  const uploadResourceUri = bindings[0].fileUrl.value;

  const filePath = uploadResourceUri.replace("share://", storageFolderPath);
  if (!existsSync(filePath)) {
    return res
      .status(500)
      .send(
        "Could not find file in path. Check if the physical file is available on the server and if this service has the right mountpoint."
      );
  }

  const fileName = req.query.name || path.basename(filePath);
  const disposition =
    req.header("content-disposition")?.toLowerCase() === "inline"
      ? "inline"
      : "attachment";

  res.setHeader(
    "Content-Disposition",
    `${disposition}; filename="${fileName}"`
  );
  return res.sendFile(filePath);
});

async function translateToRdf(bpmn) {
  if (!bpmn || bpmn.trim().length === 0) {
    const error = new Error(
      "Invalid content: The provided file does not contain any content."
    );
    error.statusCode = 400;
    throw error;
  }

  const inputFiles = {
    "input.bpmn": bpmn,
  };
  const options = {
    compact: {
      "@base": "https://example.org/",
    },
    toRDF: true,
    xpathLib: "xpath",
  };

  const bboTriples = await RmlMapper.parseTurtle(bboMapping, inputFiles, options);
  const uuidTriples = await RmlMapper.parseTurtle(uuidMapping, inputFiles, options);
  const triples = bboTriples + uuidTriples
  if (!triples || triples.trim().length === 0) {
    const error = new Error(
      "Invalid content: The provided file does not contain valid content."
    );
    error.statusCode = 400;
    throw error;
  }

  return triples;
}

function generateUpdateQuery(triples) {
  return `INSERT DATA { ${triples} }`;
}

function generateFileUpdateQuery(
  uploadResourceName,
  uploadResourceUuid,
  fileFormat,
  fileSize,
  fileExtension,
  fileResourceName,
  fileResourceUuid
) {
  const uploadResourceUri = `http://mu.semte.ch/services/file-service/files/${uploadResourceUuid}`;
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

function generateFileSelectQuery(uploadResourceUuid) {
  let query = `SELECT ?uri ?name ?format ?size ?extension WHERE {\n`;
  query += `?uri <${muCore}uuid> ${sparqlEscapeString(uploadResourceUuid)} ;\n`;
  query += `<${nfo}fileName> ?name ;\n`;
  query += `<${dct}format> ?format ;\n`;
  query += `<${dbpedia}fileExtension> ?extension ;\n`;
  query += `<${nfo}fileSize> ?size .\n`;
  query += `}`;

  return query;
}

function generateUploadResourceUriSelectQuery(uploadResourceUuid) {
  let query = `SELECT ?fileUrl WHERE {\n`;
  query += `?uri <${muCore}uuid> ${sparqlEscapeString(uploadResourceUuid)} .\n`;
  query += `?fileUrl <${nie}dataSource> ?uri .\n`;
  query += `}`;

  return query;
}
