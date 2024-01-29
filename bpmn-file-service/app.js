import { app, update, query, uuid } from "mu";
import multer from "multer";
import { readFile, unlink, copyFile } from "fs/promises";
import * as RmlMapper from "@comake/rmlmapper-js";
import { mapping as bboMapping } from "./bbo-mapping.js";
import path from "path";
import { existsSync, mkdirSync } from "fs";
import {
  generateUpdateQuery,
  generateFileUpdateQuery,
  generateFilesSelectQuery,
  generateFileSelectQuery,
  generateUploadResourceUriSelectQuery,
} from "./sparql-queries.js";

const storageFolderPath = "/share/";
const allowedFileExtensions = [".bpmn", ".xml"];
const upload = multer({ dest: "temp/" });

/**
 * POST `/` - Upload a BPMN file
 * Description: This endpoint allows for the uploading of BPMN (.bpmn, .xml) files.
 * Headers:
 *  - x-rewrite-url: Required. Used for generating resource links.
 * Body:
 *  - multipart/form-data with a file field.
 * Response:
 *  - 201 Created: Successfully created and stored the file.
 *  - 400 Bad Request: If the required x-rewrite-url header is missing or if the file extension is not allowed.
 *  - 500 Internal Server Error: In case of unexpected server errors.
 */
app.post("/", upload.single("file"), async (req, res) => {
  const rewriteUrl = req.get("x-rewrite-url");
  if (!rewriteUrl) {
    return res.status(400).send("X-Rewrite-URL header is missing.");
  }

  const tempFilePath = req.file.path;
  let uploadResourceName = req.file.originalname;
  const fileExtension = path.extname(uploadResourceName);
  uploadResourceName = uploadResourceName.replace(fileExtension, "");

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

  const uploadResourceUuid = uuid();
  const uploadResourceUri = `https://example.org/services/bpmn-file-service/files/${uploadResourceUuid}`;

  const bpmn = await readFile(tempFilePath, "utf-8");
  let triples;
  try {
    triples = await translateToRdf(bpmn, uploadResourceUri);
  } catch (error) {
    await unlink(tempFilePath);
    return res.status(error.statusCode || 500).send(error.message);
  }

  const bboQuery = generateUpdateQuery(triples);
  await update(bboQuery);

  const fileFormat = req.file.mimetype;
  const fileSize = req.file.size;
  const fileResourceUuid = uuid();
  const fileResourceName = fileResourceUuid + fileExtension;

  const fileQuery = generateFileUpdateQuery(
    uploadResourceUri,
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
  await copyFile(tempFilePath, storageFolderPath + fileResourceName);
  await unlink(tempFilePath);

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

/**
 * GET `/` - List all BPMN files
 * Description: Retrieve a list of all uploaded BPMN files.
 * Headers:
 *  - x-rewrite-url: Required. Used for generating resource links.
 * Query Parameters:
 *  - filter: Optional. Filter files by name. Supports both `filter[name]=value` and `filter=value` formats.
 * Response:
 *  - 200 OK: Successfully retrieved the list of files.
 *  - 400 Bad Request: If the x-rewrite-url header is missing.
 *  - 500 Internal Server Error: In case of unexpected server errors.
 */
app.get("/", async (req, res) => {
  const rewriteUrl = req.get("x-rewrite-url");
  if (!rewriteUrl) {
    return res.status(400).send("X-Rewrite-URL header is missing.");
  }

  let nameFilter;
  const filterParam = req.query.filter;
  if (typeof filterParam === "object" && filterParam.name) {
    nameFilter = filterParam.name;
  } else if (typeof filterParam === "string") {
    nameFilter = filterParam;
  }

  const selectQuery = generateFilesSelectQuery(nameFilter);
  const result = await query(selectQuery);
  const bindings = result.results.bindings;

  return res
    .status(200)
    .contentType("application/vnd.api+json")
    .json({
      data: bindings.map((binding) => ({
        type: "bpmn-files",
        id: binding.uuid.value,
        attributes: {
          name: binding.name.value,
          format: binding.format.value,
          size: binding.size.value,
          extension: binding.extension.value,
        },
        links: {
          self: `${req.protocol}://${req.hostname}${rewriteUrl}/${binding.uuid.value}`,
        },
      })),
      links: {
        self: `${req.protocol}://${req.hostname}${rewriteUrl}`,
      },
    });
});

/**
 * GET `/:id` - Retrieve a specific BPMN file
 * Description: Get detailed information about a specific BPMN file by its UUID.
 * Headers:
 *  - x-rewrite-url: Required. Used for generating resource links.
 * Path Parameters:
 *  - id: Required. UUID of the BPMN file.
 * Response:
 *  - 200 OK: Successfully retrieved file details.
 *  - 400 Bad Request: If the x-rewrite-url header is missing.
 *  - 404 Not Found: If no file matches the given UUID.
 *  - 500 Internal Server Error: In case of unexpected server errors.
 */
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
          created: firstBinding.created.value,
          modified: firstBinding.modified.value,
        },
      },
      links: {
        self: `${req.protocol}://${req.hostname}${rewriteUrl}`,
      },
    });
});

/**
 * GET `/:id/download` - Download a BPMN file
 * Description: Download a specific BPMN file by its UUID.
 * Headers:
 *  - x-rewrite-url: Required. Used for generating resource links.
 *  - content-disposition: Optional. Determines the Content-Disposition header in the response (inline or attachment).
 * Path Parameters:
 *  - id: Required. UUID of the BPMN file.
 * Response:
 *  - 200 OK: Successfully retrieved the file for download.
 *  - 400 Bad Request: If the x-rewrite-url header is missing.
 *  - 404 Not Found: If no file matches the given UUID.
 *  - 500 Internal Server Error: If the file exists in the database but is missing in the server storage.
 */
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

  const fileName = bindings[0].fileName.value;
  const extension = bindings[0].extension.value;
  const disposition =
    req.header("content-disposition")?.toLowerCase() === "inline"
      ? "inline"
      : "attachment";

  res.setHeader(
    "Content-Disposition",
    `${disposition}; filename="${fileName}${extension}"`
  );
  return res.sendFile(filePath);
});

async function translateToRdf(bpmn, uploadResourceUri) {
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

  const triples = await RmlMapper.parseTurtle(
    bboMapping(uploadResourceUri),
    inputFiles,
    options
  );
  if (!triples || triples.trim().length === 0) {
    const error = new Error(
      "Invalid content: The provided file does not contain valid content."
    );
    error.statusCode = 400;
    throw error;
  }

  return triples;
}
