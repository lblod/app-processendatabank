import { app, update, uuid } from "mu";
import multer from "multer";
import { readFile, unlink, rename } from "fs/promises";
import * as RmlMapper from "@comake/rmlmapper-js";
import { mapping } from "./rml-mapping.js";
import path from "path";
import { existsSync, mkdirSync } from "fs";

const storageFolderPath = "/share/";
if (!existsSync(storageFolderPath)) {
  mkdirSync(storageFolderPath);
}

const allowedFileExtensions = [".bpmn", ".xml"];
const upload = multer({ dest: "temp/" });

app.post("/", upload.single("file"), async (req, res) => {
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

  const query = generateUpdateQuery(triples);
  await update(query);

  const uploadResourceUuid = uuid();
  const uploadResourceUri = `http://mu.semte.ch/services/file-service/files/${uploadResourceUuid}`;
  const fileFormat = req.file.mimetype;
  const fileSize = req.file.size;
  const fileResourceUuid = uuid();
  const fileResourceName = fileResourceUuid + fileExtension;
  const fileResourceUri = `share://${fileResourceName}`;
  const now = new Date().toISOString();

  await rename(tempFilePath, storageFolderPath + fileResourceName);

  return res
    .status(201)
    .contentType("application/vnd.api+json")
    .json({
      data: {
        type: "files",
        id: uploadResourceUuid,
        attributes: {
          name: uploadResourceName,
          format: fileFormat,
          size: fileSize,
          extension: fileExtension,
        },
      },
      links: {
        self: `${req.protocol}://${req.host}${req.get(
          "x-rewrite-url"
        )}/${uploadResourceUuid}`,
      },
    });
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

  const triples = await RmlMapper.parseTurtle(mapping, inputFiles, options);
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
