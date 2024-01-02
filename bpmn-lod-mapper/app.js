import { app, update } from "mu";
import multer from "multer";
import { readFile, unlink } from "fs/promises";
import * as RmlMapper from "@comake/rmlmapper-js";
import { mapping } from "./rml-mapping.js";

const upload = multer({ dest: "uploads/" });

app.post("/", upload.single("file"), async (req, res) => {
  const filePath = req.file.path;

  let bpmn = await readFile(filePath, "utf-8");
  await unlink(filePath);

  const triples = await mapBpmnToRdf(bpmn);
  const query = generateUpdateQuery(triples);
  await update(query);

  res.send("Hello world");
});

async function mapBpmnToRdf(bpmn) {
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

  return await RmlMapper.parseTurtle(mapping, inputFiles, options);
}

function generateUpdateQuery(triples) {
  return `INSERT DATA { ${triples} }`;
}
