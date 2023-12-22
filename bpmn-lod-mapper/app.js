import { app } from "mu";
import * as RmlMapper from "@comake/rmlmapper-js";
import { mapping } from "./rml-mapping.js";
import { bpmn } from "./bpmn-example.js";

app.get("/", async (req, res) => {
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

  const result = await RmlMapper.parseTurtle(mapping, inputFiles, options);
  console.log(result);

  res.send("Hello world");
});
