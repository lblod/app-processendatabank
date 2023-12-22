import { app } from "mu";

app.get("/", (req, res) => {
  res.send("Hello world");
});
