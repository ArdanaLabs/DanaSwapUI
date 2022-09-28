var express = require("express");
var routes = require("./routes.json");
var app = express();

app.use(express.static(__dirname));

// set the view engine to ejs
app.set("view engine", "ejs");

// use res.render to load up an ejs view file

// index page

routes.forEach((route) => {
  const { path, dir } = route
  app.get(path, function (req, res) {
    res.render(dir, { currentUrl: req.url })
  })
})

// about page

app.listen(5000);
console.log("Server is listening on port 5000");
