var express = require("express");
var app = express();

app.use(express.static(__dirname));

// set the view engine to ejs
app.set("view engine", "ejs");

// use res.render to load up an ejs view file

// index page
app.get("/", function (req, res) {
  res.render("pages/Home/index", {currentUrl : req.url});
});

app.get("/tech", function (req, res) {
  res.render("pages/Technology/index", {currentUrl : req.url});
});

app.get("/community", function (req, res) {
  res.render("pages/Community/index", {currentUrl : req.url});
});

app.get("/news", function (req, res) {
  res.render("pages/News/index", {currentUrl : req.url});
});

app.get("/roadmap", function (req, res) {
  res.render("pages/Roadmap/index", {currentUrl : req.url});
});

app.get("/brandassets", function (req, res) {
  res.render("pages/brandassets/index", {currentUrl : req.url});
});

// about page

app.listen(5000);
console.log("Server is listening on port 5000");
