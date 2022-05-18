var express = require("express");
var app = express();

app.use(express.static(__dirname));

// set the view engine to ejs
app.set("view engine", "ejs");

// use res.render to load up an ejs view file

// index page
app.get("/", function (req, res) {
  res.render("pages/home/index", {currentUrl : req.url});
});

app.get("/tech", function (req, res) {
  res.render("pages/technology/index", {currentUrl : req.url});
});

app.get("/community", function (req, res) {
  res.render("pages/community/index", {currentUrl : req.url});
});

app.get("/news", function (req, res) {
  res.render("pages/news/index", {currentUrl : req.url});
});

app.get("/roadmap", function (req, res) {
  res.render("pages/roadmap/index", {currentUrl : req.url});
});

app.get("/brandassets", function (req, res) {
  res.render("pages/brandassets/index", {currentUrl : req.url});
});

app.get("/team", function (req, res) {
  res.render("pages/team/index", {currentUrl : req.url});
});

// about page

app.listen(5000);
console.log("Server is listening on port 5000");
