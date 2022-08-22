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

// about page

app.listen(4000);
console.log("Server is listening on port 4000");
