window.addEventListener("scroll", function () {
  var menu = document.getElementById("navMenu")
  menu.classList.toggle("sticky", window.scrollY > 0)
})
