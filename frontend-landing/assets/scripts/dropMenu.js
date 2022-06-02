const dropBtn = document.getElementById("dropBtn")
dropBtn.addEventListener("click", dropMenu)

function dropMenu() {
  var drawer = document.getElementById("navMenu")
  if (drawer.className === "drop-menu") {
    drawer.className += " responsive"
  } else {
    drawer.className = "drop-menu"
  }
}
