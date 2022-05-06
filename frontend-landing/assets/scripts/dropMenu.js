function dropMenu() {
    var drower = document.getElementById("navMenu");
    if (drower.className === "drop-menu") {
        drower.className += " responsive";
    } else {
        drower.className = "drop-menu";
    }
}