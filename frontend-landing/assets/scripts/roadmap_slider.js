const roadmap_items = document.querySelectorAll(
  `[data-name*="rec-carousel-item-"]`
)
var current_width = window.innerWidth

;(function () {
  const nextItemIndex = current_width < 600 ? 2 : 4
  roadmap_items.forEach((item, item_index) => {
    if (item_index >= nextItemIndex) {
      item.classList.add("rec-carousel-item-hidden")
    }
  })
  roadmap_items[nextItemIndex].classList.add("rec-carousel-item-next")
})()

window.addEventListener("resize", () => {
  for (var i = 0; i < roadmap_items.length; i++) {
    if (window.innerWidth < 600 && current_width >= 600) {
      if (roadmap_items[i].matches(".rec-carousel-item-next")) {
        roadmap_items[i].classList.remove("rec-carousel-item-next")
        roadmap_items[i - 1].classList.add("rec-carousel-item-hidden")
        roadmap_items[i - 2].classList.add(
          "rec-carousel-item-hidden",
          "rec-carousel-item-next"
        )
        break
      } else if (i == roadmap_items.length - 1) {
        roadmap_items[i].classList.add("rec-carousel-item-hidden")
        roadmap_items[i - 1].classList.add(
          "rec-carousel-item-hidden",
          "rec-carousel-item-next"
        )
      }
    }
    if (window.innerWidth >= 600 && current_width < 600) {
      if (roadmap_items[i].matches(".rec-carousel-item-next")) {
        roadmap_items[i].classList.remove(
          "rec-carousel-item-next",
          "rec-carousel-item-hidden"
        )
        if (i < roadmap_items.length - 1) {
          roadmap_items[i + 1].classList.remove("rec-carousel-item-hidden")
        }
        if (i == roadmap_items.length - 1) {
          roadmap_items[i - 3].classList.remove(
            "rec-carousel-item-hidden",
            "rec-carousel-item-prev"
          )
          roadmap_items[i - 4].classList.add("rec-carousel-item-prev")
        }
        if (i < roadmap_items.length - 2) {
          roadmap_items[i + 2].classList.add("rec-carousel-item-next")
        }
        break
      } else if (i == roadmap_items.length - 1) {
        roadmap_items[i - 2].classList.remove(
          "rec-carousel-item-prev",
          "rec-carousel-item-hidden"
        )
        roadmap_items[i - 3].classList.remove("rec-carousel-item-hidden")
        roadmap_items[i - 4].classList.add("rec-carousel-item-prev")
      }
    }
  }

  current_width = window.innerWidth
})

function prev() {
  for (var i = 0; i < roadmap_items.length; i++) {
    if (roadmap_items[i].className.includes("rec-carousel-item-prev")) {
      if (i > 0) roadmap_items[i - 1].classList.toggle("rec-carousel-item-prev")
      roadmap_items[i].classList.remove(
        "rec-carousel-item-hidden",
        "rec-carousel-item-prev"
      )
      if (current_width < 600) {
        roadmap_items[i + 2].classList.toggle("rec-carousel-item-hidden")
        roadmap_items[i + 2].classList.toggle("rec-carousel-item-next")
        if (i < 6)
          roadmap_items[i + 3].classList.remove("rec-carousel-item-next")
      } else {
        roadmap_items[i + 4].classList.toggle("rec-carousel-item-hidden")
        roadmap_items[i + 4].classList.toggle("rec-carousel-item-next")
        if (i < 4)
          roadmap_items[i + 5].classList.remove("rec-carousel-item-next")
      }
    }
  }
}

function next() {
  for (var i = 0; i < 9; i++) {
    if (roadmap_items[i].className.includes("rec-carousel-item-next")) {
      if (i < 8) roadmap_items[i + 1].classList.toggle("rec-carousel-item-next")
      roadmap_items[i].classList.remove(
        "rec-carousel-item-hidden",
        "rec-carousel-item-next"
      )
      if (current_width < 600) {
        roadmap_items[i - 2].classList.toggle("rec-carousel-item-hidden")
        roadmap_items[i - 2].classList.toggle("rec-carousel-item-prev")
        if (i > 2)
          roadmap_items[i - 3].classList.remove("rec-carousel-item-prev")
      } else {
        roadmap_items[i - 4].classList.toggle("rec-carousel-item-hidden")
        roadmap_items[i - 4].classList.toggle("rec-carousel-item-prev")
        if (i > 4)
          roadmap_items[i - 5].classList.remove("rec-carousel-item-prev")
      }
      break
    }
  }
}
