function onBorder(index) {
  var setCls = document.getElementsByClassName("home-main-carousel-border")
  setCls[index].classList.add("hover")
}

function offBorder() {
  var removeCls = document.getElementsByClassName(
    "home-main-carousel-border hover"
  )
  removeCls[0].classList.remove("hover")
}

const dana_cards = document.querySelectorAll(`[id^="dana_"]`)
const explore_cards = document.querySelectorAll(`[id^="ex_"]`)
const cards = [dana_cards, explore_cards]
var current_width = window.innerWidth

;(function () {
  const nextItemIndex = current_width < 600 ? 1 : 2

  cards.forEach((card_items, card_index) => {
    card_items.forEach((item, item_index) => {
      if (item_index >= nextItemIndex) {
        item.classList.add("rec-carousel-item-hidden")
      }
    })
    if (current_width >= 600 && card_index == 1) {
    } else if (dana_cards.length != 0) {
      card_items[nextItemIndex].classList.add("rec-carousel-item-next")
    }
  })
})()

window.addEventListener("resize", () => {
  cards.forEach((card_items) => {
    for (var i = 0; i < card_items.length; i++) {
      if (window.innerWidth < 600 && current_width >= 600) {
        if (card_items[i].matches(".rec-carousel-item-next")) {
          card_items[i].classList.remove("rec-carousel-item-next")
          card_items[i - 1].classList.add(
            "rec-carousel-item-hidden",
            "rec-carousel-item-next"
          )
          break
        } else if (i == card_items.length - 1) {
          card_items[i].classList.add(
            "rec-carousel-item-hidden",
            "rec-carousel-item-next"
          )
        }
      }
      if (window.innerWidth >= 600 && current_width < 600) {
        if (card_items[i].matches(".rec-carousel-item-next")) {
          card_items[i].classList.remove(
            "rec-carousel-item-next",
            "rec-carousel-item-hidden"
          )
          if (i < card_items.length - 1) {
            card_items[i + 1].classList.add("rec-carousel-item-next")
          }
          break
        } else if (i == card_items.length - 1) {
          card_items[i - 1].classList.remove(
            "rec-carousel-item-prev",
            "rec-carousel-item-hidden"
          )
          card_items[i - 2].classList.add("rec-carousel-item-prev")
        }
      }
    }
  })
  current_width = window.innerWidth
})

function prev(items) {
  for (var i = 0; i < items.length; i++) {
    if (items[i].matches(".rec-carousel-item-prev")) {
      if (i > 0) {
        items[i - 1].classList.add("rec-carousel-item-prev")
      }
      items[i].classList.remove(
        "rec-carousel-item-hidden",
        "rec-carousel-item-prev"
      )
      if (window.innerWidth < 600) {
        items[i + 1].classList.add("rec-carousel-item-hidden")
        items[i + 1].classList.add("rec-carousel-item-next")
        if (i < items.length - 2) {
          items[i + 2].classList.remove("rec-carousel-item-next")
        }
      } else {
        items[i + 2].classList.add("rec-carousel-item-hidden")
        items[i + 2].classList.add("rec-carousel-item-next")
        if (i < items.length - 3) {
          items[i + 3].classList.remove("rec-carousel-item-next")
        }
      }
    }
  }
}

function next(items) {
  for (var i = 0; i < items.length; i++) {
    if (items[i].matches(".rec-carousel-item-next")) {
      if (i < items.length - 1) {
        items[i + 1].classList.add("rec-carousel-item-next")
      }
      items[i].classList.remove(
        "rec-carousel-item-hidden",
        "rec-carousel-item-next"
      )
      if (window.innerWidth < 600) {
        items[i - 1].classList.add("rec-carousel-item-hidden")
        items[i - 1].classList.add("rec-carousel-item-prev")
        if (i > 1) {
          items[i - 2].classList.remove("rec-carousel-item-prev")
        }
      } else {
        items[i - 2].classList.add("rec-carousel-item-hidden")
        items[i - 2].classList.add("rec-carousel-item-prev")
        if (i > 2) {
          items[i - 3].classList.remove("rec-carousel-item-prev")
        }
      }
      break
    }
  }
}
