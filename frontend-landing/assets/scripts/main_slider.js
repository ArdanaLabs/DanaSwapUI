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

const danaCards = document.querySelectorAll(`[id^="dana_"]`)
const exploreCards = document.querySelectorAll(`[id^="ex_"]`)
const cards = [danaCards, exploreCards]
const tabletWidth = 600
var currentWidth = window.innerWidth

;(function () {
  const nextItemIndex = currentWidth < tabletWidth ? 1 : 2

  cards.forEach((cardItems, cardIndex) => {
    cardItems.forEach((item, itemIndex) => {
      if (itemIndex >= nextItemIndex) {
        item.classList.add("rec-carousel-item-hidden")
      }
    })
    if (currentWidth >= tabletWidth && cardIndex == 1) {
    } else if (danaCards.length != 0) {
      cardItems[nextItemIndex].classList.add("rec-carousel-item-next")
    }
    if (currentWidth < tabletWidth && danaCards.length == 0 && cardIndex == 1) {
      cardItems[nextItemIndex].classList.add("rec-carousel-item-next")
    }
  })
})()

window.addEventListener("resize", () => {
  cards.forEach((cardItems) => {
    for (var i = 0; i < cardItems.length; i++) {
      if (window.innerWidth < tabletWidth && currentWidth >= tabletWidth) {
        if (cardItems[i].matches(".rec-carousel-item-next")) {
          cardItems[i].classList.remove("rec-carousel-item-next")
          cardItems[i - 1].classList.add(
            "rec-carousel-item-hidden",
            "rec-carousel-item-next"
          )
          break
        } else if (i == cardItems.length - 1) {
          cardItems[i].classList.add(
            "rec-carousel-item-hidden",
            "rec-carousel-item-next"
          )
        }
      }
      if (window.innerWidth >= tabletWidth && currentWidth < tabletWidth) {
        if (cardItems[i].matches(".rec-carousel-item-next")) {
          cardItems[i].classList.remove(
            "rec-carousel-item-next",
            "rec-carousel-item-hidden"
          )
          if (i < cardItems.length - 1) {
            cardItems[i + 1].classList.add("rec-carousel-item-next")
          }
          break
        } else if (i == cardItems.length - 1) {
          cardItems[i - 1].classList.remove(
            "rec-carousel-item-prev",
            "rec-carousel-item-hidden"
          )
          if (i > 1) {
            cardItems[i - 2].classList.add("rec-carousel-item-prev")
          }
        }
      }
    }
  })
  currentWidth = window.innerWidth
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
      if (window.innerWidth < tabletWidth) {
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
      if (window.innerWidth < tabletWidth) {
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
