
var roadmap_items = {};

function roadmap_init() {
    for (var i = 0; i < 9; i++) {
        roadmap_items[i] = document.getElementById('roadmap_' + i);
    }
    if (window.innerWidth < 600) {
        roadmap_items[2].className += " rec-carousel-item-hidden rec-carousel-item-next";
        roadmap_items[3].className += " rec-carousel-item-hidden";
        roadmap_items[4].className += " rec-carousel-item-hidden";
        roadmap_items[5].className += " rec-carousel-item-hidden";
        roadmap_items[6].className += " rec-carousel-item-hidden";
        roadmap_items[7].className += " rec-carousel-item-hidden";
        roadmap_items[8].className += " rec-carousel-item-hidden";
    }
    else {
        roadmap_items[4].className += " rec-carousel-item-hidden rec-carousel-item-next";
        roadmap_items[5].className += " rec-carousel-item-hidden";
        roadmap_items[6].className += " rec-carousel-item-hidden";
        roadmap_items[7].className += " rec-carousel-item-hidden";
        roadmap_items[8].className += " rec-carousel-item-hidden";
    }
}

roadmap_init();

function roadmap_prev() {
    for (var i = 0; i < 9; i++) {
        if (roadmap_items[i].className.includes('rec-carousel-item-prev')) {
            if (i > 0)
                roadmap_items[i - 1].classList.toggle("rec-carousel-item-prev");
            roadmap_items[i].classList.remove("rec-carousel-item-hidden", "rec-carousel-item-prev");
            if (window.innerWidth < 600) {
                roadmap_items[i + 2].classList.toggle("rec-carousel-item-hidden");
                roadmap_items[i + 2].classList.toggle("rec-carousel-item-next");
                if (i < 6)
                    roadmap_items[i + 3].classList.remove("rec-carousel-item-next");
            }
            else {
                roadmap_items[i + 4].classList.toggle("rec-carousel-item-hidden");
                roadmap_items[i + 4].classList.toggle("rec-carousel-item-next");
                if (i < 4)
                    roadmap_items[i + 5].classList.remove("rec-carousel-item-next");
            }
        }
    }
}

function roadmap_next() {
    for (var i = 0; i < 9; i++) {
        if (roadmap_items[i].className.includes('rec-carousel-item-next')) {
            if (i < 8)
                roadmap_items[i + 1].classList.toggle("rec-carousel-item-next");
            roadmap_items[i].classList.remove("rec-carousel-item-hidden", "rec-carousel-item-next");
            if (window.innerWidth < 600) {
                roadmap_items[i - 2].classList.toggle("rec-carousel-item-hidden");
                roadmap_items[i - 2].classList.toggle("rec-carousel-item-prev");
                if (i > 2)
                    roadmap_items[i - 3].classList.remove("rec-carousel-item-prev");
            }
            else {
                roadmap_items[i - 4].classList.toggle("rec-carousel-item-hidden");
                roadmap_items[i - 4].classList.toggle("rec-carousel-item-prev");
                if (i > 3)
                    roadmap_items[i - 5].classList.remove("rec-carousel-item-prev");
            }
            break;
        }
    }
}
