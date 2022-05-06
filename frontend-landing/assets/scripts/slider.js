function onBorder(index) {
    var setCls = document.getElementsByClassName("home-main-carousel-border");
    setCls[index].className += " hover";
}

function offBorder() {
    var removeCls = document.getElementsByClassName("home-main-carousel-border hover");
    removeCls[0].className = "home-main-carousel-border";
}

var dana_items = {};
var explore_items = {};

function dana_init() {
    for (var i = 0; i < 5; i++) {
        dana_items[i] = document.getElementById('dana_' + i);
    }
    if (window.innerWidth < 600) {
        dana_items[1].className += " rec-carousel-item-hidden rec-carousel-item-next";
        dana_items[2].className += " rec-carousel-item-hidden";
        dana_items[3].className += " rec-carousel-item-hidden";
        dana_items[4].className += " rec-carousel-item-hidden";
    }
    else {
        dana_items[2].className += " rec-carousel-item-hidden rec-carousel-item-next";
        dana_items[3].className += " rec-carousel-item-hidden";
        dana_items[4].className += " rec-carousel-item-hidden";
    }
}

dana_init();

function dana_prev() {
    for (var i = 0; i < 5; i++) {
        if (dana_items[i].className.includes('rec-carousel-item-prev')) {
            if (i > 0)
                dana_items[i - 1].classList.toggle("rec-carousel-item-prev");
            dana_items[i].classList.remove("rec-carousel-item-hidden", "rec-carousel-item-prev");
            if (window.innerWidth < 600) {
                dana_items[i + 1].classList.toggle("rec-carousel-item-hidden");
                dana_items[i + 1].classList.toggle("rec-carousel-item-next");
                if (i < 3)
                    dana_items[i + 2].classList.remove("rec-carousel-item-next");
            }
            else {
                dana_items[i + 2].classList.toggle("rec-carousel-item-hidden");
                dana_items[i + 2].classList.toggle("rec-carousel-item-next");
                if (i < 2)
                    dana_items[i + 3].classList.remove("rec-carousel-item-next");
            }
        }
    }
}

function dana_next() {
    for (var i = 0; i < 5; i++) {
        if (dana_items[i].className.includes('rec-carousel-item-next')) {
            if (i < 4)
                dana_items[i + 1].classList.toggle("rec-carousel-item-next");
            dana_items[i].classList.remove("rec-carousel-item-hidden", "rec-carousel-item-next");
            if (window.innerWidth < 600) {
                dana_items[i - 1].classList.toggle("rec-carousel-item-hidden");
                dana_items[i - 1].classList.toggle("rec-carousel-item-prev");
                if (i > 1)
                    dana_items[i - 2].classList.remove("rec-carousel-item-prev");
            }
            else {
                dana_items[i - 2].classList.toggle("rec-carousel-item-hidden");
                dana_items[i - 2].classList.toggle("rec-carousel-item-prev");
                if (i > 2)
                    dana_items[i - 3].classList.remove("rec-carousel-item-prev");
            }
            break;
        }
    }
}

function explore_init() {
    explore_items[0] = document.getElementById('ex_0');
    explore_items[1] = document.getElementById('ex_1');
    if (window.innerWidth < 600)
        explore_items[1].className += " rec-carousel-item-hidden";
}
explore_init();

function explore_prev() {
    if (window.innerWidth < 600) {
        if (explore_items[0].className.includes('rec-carousel-item-hidden')) {
            explore_items[0].classList.remove('rec-carousel-item-hidden');
            explore_items[1].classList.toggle('rec-carousel-item-hidden');
        }
    }
}

function explore_next() {
    if (window.innerWidth < 600) {
        if (explore_items[1].className.includes('rec-carousel-item-hidden')) {
            explore_items[1].classList.remove('rec-carousel-item-hidden');
            explore_items[0].classList.toggle('rec-carousel-item-hidden');
        }
    }
}