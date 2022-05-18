var explore_items = {};

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