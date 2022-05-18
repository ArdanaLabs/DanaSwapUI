var expansion = {};
var collapse = {};
var info = {};
var social = {};
var bg = {};

for (var i = 0; i < 25; i++) {
    expansion[i] = document.getElementById('expansion-' + i);
    collapse[i] = document.getElementById('collapse-' + i);
    info[i] = document.getElementById('team_card-info-' + i);
    social[i] = document.getElementById('team_card-socials-' + i);
    bg[i] = document.getElementById('team_card-bg-' + i);
}

function init() {
    if (window.innerWidth < 600) {
        for (var i = 0; i < 25; i++) {
            bg[i].classList.toggle('collapsed');
            info[i].classList.toggle('hide');
            social[i].classList.toggle('hide');
        }
    }
}
init();

function expand(j) {
    expansion[j].classList.toggle('hide');
    collapse[j].classList.remove('hide');
    bg[j].classList.remove('collapsed');
    info[j].classList.remove('hide');
    social[j].classList.remove('hide');
}

function collap(j) {
    expansion[j].classList.remove('hide');
    collapse[j].classList.toggle('hide');
    bg[j].classList.toggle('collapsed');
    info[j].classList.toggle('hide');
    social[j].classList.toggle('hide');
}