"use strict";

exports.initSound = function() {
    const click = document.getElementById("click-sound");
    window.onclick = function () {
        click.play();
    };
};

exports.playTada = function() {
    const tada = document.getElementById("tada-sound");
    tada.play();
};
