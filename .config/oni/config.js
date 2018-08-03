"use strict";
exports.__esModule = true;
exports.activate = function (oni) {
    console.log("config activated");
    oni.input.unbind("<c-g>");
    oni.input.bind("<s-c-g>", function () { return oni.commands.executeCommand("sneak.show"); });
    oni.input.bind("<s-s>", function () { return oni.commands.executeCommand("command.show"); });
    oni.input.bind("<c-,>", function () { return oni.commands.executeCommand("commands.show"); });
    oni.input.bind("<m-,>", function () { return oni.commands.executeCommand("commands.show"); });
};
exports.deactivate = function (oni) {
    console.log("config deactivated");
};
exports.configuration = {
    //add custom config here, such as
    //"ui.colorscheme": "nord",
    //"oni.useDefaultConfig": true,
    //"oni.bookmarks": ["~/Documents"],
    "oni.loadInitVim": true,
    //"editor.fontSize": "12px",
    //"editor.fontFamily": "Monaco",
    // UI customizations
    "ui.animations.enabled": true,
    "ui.fontSmoothing": "auto",
    // "editor.renderer": "webgl",  // https://github.com/onivim/oni/pull/2120
    "oni.hideMenu": true,
    "oni.useDefaultConfig": false,
    "autoClosingPairs.enabled": true,
    //"editor.quickOpen.filterStrategy": "regex",
    "commandline.mode": false,
    "wildmenu.mode": false,
    "experimental.markdownPreview.enabled": true,
    //"oni.bookmarks": ["~/Documents"],
    //"editor.fontSize": "14px",
    //"editor.fontFamily": "Monaco"
    "ui.colorscheme": "molokai"
};
