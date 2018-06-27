// https://github.com/onivim/oni/wiki/Configuration
// https://github.com/onivim/oni/blob/master/browser/src/Services/Configuration/DefaultConfiguration.ts

// bindings
// https://github.com/onivim/oni/blob/master/browser/src/Input/KeyBindings.ts

"use strict"
exports.__esModule = true
exports.activate = function(oni) {
  console.log("config activated")

  oni.input.bind("<c-enter>", () => console.log("Control+Enter was pressed"))

  // oni.input.unbind("<c-p>");
  oni.input.unbind("<c-g>")
  oni.input.bind("<s-c-g>", () => oni.commands.executeCommand("sneak.show"))
  oni.input.bind("<s-s>", () => oni.commands.executeCommand("command.show"))
  oni.input.bind("<c-,>", () => oni.commands.executeCommand("commands.show"))
  oni.input.bind("<m-,>", () => oni.commands.executeCommand("commands.show"))
}
exports.deactivate = function(oni) {
  console.log("config deactivated")
}
exports.configuration = {
  // "editor.renderer": "webgl",  // https://github.com/onivim/oni/pull/2120
  "oni.hideMenu": true,
  "oni.loadInitVim": true,
  "oni.useDefaultConfig": false,
  "autoClosingPairs.enabled": true,
  //"editor.quickOpen.filterStrategy": "regex",
  "commandline.mode": false, // Do not override commandline UI
  "wildmenu.mode": false, // Do not override wildmenu UI

  "experimental.markdownPreview.enabled": true,

  //"oni.bookmarks": ["~/Documents"],
  //"editor.fontSize": "14px",
  //"editor.fontFamily": "Monaco"

  "ui.colorscheme": "onedark",
  "ui.animations.enabled": true,
  "ui.fontSmoothing": "auto",
}
