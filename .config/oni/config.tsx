
import * as React from "/Users/justin/opt/Oni.app/Contents/Resources/app/node_modules/react"
import * as Oni from "/Users/justin/opt/Oni.app/Contents/Resources/app/node_modules/oni-api"

export const activate = (oni: Oni.Plugin.Api) => {
    console.log("config activated")

  oni.input.unbind("<c-g>")
  oni.input.bind("<s-c-g>", () => oni.commands.executeCommand("sneak.show"))
  oni.input.bind("<s-s>", () => oni.commands.executeCommand("command.show"))
  oni.input.bind("<c-,>", () => oni.commands.executeCommand("commands.show"))
  oni.input.bind("<m-,>", () => oni.commands.executeCommand("commands.show"))
}

export const deactivate = (oni: Oni.Plugin.Api) => {
    console.log("config deactivated")
}

export const configuration = {
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
  "commandline.mode": false, // Do not override commandline UI
  "wildmenu.mode": false, // Do not override wildmenu UI

  "experimental.markdownPreview.enabled": true,

  //"oni.bookmarks": ["~/Documents"],
  //"editor.fontSize": "14px",
  //"editor.fontFamily": "Monaco"

  //"ui.colorscheme": "molokai",
}
