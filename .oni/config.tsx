import * as React from "/home/norbert/.local/share/Oni/resources/app/node_modules/react"
import * as Oni from "/home/norbert/.local/share/Oni/resources/app/node_modules/oni-api"
export const activate = (oni: Oni.Plugin.Api) => {
  // Add input bindings here:
  // oni.input.bind("<c-enter>", () => console.log("Control+Enter was pressed"))
  oni.input.unbind("<c-p>")
  oni.input.unbind("<c-g>") // make C-g work as expected in vim
  oni.input.unbind("<c-c>") // make C-c work as expected in vim
  oni.input.unbind("<c-v>") // make C-v work as expected in vim
  oni.input.unbind("<c-y>") // make C-y work as expected in vim
  // oni.input.bind("<s-c-g>", () => oni.commands.executeCommand("sneak.show")) // You can rebind Oni's behaviour to a new keybinding
}
export const deactivate = (oni: Oni.Plugin.Api) => {
  console.log("config deactivated")
}
export const configuration = {
  //add custom config here, such as
  "ui.colorscheme": "nord",
  "oni.hideMenu": true,
  "statusbar.fontSize": 25,
  "ui.fontSize": 17,
  //"oni.useDefaultConfig": true,
  "oni.bookmarks": ["~/Documents",
    "~/Documents/Programming/Functional Programming/Haskell"
  ],
  // "experimental.markdownPreview.enabled": true,
  "oni.loadInitVim": true,
  "autoClosingPairs.enabled": false, // disable autoclosing pairs
  "editor.fontSize": "17px",
  "sidebar.enabled": false,
  //"editor.fontFamily": "Monaco",
  // UI customizations
  "ui.animations.enabled": false,
  "ui.fontSmoothing": "auto",
}
