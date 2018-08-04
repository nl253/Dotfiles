"use strict";
exports.__esModule = true;
exports.activate = function (oni) {
    console.log('config activated');
    // Input
    //
    // Add input bindings here:
    //
    oni.input.bind('<c-enter>', function () { return console.log('Control+Enter was pressed'); });
    //
    // Or remove the default bindings here by uncommenting the below line:
    //
    // oni.input.unbind("<c-p>")
};
exports.deactivate = function (oni) {
    console.log('config deactivated');
};
exports.configuration = {
    // "oni.useDefaultConfig": true,
    'ui.colorscheme': 'fabulous',
    'oni.bookmarks': ['~/Documents', '~/Documents/Notes/', '~/Documents/Programming/'],
    'oni.loadInitVim': true,
    'editor.fontSize': '16px',
    'browser.defaultUrl': 'https://google.co.uk',
    'editor.fontFamily': 'Hack',
    "editor.cursorLine": false,
    "workspace.defaultWorkspace": "/home/norbert/Documents/Programming",
    "sidebar.marks.enabled": true,
    "sidebar.enabled": false,
    'oni.exclude': ['node_modules', '.git', 'tags'],
    'ui.animations.enabled': false,
    'ui.fontSmoothing': 'auto',
    'autoClosingPairs.default': [
        {
            open: '{',
            close: '}'
        },
        {
            open: '<',
            close: '>'
        },
        {
            open: '[',
            close: ']'
        },
        {
            open: '(',
            close: ')'
        },
    ]
};
