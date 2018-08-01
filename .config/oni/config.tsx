import * as React from 'react';
import * as Oni from 'oni-api';

export const activate = (oni: Oni.Plugin.Api) => {
  console.log('config activated');

  // Input
  //
  // Add input bindings here:
  //
  oni.input.bind('<c-enter>', () => console.log('Control+Enter was pressed'));

  //
  // Or remove the default bindings here by uncommenting the below line:
  //
  // oni.input.unbind("<c-p>")
};

export const deactivate = (oni: Oni.Plugin.Api) => {
  console.log('config deactivated');
};

export const configuration = {
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
      close: '}',
    },
    {
      open: '<',
      close: '>',
    },
    {
      open: '[',
      close: ']',
    },
    {
      open: '(',
      close: ')',
    },
  ],
};
