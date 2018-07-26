// vim:ft=javascript.typescript:

/** @format */

function getLinkStyles() {
  return {
    background: 'var(--main-bg-color)',
    border: 'none',
    fontStyle: 'italic',
  };
}

function slugify(nonURLTxt) {
  return nonURLTxt.replace(/ +/g, '-').replace(/\t/g, '_');
}

class Builder {
  private tagName: string;
  private _text: string;
  private _classes: string[];
  private _attrs: Object;
  private _flagAttrs: string[];
  private _wrap: string[];
  private _styles: Object;
  private _children: Element[];
  private _eventListeners: Object;

  constructor(tagName) {
    this.tagName = tagName;
    this._children = [];
    this._styles = {};
    this._wrap = [];
    this._flagAttrs = [];
    this._attrs = {};
    this._text = '';
    this._classes = [];
    this._eventListeners = {};
  }

  public classes(classes) {
    for (const c of classes) {
      this._classes.push(c);
    }
    return this;
  }

  public class(c) {
    this._classes.push(c);
    return this;
  }

  public id(id) {
    this._attrs.id = id;
    return this;
  }

  public on(event, funct) {
    if (!this._eventListeners[event]) {
      this._eventListeners[event] = [];
    }
    this._eventListeners[event].push(funct);
    return this;
  }

  public style(k, v) {
    this._styles[k] = v;
    return this;
  }

  public styles(dict) {
    for (const key in dict) {
      this._styles[key] = dict[key];
    }
    return this;
  }

  public attr(k, v) {
    if (v) this._attrs[k] = v;
    else this._flagAttrs.push(k);
    return this;
  }

  public attrs(attrs) {
    if (Object.getPrototypeOf(attrs).constructor.name.toLowerCase() === 'array') {
      this._flagAttrs = this._flagAttrs.concat(attrs);
    } else {
      for (const key in attrs) {
        this._attrs[key] = attrs[key];
      }
    }
    return this;
  }

  public text(text) {
    this._text = text;
    return this;
  }

  public append(text) {
    this._text += text;
    return this;
  }

  public children(children) {
    this._children = this._children.concat(children);
    return this;
  }

  public wrap(beforeTag) {
    this._wrap.push(beforeTag);
    return this;
  }

  public child(child) {
    this._children.push(child);
    return this;
  }

  public build() {
    let node = document.createElement(this.tagName);

    node.innerText = this._text;

    for (const key in this._attrs) {
      node.setAttribute(key, this._attrs[key]);
    }

    for (const attr of this._flagAttrs) {
      node.setAttribute(attr, '');
    }

    for (const child of this._children) {
      node.appendChild(child);
    }

    for (const c of this._classes) {
      node.classList.add(c);
    }

    for (const key in this._styles) {
      node.style[key] = this._styles[key];
    }

    for (const event in this._eventListeners) {
      for (const callback of this._eventListeners[event]) {
        node.addEventListener(event, callback);
      }
    }

    for (const wrapper of this._wrap) {
      const wrapperNode = document.createElement(wrapper);
      wrapperNode.appendChild(node);
      node = wrapperNode;
    }

    return node;
  }
}

/**
 * All mentions of defined terms are turned into links.
 */
function addDefinitionLinks() {
  for (const term of Array.from(document.querySelectorAll('dt, h2, h3, h4, h5, h6'))) {
    term.classList.add('seen');
    term.id = `term-${slugify(term.innerText)}`;
    const regex = new RegExp(` (${term.innerText}) `, 'i');

    const link = new Builder('a')
      .text('$1')
      .attr('href', `#${term.id}`)
      .on('click', () => term.scrollIntoView())
      .build();

    // document.body.outerHTML = document.body.outerHTML.replace(regex,` ${link.outerHTML} `);
    for (const node of Array.from(document.querySelectorAll('p, div, li'))) {
      node.outerHTML = node.outerHTML.replace(regex, ` ${link.outerHTML} `);
    }
  }
}

/**
 * Hack because pandoc wraps (for some reason) <img> in <p>.
 */
function unwrapImgs() {
  for (const img of Array.from(document.querySelectorAll('p>img[src]'))) {
    const parent = img.parentNode;
    const grandparent = parent.parentNode;
    grandparent.replaceChild(img, parent);
  }
}

/**
 * Add a single master button that hides all code blocks.
 */
function makeCodeHideBtns() {
  for (const codeNode of Array.from(document.querySelectorAll('div.sourceCode'))) {
    codeNode.prepend(
      new Builder('a')
      .attr('href', '#!')
      .text('hide')
      .on('click', function hideParent() {
        this.parentNode.style.display = 'none';
      })
      .styles(getLinkStyles())
      .styles({
        background: 'none',
        float: 'right',
        margin: '0 auto',
        marginBottom: '-30px',
        padding: '11px 10px 0px 0px',
        position: 'relative',
        right: '12px',
        textAlign: 'right',
        top: '-1px',
      })
      .build()
    );

    /* FIXME copying to clipboard doesn't work on my version of chrome */
    // noinspection JSUnresolvedFunction
    // codeNode.prepend(
      // new Builder('a')
        // .id('btn-code-copy')
        // .text('copy')
        // .attr('href', '#!')
        // .styles(getLinkStyles())
        // .styles({
          // display: 'block',
          // margin: '0 auto -25px auto',
          // position: 'relative',
          // right: '100px',
          // textAlign: 'right',
        // })
        // .build()
    // );
  }
}

/**
 * Add a button to every section that hides it.
 */
function makeSectionToggleBtns() {
  for (const section of Array.from(document.querySelectorAll('section'))) {
    section.prepend(
      new Builder('a')
        .text('hide section')
        .styles(getLinkStyles())
        .styles({
          display: 'block',
          float: 'right',
          textAlign: 'right',
        })
        .attr('href', '#!')
        .on('click', function hideParent() {
          this.parentNode.style.display = 'none';
        })
        .build()
    );
  }
}

/**
 * Does the actual hiding of code blocks (helper function - see below).
 */
function toggleCode() {
  const btn = document.querySelector('#code-btn');

  for (const x of Array.from(document.querySelectorAll('pre.sourceCode'))) {
    if (x.style.display === 'none') {
      x.style.display = 'block';
      btn.innerText = 'hide code';
    } else {
      x.style.display = 'none';
      btn.innerText = 'show code';
    }
  }
}

/**
 * Add button to each code block to hide it.
 */
function makeMasterCodeToggleBtn() {
  // no code to hide
  if (!document.querySelector('pre')) return;

  document.body.appendChild(
    new Builder('a')
      .on('click', toggleCode)
      .id('code-btn')
      .text('hide code')
      .styles(getLinkStyles())
      .attr('href', '#!')
      .styles({
        display: 'block',
        position: 'fixed',
        right: '220px',
        top: '20px',
      })
      .build()
  );
}

/**
 * The actual night mode toggler (helper function - see below).
 */
function toggleNightMode() {
  const { body } = document;
  const btn = document.querySelector('#night-mode-btn');
  if (!body.classList.contains('night-mode')) {
    btn.innerText = 'turn night mode off';
    body.classList.add('night-mode');
    for (const codeNode of Array.from(body.querySelectorAll('pre, code, kbd'))) {
      codeNode.classList.add('night-mode-code');
    }
    console.info('night mode on');
  } else {
    btn.innerText = 'turn night mode on';
    body.classList.remove('night-mode');
    for (const codeNode of Array.from(body.querySelectorAll('pre, code, kbd'))) {
      codeNode.classList.remove('night-mode-code');
    }
    console.info('night mode off');
  }
}

/**
 * Add a button to toggle night mode.
 */
function makeNightModeBtn() {
  document.body.appendChild(
    new Builder('a')
      .id('night-mode-btn')
      .attr('href', '#!')
      .text('turn night mode on')
      .on('click', toggleNightMode)
      .styles({
        position: 'fixed',
        right: '50px',
        top: '20px',
      })
      .styles(getLinkStyles())
      .build()
  );
}

/**
 * Add a button to toggle the table of contents (TOC).
 */
function makeTOCBtn() {
  // tOC too short
  const tryFindTOC = document.querySelector('#TOC > ul');
  if (tryFindTOC.childElementCount <= 1) {
    document.querySelector('#TOC').remove();
    return;
  }

  function toggleTOC() {
    const toc = document.querySelector('#TOC');
    const tocBtn = document.querySelector('#toc-btn');

    // when hidden
    if (toc.style.display === 'none') {
      toc.style.display = 'block';
      tocBtn.innerText = 'hide toc';

      // when visible
    } else {
      toc.style.display = 'none';
      tocBtn.innerText = 'show toc';
    }
  }

  document.body.appendChild(
    new Builder('a')
      .id('toc-btn')
      .text('hide toc')
      .attr('href', '#!')
      .styles({
        left: '50px',
        position: 'fixed',
        top: '20px',
      })
      .styles(getLinkStyles())
      .on('click', toggleTOC)
      .build()
  );
}

/**
 * Replace all links to markdown files.
 *
 * e.g.: <a href="./my_notes.md">here</a> with links to converted html files
 */
function fixLinks() {
  // make URL_FRIENDLY
  for (const heading of Array.from(document.querySelectorAll('h1, h2, h3, h4, h5, h6, strong'))) {
    /*
     * replace all spaces with '-'
     * noinspection JSUndefinedPropertyAssignment
     */

    heading.id = slugify(heading.innerText);
  }

  const changeSuffix = (target, suffix, newSuffix) => target.replace(new RegExp(`${suffix}$`), newSuffix);

  // see <./file.md> => See <./file.html>
  for (const link of Array.from(document.querySelectorAll('a[href]'))) {
    link.href = changeSuffix(link.href, '.md', '.html');
    link.href = changeSuffix(link.href, '.ipynb', '.html');
  }
}

/**
 * Vim-like and pager keybindings.
 * Like everything, might only work in Chrome because this is the only browser I tested it with.
 */
function overrideKeyboardShortcuts() {
  const moveAmount = 150;
  const scrollBehaviour = 'instant';
  const scrollAmount = moveAmount * 3;
  const pageAmount = moveAmount * 5;

  const move = moveBy =>
    window.scroll({
      behavior: scrollBehaviour,
      top: moveBy,
    });

  const up = () => move(window.scrollY - moveAmount);
  const down = () => move(window.scrollY + moveAmount);
  const scrollUp = () => move(window.scrollY - scrollAmount);
  const scrollDown = () => move(window.scrollY + scrollAmount);
  const pageUp = () => move(window.scrollY - pageAmount);
  const pageDown = () => move(window.scrollY + pageAmount);
  const topOfPg = () => window.scrollTo(0, -document.body.scrollHeight);
  const botOfPg = () => window.scrollTo(0, document.body.scrollHeight);

  const keymap = {
    d: scrollDown,
    g: topOfPg,
    G: botOfPg,
    h: pageUp,
    j: down,
    k: up,
    l: pageDown,
    u: scrollUp,
    x: window.close,
  };
  document.onkeydown = function captureKey(e) {
    for (const key in keymap) {
      if (key === e.key) {
        keymap[key]();
      }
    }
  };
}

// call all functions
document.addEventListener('DOMContentLoaded', () => {
  // my stuff
  fixLinks();
  makeTOCBtn();
  makeNightModeBtn();
  makeMasterCodeToggleBtn();
  makeCodeHideBtns();
  unwrapImgs();
  makeSectionToggleBtns();
  overrideKeyboardShortcuts();
  addDefinitionLinks();

  setTimeout(() => {
    // this is what MathJax tells you to paste (do not touch)

    // noinspection JSUnresolvedVariable
    MathJax.Hub.Config({
      tex2jax: {
        inlineMath: [
          ['$', '$'], // make sure that things $between$ dollar signs are rendered
          ['\\(', '\\)'],
        ],
        processEscapes: true,
      },
    });
  }, 5000);

  // hide toc
  if (document.querySelector('#toc-btn')) {
    document.querySelector('#toc-btn').dispatchEvent(new Event('click'));
  }

  // turn on night mode
  document.querySelector('#night-mode-btn').dispatchEvent(new Event('click'));
});
