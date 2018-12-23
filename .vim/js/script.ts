/** @format */

/**
 * NOTE: this is the main source file.
 *
 * A corresponding *.js file exists in this dir but it is *generated* by tsc so do not mind it.
 */

enum Theme {LIGHT, DARK}

enum TocState {// noinspection JSUnusedGlobalSymbols
  HIDDEN, VISIBLE
}

const DEFAULT_THEME: Theme = Theme.LIGHT;
const DEFAULT_TOC_STATE: TocState = TocState.HIDDEN;
const MATHJAX_TIMEOUT = 5000;

// this is what MathJax tells you to paste (do not touch)
const MATHJAX_CFG: object = {
  tex2jax: {
    inlineMath: [
      ['$', '$'], // make sure that things $between$ dollar signs are rendered
      ['\\(', '\\)'],
    ],
    processEscapes: true,
  },
};

const LINK_STYLES = {
  background: 'var(--main-bg-color)',
  border: 'none',
  fontStyle: 'italic',
};

function slugify(nonURLTxt: string): string {
  return nonURLTxt.replace(/ +/g, '-').replace(/\t/g, '_').replace(/\n/g, '_-_');
}

// noinspection TsLint
function isDefined(v: any): boolean {
  return v !== null && v !== undefined;
}

function changeSuffix(target: string, suffix: string, newSuffix: string): string {
  return target.replace(new RegExp(`${suffix}$`), newSuffix);
}

function changeExtension(target: string, ext: string, newExt: string): string {
  return changeSuffix(target, `.${ext}`, `.${newExt}`);
}

class Builder {
  private readonly tagName: string;
  private readonly _classes: string[];
  private readonly _attrs: object;
  private readonly _wrap: string[];
  private readonly _styles: object;
  private readonly _eventListeners: object;
  private _children: HTMLElement[];
  private _flagAttrs: string[];
  private _text: string;

  constructor(tagName: string) {
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

  // noinspection JSUnusedGlobalSymbols
  public classes(classes: string[]): Builder {
    for (const c of classes) {
      this._classes.push(c);
    }
    return this;
  }

  // noinspection JSUnusedGlobalSymbols
  public class(c: string): Builder {
    this._classes.push(c);
    return this;
  }

  public id(id: string): Builder {
    // @ts-ignore
    this._attrs.id = id;
    return this;
  }

  // noinspection TsLint
  public on(event: string, funct: (Builder?) => any): Builder {
    if (!this._eventListeners[event]) {
      this._eventListeners[event] = [];
    }
    this._eventListeners[event].push(funct);
    return this;
  }

  public style(key: string, value: string): Builder {
    this._styles[key] = value;
    return this;
  }

  public styles(dict: object): Builder {
    for (const key in dict) {
      this._styles[key] = dict[key];
    }
    return this;
  }

  public attr(key: string, value: string): Builder {
    if (value) this._attrs[key] = value;
    else this._flagAttrs.push(key);
    return this;
  }

  // noinspection JSUnusedGlobalSymbols
  public attrs(attrs: object | string[]): Builder {
    if (Object.getPrototypeOf(attrs).constructor.name.toLowerCase() === 'array') {
      // @ts-ignore
      this._flagAttrs = this._flagAttrs.concat(attrs);
    } else {
      for (const key in attrs) {
        this._attrs[key] = attrs[key];
      }
    }
    return this;
  }

  public text(text: string): Builder {
    this._text = text;
    return this;
  }

  // noinspection JSUnusedGlobalSymbols
  public append(text: string): Builder {
    this._text += text;
    return this;
  }

  // noinspection JSUnusedGlobalSymbols
  public children(children: HTMLElement[]): Builder {
    this._children = this._children.concat(children);
    return this;
  }

  // noinspection JSUnusedGlobalSymbols
  public wrap(beforeTag): this {
    this._wrap.push(beforeTag);
    return this;
  }

  // noinspection JSUnusedGlobalSymbols
  public child(child: HTMLElement): Builder {
    this._children.push(child);
    return this;
  }

  // noinspection TsLint
  public build(): HTMLElement {
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
function addDefinitionLinks(): void {
  const nodes: NodeListOf<HTMLElement> = document.querySelectorAll('dt, h2, h3, h4, h5, h6');
  for (const term of Array.from(nodes)) {
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
function unwrapImgs(): void {
  for (const img of Array.from(document.querySelectorAll('p>img[src]'))) {
    const parent = img.parentNode;
    const grandparent = parent.parentNode;
    grandparent.replaceChild(img, parent);
  }
}

/**
 * Add a single master button that hides all code blocks.
 */
function makeCodeHideBtns(): void {
  const nodes: NodeListOf<HTMLElement> = document.querySelectorAll('div.sourceCode');
  for (const codeNode of Array.from(nodes)) {
    // @ts-ignore
    codeNode.prepend(
      new Builder('a')
        .attr('href', '#!')
        .text('hide')
        .on('click', function hideParent() {
          this.parentNode.style.display = 'none';
        })
        .styles(LINK_STYLES)
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
        .build(),
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
function makeSectionToggleBtns(): void {
  for (const section of Array.from(document.querySelectorAll('section'))) {
    // @ts-ignore
    section.prepend(
      new Builder('a')
        .text('hide section')
        .styles(LINK_STYLES)
        .styles({
          display: 'block',
          fontSize: '0.9em',
          float: 'right',
          textAlign: 'right',
        })
        .attr('href', '#!')
        .on('click', function hideParent() {
          this.parentNode.remove();
          const toc = document.querySelector('#TOC');
          if (toc === null) return;
          const h2 = section.querySelector('h2');
          if (h2 === null) return;
          for (const li of toc.querySelectorAll('li')) {
            if (li.innerText === h2.innerText) {
              li.remove();
              break;
            }
          }
        })
        .build(),
    );
  }
}

/**
 * Does the actual hiding of code blocks (helper function - see below).
 */
function toggleCode(): void {
  const btn: HTMLElement = document.querySelector('#code-btn');
  const nodes: NodeListOf<HTMLElement> = document.querySelectorAll('pre.sourceCode');

  for (const x of Array.from(nodes)) {
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
function makeMasterCodeToggleBtn(): void {
  // no code to hide
  if (!document.querySelector('pre')) return;

  document.body.appendChild(
    new Builder('a')
      .on('click', toggleCode)
      .id('code-btn')
      .text('hide code')
      .styles(LINK_STYLES)
      .attr('href', '#!')
      .styles({
        display: 'block',
        position: 'fixed',
        right: '220px',
        top: '20px',
      })
      .build(),
  );
}

/**
 * Add a btn that removes all JavaScript extras created in this script.
 */
function makeDisbleJSBtn(): void {
  return document.body.insertAdjacentElement(
    'afterBegin',
    new Builder('a').text('clear')
    .style('position', 'fixed')
    .style('top', '55px')
    .style('right', '168px')
    .attr('href', '#!')
    .on('click', (event) => {
      event.preventDefault();
      for (const a of document.body.querySelectorAll('a[href^="#!"]')) {
        a.remove();
      }
    }).build());
}

/**
 * The actual night mode toggler (helper function - see below).
 */
function toggleNightMode(): void {
  const {body} = document;
  const btn: HTMLElement = document.querySelector('#night-mode-btn');
  if (body.classList.contains('night-mode')) {
    btn.innerText = 'turn night mode on';
    body.classList.remove('night-mode');
    for (const codeNode of Array.from(body.querySelectorAll('pre, code, kbd'))) {
      codeNode.classList.remove('night-mode-code');
    }
    console.info('night mode off');
  } else {
    btn.innerText = 'turn night mode off';
    body.classList.add('night-mode');
    for (const codeNode of Array.from(body.querySelectorAll('pre, code, kbd'))) {
      codeNode.classList.add('night-mode-code');
    }
    console.info('night mode on');
  }
}

/**
 * Add a button to toggle night mode.
 */
function makeNightModeBtn(): void {
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
      .styles(LINK_STYLES)
      .build(),
  );
}

/**
 * Add a button to toggle the table of contents (TOC).
 */
function makeTOCBtn(): void {
  const tryFindTOC: HTMLElement = document.querySelector('#TOC');

  if (tryFindTOC === null) return; 

  // TOC too short
  if (Array.from(tryFindTOC.querySelectorAll('li')).length <= 2) {
    return document.querySelector('#TOC').remove();
  }

  function toggleTOC(): void {
    const toc: HTMLElement = document.querySelector('#TOC');
    const tocBtn: HTMLElement = document.querySelector('#toc-btn');

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
      .styles(LINK_STYLES)
      .on('click', toggleTOC)
      .build(),
  );
}

function countWords(threshold = 10) {
const words = document.body.innerText.replace(/[^a-zA-Z0-9 \n]+/g, '').replace(/\s+/g, ' ').split(/\s/).filter(w => w.match(/^[a-zA-Z]{2,}$/));
  const counter = {};
  for (const w of words) {
    if (counter[w] === undefined) counter[w] = 1;
    else counter[w]++;
  }
  let bodyCpy = document.body.outerHTML; 
  for (const w of words) {
    bodyCpy = bodyCpy.replace(w, `<span style="color: red;">${w}</span>`);
  }
  console.log(counter);
  return counter;
}

/**
 * Add the `id` attribute to heading nodes.
 *
 * e.g.: <h1>Subject of Discussion</h1> becomes <h1 id="Subject_of_Discussion">Subject of Discussion</h1>
 */
function addIds() {
  const nodes: NodeListOf<HTMLElement> = document.querySelectorAll('h1, h2, h3, h4, h5, h6, strong');

  for (const heading of Array.from(nodes)) {
    heading.id = slugify(heading.innerText);
  }
}

/**
 * Replace all links to markdown files.
 *
 * e.g.: <a href="./my_notes.md">here</a> with links to converted html files
 */
function fixLinks(): void {

  // see <./file.md> => See <./file.html>
  const links: NodeListOf<HTMLElement> = document.querySelectorAll('a[href]');

  for (const link of Array.from(links)) {
    for (const badExt in ['md', 'ipynb']) {
      // @ts-ignore
      link.href = changeExtension(link.href, badExt, 'html');
    }
  }
}

/**
 * Vim-like and pager keybindings.
 * Like everything, might only work in Chrome because this is the only browser I tested it with.
 */
function overrideKeyboardShortcuts(): void {
  const moveAmount = 150;
  const scrollBehaviour = 'instant';
  // noinspection TsLint
  const scrollAmount = moveAmount * 3;
  // noinspection TsLint
  const pageAmount = moveAmount * 5;

  const move: (moveBy: number) => void = moveBy =>
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

  // noinspection JSUnusedGlobalSymbols
  const keymap = {
    G: botOfPg,
    d: scrollDown,
    g: topOfPg,
    h: pageUp,
    j: down,
    k: up,
    l: pageDown,
    u: scrollUp,
    x: window.close,
  };
  document.onkeydown = e => {
    for (const key in keymap) {
      if (key === e.key) {
        keymap[key]();
      }
    }
  };
}

// noinspection TsLint
/**
 * Call all functions.
 */
function main(): void {

  // my stuff
  addIds();
  fixLinks();
  makeTOCBtn();
  makeNightModeBtn();
  makeMasterCodeToggleBtn();
  makeCodeHideBtns();
  unwrapImgs();
  makeSectionToggleBtns();
  overrideKeyboardShortcuts();
  addDefinitionLinks();
  makeDisbleJSBtn();

  if (isDefined(MATHJAX_CFG)) {
    // @ts-ignore
    // noinspection JSUnresolvedVariable
    setTimeout(() => MathJax.Hub.Config(MATHJAX_CFG), MATHJAX_TIMEOUT);
  } else {
    console.error(`MATHJAX_CFG not defined`);

  }

  // hide toc
  if (isDefined(DEFAULT_TOC_STATE)) {
    if (DEFAULT_TOC_STATE === TocState.HIDDEN) {
      const tryFind = document.querySelector('#toc-btn');
      if (tryFind) {
        tryFind.dispatchEvent(new Event('click'));
      } else {
        console.warn('could not find the toc');
      }
    }
  } else {
    console.info('DEFAULT_TOC_STATE not set');
  }

  // turn on night mode
  if (isDefined(DEFAULT_THEME) && DEFAULT_THEME === Theme.DARK) {
    document.querySelector('#night-mode-btn').dispatchEvent(new Event('click'));
  } else {
    console.info('DEFAULT_THEME not set');
  }
}

// register
document.addEventListener('DOMContentLoaded', main);
