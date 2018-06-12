/**
 * File description.
 *
 * @format
 * @copyright 2018
 * @author nl253
 * @see {@link  details and explanation}
 * @requires
 */


class VNode {
  constructor(type) {
    this._type = type;
    this._cache = null;
  }

  get hasChanged() {
    return false;
  }

  get type() {
    return this._type;
  }

  get DOM() {

    if (this.type === 'text') {
      return this.changed ? document.createTextNode(this.text) : this._cache;

    } else if (this.hasChanged || this.children.some(vChild => vChild.hasChanged)) {

      if (this._changed.tag) return this._rerenderAll();

      for (const child of this.children.map(vChild => vChild.DOM)) {
        this._cache.appendChild(child);
      }

      if (this._changed.styles) Object.assign(this._cache.style, this.styles);

      if (this._changed.classes) {

        // clear all classes from DOM node
        for (const currentClass of this._cache.classList) {
          this._cache.classList.toggle(currentClass)
        }

        // add all classes from VNode
        for (const newClass of this.classes) {
          this._cache.classList.add(newClass);
        }
      }
      
      // reset
      this._changed.classes = false;
      this._changed.styles = false;

      return this._cache;

    } else return this._cache;
  }
}

class VText extends VNode {

  constructor(text = "") {
    super('text');
    this._text = text;
    this.hasChanged = false;
  }

  setText(newText) {
    this._text = newText;
    this.hasChanged = true;
    return this;
  }

  get text() {
    return this.text;
  }
}

class VTag extends VNode {
  constructor(tag = 'div', classes = [], styles = {}, children = []) {
    super('tag');
    this.tag = tag;
    this.classes = classes;
    this.styles = styles;
    this.children = children;
    this._changed = { classes: false, styles: false, tag: false };
  }

  get hasChanged() {
    return !this._changed.classes && !this._changed.styles && !this._changed.tag;
  }

  upClasses(newClasses) {
    this.classes.push(...newClasses);
    this._changed.classes.push;
    return this;
  }

  setClasses(newClasses) {
    this.classes = newClasses;
    return this;
  }

  addClass(newClass) {
    this.classes.push(newClass);
    return this;
  }

  rmClass(c) {
    for (let i = 0; i < this.classes.length; i++) {
      let className = this.classes[i];
      if (className === c) {
        this.classes.splice(i, i + 1);
        return this;
      }
    }
    this.classes.remove(c);
    return this;
  }

  upStyles(newStyles) {
    Object.assign(this.styles, newStyles);
    return this;
  }

  setStyles(newStyles) {
    this.styles = newStyles;
    return this;
  }

  upStyle(k, v) {
    this.styles[k] = v;
    return this;
  }

  _rerenderAll() {

    const $el = document.createElement(this.tag);

    for (const c of this.classes) {
      $el.classList.add(c);
    }

    Object.assign($el.style, this.styles);

    for (const child of this.children.map(vChild => vChild.DOM)) {
      $el.appendChild(child);
    }

    this._cache = $el;

    // reset
    this._changed.classes = false;
    this._changed.styles = false;
    this._changed.tag = false;

    return $el;

  }
}

// ---------------------------------------------------------------------

function mount(vRoot, atSelector = 'body', refreshRate = 1000) {
  if (vRoot.hasChanged) {
    document.querySelector(atSelector).appendChild(vRoot.DOM);
  }
  setTimeout(() => mount(vRoot, atSelector), refreshRate);
}

setTimeout(() => mount(new VNode()));

