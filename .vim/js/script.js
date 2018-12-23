var Theme;
(function (Theme) {
    Theme[Theme["LIGHT"] = 0] = "LIGHT";
    Theme[Theme["DARK"] = 1] = "DARK";
})(Theme || (Theme = {}));
var TocState;
(function (TocState) {
    TocState[TocState["HIDDEN"] = 0] = "HIDDEN";
    TocState[TocState["VISIBLE"] = 1] = "VISIBLE";
})(TocState || (TocState = {}));
var DEFAULT_THEME = Theme.LIGHT;
var DEFAULT_TOC_STATE = TocState.HIDDEN;
var MATHJAX_TIMEOUT = 5000;
var MATHJAX_CFG = {
    tex2jax: {
        inlineMath: [
            ['$', '$'],
            ['\\(', '\\)'],
        ],
        processEscapes: true
    }
};
var LINK_STYLES = {
    background: 'var(--main-bg-color)',
    border: 'none',
    fontStyle: 'italic'
};
function slugify(nonURLTxt) {
    return nonURLTxt.replace(/ +/g, '-').replace(/\t/g, '_').replace(/\n/g, '_-_');
}
function isDefined(v) {
    return v !== null && v !== undefined;
}
function changeSuffix(target, suffix, newSuffix) {
    return target.replace(new RegExp(suffix + "$"), newSuffix);
}
function changeExtension(target, ext, newExt) {
    return changeSuffix(target, "." + ext, "." + newExt);
}
var Builder = (function () {
    function Builder(tagName) {
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
    Builder.prototype.classes = function (classes) {
        for (var _i = 0, classes_1 = classes; _i < classes_1.length; _i++) {
            var c = classes_1[_i];
            this._classes.push(c);
        }
        return this;
    };
    Builder.prototype["class"] = function (c) {
        this._classes.push(c);
        return this;
    };
    Builder.prototype.id = function (id) {
        this._attrs.id = id;
        return this;
    };
    Builder.prototype.on = function (event, funct) {
        if (!this._eventListeners[event]) {
            this._eventListeners[event] = [];
        }
        this._eventListeners[event].push(funct);
        return this;
    };
    Builder.prototype.style = function (key, value) {
        this._styles[key] = value;
        return this;
    };
    Builder.prototype.styles = function (dict) {
        for (var key in dict) {
            this._styles[key] = dict[key];
        }
        return this;
    };
    Builder.prototype.attr = function (key, value) {
        if (value)
            this._attrs[key] = value;
        else
            this._flagAttrs.push(key);
        return this;
    };
    Builder.prototype.attrs = function (attrs) {
        if (Object.getPrototypeOf(attrs).constructor.name.toLowerCase() === 'array') {
            this._flagAttrs = this._flagAttrs.concat(attrs);
        }
        else {
            for (var key in attrs) {
                this._attrs[key] = attrs[key];
            }
        }
        return this;
    };
    Builder.prototype.text = function (text) {
        this._text = text;
        return this;
    };
    Builder.prototype.append = function (text) {
        this._text += text;
        return this;
    };
    Builder.prototype.children = function (children) {
        this._children = this._children.concat(children);
        return this;
    };
    Builder.prototype.wrap = function (beforeTag) {
        this._wrap.push(beforeTag);
        return this;
    };
    Builder.prototype.child = function (child) {
        this._children.push(child);
        return this;
    };
    Builder.prototype.build = function () {
        var node = document.createElement(this.tagName);
        node.innerText = this._text;
        for (var key in this._attrs) {
            node.setAttribute(key, this._attrs[key]);
        }
        for (var _i = 0, _a = this._flagAttrs; _i < _a.length; _i++) {
            var attr = _a[_i];
            node.setAttribute(attr, '');
        }
        for (var _b = 0, _c = this._children; _b < _c.length; _b++) {
            var child = _c[_b];
            node.appendChild(child);
        }
        for (var _d = 0, _e = this._classes; _d < _e.length; _d++) {
            var c = _e[_d];
            node.classList.add(c);
        }
        for (var key in this._styles) {
            node.style[key] = this._styles[key];
        }
        for (var event_1 in this._eventListeners) {
            for (var _f = 0, _g = this._eventListeners[event_1]; _f < _g.length; _f++) {
                var callback = _g[_f];
                node.addEventListener(event_1, callback);
            }
        }
        for (var _h = 0, _j = this._wrap; _h < _j.length; _h++) {
            var wrapper = _j[_h];
            var wrapperNode = document.createElement(wrapper);
            wrapperNode.appendChild(node);
            node = wrapperNode;
        }
        return node;
    };
    return Builder;
}());
function addDefinitionLinks() {
    var nodes = document.querySelectorAll('dt, h2, h3, h4, h5, h6');
    var _loop_1 = function (term) {
        term.classList.add('seen');
        term.id = "term-" + slugify(term.innerText);
        var regex = new RegExp(" (" + term.innerText + ") ", 'i');
        var link = new Builder('a')
            .text('$1')
            .attr('href', "#" + term.id)
            .on('click', function () { return term.scrollIntoView(); })
            .build();
        for (var _i = 0, _a = Array.from(document.querySelectorAll('p, div, li')); _i < _a.length; _i++) {
            var node = _a[_i];
            node.outerHTML = node.outerHTML.replace(regex, " " + link.outerHTML + " ");
        }
    };
    for (var _i = 0, _a = Array.from(nodes); _i < _a.length; _i++) {
        var term = _a[_i];
        _loop_1(term);
    }
}
function unwrapImgs() {
    for (var _i = 0, _a = Array.from(document.querySelectorAll('p>img[src]')); _i < _a.length; _i++) {
        var img = _a[_i];
        var parent_1 = img.parentNode;
        var grandparent = parent_1.parentNode;
        grandparent.replaceChild(img, parent_1);
    }
}
function makeCodeHideBtns() {
    var nodes = document.querySelectorAll('div.sourceCode');
    for (var _i = 0, _a = Array.from(nodes); _i < _a.length; _i++) {
        var codeNode = _a[_i];
        codeNode.prepend(new Builder('a')
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
            top: '-1px'
        })
            .build());
    }
}
function makeSectionToggleBtns() {
    var _loop_2 = function (section) {
        section.prepend(new Builder('a')
            .text('hide section')
            .styles(LINK_STYLES)
            .styles({
            display: 'block',
            fontSize: '0.9em',
            float: 'right',
            textAlign: 'right'
        })
            .attr('href', '#!')
            .on('click', function hideParent() {
            this.parentNode.remove();
            var toc = document.querySelector('#TOC');
            if (toc === null)
                return;
            var h2 = section.querySelector('h2');
            if (h2 === null)
                return;
            for (var _i = 0, _a = toc.querySelectorAll('li'); _i < _a.length; _i++) {
                var li = _a[_i];
                if (li.innerText === h2.innerText) {
                    li.remove();
                    break;
                }
            }
        })
            .build());
    };
    for (var _i = 0, _a = Array.from(document.querySelectorAll('section')); _i < _a.length; _i++) {
        var section = _a[_i];
        _loop_2(section);
    }
}
function toggleCode() {
    var btn = document.querySelector('#code-btn');
    var nodes = document.querySelectorAll('pre.sourceCode');
    for (var _i = 0, _a = Array.from(nodes); _i < _a.length; _i++) {
        var x = _a[_i];
        if (x.style.display === 'none') {
            x.style.display = 'block';
            btn.innerText = 'hide code';
        }
        else {
            x.style.display = 'none';
            btn.innerText = 'show code';
        }
    }
}
function makeMasterCodeToggleBtn() {
    if (!document.querySelector('pre'))
        return;
    document.body.appendChild(new Builder('a')
        .on('click', toggleCode)
        .id('code-btn')
        .text('hide code')
        .styles(LINK_STYLES)
        .attr('href', '#!')
        .styles({
        display: 'block',
        position: 'fixed',
        right: '220px',
        top: '20px'
    })
        .build());
}
function makeDisbleJSBtn() {
    return document.body.insertAdjacentElement('afterBegin', new Builder('a').text('clear')
        .style('position', 'fixed')
        .style('top', '55px')
        .style('right', '168px')
        .attr('href', '#!')
        .on('click', function (event) {
        event.preventDefault();
        for (var _i = 0, _a = document.body.querySelectorAll('a[href^="#!"]'); _i < _a.length; _i++) {
            var a = _a[_i];
            a.remove();
        }
    }).build());
}
function toggleNightMode() {
    var body = document.body;
    var btn = document.querySelector('#night-mode-btn');
    if (body.classList.contains('night-mode')) {
        btn.innerText = 'turn night mode on';
        body.classList.remove('night-mode');
        for (var _i = 0, _a = Array.from(body.querySelectorAll('pre, code, kbd')); _i < _a.length; _i++) {
            var codeNode = _a[_i];
            codeNode.classList.remove('night-mode-code');
        }
        console.info('night mode off');
    }
    else {
        btn.innerText = 'turn night mode off';
        body.classList.add('night-mode');
        for (var _b = 0, _c = Array.from(body.querySelectorAll('pre, code, kbd')); _b < _c.length; _b++) {
            var codeNode = _c[_b];
            codeNode.classList.add('night-mode-code');
        }
        console.info('night mode on');
    }
}
function makeNightModeBtn() {
    document.body.appendChild(new Builder('a')
        .id('night-mode-btn')
        .attr('href', '#!')
        .text('turn night mode on')
        .on('click', toggleNightMode)
        .styles({
        position: 'fixed',
        right: '50px',
        top: '20px'
    })
        .styles(LINK_STYLES)
        .build());
}
function makeTOCBtn() {
    var tryFindTOC = document.querySelector('#TOC');
    if (tryFindTOC === null)
        return;
    if (Array.from(tryFindTOC.querySelectorAll('li')).length <= 2) {
        return document.querySelector('#TOC').remove();
    }
    function toggleTOC() {
        var toc = document.querySelector('#TOC');
        var tocBtn = document.querySelector('#toc-btn');
        if (toc.style.display === 'none') {
            toc.style.display = 'block';
            tocBtn.innerText = 'hide toc';
        }
        else {
            toc.style.display = 'none';
            tocBtn.innerText = 'show toc';
        }
    }
    document.body.appendChild(new Builder('a')
        .id('toc-btn')
        .text('hide toc')
        .attr('href', '#!')
        .styles({
        left: '50px',
        position: 'fixed',
        top: '20px'
    })
        .styles(LINK_STYLES)
        .on('click', toggleTOC)
        .build());
}
function addIds() {
    var nodes = document.querySelectorAll('h1, h2, h3, h4, h5, h6, strong');
    for (var _i = 0, _a = Array.from(nodes); _i < _a.length; _i++) {
        var heading = _a[_i];
        heading.id = slugify(heading.innerText);
    }
}
function fixLinks() {
    var links = document.querySelectorAll('a[href]');
    for (var _i = 0, _a = Array.from(links); _i < _a.length; _i++) {
        var link = _a[_i];
        for (var badExt in ['md', 'ipynb']) {
            link.href = changeExtension(link.href, badExt, 'html');
        }
    }
}
function overrideKeyboardShortcuts() {
    var moveAmount = 150;
    var scrollBehaviour = 'instant';
    var scrollAmount = moveAmount * 3;
    var pageAmount = moveAmount * 5;
    var move = function (moveBy) {
        return window.scroll({
            behavior: scrollBehaviour,
            top: moveBy
        });
    };
    var up = function () { return move(window.scrollY - moveAmount); };
    var down = function () { return move(window.scrollY + moveAmount); };
    var scrollUp = function () { return move(window.scrollY - scrollAmount); };
    var scrollDown = function () { return move(window.scrollY + scrollAmount); };
    var pageUp = function () { return move(window.scrollY - pageAmount); };
    var pageDown = function () { return move(window.scrollY + pageAmount); };
    var topOfPg = function () { return window.scrollTo(0, -document.body.scrollHeight); };
    var botOfPg = function () { return window.scrollTo(0, document.body.scrollHeight); };
    var keymap = {
        G: botOfPg,
        d: scrollDown,
        g: topOfPg,
        h: pageUp,
        j: down,
        k: up,
        l: pageDown,
        u: scrollUp,
        x: window.close
    };
    document.onkeydown = function (e) {
        for (var key in keymap) {
            if (key === e.key) {
                keymap[key]();
            }
        }
    };
}
function main() {
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
        setTimeout(function () { return MathJax.Hub.Config(MATHJAX_CFG); }, MATHJAX_TIMEOUT);
    }
    else {
        console.error("MATHJAX_CFG not defined");
    }
    if (isDefined(DEFAULT_TOC_STATE)) {
        if (DEFAULT_TOC_STATE === TocState.HIDDEN) {
            var tryFind = document.querySelector('#toc-btn');
            if (tryFind) {
                tryFind.dispatchEvent(new Event('click'));
            }
            else {
                console.warn('could not find the toc');
            }
        }
    }
    else {
        console.info('DEFAULT_TOC_STATE not set');
    }
    if (isDefined(DEFAULT_THEME) && DEFAULT_THEME === Theme.DARK) {
        document.querySelector('#night-mode-btn').dispatchEvent(new Event('click'));
    }
    else {
        console.info('DEFAULT_THEME not set');
    }
}
document.addEventListener('DOMContentLoaded', main);
//# sourceMappingURL=script.js.map