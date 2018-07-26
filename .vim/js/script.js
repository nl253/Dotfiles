function getLinkStyles() {
    return {
        background: 'var(--main-bg-color)',
        border: 'none',
        fontStyle: 'italic'
    };
}
function slugify(nonURLTxt) {
    return nonURLTxt.replace(/ +/g, '-').replace(/\t/g, '_');
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
    Builder.prototype.style = function (k, v) {
        this._styles[k] = v;
        return this;
    };
    Builder.prototype.styles = function (dict) {
        for (var key in dict) {
            this._styles[key] = dict[key];
        }
        return this;
    };
    Builder.prototype.attr = function (k, v) {
        if (v)
            this._attrs[k] = v;
        else
            this._flagAttrs.push(k);
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
    for (var _i = 0, _a = Array.from(document.querySelectorAll('dt, h2, h3, h4, h5, h6')); _i < _a.length; _i++) {
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
    for (var _i = 0, _a = Array.from(document.querySelectorAll('div.sourceCode')); _i < _a.length; _i++) {
        var codeNode = _a[_i];
        codeNode.prepend(new Builder('a')
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
            top: '-1px'
        })
            .build());
    }
}
function makeSectionToggleBtns() {
    for (var _i = 0, _a = Array.from(document.querySelectorAll('section')); _i < _a.length; _i++) {
        var section = _a[_i];
        section.prepend(new Builder('a')
            .text('hide section')
            .styles(getLinkStyles())
            .styles({
            display: 'block',
            float: 'right',
            textAlign: 'right'
        })
            .attr('href', '#!')
            .on('click', function hideParent() {
            this.parentNode.style.display = 'none';
        })
            .build());
    }
}
function toggleCode() {
    var btn = document.querySelector('#code-btn');
    for (var _i = 0, _a = Array.from(document.querySelectorAll('pre.sourceCode')); _i < _a.length; _i++) {
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
        .styles(getLinkStyles())
        .attr('href', '#!')
        .styles({
        display: 'block',
        position: 'fixed',
        right: '220px',
        top: '20px'
    })
        .build());
}
function toggleNightMode() {
    var body = document.body;
    var btn = document.querySelector('#night-mode-btn');
    if (!body.classList.contains('night-mode')) {
        btn.innerText = 'turn night mode off';
        body.classList.add('night-mode');
        for (var _i = 0, _a = Array.from(body.querySelectorAll('pre, code, kbd')); _i < _a.length; _i++) {
            var codeNode = _a[_i];
            codeNode.classList.add('night-mode-code');
        }
        console.info('night mode on');
    }
    else {
        btn.innerText = 'turn night mode on';
        body.classList.remove('night-mode');
        for (var _b = 0, _c = Array.from(body.querySelectorAll('pre, code, kbd')); _b < _c.length; _b++) {
            var codeNode = _c[_b];
            codeNode.classList.remove('night-mode-code');
        }
        console.info('night mode off');
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
        .styles(getLinkStyles())
        .build());
}
function makeTOCBtn() {
    var tryFindTOC = document.querySelector('#TOC > ul');
    if (tryFindTOC.childElementCount <= 1) {
        document.querySelector('#TOC').remove();
        return;
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
        .styles(getLinkStyles())
        .on('click', toggleTOC)
        .build());
}
function fixLinks() {
    for (var _i = 0, _a = Array.from(document.querySelectorAll('h1, h2, h3, h4, h5, h6, strong')); _i < _a.length; _i++) {
        var heading = _a[_i];
        heading.id = slugify(heading.innerText);
    }
    var changeSuffix = function (target, suffix, newSuffix) { return target.replace(new RegExp(suffix + "$"), newSuffix); };
    for (var _b = 0, _c = Array.from(document.querySelectorAll('a[href]')); _b < _c.length; _b++) {
        var link = _c[_b];
        link.href = changeSuffix(link.href, '.md', '.html');
        link.href = changeSuffix(link.href, '.ipynb', '.html');
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
        d: scrollDown,
        g: topOfPg,
        G: botOfPg,
        h: pageUp,
        j: down,
        k: up,
        l: pageDown,
        u: scrollUp,
        x: window.close
    };
    document.onkeydown = function captureKey(e) {
        for (var key in keymap) {
            if (key === e.key) {
                keymap[key]();
            }
        }
    };
}
document.addEventListener('DOMContentLoaded', function () {
    fixLinks();
    makeTOCBtn();
    makeNightModeBtn();
    makeMasterCodeToggleBtn();
    makeCodeHideBtns();
    unwrapImgs();
    makeSectionToggleBtns();
    overrideKeyboardShortcuts();
    addDefinitionLinks();
    setTimeout(function () {
        MathJax.Hub.Config({
            tex2jax: {
                inlineMath: [
                    ['$', '$'],
                    ['\\(', '\\)'],
                ],
                processEscapes: true
            }
        });
    }, 5000);
    if (document.querySelector('#toc-btn')) {
        document.querySelector('#toc-btn').dispatchEvent(new Event('click'));
    }
    document.querySelector('#night-mode-btn').dispatchEvent(new Event('click'));
});
