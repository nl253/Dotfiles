" sy match cssFunction "\v<var\("
" sy match cssFunction ")"
" hi link cssFunctionName Function
" hi link cssFunction Function
" hi link cssFunction Function
" sy match cssVar "\v--[a-z]+(-[a-z]+)*"
" hi link cssVar Constant

" From <https://github.com/hail2u/vim-css3-syntax>

syn keyword cssFontProp contained isolation
syn match cssFontProp contained "\<\(mix\|background\)-blend-mode\>"
syn keyword cssFontAttr contained multiply screen overlay darken lighten color-dodge color-burn hard-light soft-light difference exclusion hue saturation color luminosity
syn match cssBoxProp contained "\<overflow-\(style\|x\|y\)\>"
syn match cssBoxProp contained "\<rotation\(-point\)\=\>"
syn keyword cssBoxAttr contained scrollbar panner marquee
syn match cssBoxAttr contained "\<ruby\(-\(base\(-group\)\=\|text\(-group\)\=\)\)\=\>"
syn match cssBoxAttr contained "\<no-\(display\|content\)\>"
syn region cssInclude start=/@supports\>/ end=/\ze{/ skipwhite skipnl contains=css.*Prop,css.*Attr,cssValueInteger,cssValueLength,cssMediaKeyword,cssVendor,cssIncludeKeyword,cssComment nextgroup=cssMediaBlock
syn match cssFontProp contained "\<wrap-\(flow\|through\)\>"
syn keyword cssFontAttr contained minimum maximum
syn match cssFontProp contained "\<object-\(fit\|position\)\>"
syn match cssFontProp contained "\<image-orientation\>"
syn keyword cssFontAttr contained contain cover snap
syn match cssFontAttr contained "\<from-image\>"
syn match cssFontAttr contained "\<scale-down\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(\(repeating-\)\=\(linear\|radial\)-gradient\)\s*(" end=")" oneline keepend
syn keyword cssFontAttr contained recto verso
syn match cssFontAttr contained "\<\(avoid-\)\=page\>"
syn match cssAuralProp contained "\<voice-\(volume\|balance\|rate\|pitch\|range\|stress\|duration\)\>"
syn match cssAuralProp contained "\<rest\(-\(before\|after\)\)\=\>"
syn keyword cssAuralAttr contained young old neutral preserve moderate reduced
syn match cssAuralAttr contained "\<\(literal\|no\)-punctuation\>"
syn match cssAuralAttr contained "\<\(x-\)\=\(weak\|strong\)\>"
syn match cssValueNumber contained "[-+]\=\d\+\(dB\|st\)"
syn match cssFontProp contained "\<\(justify\|align\|place\)-\(self\|content\|items\)\>"
syn match cssFontProp contained "\<\(\(row\|column\)-\)\=gap\>"
syn keyword cssFontAttr contained safe unsafe legacy
syn match cssFontAttr contained "\<\(self\|flex\)-\(start\|end\)\>"
syn match cssFontAttr contained "\<space-\(between\|around\|evenly\)\>"
syn match cssFontProp contained "\<animation\(-\(name\|duration\|timing-function\|iteration-count\|direction\|play-state\|delay\|fill-mode\)\)\=\>"
syn keyword cssFontAttr contained forwards backwards running paused
syn match cssFontAttr contained "\<alternate-reverse\>"
syn match cssFontProp contained "\<background-\(clip\|origin\|size\)\>"
syn match cssFontProp contained "\<border-image\(-\(source\|slice\|width\|outset\|repeat\)\)\=\>"
syn match cssFontProp contained "\<border-\(\(top-right\|bottom-right\|bottom-left\|top-left\)-\)\=radius\>"
syn match cssFontProp contained "\<box-shadow\>"
syn keyword cssFontAttr contained space round local fill stretch clone slice
syn match cssFontAttr contained "\<\(padding\|border\|content\)-box\>"
syn keyword cssFontProp contained corners
syn match cssFontProp contained "\<background-position-\(x\|y\|inline\|block\)\>"
syn match cssFontProp contained "\<corner-shape\>"
syn match cssFontProp contained "\<border-limit\>"
syn match cssFontProp contained "\<border-clip\(-\(top\|right\|bottom\|left\)\)\=\>"
syn keyword cssFontAttr contained bevel scoop notch
syn match cssFontAttr contained "\<\(x\|y\)-\(start\|end\)\>"
syn match cssFontProp contained "\<break-\(after\|before\|inside\)\>"
syn match cssFontProp contained "\<box-decoration-break\>"
syn match cssFontAttr contained "\<\(avoid-\)\=column\>"
syn keyword cssFontProp all
syn keyword cssCommonAttr contained initial unset
syn keyword cssCommonAttr contained revert
syn region cssURL contained matchgroup=cssFunctionName start="\<supports\s*(" end=")" oneline keepend
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(rgba\|hsla\=\)\s*(" end=")" oneline keepend
syn keyword cssColorProp contained opacity
syn match cssColor contained "\<currentcolor\>"
syn match cssColorProp contained "\<color-adjust\>"
syn keyword cssColor contained rebeccapurple
syn match cssColor contained "#[0-9A-Fa-f]\{8\}\>" contains=cssUnitDecorators
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(hwb\|lab\|lch\|gray\|color\|device-cmyk\|color-mod\=\)\s*(" end=")" oneline keepend
syn match cssFontDescriptor "@color-profile\>" nextgroup=cssFontDescriptorBlock skipwhite skipnl
syn keyword cssFontProp contained contain
syn keyword cssFontAttr contained layout paint size
syn match cssGeneratedContentProp contained "\<string-set\>"
syn match cssGeneratedContentProp contained "\<bookmark-\(label\|level\|state\)\>"
syn keyword cssGeneratedContentAttr contained open closed
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(leader\|string\|target-\(counter\|counters\|text\)\)\s*(" end=")" oneline keepend
syn region cssInclude start=/@counter-style\>/ end=/\ze{/ skipwhite skipnl contains=css.*Prop,css.*Attr,cssValueInteger,cssValueLength,cssMediaKeyword,cssVendor,cssIncludeKeyword,cssComment nextgroup=cssMediaBlock
syn keyword cssGeneratedContentProp contained system negative prefix suffix range pad fallback
syn match cssGeneratedContentProp contained "\<\(additive-\)\=symbols\>"
syn match cssGeneratedContentProp contained "\<speak-as\>"
syn keyword cssGeneratedContentAttr contained cyclic symbolic additive extends bullets numbers words bengali cambodian khmer devanagari gujarati gurmukhi kannada lao malayalam mongolian myanmar oriya persian tamil telugu thai tibetan
syn match cssGeneratedContentAttr contained "\<\(ethiopic-\)\=numeric\>"
syn match cssGeneratedContentAttr contained "\<arabic-indic\>"
syn match cssGeneratedContentAttr contained "\<\(upper\|lower\)-armenian\>"
syn match cssGeneratedContentAttr contained "\<cjk-\(decimal\|earthly-branch\|heavenly-stem\)\>"
syn match cssGeneratedContentAttr contained "\<disclosure-\(open\|closed\)\>"
syn match cssGeneratedContentAttr contained "\<\(japanese\|korean-hanja\|\(simp\|trad\)-chinese\)-\(in\)\=formal\>"
syn match cssGeneratedContentAttr contained "\<korean-hangul-formal\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<symbols\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<\(min\|max\|user\)-zoom\>"
syn region cssInclude start=/@viewport\>/ end=/\ze{/ skipwhite skipnl contains=css.*Prop,css.*Attr,cssValueInteger,cssValueLength,cssMediaKeyword,cssVendor,cssIncludeKeyword,cssComment nextgroup=cssMediaBlock
syn keyword cssFontAttr contained contents discard
syn match cssFontAttr contained "\<flow\(-root\)\=\>"
syn keyword cssFontProp order
syn match cssFontProp contained "\<flex\(-\(basis\|direction\|flow\|grow\|shrink\|wrap\)\)\=\>"
syn keyword cssFontAttr contained flex row wrap
syn match cssFontAttr contained "\<inline-flex\>"
syn match cssFontAttr contained "\<\(row\|column\|wrap\)-reverse\>"
syn match cssFontProp contained "\<font-feature-settings\>"
syn match cssFontProp contained "\<font-kerning\>"
syn match cssFontProp contained "\<font-synthesis\>"
syn match cssFontProp contained "\<font-variant-\(alternates\|caps\|east-asian\|ligatures\|numeric\|position\)\>"
syn keyword cssFontAttr contained unicase ordinal jis78 jis83 jis90 jis04 simplified traditional
syn match cssFontAttr contained "\<\(no-\)\=\(common\|discretionary\|historical\)-ligatures\>"
syn match cssFontAttr contained "\<\(no-\)\=contextual\>"
syn match cssFontAttr contained "\<historical-forms\>"
syn match cssFontAttr contained "\<all-small-caps\>"
syn match cssFontAttr contained "\<\(all-\)\=petite-caps\>"
syn match cssFontAttr contained "\<titling-caps\>"
syn match cssFontAttr contained "\<\(lining\|oldstyle\|proportional\|tabular\)-nums\>"
syn match cssFontAttr contained "\<\(diagonal\|stacked\)-fractions\>"
syn match cssFontAttr contained "\<slashed-zero\>"
syn match cssFontAttr contained "\<proportional-width\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(stylistic\|styleset\|character-variant\|swash\|ornaments\|annotation\)\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<font-\(min\|max\)-size\>"
syn match cssFontProp contained "\<font-display\>"
syn match cssFontProp contained "\<font-optical-sizing\>"
syn match cssFontProp contained "\<font-variation-settings\>"
syn match cssFontProp contained "\<font-palette\>"
syn match cssFontProp contained "\<font-presentation\>"
syn match cssFontProp contained "\<base-palette\>"
syn match cssFontProp contained "\<font-language-override\>"
syn match cssFontProp contained "\<font-variant-emoji\>"
syn keyword cssFontAttr contained emoji math fangsong infinity swap fallback optional light dark
syn match cssFontAttr contained "\<system-ui\>"
syn region cssInclude start=/@font-palette-values\>/ end=/\ze{/ skipwhite skipnl contains=css.*Prop,css.*Attr,cssValueInteger,cssValueLength,cssMediaKeyword,cssVendor,cssIncludeKeyword,cssComment nextgroup=cssMediaBlock
syn keyword cssGeneratedContentProp contained running
syn match cssGeneratedContentProp contained "\<footnote-\(display\|policy\)\>"
syn keyword cssGeneratedContentAttr contained footnote line
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(element\|running\)\s*(" end=")" oneline keepend
syn match cssPseudoClassId contained "\<footnote-\(call\|marker\)\>"
syn match cssFontProp contained "\<grid-template\(-\(columns\|rows\|areas\)\)\=\>"
syn match cssFontProp contained "\<grid-auto-\(columns\|rows\|flow\)\>"
syn match cssFontProp contained "\<grid-\(row\|column\)\(-\(start\|end\)\)\=\>"
syn match cssFontProp contained "\<grid-area\>"
syn keyword cssFontAttr contained grid dense span
syn match cssFontAttr contained "\<inline-grid\>"
syn match cssFontAttr contained "\<auto-flow\>"
syn match cssValueNumber contained "[01]\(.\d\+\)\=fr"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(minmax\|repeat\)\s*(" end=")" oneline keepend
syn keyword cssFontAttr contained subgrid
syn match cssValueNumber contained "\d\(.\d\+\)\=ar"
syn match cssFontProp contained "\<image-resolution\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(image\|element\|conic-gradient\)\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<\(dominant\|alignment\)-baseline\>"
syn match cssFontProp contained "\<baseline-shift\>"
syn match cssFontProp contained "\<initial-letter\(-\(align\|wrap\)\)\="
syn keyword cssFontAttr contained mathematical ideographic
syn match cssFontProp contained "\<line-\(grid\|snap\)"
syn match cssFontProp contained "\<box-snap\>"
syn keyword cssFontAttr contained create
syn match cssFontAttr contained "\<block-\(start\|end\)\>"
syn match cssFontAttr contained "\<\(first\|last\)-baseline\>"
syn match cssGeneratedContentProp contained "\<marker-side\>"
syn match cssGeneratedContentProp contained "\<counter-set\>"
syn keyword cssGeneratedContentAttr contained marker
syn match cssGeneratedContentAttr contained "\<list-container\>"
syn keyword cssPseudoClassId contained marker
syn region cssFunction contained matchgroup=cssFunctionName start="\<counters\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<\(\(min\|max\)-\)\=\(block\|inline\)-size\>"
syn match cssFontProp contained "\<\(margin\|inset\|padding\)\(-\(block\|inline\)\(-\(start\|end\)\)\=\)\=\>"
syn match cssFontProp contained "\<border-\(block\|inline\)\(\(-\(start\|end\)\)\=\(-\(width\|style\|color\)\)\=\)\=\>"
syn match cssFontProp contained "\<\(background\|border\)-image-transform\>"
syn keyword cssFontAttr contained logical physical rotate
syn match cssFontProp contained "\<clip-\(path\|rule\)\>"
syn match cssFontProp contained "\<mask\(-\(image\|mode\|repeat\|position\|clip\|origin\|size\|composite\|type\)\)\=\>"
syn match cssFontProp contained "\<mask-border\(-\(source\|mode\|slice\|width\|outset\|repeat\)\)\=\>"
syn keyword cssFontAttr contained nonzero evenodd alpha luminance add subtract intersect exclude
syn match cssFontAttr contained "\<\(fill\|stroke\|view\)-box\>"
syn match cssFontAttr contained "\<no-clip\>"
syn keyword cssTagName mask
syn keyword cssFontProp contained columns
syn match cssFontProp contained "\<column-\(count\|fill\|rule\(-\(color\|style\|width\)\)\=\|span\|width\)\>"
syn match cssFontAttr contained "\<balance\(-all\)\=\>"
syn match cssFontProp contained "\<scroll-behavior\>"
syn keyword cssFontAttr contained smooth
syn match cssFontProp contained "\<max-lines\>"
syn keyword cssFontProp contained continue
syn match cssFontProp contained "\<scrollbar-gutter\>"
syn keyword cssFontAttr contained stable force overflow paginate fragments
syn region cssPseudoClassLang matchgroup=cssPseudoClassId start=":\(nth-fragment\)(" end=")" oneline
syn match cssPositioningProp contained "\<float-\(reference\|defer\|offset\)\>"
syn match cssPositioningAttr contained "\<inline-\(start\|end\)\>"
syn match cssPositioningAttr contained "\<snap-\(block\|inline\)\>"
syn region cssURL contained matchgroup=cssFunctionName start="\<snap-\(block\|inline\)\s*(" end=")" oneline keepend
syn region cssURL contained matchgroup=cssFunctionName start="\<paint\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<offset-\(before\|end\|after\|start\)\>"
syn keyword cssFontAttr contained sticky
syn match cssPseudoClassId contained "\<inactive-selection\>"
syn match cssPseudoClassId contained "\<\(spelling\|grammer\)-error\>"
syn match cssFontProp contained "\<flow-\(into\|from\)\>"
syn match cssFontProp contained "\<region-fragment\>"
syn keyword cssFontAttr contained element content break
syn match cssFontAttr contained "\<\(avoid-\)\=region\>"
syn keyword cssPseudoClassId contained region
syn match cssFontProp contained "\<line-height-step\>"
syn match cssFontProp contained "\<block-step\(-\(size\|insert\|align\|round\)\)\=\>"
syn keyword cssFontAttr contained margin up down nearest
syn match cssFontProp contained "\<shape-inside\>"
syn match cssFontProp contained "\<border-boundary\>"
syn match cssFontProp contained "\<polar-\(angle\|distance\)\>"
syn keyword cssFontAttr contained parent polar
syn match cssFontAttr contained "\<outside-shape\>"
syn match cssFontAttr contained "\<shape-box\>"
syn match cssMediaProp contained /device-radius/
syn match cssFontProp contained "\<ruby-\(position\|merge\|align\)"
syn match cssFontAttr contained "\<ruby-\(base\|text\)-container\>"
syn match cssFontAttr contained "\<inter-character\>"
syn region cssInclude start=/@scope\>/ end=/\ze{/ skipwhite skipnl nextgroup=cssMediaBlock
syn keyword cssPseudoClassId contained host shadow content
syn match cssPseudoClassId contained "\<\(scope\|host\)-context\>"
syn region cssPseudoClassLang matchgroup=cssPseudoClassId start=":\(host\)(" end=")" oneline
syn match cssSelectorOp2 "/deep/"
syn match cssFontProp contained "\<scroll-snap-\(type\|align\|stop\)\>"
syn match cssFontProp contained "\<scroll-\(padding\|margin\)\(-\(top\|bottom\|right\|left\|\(block\|inline\)\(-\(end\|start\)\)\=\)\)\=\>"
syn keyword cssFontAttr contained x y mandatory proximity
syn match cssFontProp contained "\<shape-\(outside\|image-threshold\|margin\)\>"
syn match cssFontAttr contained "\<margin-box\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(inset\|circle\|ellipse\|polygon\)\s*(" end=")" oneline keepend
syn match cssFontAttr contained "\<\(\(min\|max\)-content\)\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(fit-content\)\s*(" end=")" oneline keepend
syn keyword cssTextProp contained hyphens
syn match cssTextProp contained "\<line-break\>"
syn match cssTextProp contained "\<overflow-wrap\>"
syn match cssTextProp contained "\<tab-size\>"
syn match cssTextProp contained "\<text-align-all\>"
syn keyword cssTextAttr contained manual loose strict hanging anywhere
syn match cssTextAttr contained "\<match-parent\>"
syn match cssTextAttr contained "\<each-line\>"
syn match cssTextAttr contained "\<full-width\>"
syn match cssTextProp contained "\<text-space-\(collapse\|trim\)\>"
syn match cssTextProp contained "\<text-\(wrap\|spacing\)\>"
syn match cssTextProp contained "\<wrap-\(before\|after\|inside\)\>"
syn match cssTextProp contained "\<hyphenate-\(character\|limit-\(zone\|chars\|lines\|last\)\)\>"
syn keyword cssTextAttr contained spread punctuation
syn match cssTextAttr contained "\<preserve-\(auto\|trim\|breaks\|spaces\)\>"
syn match cssTextAttr contained "\<trim-inner\>"
syn match cssTextAttr contained "\<discard-\(before\|after\)\>"
syn match cssTextAttr contained "\<avoid-\(line\|flex\)\>"
syn match cssTextAttr contained "\<pre-wrap-auto\>"
syn match cssTextAttr contained "\<no-limit\>"
syn match cssTextAttr contained "\<\(trim\|space\)-\(start\|end\|adjacent\)\>"
syn match cssTextAttr contained "\<no-compress\>"
syn match cssTextAttr contained "\<ideograph-\(alpha\|numeric\)\>"
syn match cssTextProp contained "\<text-\(decoration\(-\(color\|line\|style\)\)\=\|underline-position\|emphasis\(-\(color\|position\|style\)\)\=\)\>"
syn keyword cssTextAttr contained wavy alphabetic ink under filled dot triangle sesame over
syn match cssTextProp contained "\<text-\(decoration-\(width\|skip\|skip-ink\)\|underline-offset\|emphasis-skip\)\>"
syn keyword cssTextAttr contained objects edges symbols narrow
syn match cssTextAttr contained "\<box-decoration\>"
syn match cssTextAttr contained "\<\(\(leading\|trailing\)-\)\=spaces\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<frames\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<backface-visibility\>"
syn match cssFontProp contained "\<perspective\(-origin\)\=\>"
syn match cssFontProp contained "\<transform\(-\(origin\|style\)\)\=\>"
syn keyword cssFontAttr contained flat
syn match cssFontAttr contained "\<preserve-3d\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(matrix\(3d\)\=\|translate\(3d\|X\|Y\|Z\)\=\|scale\(3d\|X\|Y\|Z\)\=\|rotate\(3d\|X\|Y\|Z\)\=\|skew\(X\|Y\)\=\|perspective\)\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<transition\(-\(property\|duration\|timing-function\|delay\)\)\=\>"
syn keyword cssFontAttr contained linear
syn match cssFontAttr contained "\<ease\(-\(in\|out\|in-out\)\)\=\>"
syn match cssFontAttr contained "\<step-\(start\|end\)\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(steps\|cubic-bezier\)\s*(" end=")" oneline keepend
syn match cssUIProp contained "\<caret-color\>"
syn keyword cssUIAttr contained grab grabbing
syn match cssUIProp contained "\<caret\(-shape\)\=\>"
syn keyword cssUIAttr contained fade underscore
syn region cssFunction contained matchgroup=cssFunctionName start="\<fade\s*(" end=")" oneline keepend
syn match cssValueLength contained "[-+]\=\d\+\(\.\d*\)\=\(ch\|vw\|vh\|vmin\|vmax\|q\)"
syn match cssValueAngle contained "[-+]\=\d\+\(\.\d*\)\=turn"
syn region cssFunction contained matchgroup=cssFunctionName start="\<calc\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<--[A-Za-z_][A-Za-z0-9_-]\+\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<var\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<will-change\>"
syn match cssFontAttr contained "\<scroll-position\>"
syn match cssFontProp contained "\<writing-mode\>"
syn match cssFontProp contained "\<text-\(orientation\|combine-upright\)\>"
syn match cssFontProp contained "\<glyph-orientation-vertical\>"
syn keyword cssFontAttr contained before after mixed upright plaintext sideways
syn match cssFontAttr contained "\<isolate\(-override\)\=\>"
syn match cssFontAttr contained "\<horizontal-tb\>"
syn match cssFontAttr contained "\<vertical-\(rl\|lr\)\>"
syn match cssFontAttr contained "\<sideways-\(rl\|lr\)\>"
syn match cssFontProp contained "\<\(fill\|stroke\)\(-\(break\|color\|image\|origin\|position\|size\|repeat\|opacity\)\)\=\>"
syn match cssFontProp contained "\<fill-rule\>"
syn match cssFontProp contained "\<stroke-\(width\|align\|line\(cap\|join\)\|miterlimit\|dash\(array\|offset\)\|dash-\(corner\|justify\)\)\>"
syn keyword cssFontAttr contained butt arcs stupid compress dashes gaps
syn match cssFontAttr contained "\<bounding-box\>"
syn match cssFontProp contained "\<flood-\(color\|opacity\)\>"
syn match cssFontProp contained "\<color-interpolation-filters\>"
syn match cssFontProp contained "\<lighting-color\>"
syn keyword cssFontAttr sRGB linearRGB
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(blur\|brightness\|contrast\|drop-shadow\|grayscale\|hue-rotate\|invert\|opacity\|saturate\|sepia\)\s*(" end=")" oneline keepend
syn keyword cssTagName picture rb rtc slot template
syn match cssFontProp contained "\<offset\(-\(path\|distance\|anchor\|rotate\)\)\=\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(ray\|path\)\s*(" end=")" oneline keepend
syn match cssFontProp contained "\<touch-action\>"
syn keyword cssFontAttr contained manipulation
syn match cssFontAttr contained "\<pan-\(x\|y\)\>"
syn keyword cssPseudoClassId contained target enabled disabled checked indeterminate root empty
syn match cssPseudoClassId contained "\<last-\(child\|of-type\)\>"
syn match cssPseudoClassId contained "\<first-of-type\>"
syn match cssPseudoClassId contained "\<only-\(child\|of-type\)\>"
syn region cssPseudoClassLang matchgroup=cssPseudoClassId start=":\(nth\(-last\)\=-\(child\|of-type\)\|not\)(" end=")" oneline
syn match cssSelectorOp "[|]"
syn keyword cssPseudoClassId contained scope current past future default valid invalid required optional blank playing paused
syn match cssPseudoClassId contained "\<\(any\|local\)-link\>"
syn match cssPseudoClassId contained "\<read-\(only\|write\)\>"
syn match cssPseudoClassId contained "\<placeholder-shown\>"
syn match cssPseudoClassId contained "\<\(in\|out-of\)-range\>"
syn match cssPseudoClassId contained "\<user-invalid\>"
syn match cssPseudoClassId contained "\<target-within\>"
syn match cssPseudoClassId contained "\<focus-\(within\|visible\)\>"
syn region cssPseudoClassLang matchgroup=cssPseudoClassId start=":\(matches\|dir\|local-link\|current\|nth\(-last\)\=-col\|has\|drop\)(" end=")" oneline
syn region cssPseudoClassLang matchgroup=cssPseudoClassId start="::attr(" end=")" oneline
syn keyword cssTagName animate animateMotion animateTransform circle clipPath cursor defs desc discard ellipse feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting feDisplacementMap feDistantLight feDropShadow feFlood feFuncA feFuncB feFuncG feFuncR feGaussianBlur feImage feMerge feMergeNode feMorphology feOffset fePointLight feSpecularLighting feSpotLight feTile feTurbulence filter foreignObject g hatch hatchpath image line linearGradient marker mesh meshgradient meshpatch meshrow metadata mpath path pattern polygon polyline radialGradient rect set solidcolor stop switch symbol text textPath tspan unknown use view
syn keyword cssFontProp contained cx cy d r rx ry x y
syn match cssFontProp contained "\<color-\(interpolation\|rendering\)\>"
syn match cssFontProp contained "\<image-rendering\>"
syn match cssFontProp contained "\<marker-\(end\|mid\|start\)\>"
syn match cssFontProp contained "\<pointer-events\>"
syn match cssFontProp contained "\<shape-rendering\>"
syn match cssFontProp contained "\<solid-\(color\|opacity\)\>"
syn match cssFontProp contained "\<stop-\(color\|opacity\)\>"
syn match cssFontProp contained "\<text-anchor\>"
syn match cssFontProp contained "\<vector-effect\>"
syn keyword cssFontAttr contained crispEdges geometricPrecision optimizeQuality painted stroke viewport visibleFill visiblePainted visibleStroke
syn match cssFontAttr contained "\<context-\(fill\|stroke\)\>"
syn match cssFontAttr contained "\<fixed-position\>"
syn match cssFontAttr contained "\<miter\(-clip\)\=\>"
syn match cssFontAttr contained "\<non-\(scaling-stroke\|scaling-size\|rotation\)\>"
syn region cssFunction contained matchgroup=cssFunctionName start="\<\(child\|icc-color\)\s*(" end=")" oneline keepend
syn match cssPseudoClassId contained "::\<cue\(-region\)\=\>"
syn region cssPseudoClassLang matchgroup=cssPseudoClassId start="::cue\(-region\)\=(" end=")" oneline
