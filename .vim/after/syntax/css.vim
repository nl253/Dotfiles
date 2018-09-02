" sy match cssFunction "\v<var\("
" sy match cssFunction ")"
" hi link cssFunctionName Function
" hi link cssFunction Function
" hi link cssFunction Function
" sy match cssVar "\v--[a-z]+(-[a-z]+)*"
" hi link cssVar Constant

" From <https://github.com/hail2u/vim-css3-syntax>

sy keyword cssFontProp contained isolation
sy match cssFontProp contained "\<\(mix\|background\)-blend-mode\>"
sy keyword cssFontAttr contained multiply screen overlay darken lighten color-dodge color-burn hard-light soft-light difference exclusion hue saturation color luminosity
sy match cssBoxProp contained "\<overflow-\(style\|x\|y\)\>"
sy match cssBoxProp contained "\<rotation\(-point\)\=\>"
sy keyword cssBoxAttr contained scrollbar panner marquee
sy match cssBoxAttr contained "\<ruby\(-\(base\(-group\)\=\|text\(-group\)\=\)\)\=\>"
sy match cssBoxAttr contained "\<no-\(display\|content\)\>"
sy region cssInclude start=/@supports\>/ end=/\ze{/ skipwhite skipnl contains=css.*Prop,css.*Attr,cssValueInteger,cssValueLength,cssMediaKeyword,cssVendor,cssIncludeKeyword,cssComment nextgroup=cssMediaBlock
sy match cssFontProp contained "\<wrap-\(flow\|through\)\>"
sy keyword cssFontAttr contained minimum maximum
sy match cssFontProp contained "\<object-\(fit\|position\)\>"
sy match cssFontProp contained "\<image-orientation\>"
sy keyword cssFontAttr contained contain cover snap
sy match cssFontAttr contained "\<from-image\>"
sy match cssFontAttr contained "\<scale-down\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(\(repeating-\)\=\(linear\|radial\)-gradient\)\s*(" end=")" oneline keepend
sy keyword cssFontAttr contained recto verso
sy match cssFontAttr contained "\<\(avoid-\)\=page\>"
sy match cssAuralProp contained "\<voice-\(volume\|balance\|rate\|pitch\|range\|stress\|duration\)\>"
sy match cssAuralProp contained "\<rest\(-\(before\|after\)\)\=\>"
sy keyword cssAuralAttr contained young old neutral preserve moderate reduced
sy match cssAuralAttr contained "\<\(literal\|no\)-punctuation\>"
sy match cssAuralAttr contained "\<\(x-\)\=\(weak\|strong\)\>"
sy match cssValueNumber contained "[-+]\=\d\+\(dB\|st\)"
sy match cssFontProp contained "\<\(justify\|align\|place\)-\(self\|content\|items\)\>"
sy match cssFontProp contained "\<\(\(row\|column\)-\)\=gap\>"
sy keyword cssFontAttr contained safe unsafe legacy
sy match cssFontAttr contained "\<\(self\|flex\)-\(start\|end\)\>"
sy match cssFontAttr contained "\<space-\(between\|around\|evenly\)\>"
sy match cssFontProp contained "\<animation\(-\(name\|duration\|timing-function\|iteration-count\|direction\|play-state\|delay\|fill-mode\)\)\=\>"
sy keyword cssFontAttr contained forwards backwards running paused
sy match cssFontAttr contained "\<alternate-reverse\>"
sy match cssFontProp contained "\<background-\(clip\|origin\|size\)\>"
sy match cssFontProp contained "\<border-image\(-\(source\|slice\|width\|outset\|repeat\)\)\=\>"
sy match cssFontProp contained "\<border-\(\(top-right\|bottom-right\|bottom-left\|top-left\)-\)\=radius\>"
sy match cssFontProp contained "\<box-shadow\>"
sy keyword cssFontAttr contained space round local fill stretch clone slice
sy match cssFontAttr contained "\<\(padding\|border\|content\)-box\>"
sy keyword cssFontProp contained corners
sy match cssFontProp contained "\<background-position-\(x\|y\|inline\|block\)\>"
sy match cssFontProp contained "\<corner-shape\>"
sy match cssFontProp contained "\<border-limit\>"
sy match cssFontProp contained "\<border-clip\(-\(top\|right\|bottom\|left\)\)\=\>"
sy keyword cssFontAttr contained bevel scoop notch
sy match cssFontAttr contained "\<\(x\|y\)-\(start\|end\)\>"
sy match cssFontProp contained "\<break-\(after\|before\|inside\)\>"
sy match cssFontProp contained "\<box-decoration-break\>"
sy match cssFontAttr contained "\<\(avoid-\)\=column\>"
sy keyword cssFontProp all
sy keyword cssCommonAttr contained initial unset
sy keyword cssCommonAttr contained revert
sy region cssURL contained matchgroup=cssFunctionName start="\<supports\s*(" end=")" oneline keepend
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(rgba\|hsla\=\)\s*(" end=")" oneline keepend
sy keyword cssColorProp contained opacity
sy match cssColor contained "\<currentcolor\>"
sy match cssColorProp contained "\<color-adjust\>"
sy keyword cssColor contained rebeccapurple
sy match cssColor contained "#[0-9A-Fa-f]\{8\}\>" contains=cssUnitDecorators
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(hwb\|lab\|lch\|gray\|color\|device-cmyk\|color-mod\=\)\s*(" end=")" oneline keepend
sy match cssFontDescriptor "@color-profile\>" nextgroup=cssFontDescriptorBlock skipwhite skipnl
sy keyword cssFontProp contained contain
sy keyword cssFontAttr contained layout paint size
sy match cssGeneratedContentProp contained "\<string-set\>"
sy match cssGeneratedContentProp contained "\<bookmark-\(label\|level\|state\)\>"
sy keyword cssGeneratedContentAttr contained open closed
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(leader\|string\|target-\(counter\|counters\|text\)\)\s*(" end=")" oneline keepend
sy region cssInclude start=/@counter-style\>/ end=/\ze{/ skipwhite skipnl contains=css.*Prop,css.*Attr,cssValueInteger,cssValueLength,cssMediaKeyword,cssVendor,cssIncludeKeyword,cssComment nextgroup=cssMediaBlock
sy keyword cssGeneratedContentProp contained system negative prefix suffix range pad fallback
sy match cssGeneratedContentProp contained "\<\(additive-\)\=symbols\>"
sy match cssGeneratedContentProp contained "\<speak-as\>"
sy keyword cssGeneratedContentAttr contained cyclic symbolic additive extends bullets numbers words bengali cambodian khmer devanagari gujarati gurmukhi kannada lao malayalam mongolian myanmar oriya persian tamil telugu thai tibetan
sy match cssGeneratedContentAttr contained "\<\(ethiopic-\)\=numeric\>"
sy match cssGeneratedContentAttr contained "\<arabic-indic\>"
sy match cssGeneratedContentAttr contained "\<\(upper\|lower\)-armenian\>"
sy match cssGeneratedContentAttr contained "\<cjk-\(decimal\|earthly-branch\|heavenly-stem\)\>"
sy match cssGeneratedContentAttr contained "\<disclosure-\(open\|closed\)\>"
sy match cssGeneratedContentAttr contained "\<\(japanese\|korean-hanja\|\(simp\|trad\)-chinese\)-\(in\)\=formal\>"
sy match cssGeneratedContentAttr contained "\<korean-hangul-formal\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<symbols\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<\(min\|max\|user\)-zoom\>"
sy region cssInclude start=/@viewport\>/ end=/\ze{/ skipwhite skipnl contains=css.*Prop,css.*Attr,cssValueInteger,cssValueLength,cssMediaKeyword,cssVendor,cssIncludeKeyword,cssComment nextgroup=cssMediaBlock
sy keyword cssFontAttr contained contents discard
sy match cssFontAttr contained "\<flow\(-root\)\=\>"
sy keyword cssFontProp order
sy match cssFontProp contained "\<flex\(-\(basis\|direction\|flow\|grow\|shrink\|wrap\)\)\=\>"
sy keyword cssFontAttr contained flex row wrap
sy match cssFontAttr contained "\<inline-flex\>"
sy match cssFontAttr contained "\<\(row\|column\|wrap\)-reverse\>"
sy match cssFontProp contained "\<font-feature-settings\>"
sy match cssFontProp contained "\<font-kerning\>"
sy match cssFontProp contained "\<font-synthesis\>"
sy match cssFontProp contained "\<font-variant-\(alternates\|caps\|east-asian\|ligatures\|numeric\|position\)\>"
sy keyword cssFontAttr contained unicase ordinal jis78 jis83 jis90 jis04 simplified traditional
sy match cssFontAttr contained "\<\(no-\)\=\(common\|discretionary\|historical\)-ligatures\>"
sy match cssFontAttr contained "\<\(no-\)\=contextual\>"
sy match cssFontAttr contained "\<historical-forms\>"
sy match cssFontAttr contained "\<all-small-caps\>"
sy match cssFontAttr contained "\<\(all-\)\=petite-caps\>"
sy match cssFontAttr contained "\<titling-caps\>"
sy match cssFontAttr contained "\<\(lining\|oldstyle\|proportional\|tabular\)-nums\>"
sy match cssFontAttr contained "\<\(diagonal\|stacked\)-fractions\>"
sy match cssFontAttr contained "\<slashed-zero\>"
sy match cssFontAttr contained "\<proportional-width\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(stylistic\|styleset\|character-variant\|swash\|ornaments\|annotation\)\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<font-\(min\|max\)-size\>"
sy match cssFontProp contained "\<font-display\>"
sy match cssFontProp contained "\<font-optical-sizing\>"
sy match cssFontProp contained "\<font-variation-settings\>"
sy match cssFontProp contained "\<font-palette\>"
sy match cssFontProp contained "\<font-presentation\>"
sy match cssFontProp contained "\<base-palette\>"
sy match cssFontProp contained "\<font-language-override\>"
sy match cssFontProp contained "\<font-variant-emoji\>"
sy keyword cssFontAttr contained emoji math fangsong infinity swap fallback optional light dark
sy match cssFontAttr contained "\<system-ui\>"
sy region cssInclude start=/@font-palette-values\>/ end=/\ze{/ skipwhite skipnl contains=css.*Prop,css.*Attr,cssValueInteger,cssValueLength,cssMediaKeyword,cssVendor,cssIncludeKeyword,cssComment nextgroup=cssMediaBlock
sy keyword cssGeneratedContentProp contained running
sy match cssGeneratedContentProp contained "\<footnote-\(display\|policy\)\>"
sy keyword cssGeneratedContentAttr contained footnote line
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(element\|running\)\s*(" end=")" oneline keepend
sy match cssPseudoClassId contained "\<footnote-\(call\|marker\)\>"
sy match cssFontProp contained "\<grid-template\(-\(columns\|rows\|areas\)\)\=\>"
sy match cssFontProp contained "\<grid-auto-\(columns\|rows\|flow\)\>"
sy match cssFontProp contained "\<grid-\(row\|column\)\(-\(start\|end\)\)\=\>"
sy match cssFontProp contained "\<grid-area\>"
sy keyword cssFontAttr contained grid dense span
sy match cssFontAttr contained "\<inline-grid\>"
sy match cssFontAttr contained "\<auto-flow\>"
sy match cssValueNumber contained "[01]\(.\d\+\)\=fr"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(minmax\|repeat\)\s*(" end=")" oneline keepend
sy keyword cssFontAttr contained subgrid
sy match cssValueNumber contained "\d\(.\d\+\)\=ar"
sy match cssFontProp contained "\<image-resolution\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(image\|element\|conic-gradient\)\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<\(dominant\|alignment\)-baseline\>"
sy match cssFontProp contained "\<baseline-shift\>"
sy match cssFontProp contained "\<initial-letter\(-\(align\|wrap\)\)\="
sy keyword cssFontAttr contained mathematical ideographic
sy match cssFontProp contained "\<line-\(grid\|snap\)"
sy match cssFontProp contained "\<box-snap\>"
sy keyword cssFontAttr contained create
sy match cssFontAttr contained "\<block-\(start\|end\)\>"
sy match cssFontAttr contained "\<\(first\|last\)-baseline\>"
sy match cssGeneratedContentProp contained "\<marker-side\>"
sy match cssGeneratedContentProp contained "\<counter-set\>"
sy keyword cssGeneratedContentAttr contained marker
sy match cssGeneratedContentAttr contained "\<list-container\>"
sy keyword cssPseudoClassId contained marker
sy region cssFunction contained matchgroup=cssFunctionName start="\<counters\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<\(\(min\|max\)-\)\=\(block\|inline\)-size\>"
sy match cssFontProp contained "\<\(margin\|inset\|padding\)\(-\(block\|inline\)\(-\(start\|end\)\)\=\)\=\>"
sy match cssFontProp contained "\<border-\(block\|inline\)\(\(-\(start\|end\)\)\=\(-\(width\|style\|color\)\)\=\)\=\>"
sy match cssFontProp contained "\<\(background\|border\)-image-transform\>"
sy keyword cssFontAttr contained logical physical rotate
sy match cssFontProp contained "\<clip-\(path\|rule\)\>"
sy match cssFontProp contained "\<mask\(-\(image\|mode\|repeat\|position\|clip\|origin\|size\|composite\|type\)\)\=\>"
sy match cssFontProp contained "\<mask-border\(-\(source\|mode\|slice\|width\|outset\|repeat\)\)\=\>"
sy keyword cssFontAttr contained nonzero evenodd alpha luminance add subtract intersect exclude
sy match cssFontAttr contained "\<\(fill\|stroke\|view\)-box\>"
sy match cssFontAttr contained "\<no-clip\>"
sy keyword cssTagName mask
sy keyword cssFontProp contained columns
sy match cssFontProp contained "\<column-\(count\|fill\|rule\(-\(color\|style\|width\)\)\=\|span\|width\)\>"
sy match cssFontAttr contained "\<balance\(-all\)\=\>"
sy match cssFontProp contained "\<scroll-behavior\>"
sy keyword cssFontAttr contained smooth
sy match cssFontProp contained "\<max-lines\>"
sy keyword cssFontProp contained continue
sy match cssFontProp contained "\<scrollbar-gutter\>"
sy keyword cssFontAttr contained stable force overflow paginate fragments
sy region cssPseudoClassLang matchgroup=cssPseudoClassId start=":\(nth-fragment\)(" end=")" oneline
sy match cssPositioningProp contained "\<float-\(reference\|defer\|offset\)\>"
sy match cssPositioningAttr contained "\<inline-\(start\|end\)\>"
sy match cssPositioningAttr contained "\<snap-\(block\|inline\)\>"
sy region cssURL contained matchgroup=cssFunctionName start="\<snap-\(block\|inline\)\s*(" end=")" oneline keepend
sy region cssURL contained matchgroup=cssFunctionName start="\<paint\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<offset-\(before\|end\|after\|start\)\>"
sy keyword cssFontAttr contained sticky
sy match cssPseudoClassId contained "\<inactive-selection\>"
sy match cssPseudoClassId contained "\<\(spelling\|grammer\)-error\>"
sy match cssFontProp contained "\<flow-\(into\|from\)\>"
sy match cssFontProp contained "\<region-fragment\>"
sy keyword cssFontAttr contained element content break
sy match cssFontAttr contained "\<\(avoid-\)\=region\>"
sy keyword cssPseudoClassId contained region
sy match cssFontProp contained "\<line-height-step\>"
sy match cssFontProp contained "\<block-step\(-\(size\|insert\|align\|round\)\)\=\>"
sy keyword cssFontAttr contained margin up down nearest
sy match cssFontProp contained "\<shape-inside\>"
sy match cssFontProp contained "\<border-boundary\>"
sy match cssFontProp contained "\<polar-\(angle\|distance\)\>"
sy keyword cssFontAttr contained parent polar
sy match cssFontAttr contained "\<outside-shape\>"
sy match cssFontAttr contained "\<shape-box\>"
sy match cssMediaProp contained /device-radius/
sy match cssFontProp contained "\<ruby-\(position\|merge\|align\)"
sy match cssFontAttr contained "\<ruby-\(base\|text\)-container\>"
sy match cssFontAttr contained "\<inter-character\>"
sy region cssInclude start=/@scope\>/ end=/\ze{/ skipwhite skipnl nextgroup=cssMediaBlock
sy keyword cssPseudoClassId contained host shadow content
sy match cssPseudoClassId contained "\<\(scope\|host\)-context\>"
sy region cssPseudoClassLang matchgroup=cssPseudoClassId start=":\(host\)(" end=")" oneline
sy match cssSelectorOp2 "/deep/"
sy match cssFontProp contained "\<scroll-snap-\(type\|align\|stop\)\>"
sy match cssFontProp contained "\<scroll-\(padding\|margin\)\(-\(top\|bottom\|right\|left\|\(block\|inline\)\(-\(end\|start\)\)\=\)\)\=\>"
sy keyword cssFontAttr contained x y mandatory proximity
sy match cssFontProp contained "\<shape-\(outside\|image-threshold\|margin\)\>"
sy match cssFontAttr contained "\<margin-box\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(inset\|circle\|ellipse\|polygon\)\s*(" end=")" oneline keepend
sy match cssFontAttr contained "\<\(\(min\|max\)-content\)\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(fit-content\)\s*(" end=")" oneline keepend
sy keyword cssTextProp contained hyphens
sy match cssTextProp contained "\<line-break\>"
sy match cssTextProp contained "\<overflow-wrap\>"
sy match cssTextProp contained "\<tab-size\>"
sy match cssTextProp contained "\<text-align-all\>"
sy keyword cssTextAttr contained manual loose strict hanging anywhere
sy match cssTextAttr contained "\<match-parent\>"
sy match cssTextAttr contained "\<each-line\>"
sy match cssTextAttr contained "\<full-width\>"
sy match cssTextProp contained "\<text-space-\(collapse\|trim\)\>"
sy match cssTextProp contained "\<text-\(wrap\|spacing\)\>"
sy match cssTextProp contained "\<wrap-\(before\|after\|inside\)\>"
sy match cssTextProp contained "\<hyphenate-\(character\|limit-\(zone\|chars\|lines\|last\)\)\>"
sy keyword cssTextAttr contained spread punctuation
sy match cssTextAttr contained "\<preserve-\(auto\|trim\|breaks\|spaces\)\>"
sy match cssTextAttr contained "\<trim-inner\>"
sy match cssTextAttr contained "\<discard-\(before\|after\)\>"
sy match cssTextAttr contained "\<avoid-\(line\|flex\)\>"
sy match cssTextAttr contained "\<pre-wrap-auto\>"
sy match cssTextAttr contained "\<no-limit\>"
sy match cssTextAttr contained "\<\(trim\|space\)-\(start\|end\|adjacent\)\>"
sy match cssTextAttr contained "\<no-compress\>"
sy match cssTextAttr contained "\<ideograph-\(alpha\|numeric\)\>"
sy match cssTextProp contained "\<text-\(decoration\(-\(color\|line\|style\)\)\=\|underline-position\|emphasis\(-\(color\|position\|style\)\)\=\)\>"
sy keyword cssTextAttr contained wavy alphabetic ink under filled dot triangle sesame over
sy match cssTextProp contained "\<text-\(decoration-\(width\|skip\|skip-ink\)\|underline-offset\|emphasis-skip\)\>"
sy keyword cssTextAttr contained objects edges symbols narrow
sy match cssTextAttr contained "\<box-decoration\>"
sy match cssTextAttr contained "\<\(\(leading\|trailing\)-\)\=spaces\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<frames\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<backface-visibility\>"
sy match cssFontProp contained "\<perspective\(-origin\)\=\>"
sy match cssFontProp contained "\<transform\(-\(origin\|style\)\)\=\>"
sy keyword cssFontAttr contained flat
sy match cssFontAttr contained "\<preserve-3d\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(matrix\(3d\)\=\|translate\(3d\|X\|Y\|Z\)\=\|scale\(3d\|X\|Y\|Z\)\=\|rotate\(3d\|X\|Y\|Z\)\=\|skew\(X\|Y\)\=\|perspective\)\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<transition\(-\(property\|duration\|timing-function\|delay\)\)\=\>"
sy keyword cssFontAttr contained linear
sy match cssFontAttr contained "\<ease\(-\(in\|out\|in-out\)\)\=\>"
sy match cssFontAttr contained "\<step-\(start\|end\)\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(steps\|cubic-bezier\)\s*(" end=")" oneline keepend
sy match cssUIProp contained "\<caret-color\>"
sy keyword cssUIAttr contained grab grabbing
sy match cssUIProp contained "\<caret\(-shape\)\=\>"
sy keyword cssUIAttr contained fade underscore
sy region cssFunction contained matchgroup=cssFunctionName start="\<fade\s*(" end=")" oneline keepend
sy match cssValueLength contained "[-+]\=\d\+\(\.\d*\)\=\(ch\|vw\|vh\|vmin\|vmax\|q\)"
sy match cssValueAngle contained "[-+]\=\d\+\(\.\d*\)\=turn"
sy region cssFunction contained matchgroup=cssFunctionName start="\<calc\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<--[A-Za-z_][A-Za-z0-9_-]\+\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<var\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<will-change\>"
sy match cssFontAttr contained "\<scroll-position\>"
sy match cssFontProp contained "\<writing-mode\>"
sy match cssFontProp contained "\<text-\(orientation\|combine-upright\)\>"
sy match cssFontProp contained "\<glyph-orientation-vertical\>"
sy keyword cssFontAttr contained before after mixed upright plaintext sideways
sy match cssFontAttr contained "\<isolate\(-override\)\=\>"
sy match cssFontAttr contained "\<horizontal-tb\>"
sy match cssFontAttr contained "\<vertical-\(rl\|lr\)\>"
sy match cssFontAttr contained "\<sideways-\(rl\|lr\)\>"
sy match cssFontProp contained "\<\(fill\|stroke\)\(-\(break\|color\|image\|origin\|position\|size\|repeat\|opacity\)\)\=\>"
sy match cssFontProp contained "\<fill-rule\>"
sy match cssFontProp contained "\<stroke-\(width\|align\|line\(cap\|join\)\|miterlimit\|dash\(array\|offset\)\|dash-\(corner\|justify\)\)\>"
sy keyword cssFontAttr contained butt arcs stupid compress dashes gaps
sy match cssFontAttr contained "\<bounding-box\>"
sy match cssFontProp contained "\<flood-\(color\|opacity\)\>"
sy match cssFontProp contained "\<color-interpolation-filters\>"
sy match cssFontProp contained "\<lighting-color\>"
sy keyword cssFontAttr sRGB linearRGB
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(blur\|brightness\|contrast\|drop-shadow\|grayscale\|hue-rotate\|invert\|opacity\|saturate\|sepia\)\s*(" end=")" oneline keepend
sy keyword cssTagName picture rb rtc slot template
sy match cssFontProp contained "\<offset\(-\(path\|distance\|anchor\|rotate\)\)\=\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(ray\|path\)\s*(" end=")" oneline keepend
sy match cssFontProp contained "\<touch-action\>"
sy keyword cssFontAttr contained manipulation
sy match cssFontAttr contained "\<pan-\(x\|y\)\>"
sy keyword cssPseudoClassId contained target enabled disabled checked indeterminate root empty
sy match cssPseudoClassId contained "\<last-\(child\|of-type\)\>"
sy match cssPseudoClassId contained "\<first-of-type\>"
sy match cssPseudoClassId contained "\<only-\(child\|of-type\)\>"
sy region cssPseudoClassLang matchgroup=cssPseudoClassId start=":\(nth\(-last\)\=-\(child\|of-type\)\|not\)(" end=")" oneline
sy match cssSelectorOp "[|]"
sy keyword cssPseudoClassId contained scope current past future default valid invalid required optional blank playing paused
sy match cssPseudoClassId contained "\<\(any\|local\)-link\>"
sy match cssPseudoClassId contained "\<read-\(only\|write\)\>"
sy match cssPseudoClassId contained "\<placeholder-shown\>"
sy match cssPseudoClassId contained "\<\(in\|out-of\)-range\>"
sy match cssPseudoClassId contained "\<user-invalid\>"
sy match cssPseudoClassId contained "\<target-within\>"
sy match cssPseudoClassId contained "\<focus-\(within\|visible\)\>"
sy region cssPseudoClassLang matchgroup=cssPseudoClassId start=":\(matches\|dir\|local-link\|current\|nth\(-last\)\=-col\|has\|drop\)(" end=")" oneline
sy region cssPseudoClassLang matchgroup=cssPseudoClassId start="::attr(" end=")" oneline
sy keyword cssTagName animate animateMotion animateTransform circle clipPath cursor defs desc discard ellipse feBlend feColorMatrix feComponentTransfer feComposite feConvolveMatrix feDiffuseLighting feDisplacementMap feDistantLight feDropShadow feFlood feFuncA feFuncB feFuncG feFuncR feGaussianBlur feImage feMerge feMergeNode feMorphology feOffset fePointLight feSpecularLighting feSpotLight feTile feTurbulence filter foreignObject g hatch hatchpath image line linearGradient marker mesh meshgradient meshpatch meshrow metadata mpath path pattern polygon polyline radialGradient rect set solidcolor stop switch symbol text textPath tspan unknown use view
sy keyword cssFontProp contained cx cy d r rx ry x y
sy match cssFontProp contained "\<color-\(interpolation\|rendering\)\>"
sy match cssFontProp contained "\<image-rendering\>"
sy match cssFontProp contained "\<marker-\(end\|mid\|start\)\>"
sy match cssFontProp contained "\<pointer-events\>"
sy match cssFontProp contained "\<shape-rendering\>"
sy match cssFontProp contained "\<solid-\(color\|opacity\)\>"
sy match cssFontProp contained "\<stop-\(color\|opacity\)\>"
sy match cssFontProp contained "\<text-anchor\>"
sy match cssFontProp contained "\<vector-effect\>"
sy keyword cssFontAttr contained crispEdges geometricPrecision optimizeQuality painted stroke viewport visibleFill visiblePainted visibleStroke
sy match cssFontAttr contained "\<context-\(fill\|stroke\)\>"
sy match cssFontAttr contained "\<fixed-position\>"
sy match cssFontAttr contained "\<miter\(-clip\)\=\>"
sy match cssFontAttr contained "\<non-\(scaling-stroke\|scaling-size\|rotation\)\>"
sy region cssFunction contained matchgroup=cssFunctionName start="\<\(child\|icc-color\)\s*(" end=")" oneline keepend
sy match cssPseudoClassId contained "::\<cue\(-region\)\=\>"
sy region cssPseudoClassLang matchgroup=cssPseudoClassId start="::cue\(-region\)\=(" end=")" oneline
