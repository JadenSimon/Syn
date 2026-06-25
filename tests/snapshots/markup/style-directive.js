// @filename: main.syn
var __template = ((c,p) => (s,b) => {
  c[0] ||= setTimeout(() => c={})
  return s in c ? (c[s][1] ||= p(s,b)).cloneNode(1) : (c[s] = [p(s,b)])[0]
})({}, (s,b,t=document.createElement('template')) => {
  t.innerHTML = s
  return b ? t.content : t.content.firstChild
})
var __style = (c => s => c[s]||=(document.head.insertAdjacentHTML('beforeend', `<style>${s}</style>`),1))({})
var __sym_upd = Symbol.update ||= Symbol.for('update')
var __slot_s = (a,b,v) => {
  let p, n = a.nextSibling
  for (p of v) n === p ? n=n.nextSibling : n.before(p)
  while (p = n, n = p.nextSibling, p !== b) p.remove()
}
function compiledClass() {
  __style(`
        .a[_hgcccz46] { color: red; }
    `);
  return __template(`<div _hgcccz46 class="a">red!`)
}
function sharedScope() {
  __style(`
        div[_a4crlzfc] { color: red; }
    
        div[_a4crlzfc] { background: blue; }
    `);
  return __template(`<div _a4crlzfc>`)
}
function nestedScopes(cond, cond2) {
  __style(`
        div[_49xuiccy] { color: red; background: black; }
    `);
  if (cond) {
    __style(`
            div[_49xuiccy]:where([_s-1]) { color: blue; }
        `);
    if (cond2) {
      __style(`
                div:hover[_49xuiccy]:where([_s-1][_s-2]) { color: purple; }
            `);
      return __template(`<div _49xuiccy _s-1 _s-2>`)
    }
    return __template(`<div _49xuiccy _s-1>`)
  }
  return __template(`<div _49xuiccy>`)
}
function nestedFnScopes() {
  __style(`
        div[_7xd9vopg] { color: red; }
    `);
  function f() {
    __style(`
            div[_7xd9vopg]:where([_s-1]) { background: black; }
        `);
    return __template(`<div _7xd9vopg _s-1>`)
  }
  return f
}
function selectorList() {
  __style(`
        div[_r0uxdr5t], button[_r0uxdr5t] { color: red; }
    `);
  return __template(`<div _r0uxdr5t><button _r0uxdr5t>`)
}
function unscoped() {
  __style(`
        div.foo[_r2wz7amj] { color: red; }
        .foo { color: blue; }
        div.foo[_r2wz7amj] span:where([_r2wz7amj]) { color: green; } /* TODO: reconsider this case, it's functionally useless */
        div.foo span[_r2wz7amj] { color: green; }
    `);
  return __template(`<div _r2wz7amj class="foo">`)
}
function unscopedAtRule() {
  __style(`
        
            div.foo { color: red; animation: bar; }
            @keyframes bar {}
            .bar { animation-name: bar; }
        
    `);
  return __template(`<div class="foo">`)
}
function scopeAtRule() {
  __style(`
        .x[_tp61005w] {}
        @scope (.x[_tp61005w]) {
            .y[_tp61005w] {}
            @keyframes k_wwwhf13t {}
        }
    `);
  return __template(`<div _tp61005w class="x"><div _tp61005w class="y">`)
}
function layerAtRule() {
  __style(`
        @layer t { .btn[_lp8rur0j] {} }
    `);
  return __template(`<button _lp8rur0j class="btn">`)
}
function fontFaceAtRule() {
  __style(`
        @font-face { font-family: 'test' }
    `);
}
function toScope() {
  __style(`
        .x[_sxhda72d] {}
        .y[_sxhda72d] {}
        @scope (.x[_sxhda72d]) to (.y[_sxhda72d]) {
            .z[_sxhda72d] {}
        }
    `);
  return __template(`<div _sxhda72d class="x"><div _sxhda72d class="y"><div _sxhda72d class="z">z`)
}
function mediaAtRule() {
  __style(`
        @media screen and (width = 900px) {
            @keyframes k_wwwhf13t {}
            div[_z65ezzvy] { color: red; }
        }
    `);
  return __template(`<div _z65ezzvy>`)
}
function pseudoAttr() {
  __style(`
        div[_abuvugeu]::before {
            content: "before";
            color: blue;
        }
    `);
  return __template(`<div _abuvugeu>`)
}
function nestedCss() {
  __style(`
        .a[_sl2a43rj] {
            color: blue;
            & { background: black; }
            &,.b2[_sl2a43rj] { background: black; }
            & .b2[_sl2a43rj] { background: black; }
            .b2[data-x="&"][_sl2a43rj] { background: black; }
            .b[_sl2a43rj] {
                color: red;
                .c[_sl2a43rj] { border: green; }
            }
        }
    `);
  return __template(`<div _sl2a43rj class="a">a
<div _sl2a43rj class="b">b
<div _sl2a43rj class="c">c`)
}
function keyframes() {
  __style(`
        .test[_23hul4ev] {
            color: blue;
            animation: test_itwaq4g9 1s infinite;
        }
        @keyframes test_itwaq4g9 {
            to {
                color: red;
            }
        }
    `);
  return __template(`<div _23hul4ev class="test">`)
}
function keyframesDifferentName() {
  __style(`
        .test[_911znijm] {
            color: blue;
            animation: test2 1s infinite;
        }
        @keyframes test_nlsd6u5g {
            to { color: red; }
        }
    `);
  return __template(`<div _911znijm class="test">`)
}
function keyframesAnimationName() {
  __style(`
        .test[_ikpxsniu] {
            animation: 1s infinite;
            animation-name: test_nlsd6u5g;
        }
        @keyframes test_nlsd6u5g {
            to { color: red; }
        }
    `);
  return __template(`<div _ikpxsniu class="test">`)
}
function keyframesMultiAnimation() {
  __style(`
        .test[_1wysftu0] {
            color: blue;
            background: red;
            animation: t1_nlsd6u5g 1s, t2_r2crj438 1s;
            animation-name: t1_nlsd6u5g, t2_r2crj438;
        }
        @keyframes t1_nlsd6u5g {
            to { color: red; }
        }
        @keyframes t2_r2crj438 {
            to { background: blue; }
        }
    `);
  return __template(`<div _1wysftu0 class="test">`)
}
function pseudoClasses() {
  __style(`
        [_a9ifkls5]:is(div) {}
        [_a9ifkls5]:where(div) {}
        [_a9ifkls5]:has(div[_a9ifkls5]) {}
        [_a9ifkls5]:not(div) {}
        [_a9ifkls5]:not(:has(.foo[_a9ifkls5], .bar[_a9ifkls5])) {}
        [_a9ifkls5]:has(>div[_a9ifkls5]) {}
        [_a9ifkls5]:has(>div[_a9ifkls5]+.a:where([_a9ifkls5])) {}
        [_a9ifkls5]:has(+div[_a9ifkls5]~.a:where([_a9ifkls5])) {}
        div[_a9ifkls5]:not(:is(.foo, .bar)) {}
    `);
}
function unknownAtRule() {
  __style(`
        @unknown-rule {}
        @unknown-rule;
        div[_4hozapda] {
            @unknown-rule {}
            @unknown-rule;
            color: red;
            div[_4hozapda] { div[_4hozapda] { color: red } }
        }
    `);
}
function ambiguousNestedCss() {
  __style(`
        background:color[_5x8xn6oe] {
            div[_5x8xn6oe] { color: red }
            background: black
        }
    `);
}
function escapes() {
  __style(`
        [data-x=\\{][_ure4a2b3] {
            color: red;
            &::before {
                content: "\\"";
            }
            div[_ure4a2b3] { & { color: blue } [attr=\\&][_ure4a2b3] { color: red } }
        }
        .\\A9 0[_ure4a2b3] {}
        .\\0000A90[_ure4a2b3] {}
        div[_ure4a2b3], \\\`[_ure4a2b3] {
            --\\@: red;
            color: var(--\\@);
        }
        /* TODO: we need to normalize property names when parsing to handle this case
        /* div {
            \\-\\-x:div{color:red;};
            color:green;
        } */
    `);
}
function comments() {
  __style(`
        div[_lkam5g98] /*.a*/ {
            color: green;
            div/*:red;*/[_lkam5g98]{}
            /*}*/
            background: /* ; */ black;
        }
        /* div {} */
        @keyframes a_wwwhf13t {}
        div[_lkam5g98] {
            animation-name: /* a */ b;
            animation-name: a_wwwhf13t;
        }
    `);
}
function multibyteToken() {
  __style(`
        .😊[_8cn33r5a]{}
    `);
}
function customPropAtRule() {
  __style(`
        div[_z6x90jlq] {
            --foo: @keyframes a {};
            --foo: @keyframes a {}
        }
    `);
}
function customPropNestedRule() {
  __style(`
        div[_u0sfinlq] {
            --foo: div{color:blue};
            --foo: div{color:blue}
        }
    `);
}
function customPropMixedBlocks() {
  __style(`
        div[_55mx149e] {
            --foo: ([}][(])]);color:red;div[_55mx149e]{color:blue}
        }
    `);
}
function componentTypeSelectors() {
  function Foo(_attrs, children = []) {
    const __ret = __template(`<div _59jo03zx _s-1>children:
<!><!>`)
    let _v0 = __ret.firstChild // #text
    _v0 = _v0.nextSibling; // <!> - {...}
    let _v1 = _v0.nextSibling // {...} - <!>
    __style(`
            div[_59jo03zx]:where([_s-1]) {
                
                    > :first-child::before {
                        content: "( ";
                        color: black;
                    }
                    > :last-child::after {
                        content: " )";
                        color: black;
                    }
                
            }
        `);
    return {
      root: __ret,
      [__sym_upd]: () => {
        __slot_s(_v0, _v1, children);
      }
    }
  }
  __style(`
        [_comp_lfk0x6fk] div:where([_59jo03zx]) {
            color: red;
            [_comp_lfk0x6fk]:hover {
                color: blue;
            }
        }
    `);
  let __ret = void 0
  {
    const _v0 = []
    const _v1 = __template(`<div _59jo03zx>red</div><div _59jo03zx><!>`)
    let _v2 = _v1 // div
    _v2 = _v2.nextSibling; // div
    _v2 = _v2.firstChild; // Foo
    let _v3
    __ret = Foo(void 0, _v0);
    _v3 ??= Foo(void 0, []);
    _v3[Symbol.update]();
    if (_v2) _v2 = void _v2.replaceWith(_v3.root);
    __ret[Symbol.update]();
  }
  return __ret
}
function deadAttributeElimination() {
  __style(`
        div[_do850bjp] { color: red; }
        
            span { background: gray; }
        
    `);
  return __template(`<span>red?`)
}
function deadAttributeElimination2() {
  __style(`
        div.foo[_zovaszno] { color: red; }
    `);
  return __template(`<div _zovaszno class="foo">red!`)
}
function deadAttributeElimination3() {
  __style(`
        div#foo[_d3dm9dua] { color: red; }
    `);
  return __template(`<div _d3dm9dua>red!`)
}
