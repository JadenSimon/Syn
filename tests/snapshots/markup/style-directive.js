// @filename: main.syn
var __template = ((p,c,u) => s => {
  u ||= setTimeout(() => (c = {}, u = 0))
  return s in c ? (c[s].n ||= p(s)).cloneNode(true) : c[s] = p(s)
})((s,t = document.createElement('template')) => (t.innerHTML = s, t.content.firstChild), {})
var __style = (c => s => c[s] ||= (document.head.insertAdjacentHTML('beforeend', s),1))({})
function compiledClass() {
  __style(`<style>
        .a[_hgcccz46] { color: red; }
    </style>`);
  return __template(`<div _hgcccz46 class="a">red!</div>`)
}
function sharedScope() {
  __style(`<style>
        div[_a4crlzfc] { color: red; }
    
        div[_a4crlzfc] { background: blue; }
    </style>`);
  return __template(`<div _a4crlzfc></div>`)
}
function nestedScopes(cond, cond2) {
  __style(`<style>
        div[_49xuiccy] { color: red; background: black; }
    </style>`);
  if (cond) {
    __style(`<style>
            div[_49xuiccy]:where([_s-1]) { color: blue; }
        </style>`);
    if (cond2) {
      __style(`<style>
                div:hover[_49xuiccy]:where([_s-1][_s-2]) { color: purple; }
            </style>`);
      return __template(`<div _49xuiccy _s-1 _s-2></div>`)
    }
    return __template(`<div _49xuiccy _s-1></div>`)
  }
  return __template(`<div _49xuiccy></div>`)
}
function nestedFnScopes() {
  __style(`<style>
        div[_7xd9vopg] { color: red; }
    </style>`);
  function f() {
    __style(`<style>
            div[_7xd9vopg]:where([_s-1]) { background: black; }
        </style>`);
    return __template(`<div _7xd9vopg _s-1></div>`)
  }
  return f
}
function unscoped() {
  __style(`<style>
        div.foo[_r2wz7amj] { color: red; }
        .foo { color: blue; }
        div.foo[_r2wz7amj] span:where([_r2wz7amj]) { color: green; } /* TODO: reconsider this case, it's functionally useless */
        div.foo span[_r2wz7amj] { color: green; }
    </style>`);
  return __template(`<div _r2wz7amj class="foo"></div>`)
}
function unscopedAtRule() {
  __style(`<style>
        
            div.foo { color: red; animation: bar; }
            @keyframes bar {}
            .bar { animation-name: bar; }
        
    </style>`);
  return __template(`<div _sz2398f8 class="foo"></div>`)
}
function scopeAtRule() {
  __style(`<style>
        .x[_95qhnuio] {}
        @scope (.x[_95qhnuio]) {
            .y {}
            @keyframes k_wwwhf13t {} /* still scoped */
        }
    </style>`);
  return __template(`<div _95qhnuio class="x"><div _95qhnuio class="y"></div></div>`)
}
function layerAtRule() {
  __style(`<style>
        @layer t { .btn[_lp8rur0j] {} }
    </style>`);
  return __template(`<button _lp8rur0j class="btn"></button>`)
}
function fontFaceAtRule() {
  __style(`<style>
        @font-face { font-family: 'test' }
    </style>`);
}
function toScope() {
  __style(`<style>
        .x[_sxhda72d] {}
        .y[_sxhda72d] {}
        @scope (.x[_sxhda72d]) to (.y[_sxhda72d]) {
            .z {}
        }
    </style>`);
  return __template(`<div _sxhda72d class="x"><div _sxhda72d class="y"><div _sxhda72d class="z">z</div></div></div>`)
}
function mediaAtRule() {
  __style(`<style>
        @media screen and (width = 900px) {
            @keyframes k_wwwhf13t {}
            div[_z65ezzvy] { color: red; }
        }
    </style>`);
  return __template(`<div _z65ezzvy></div>`)
}
function pseudoAttr() {
  __style(`<style>
        div[_abuvugeu]::before {
            content: "before";
            color: blue;
        }
    </style>`);
  return __template(`<div _abuvugeu></div>`)
}
function nestedCss() {
  __style(`<style>
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
    </style>`);
  return __template(`<div _sl2a43rj class="a">a
<div _sl2a43rj class="b">b
<div _sl2a43rj class="c">c</div></div></div>`)
}
function keyframes() {
  __style(`<style>
        .test[_23hul4ev] {
            color: blue;
            animation: test_itwaq4g9 1s infinite;
        }
        @keyframes test_itwaq4g9 {
            to {
                color: red;
            }
        }
    </style>`);
  return __template(`<div _23hul4ev class="test"></div>`)
}
function keyframesDifferentName() {
  __style(`<style>
        .test[_911znijm] {
            color: blue;
            animation: test2 1s infinite;
        }
        @keyframes test_nlsd6u5g {
            to { color: red; }
        }
    </style>`);
  return __template(`<div _911znijm class="test"></div>`)
}
function keyframesAnimationName() {
  __style(`<style>
        .test[_ikpxsniu] {
            animation: 1s infinite;
            animation-name: test_nlsd6u5g;
        }
        @keyframes test_nlsd6u5g {
            to { color: red; }
        }
    </style>`);
  return __template(`<div _ikpxsniu class="test"></div>`)
}
function keyframesMultiAnimation() {
  __style(`<style>
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
    </style>`);
  return __template(`<div _1wysftu0 class="test"></div>`)
}
function pseudoClasses() {
  __style(`<style>
        :is(div[_h3jhjffk]) {}
        :where(div[_h3jhjffk]) {}
        :has(div[_h3jhjffk]) {}
        :not(div[_h3jhjffk]) {}
    </style>`);
}
function unknownAtRule() {
  __style(`<style>
        @unknown-rule {}
        @unknown-rule;
        div[_4hozapda] {
            @unknown-rule {}
            @unknown-rule;
            color: red;
            div[_4hozapda] { div[_4hozapda] { color: red } }
        }
    </style>`);
}
function ambiguousNestedCss() {
  __style(`<style>
        background:color[_5x8xn6oe] {
            div[_5x8xn6oe] { color: red }
            background: black
        }
    </style>`);
}
function escapes() {
  __style(`<style>
        [data-x=\{][_x2b9660v] {
            color: red;
            &::before {
                content: "\"";
            }
            div[_x2b9660v] { & { color: blue } [attr=\&][_x2b9660v] { color: red } }
        }
        .\A9 0[_x2b9660v] {}
        .\0000A90[_x2b9660v] {}
        div[_x2b9660v] {
            --\@: red;
            color: var(--\@);
        }
        /* TODO: we need to normalize property names when parsing to handle this case
        /* div {
            \-\-x:div{color:red;};
            color:green;
        } */
    </style>`);
}
function comments() {
  __style(`<style>
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
    </style>`);
}
function customPropAtRule() {
  __style(`<style>
        div[_z6x90jlq] {
            --foo: @keyframes a {};
            --foo: @keyframes a {}
        }
    </style>`);
}
function customPropNestedRule() {
  __style(`<style>
        div[_u0sfinlq] {
            --foo: div{color:blue};
            --foo: div{color:blue}
        }
    </style>`);
}
function customPropMixedBlocks() {
  __style(`<style>
        div[_55mx149e] {
            --foo: ([}][(])]);color:red;div[_55mx149e]{color:blue}
        }
    </style>`);
}
