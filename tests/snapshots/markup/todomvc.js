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
var __slot = (a,c,v) => {
  const q = c?.nextSibling === a
  if (typeof v !== 'object') {
    if (q && c.nodeType === 3) { c.data != v && (c.data = v); return c }
    v = new Text(v)
  }
  q ? c !== v && c.replaceWith(v) : a.parentNode?.insertBefore(v,a)
  return v
}
function App() {
  let editingTodo
  let filterState = 'all'
  const todos = []
  function deleteTodo(d) {
    const ind = todos.indexOf(d)
    if (ind === -1) return 
    todos.splice(ind, 1);
    if (editingTodo === d) {
      editingTodo = undefined;
    }
    root[Symbol.update]();
  }
  const stored = localStorage.getItem('todos')
  if (stored) {
    todos.push(...JSON.parse(stored));
  }
  function serializeTodos() {
    const str = JSON.stringify(todos.map(d => ({
      ...d,
      _comp: undefined
    })))
    localStorage.setItem('todos', str);
  }
  const root = __template(`<div _mvrhbcb5><input _mvrhbcb5 class="label-maker" autofocus placeholder="enter a todo"><!><!><!><!>`)
  const _v0 = root.firstChild
  const _v1 = _v0.nextSibling
  let _v2 = _v1.nextSibling
  const _v3 = _v2.nextSibling
  let _v4 = _v3.nextSibling
  _v0.addEventListener('keypress', (ev) => {
    if (ev.key === 'Enter') {
      const val = _v0.value.trim()
      if (!val) return 
      todos.push({
        label: val
      });
      _v0.value = '';
      root[Symbol.update]();
    }
  });
  let _v5, _v23, _v26, _v29
  root[__sym_upd] = () => {
    serializeTodos();
    const _v24 = !!todos.length
    if (_v24) {
      if (!_v5) {
        let _v17 = 0x1
        const _v6 = []
        _v5 = __template(`<main _mvrhbcb5><ul _mvrhbcb5 class="list"><!><!></ul><div _mvrhbcb5 class="filters"><!><!></div><button _mvrhbcb5>toggle all <!></button></main><footer _mvrhbcb5><div _mvrhbcb5 class="count"><!> active TODOs`, 1);
        _v6.push(..._v5.childNodes);
        const _v7 = _v5.firstChild
        const _v8 = _v7.nextSibling
        const _v9 = _v7.firstChild
        const _v10 = _v9.nextSibling
        const _v11 = _v10.nextSibling
        let _v12 = _v9.firstChild
        let _v13 = _v12.nextSibling
        const _v14 = _v10.firstChild
        let _v15 = _v14.nextSibling
        const m = {
          'all': 'ALL',
          'completed': 'Completed?',
          'active': 'Active!'
        }
        let _v16
        _v11.addEventListener('click', () => {
          for (const d of todos) {
            d.completed = !allCompleted;
          }
          root[Symbol.update]();
        });
        let _v18 = _v11.firstChild
        _v18 = _v18.nextSibling;
        let _v19
        const _v20 = _v8.firstChild
        let _v21 = _v20.firstChild
        let _v22
        _v5 = _v6;
        _v5[__sym_upd] = () => {
          for (const d of todos) {
            if (d._comp) d._comp[Symbol.update]();
             else d._comp = (() => {
              let _v0 = Todo({
                todo: d
              })
              _v0[Symbol.update]();
              return _v0
            })();
          }
          __slot_s(_v12, _v13, todos.filter(d => {
            switch (filterState) {
              case 'all': 
                return true
              case 'completed': 
                return d.completed
              case 'active': 
                return !d.completed
            }
          }).map(d => d._comp.root));
          if (_v17) {
            _v17 >>= 1;
            _v16 = Object.entries(m).map(([k, v]) => ((() => {
              const _v0 = __template(`<button _mvrhbcb5><!>`)
              _v0.addEventListener('click', () => {
                if (filterState === k) return 
                filterState = k;
                root[Symbol.update]();
                for (const b of buttons) b[Symbol.update]();
              });
              let _v1 = _v0.firstChild
              let _v2
              ;(_v0[__sym_upd] = () => {
                _v0.classList.toggle('active', k === filterState);
                _v2 = __slot(_v1, _v2, v)
              })();
              return _v0
            })()));
          }
          const buttons = _v16
          __slot_s(_v14, _v15, buttons);
          const allCompleted = todos.every(d => d.completed)
          _v19 = __slot(_v18, _v19, allCompleted ? 'active' : 'completed')
          _v22 = __slot(_v21, _v22, todos.filter(d => !d.completed).length)
        };
      }
    }
    const _v25 = _v24 ? _v5 : []
    if (_v24 !== _v23) __slot_s(_v1, _v2, _v25);
    _v23 = _v24;
    _v25[Symbol.update]?.()
    const _v30 = !!todos.some(d => d.completed)
    if (_v30) {
      if (!_v26) {
        const _v27 = []
        _v26 = __template(`<button _mvrhbcb5>clear completed todos`);
        _v27.push(_v26);
        let _v28 = _v26
        _v28.addEventListener('click', () => {
          const active = todos.filter(d => !d.completed)
          todos.splice(0, todos.length, ...active);
          root[Symbol.update]();
        });
        _v26 = _v27;
      }
    }
    const _v28 = _v30 ? _v26 : []
    if (_v30 !== _v29) __slot_s(_v3, _v4, _v28);
    _v29 = _v30;
    _v28[Symbol.update]?.()
  };
  function Todo(attrs) {
    let tmpLabel = attrs.todo.label
    function save(val) {
      if (editingTodo !== attrs.todo) return 
      const trimmed = val.trim()
      if (!trimmed) return deleteTodo(attrs.todo)
      editingTodo = undefined;
      attrs.todo.label = val;
      tmpLabel = val;
      root[Symbol.update]();
    }
    const __ret = __template(`<li _mvrhbcb5><label _mvrhbcb5>completed
<input _mvrhbcb5 type="checkbox"></label><!><!><label _mvrhbcb5>delete
<button _mvrhbcb5 type="button">X`)
    const _v31 = __ret.firstChild
    const _v32 = _v31.nextSibling
    let _v33 = _v32.nextSibling
    const _v34 = _v33.nextSibling
    let _v35 = _v31.firstChild
    _v35 = _v35.nextSibling;
    _v35.addEventListener('change', () => {
      attrs.todo.completed = _v35.checked;
      root[Symbol.update]();
    });
    let _v36, _v41, _v44
    let _v47 = _v34.firstChild
    _v47 = _v47.nextSibling;
    _v47.addEventListener('click', () => {
      deleteTodo(attrs.todo);
    });
    return {
      root: __ret,
      [__sym_upd]: () => {
        __ret.classList.toggle('completed', !!attrs.todo.completed);
        __ret.classList.toggle('editing', editingTodo === attrs.todo);
        _v35.checked = attrs.todo.completed;
        const _v45 = editingTodo !== attrs.todo
        if (_v45) {
          if (!_v36) {
            const _v37 = []
            _v36 = __template(`<label _mvrhbcb5><!>`);
            _v37.push(_v36);
            const _v38 = _v36
            _v38.addEventListener('dblclick', () => {
              if (editingTodo === attrs.todo) return 
              const p = _v38.parentElement
              editingTodo = attrs.todo;
              root[Symbol.update]();
              p?.querySelector('input[type="text"]')?.focus();
            });
            let _v39 = _v38.firstChild
            let _v40
            _v36 = _v37;
            _v36[__sym_upd] = () => {
              _v40 = __slot(_v39, _v40, attrs.todo.label)
            };
          }
        } else {
          if (!_v41) {
            const _v42 = []
            _v41 = __template(`<input _mvrhbcb5 type="text">`);
            _v42.push(_v41);
            const _v43 = _v41
            _v43.addEventListener('input', () => {
              tmpLabel = _v43.value;
            });
            _v43.addEventListener('blur', () => {
              save(tmpLabel);
            });
            _v43.addEventListener('keydown', (ev) => {
              if (editingTodo !== attrs.todo) return 
              if (ev.key === 'Enter') {
                save(tmpLabel);
              } else if (ev.key === 'Escape') {
                editingTodo = undefined;
                tmpLabel = attrs.todo.label;
                root[Symbol.update]();
              }
            });
            _v41 = _v42;
            _v41[__sym_upd] = () => {
              _v43.value = tmpLabel;
            };
          }
        }
        const _v46 = _v45 ? _v36 : _v41
        if (_v45 !== _v44) __slot_s(_v32, _v33, _v46);
        _v44 = _v45;
        _v46[Symbol.update]?.()
      }
    }
  }
  __style(`
        *[_mvrhbcb5] {
            box-sizing: border-box;
            margin: 0;
            padding: 0;
        }

        input.label-maker[_mvrhbcb5] {
            width: 100%;
            padding: 12px 16px;
            font-size: 16px;
            background: var(--color-background-primary);
            border: 0.5px solid var(--color-border-secondary);
            border-radius: 8px;
            color: var(--color-text-primary);
            outline: none;
        }

        input.label-maker:focus[_mvrhbcb5] {
            border-color: var(--color-border-primary);
            box-shadow: 0 0 0 3px rgba(0, 0, 0, 0.06);
        }

        input.label-maker[_mvrhbcb5]::placeholder {
            color: var(--color-text-tertiary);
        }

        main[_mvrhbcb5] {
            margin-top: 1rem;
            background: var(--color-background-primary);
            border: 0.5px solid var(--color-border-tertiary);
            border-radius: 12px;
            overflow: hidden;
        }

        ul.list[_mvrhbcb5] {
            list-style: none;
        }

        li[_mvrhbcb5] {
            display: flex;
            align-items: center;
            gap: 12px;
            padding: 12px 16px;
            border-bottom: 0.5px solid var(--color-border-tertiary);
        }

        li:last-child[_mvrhbcb5] {
            border-bottom: none;
        }

        li.completed[_mvrhbcb5] label:nth-child(2):where([_mvrhbcb5]) {
            text-decoration: line-through;
            opacity: 0.45;
        }

        li[_mvrhbcb5] label:where([_mvrhbcb5]), li[_mvrhbcb5] input[type="text"]:where([_mvrhbcb5]) {
            display: flex;
            align-items: center;
            gap: 8px;
            font-size: 14px;
            color: var(--color-text-secondary);
            cursor: pointer;
            user-select: none;
        }

        li[_mvrhbcb5] label:nth-child(2):where([_mvrhbcb5]), li[_mvrhbcb5] input[type="text"]:where([_mvrhbcb5]) {
            flex: 1;
        }

        li[_mvrhbcb5] input[type="text"]:where([_mvrhbcb5]) {
            border: none;
            background: transparent;
            font-size: 15px;
            color: var(--color-text-primary);
            outline: none;
            padding: 0;
            pointer-events: none;
        }

        li.editing[_mvrhbcb5] input[type="text"]:where([_mvrhbcb5]) {
            pointer-events: auto;
            border-bottom: 1.5px solid var(--color-border-primary);
            padding-bottom: 2px;
        }

        li[_mvrhbcb5] input[type="checkbox"]:where([_mvrhbcb5]) {
            width: 18px;
            height: 18px;
            accent-color: #534ab7;
            cursor: pointer;
        }

        li[_mvrhbcb5] label:nth-child(3):where([_mvrhbcb5]) button:where([_mvrhbcb5]) {
            background: none;
            border: none;
            cursor: pointer;
            color: var(--color-text-tertiary);
            padding: 4px 6px;
            border-radius: 4px;
            opacity: 0;
            transition: opacity 0.1s, color 0.1s;
        }

        li:hover[_mvrhbcb5] label:nth-child(3):where([_mvrhbcb5]) button:where([_mvrhbcb5]) {
            opacity: 1;
        }

        li[_mvrhbcb5] label:nth-child(3):where([_mvrhbcb5]) button:hover:where([_mvrhbcb5]) {
            color: #e24b4a;
        }

        div.filters[_mvrhbcb5] {
            display: flex;
            gap: 6px;
            padding: 10px 16px;
            border-top: 0.5px solid var(--color-border-tertiary);
        }

        div.filters[_mvrhbcb5] button:where([_mvrhbcb5]) {
            background: none;
            border: 0.5px solid transparent;
            padding: 4px 10px;
            border-radius: 6px;
            font-size: 13px;
            cursor: pointer;
            color: var(--color-text-secondary);
        }

        div.filters[_mvrhbcb5] button:hover:where([_mvrhbcb5]) {
            background: var(--color-background-secondary);
        }

        div.filters[_mvrhbcb5] button.active:where([_mvrhbcb5]) {
            border-color: var(--color-border-secondary);
            color: var(--color-text-primary);
            font-weight: 500;
        }

        main[_mvrhbcb5] > button:where([_mvrhbcb5]) {
            display: block;
            width: 100%;
            padding: 10px 16px;
            background: none;
            border: none;
            border-top: 0.5px solid var(--color-border-tertiary);
            cursor: pointer;
            font-size: 13px;
            color: var(--color-text-secondary);
            text-align: left;
        }

        main[_mvrhbcb5] > button:hover:where([_mvrhbcb5]) {
            background: var(--color-background-secondary);
            color: var(--color-text-primary);
        }

        footer[_mvrhbcb5] {
            margin-top: 12px;
            padding: 0 4px;
        }

        div.count[_mvrhbcb5] {
            font-size: 13px;
            color: var(--color-text-tertiary);
        }
    `);
  return {
    root,
    [__sym_upd]: () => {
      root[__sym_upd]();
    }
  }
}
document.body.append((() => {
  const _v0 = __template(`<div><div _d85ve5wj><!>`)
  let _v1 = _v0.firstChild
  _v1 = _v1.firstChild;
  let _v2
  __style(`
        div[_d85ve5wj] {
            max-width: 500px;
            margin-left: auto;
            margin-right: auto;
            margin-top: 100px;
        }

        
            @media (prefers-color-scheme: dark) {
                :root { color-scheme: dark; }
            }
        

        /* theme vars */
        div[_d85ve5wj] {
            --color-background-primary: light-dark(rgba(255, 255, 255, 1), rgba(48, 48, 46, 1));
            --color-background-secondary: light-dark(rgba(245, 244, 237, 1), rgba(38, 38, 36, 1));
            --color-background-tertiary: light-dark(rgba(250, 249, 245, 1), rgba(20, 20, 19, 1));
            --color-text-primary: light-dark(rgba(20, 20, 19, 1), rgba(250, 249, 245, 1));
            --color-text-secondary: light-dark(rgba(61, 61, 58, 1), rgba(194, 192, 182, 1));
            --color-text-tertiary: light-dark(rgba(115, 114, 108, 1), rgba(156, 154, 146, 1));
            --color-border-primary: light-dark(rgba(31, 30, 29, 0.4), rgba(222, 220, 209, 0.4));
            --color-border-secondary: light-dark(rgba(31, 30, 29, 0.3), rgba(222, 220, 209, 0.3));
            --color-border-tertiary: light-dark(rgba(31, 30, 29, 0.15), rgba(222, 220, 209, 0.15));
        }
    `);
  ;(_v0[__sym_upd] = () => {
    _v2 ??= App();
    _v2[Symbol.update]();
    if (_v1) _v1 = void _v1.replaceWith(_v2.root);
  })();
  return _v0
})());
