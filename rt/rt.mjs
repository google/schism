// -*- javascript -*-
//
// Copyright 2018 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

class SchemeError extends Error {
    constructor(where, what) {
        super(`Scheme runtime error in ${where}: ${what}`);
        this.where = where
        this.what = what
    }
}

function addToMap(map, key, val) {
    map.set(key, val);
    return val;
}

export class Pair {
    constructor(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }
}

export class Closure {
    constructor(index, nfree) {
        this.index = index | 0;
        this.env = [];
    }
}

export class EOF {};
const eofValue = new EOF;

function toJS(x, map) {
    switch (typeof x) {
    case 'number':
        if (x & 1)
            return String.fromCharCode(x >>> 1);
        return x >> 1;
    case 'string':
        if (x.charAt(0) === 's')
            return x.substring(1);
        return x.substring(2);
    case 'boolean':
    case 'undefined':
    case 'null':
        return x;
    case 'object':
        if (x instanceof String) {
            return x.substring(2);
        }
        if (x instanceof Pair) {
            map = map || new Map;
            if (map.has(x)) return map.get(x);
            // Store an empty pair early, to allow for circular
            // references.
            const pair = addToMap(map, x, []);
            pair[0] = toJS(x.car, map);
            pair[1] = toJS(x.cdr, map);
            return pair;
        }
        return x;
    default:
        throw new SchemeError("schemeFromJs", "unhandled value: " + value);
    }
}

function rt(engine) {
    function peek() {
        if (engine.input_index < engine.input_port_data.length) {
            return engine.input_port_data[engine.input_index];
        }
        return -1;
    }

    function makeString(str) { return 's' + str; }
    function makeSymbol(str) { return 'S' + str; }
    function stringValue(x) { return x.substring(1); }
    function symbolValue(x) { return x.substring(1); }

    return {
        // The allocators and accessors should be replaced by the GC
        // proposal.

        // FIXME: It would be nice if these refs were eq-able, but the
        // reference types proposal doesn't include that.
        'eq?': (x, y) => x === y,

        // Small integers and characters are represented as tagged
        // numbers.  This allows them to be compared using ===.
        'number?': x => typeof(x) == 'number' && (x & 1) === 0,
        'char?': x => typeof(x) == 'number' && (x & 1) === 1,

        '%make-number': n => n << 1,
        '%make-char': n => (n << 1) | 1,

        '%number-value': n => n >> 1,
        '%char-value': n => n >>> 1,

        // Strings and symbols are represented as tagged strings.  Using
        // JS strings for strings is a little bit better on memory
        // usage.  Though it allows us to compare values with ===,
        // that's not specified on the Scheme language level.  Using JS
        // strings for symbols allows us to compare with ===, as is
        // required.
        'string?': x => typeof(x) === 'string' && x.charAt(0) === 's',
        '%symbol?': x => typeof(x) === 'string' && x.charAt(0) === 'S',

        // The code still wants to access chars as a list.
        '%list->string': chars => {
            let ret = 's';
            for (; chars != null; chars = chars.cdr)
                ret += String.fromCharCode(chars.car >>> 1);
            return ret;
        },
        '%string->list': x => {
            let ret = null;
            for (let i = x.length - 1; i > 0; i--)
                ret = new Pair((x.charCodeAt(i) << 1) | 1, ret);
            return ret;
        },

        '%string->symbol': value => 'S' + value,
        '%symbol->string': x => x.substring(1),

        // Gensyms are instances of String (objects with identity).
        '%make-gensym': str => new String('S' + str),
        '%gensym?': x => x instanceof String,

        'cons': (car, cdr) => new Pair(car, cdr),
        'pair?': x => x instanceof Pair,
        '%car': x => x.car,
        '%cdr': x => x.cdr,
        '%set-car!': (x, y) => x.car = y,
        '%set-cdr!': (x, y) => x.cdr = y,

        '%get-false': () => false,
        '%get-true': () => true,
        '%get-null': () => null,
        'eof-object': () => eofValue,
        '%get-void': () => undefined,

        '%make-closure': (index, nfree) => new Closure(index, nfree),
        'procedure?': x => x instanceof Closure,
        '%closure-index': x => x.index,
        '%set-closure-free-var!': (x, i, v) => x.env[i] = v,
        '%closure-free-var': (x, i) => x.env[i],

        '%read-char': () => {
            const val = peek();
            if (val >= 0) {
                engine.input_index++;
            }
            return val;
        },
        '%peek-char': peek,
        '%write-char': byte => engine.output_data.push(byte),
        'error': function (where, what) {
            throw new SchemeError(where, what);
        },
        '%log-char': byte => engine.log += String.fromCharCode(byte),
        '%flush-log': () => {
            console.info(engine.log);
            engine.log = "";
        }
    }
}

class Module {
    get exports() {
        return this.wasm_instance.exports;
    }
}

export class Engine {
    constructor() {
        this.rt = rt(this);
        this.input_port_data = [];
        this.input_index = 0;
        this.output_data = [];
        this.modules = [];
        this.log = "";
    }

    async loadWasmModule(bytes) {
        const import_object = { 'rt': this.rt };
        const result = await WebAssembly.instantiate(bytes, import_object);

        let schism_module = new Module();
        schism_module.wasm_instance = result.instance;
        schism_module.engine = this;
        this.modules.push(schism_module);

        return schism_module;
    }

    setCurrentInputPort(data) {
        this.input_port_data = data;
        this.input_index = 0;
    }

    setCurrentInputPortChars(chars) {
        const data = []
        for (let c of chars) {
            data.push(c.charCodeAt(0));
        }
        this.setCurrentInputPort(data);
    }

    clearOutputBuffer() {
        this.output_data.length = 0;
    }

    jsFromScheme(x) { return toJS(x); }

    schemeFromJs(value) {
        switch (typeof value) {
        case 'number':
            return value << 1;
        case 'boolean':
        case 'undefined':
        case 'null':
            return value;
        default:
            throw new SchemeError("schemeFromJs", "unhandled value: " + value);
        }
    }
}
