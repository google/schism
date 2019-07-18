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

export class SmallInteger {
    constructor(value) {
        this.value = value | 0;
    }
    toJS(heap, map) {
        return this.value;
    }
    copy(old_heap, new_heap, map) {
        return new_heap.getSmallInteger(this.value);
    }
}

export class Pair {
    constructor(car, cdr) {
        this.car = car;
        this.cdr = cdr;
    }
    toJS(heap, map) {
        if (map.has(this)) return map.get(this);
        // Store an empty pair early, to allow for circular
        // references.
        const pair = addToMap(map, this, []);
        pair[0] = heap.get(this.car).toJS(heap, map);
        pair[1] = heap.get(this.cdr).toJS(heap, map);
        return pair;
    }
    copy(old_heap, new_heap, map) {
        if (map.has(this)) return map.get(this);
        // Store an empty pair early, to allow for circular
        // references.
        const copy = new Pair(0, 0);
        const handle = addToMap(map, this, new_heap.put(copy));
        copy.car = old_heap.get(this.car).copy(old_heap, new_heap, map);
        copy.cdr = old_heap.get(this.cdr).copy(old_heap, new_heap, map);
        return handle;
    }
}

export class Character {
    constructor(value) {
        this.value = value | 0;
    }
    toJS(heap, map) {
        return String.fromCharCode(this.value);
    }
    copy(old_heap, new_heap, map) {
        return new_heap.getCharacter(this.value);
    }
}

export class Boolean {
    constructor(value) {
        this.value = value | 0;
    }
    toJS(heap, map) {
        return this.value ? true : false;
    }
    copy(old_heap, new_heap, map) {
        return this.value ? new_heap.trueHandle : new_heap.falseHandle;
    }
}

export class Null {
    toJS(heap, map) {
        return null;
    }
    copy(old_heap, new_heap, map) {
        return new_heap.nullHandle;
    }
}
export class EOF {
    toJS(heap, map) {
        return this;
    }
    copy(old_heap, new_heap, map) {
        return new_heap.eofHandle;
    }
}
export class Void {
    toJS(heap, map) {
        return undefined;
    }
    copy(old_heap, new_heap, map) {
        return new_heap.voidHandle;
    }
}

export class Str {
    constructor(chars) {
        this.chars = chars;
    }
    toString(heap) {
        let res = "";
        for (let chars = heap.get(this.chars);
             !(chars instanceof Null);
             chars = heap.get(chars.cdr)) {
            res += String.fromCharCode(heap.get(chars.car).value);
        }
        return res;
    }
    toJS(heap, map) {
        return this.toString(heap);
    }
    copy(old_heap, new_heap, map) {
        if (map.has(this)) return map.get(this);
        return addToMap(map, this, new_heap.put(new Str(
            old_heap.get(this.chars).copy(old_heap, new_heap, map))));
    }
}

export class Symbol {
    constructor(value) {
        this.value = value;
    }
    toJS(heap, map) {
        return this;
    }
    copy(old_heap, new_heap, map) {
        if (map.has(this)) return map.get(this);
        const value = old_heap.get(this.value)
        const str = value.toString(old_heap);
        const interned = old_heap.symbols.has(str) &&
              old_heap.get(old_heap.symbols.get(str)) == this;
        if (interned && new_heap.symbols.has(str))
            return addToMap(map, this, new_heap.symbols.get(str));
        const copied = value.copy(old_heap, new_heap, map);
        const handle = addToMap(map, this, new_heap.put(new Symbol(copied)));
        if (interned) new_heap.symbols.set(str, handle);
        return handle;
    }
}

export class Closure {
    constructor(index, nfree) {
        this.index = index | 0;
        this.env = [];
    }
    toJS(heap, map) {
        return this;
    }
    copy(old_heap, new_heap, map) {
        if (map.has(this)) return map.get(this);
        const copy = new Closure(this.index, this.env.length);
        const handle = addToMap(map, this, new_heap.put(copy));
        copy.env = this.env.map(h => {
            return old_heap.get(h).copy(old_heap, new_heap, map);
        });
        return handle;
    }
}

function rt(engine, heap) {
    function peek() {
        if (engine.input_index < engine.input_port_data.length) {
            return engine.input_port_data[engine.input_index];
        }
        return -1;
    }

    return {
        // Currently we compile heap object access as indexes into a
        // table, mediated by heap.get and heap.put.  When we get anyref
        // support, heap.get and heap.put can become no-ops.

        // The allocators and accessors should be replaced by the GC
        // proposal.

        getSmallInteger(value) { return heap.getSmallInteger(value); },
        isSmallInteger(x) { return heap.get(x) instanceof SmallInteger; },
        smallIntegerValue(x) { return heap.get(x).value; },

        newPair(car, cdr) { return heap.put(new Pair(car, cdr)); },
        isPair(x) { return heap.get(x) instanceof Pair; },
        getCar(x) { return heap.get(x).car; },
        getCdr(x) { return heap.get(x).cdr; },
        setCar(x, y) { heap.get(x).car = y; },
        setCdr(x, y) { heap.get(x).cdr = y; },

        getCharacter(value) { return heap.getCharacter(value); },
        isCharacter(x) { return heap.get(x) instanceof Character; },
        characterValue(x) { return heap.get(x).value; },

        getFalse() { return heap.falseHandle; },
        getTrue() { return heap.trueHandle; },
        getNull() { return heap.nullHandle; },
        getEOF() { return heap.eofHandle; },
        getVoid() { return heap.voidHandle; },

        newString(chars) { return heap.put(new Str(chars)); },
        isString(x) { return heap.get(x) instanceof Str; },
        stringChars(x) { return heap.get(x).chars; },

        newSymbol(value) { return heap.put(new Symbol(value)); },
        getSymbol(value) {
            return heap.getSymbol(heap.get(value).toString(heap), value);
        },
        isSymbol(x) { return heap.get(x) instanceof Symbol; },
        isInternedSymbol(x) {
            const str = heap.get(heap.get(x).value).toString(heap);
            return heap.symbols.get(str) == x;
        },
        symbolValue(x) { return heap.get(x).value; },

        newClosure(index, nfree) { return heap.put(new Closure(index, nfree)); },
        isClosure(x) { return heap.get(x) instanceof Closure; },
        closureIndex(x) { return heap.get(x).index; },
        initClosureFreeVar(x, i, v) { heap.get(x).env[i] = v; },
        getClosureFreeVar(x, i) { return heap.get(x).env[i]; },

        '%read-char': function () {
            const val = peek();
            if (val >= 0) {
                engine.input_index++;
            }
            return val;
        },
        '%peek-char': peek,
        '%write-char': function (byte) {
            engine.output_data.push(byte);
        },
        'error': function (where, what) {
            if (heap.get(where) instanceof Symbol)
                where = heap.get(heap.get(where).value).toString(heap);
            if (heap.get(what) instanceof Str)
                what = heap.get(what).toString(heap);
            throw new SchemeError(where, what);
        },
        '%log-char': byte => {
            engine.log += String.fromCharCode(byte);
        },
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

class Heap {
    constructor() {
        // This compilation strategy represents gc-managed values using
        // indexes into a table.  get(h) returns the value, given a
        // handle; put(v) allocates and returns a new handle associated
        // with a GC value.
        this.refs = [];
        this.smallIntegers = new Map; // int -> handle
        this.characters = new Map; // codepoint -> handle
        this.symbols = new Map; // string -> handle
        this.falseHandle = this.put(new Boolean(0));
        this.trueHandle = this.put(new Boolean(1));
        this.nullHandle = this.put(new Null);
        this.eofHandle = this.put(new EOF);
        this.voidHandle = this.put(new Void);
    }
    get(handle) { return this.refs[handle]; }
    put(x) { return this.refs.push(x) - 1; }

    getSmallInteger(value) {
        return this.smallIntegers.has(value)
            ? this.smallIntegers.get(value)
            : addToMap(this.smallIntegers, value, this.put(new SmallInteger(value)));
    }
    getCharacter(value) {
        return this.characters.has(value)
            ? this.characters.get(value)
            : addToMap(this.characters, value, this.put(new Character(value)));
    }
    getSymbol(str, value) {
        return this.symbols.has(str)
            ? this.symbols.get(str)
            : addToMap(this.symbols, str, this.put(new Symbol(value)));
    }
}

export class Engine {
    constructor() {
        this.memory = new WebAssembly.Memory({ initial: 4096 });
        this.mem_i32 = new Uint32Array(this.memory.buffer);
        this.heap = new Heap;
        this.rt = rt(this, this.heap);
        this.input_port_data = [];
        this.input_index = 0;
        this.output_data = [];
        this.modules = [];
        this.log = "";
    }

    async loadWasmModule(bytes) {
        const import_object = {
            'rt': this.rt,
            'rt2': this.rt,
            'memory': { 'memory': this.memory }
        };

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

    clearOutputBuffer() {
        this.output_data.length = 0;
    }

    jsFromScheme(handle) {
        const heap = this.heap;
        return heap.get(handle).toJS(heap, new Map);
    }

    schemeFromJs(value) {
        switch (typeof value) {
        case 'number':
            return this.heap.getSmallInteger(value)
        case 'boolean':
            return value ? this.heap.trueHandle : this.heap.falseHandle;
        default:
            throw new SchemeError("schemeFromJs", "unhandled value: " + value);
        }
    }

    get bytesAllocated() {
        // Guess that each object is 16 bytes in length, on average, and
        // that the table entry takes 8 bytes.
        return this.heap.refs.length * 24;
    }

    // Does a basic garbage collection and compaction. Assumes all items in the
    // root set are reachable from root, and returns a pointer into the new heap
    // that is the new location of root.
    collect(root) {
        // const start_bytes = this.bytesAllocated;
        const old_heap = this.heap;
        const new_heap = new Heap;
        const result = old_heap.get(root).copy(old_heap, new_heap, new Map);

        for (let k in new_heap) {
            this.heap[k] = new_heap[k];
        }

        // const end_bytes = this.bytesAllocated;
        // const reclaimed = start_bytes - end_bytes;
        // console.info(`Reclaimed ${reclaimed / 1024 / 1024} MiB (${reclaimed / start_bytes * 100}%)`);

        return result;
    }
}
