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

const TAG_SIZE = 3;
const TAGS = {
    fixnum: 0,
    constant: 1,
    pair: 2,
    character: 3,
    string: 4,
    symbol: 5,
};

function tag_constant(value, tag) {
    return (value << TAG_SIZE) | tag;
}

function replace_tag(ptr, tag) {
    return tag_constant(extract_value(ptr), tag);
}

function extract_tag(value) {
    return value & ((1 << TAG_SIZE) - 1);
}

function extract_value(value) {
    return value >>> TAG_SIZE;
}

const SCHEME_CONSTANTS = {
    0: false,
    1: true,
    2: null,
    3: "#<eof-object>",
}

const CONSTANTS = {
    false: tag_constant(0, TAGS.constant),
    true: tag_constant(1, TAGS.constant),
    null: tag_constant(2, TAGS.constant),
    eof: tag_constant(3, TAGS.constant)
}

// Convert a Scheme ptr into a corresponding JavaScript value
function js_from_scheme(ptr) {
    switch (extract_tag(ptr)) {
        case TAGS.fixnum:
            return ptr >> TAG_SIZE; // sign extending shift so negatives work.
        case TAGS.constant:
            return SCHEME_CONSTANTS[extract_value(ptr)];
        case TAGS.character:
            return String.fromCharCode(extract_value(ptr));
    }
}

// Convert a JS value into a Scheme ptr
function scheme_from_js(value) {
    const type = typeof value;
    switch(type) {
        case 'number':
            return value << TAG_SIZE;
        case 'boolean':
            return CONSTANTS[value];
        default:
            throw new Error(`Converting a ${type} has not been implemented`);
    }
}

function fixnum_from_number(n) {
    return n << TAG_SIZE;
}

class SchemeError extends Error {
    constructor(where, what) {
        super(`Scheme runtime error in ${where}: ${what}`);
        this.where = where
        this.what = what
    }
}

function rt(engine) {
    function peek() {
        if (engine.input_index < engine.input_port_data.length) {
            const val = engine.input_port_data[engine.input_index];
            return tag_constant(val, TAGS.character);
        }
        return CONSTANTS.eof;
    }

    return {
        'rt-add1': function (ptr) {
            // This is a trivial function that is mostly used to test function imports.
            return fixnum_from_number(js_from_scheme(ptr) + 1);
        },
        'read-char': function () {
            const val = peek();
            if (peek != CONSTANTS.eof) {
                engine.input_index++;
            }
            return val;
        },
        'peek-char': peek,
        'write-char': function (ptr) {
            const byte = js_from_scheme(ptr).charCodeAt(0);
            engine.output_data.push(byte);
        },
        'error': function (where, what) {
            throw new SchemeError(engine.schemeToString(where), engine.schemeToString(what));
        },
        '%log-char': c => {
            engine.log += engine.jsFromScheme(c);
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

export class Engine {
    constructor() {
        this.memory = new WebAssembly.Memory({ initial: 4096 });
        this.mem_i32 = new Uint32Array(this.memory.buffer);
        this.rt = rt(this);
        this.input_port_data = [];
        this.input_index = 0;
        this.output_data = [];
        this.modules = [];
        this.log = "";
    }

    async loadWasmModule(bytes) {
        const import_object = {
            'rt': this.rt,
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

    jsFromScheme(ptr) {
        return js_from_scheme(ptr);
    }

    schemeFromJs(value) {
        return scheme_from_js(value);
    }

    carOf(ptr) {
        return this.mem_i32[extract_value(ptr) * 2];
    }

    cdrOf(ptr) {
        return this.mem_i32[extract_value(ptr) * 2 + 1];
    }
    schemeToString(ptr) {
        const tag = extract_tag(ptr);

        if (tag == TAGS.symbol) {
            const sym_val = this.carOf(ptr);
            if (sym_val == CONSTANTS[null]) {
                return `<gensym #{extract_value(ptr)}>`;
            }
            return this.schemeToString(sym_val);
        } else if (tag == TAGS.string) {
            let x = ptr;
            let s = "";
            while (x != CONSTANTS[null]) {
                const c = this.jsFromScheme(this.carOf(x));
                s += c;
                x = this.cdrOf(x);

                if (s.length > 100) return x;
            }
            return s;
        } else {
            throw new Error("To string not implemented for tag " + tag);
        }
    }

    get bytesAllocated() {
        return this.mem_i32[0];
    }

    // Does a basic garbage collection and compaction. Assumes all items in the
    // root set are reachable from root, and returns a pointer into the new heap
    // that is the new location of root.
    collect(root) {
        const start_bytes = this.bytesAllocated;
        const from_space = this.mem_i32.slice();
        const mem_i32 = this.mem_i32;
        const forwards = new Map;

        function alloc(num_words) {
            const ptr = mem_i32[0];
            mem_i32[0] += num_words * 4;
            return ptr + 8;
        }

        function deep_copy(ptr) {
            if (forwards.has(ptr)) {
                return forwards.get(ptr);
            }
            const tag = extract_tag(ptr);
            switch (tag) {
                case TAGS.fixnum: return ptr;
                case TAGS.constant: return ptr;
                case TAGS.pair: {
                    let car = from_space[extract_value(ptr) * 2];
                    let cdr = from_space[extract_value(ptr) * 2 + 1];
                    car = deep_copy(car);
                    cdr = deep_copy(cdr);
                    const p = alloc(2);
                    mem_i32[p / 4] = car;
                    mem_i32[p / 4 + 1] = cdr;
                    const new_ptr = replace_tag(p, TAGS.pair);
                    forwards.set(ptr, new_ptr);
                    return new_ptr;
                }
                case TAGS.character: return ptr;
                case TAGS.string: {
                    return replace_tag(deep_copy(replace_tag(ptr, TAGS.pair)), TAGS.string);
                }
                case TAGS.symbol: {
                    return replace_tag(deep_copy(replace_tag(ptr, TAGS.pair)), TAGS.symbol);
                }
                default:
                    throw new Error(`Unrecognized object tag ${tag}`);
            }
        }

        // Reset the allocation pointer.
        mem_i32[0] = 0;
        // Copy and relocate the symbol table.
        mem_i32[1] = deep_copy(mem_i32[1]);
        // Copy the root.
        const result = deep_copy(root);

        //const end_bytes = this.bytesAllocated;
        //const reclaimed = start_bytes - end_bytes;
        //console.info(`Reclaimed ${reclaimed / 1024 / 1024} MiB (${reclaimed / start_bytes * 100}%)`);

        return result;
    }
}
