// -*- javascript -*-
const TAG_SIZE = 3;
const TAGS = {
  fixnum: 0,
  constant: 1,
  pair: 2,
  character: 3,
};

function tag_constant(value, tag) {
    return (value << TAG_SIZE) | tag;
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
  true:  tag_constant(1, TAGS.constant),
  null:  tag_constant(2, TAGS.constant),
  eof:   tag_constant(3, TAGS.constant)
}

// Convert a Scheme ptr into a corresponding JavaScript value
export function js_from_scheme(ptr) {
    switch (extract_tag(ptr)) {
    case TAGS.fixnum:
	return ptr >> TAG_SIZE; // sign extending shift so negatives work.
    case TAGS.constant:
	return SCHEME_CONSTANTS[extract_value(ptr)];
    case TAGS.character:
	return String.fromCharCode(extract_value(ptr));
    }
}

function fixnum_from_number(n) {
  return n << TAG_SIZE;
}

let input_port_data = []
let input_index = 0;

export function set_current_input_port(data) {
  input_port_data = data;
  input_index = 0;
}

export let output_data = []

export const rt = {
    'rt-add1': function(ptr) {
	// This is a trivial function that is mostly used to test function imports.
	return fixnum_from_number(js_from_scheme(ptr) + 1);
    },
    'read-char': function() {
	if (input_index < input_port_data.length) {
	    return tag_constant(input_port_data[input_index++], TAGS.character);
	}
	return CONSTANTS.eof;
    },
    'peek-char': function() {
	if (input_index < input_port_data.length) {
	    return tag_constant(input_port_data[input_index], TAGS.character);
	}
	return CONSTANTS.eof;
    },
    'write-char': function(ptr) {
	const byte = js_from_scheme(ptr).charCodeAt(0);
	output_data.push(byte);
    }
};
