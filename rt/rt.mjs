const TAG_SIZE = 3;
const TAGS = {
    fixnum: 0,
    constant: 1
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
    1: true
}

// Convert a Scheme ptr into a corresponding JavaScript value
export function js_from_scheme(ptr) {
    switch (extract_tag(ptr)) {
    case TAGS.fixnum:
	return ptr >> TAG_SIZE; // sign extending shift so negatives work.
    case TAGS.constant:
	return SCHEME_CONSTANTS[extract_value(ptr)];
    }
}

function fixnum_from_number(n) {
  return n << TAG_SIZE;
}

export const rt = {
  'rt-add1': function(ptr) {
    // This is a trivial function that is mostly used to test function imports.
    return fixnum_from_number(js_from_scheme(ptr) + 1);
  }
};
