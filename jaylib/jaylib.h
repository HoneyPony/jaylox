#ifndef JAYLIB_H
#define JAYLIB_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

struct jay_value;
struct jay_instance;
struct jay_closure;

typedef struct jay_value {
	uint64_t tag;
	union {
		double               as_double;
		struct jay_instance *as_instance;
		char                *as_string;
	};
} jay_value;

typedef jay_value (*jay_function_impl)(jay_value *args, struct jay_closure *closure);

typedef struct jay_object {
	jay_object *gc_next;
	jay_object *gc_prev;
	uint32_t type;
	uint32_t gc_info;
} jay_object;

typedef struct jay_closure {
	jay_object object;
	struct jay_closure *parent;

	size_t count;
	jay_value values[]; // NOTE: Requires C11
} jay_closure;

typedef struct jay_stackframe {
	size_t count;
	jay_value values[];
} jay_stackframe;

typedef struct jay_function {
	jay_object object;
	jay_object *closure;
	jay_function_impl implementation;
	size_t arity;
} jay_function;

#define JAY_STACK_SIZE 1048576

static jay_value jay_stack[JAY_STACK_SIZE];
static jay_value *jay_stack_ptr;

static jay_stackframe *jay_frames[4096];
static size_t jay_frames_ptr;

static inline
void
jay_push_frame(void *ptr) {
	jay_frames[jay_frames_ptr++] = ptr;
}

static inline
void
jay_pop_frame() {
	jay_frames_ptr -= 1;
}

static inline
void
jay_push(jay_value v) {
	*jay_stack_ptr = v;
	jay_stack_ptr++;
}

static inline
jay_value
jay_pop() {
	jay_stack_ptr -= 1;
	return *jay_stack_ptr;
}

void
jay_call(struct jay_function *fun, size_t arity) {
	if(arity != fun->arity) {
		oops("wrong arity");
	}

	// The arguments remain on the stack until the function returns.
	jay_value result = fun->implementation(jay_stack_ptr - arity, fun->closure);

	jay_stack_ptr -= arity;
	jay_push(result);
}

/*
jay_value
example_fn(jay_value *args, jay_closure *closure) {
	struct {
		size_t count;
		jay_value values[10];
	} frame;
	jay_closure *scope = jay_new_closure(closure, 5);
	jay_push_frame((jay_stackframe*)&frame);

	frame.values[0] = jay_pop();

	jay_pop_frame();
}
*/

jay_value
other_example(jay_value *args, jay_closure *closure) {
	struct {
		size_t count;
		jay_value values[10];
	} locals;
	locals.count = 10;


}

typedef struct {
	size_t name;
	struct jay_value value;
} jay_hash_entry;

typedef struct jay_instance {
	jay_object object;

	jay_hash_entry *members;
	size_t array_size;
	size_t used_entries;

	// The class of this instance, or the superclass if we're a class.
	struct jay_instance *class;
} jay_instance;

typedef jay_value (*jay_fn)(jay_value *args, jay_instance *closure);

#define JAY_NIL 0
#define JAY_TRUE 1
#define JAY_FALSE 2
#define JAY_NUMBER 3
#define JAY_FUNCTION 4
#define JAY_INSTANCE 5
#define JAY_STRING 6
#define JAY_CLASS 7

// Use 0 for the tombstone so that we can memset() the array to 0 / use calloc.
#define JAY_NAME_TOMBSTONE 0



void
oops(const char *message) {
	printf("runtime error: %s\n", message);
	exit(1);
}

jay_instance*
jay_as_instance(jay_value value, const char *message) {
	if(value.tag != JAY_INSTANCE) {
		oops(message);
	}

	return value.as_instance;
}

jay_instance*
jay_as_callable(jay_value value, const char *message) {
	if(value.tag != JAY_FUNCTION && value.tag != JAY_CLASS) {
		oops(message);
	}

	return value.as_instance;
}

jay_instance*
jay_as_class(jay_value value, const char *message) {
	if(value.tag != JAY_CLASS) {
		oops(message);
	}

	return value.as_instance;
}

bool
jay_is_null(jay_value value) {
	return value.tag == JAY_NIL;
}

double
jay_as_number(jay_value value, const char *message) {
	if(value.tag != JAY_NUMBER) {
		oops(message);
	}

	return value.as_double;
}

/* --- Literals --- */

jay_value
jay_null() {
	jay_value res;
	res.tag = JAY_NIL;
	return res;
}

jay_value
jay_number(double input) {
	jay_value res;
	res.tag = JAY_NUMBER;
	res.as_double = input;
	return res;
}

jay_value
jay_boolean(bool input) {
	jay_value res;
	res.tag = input ? JAY_TRUE : JAY_FALSE;
	return res;
}

jay_value
jay_class(jay_instance *class) {
	jay_value res;
	res.tag = JAY_CLASS;
	res.as_instance = class;
	return res;
}

jay_value
jay_function(jay_instance *function) {
	jay_value res;
	res.tag = JAY_FUNCTION;
	res.as_instance = function;
	return res;
}

jay_value
jay_string_into(char *ptr) {
	jay_value res;
	res.tag = JAY_STRING;
	res.as_string = ptr;
	return res;
}

jay_value
jay_string(const char *literal) {
	jay_value res;
	res.tag = JAY_STRING;

	size_t bytes = strlen(literal) + 1;
	res.as_string = malloc(bytes);
	memcpy(res.as_string, literal, bytes);

	return res;
}

/* --- Operators --- */

void
jay_print(jay_value value) {
	switch(value.tag) {
		case JAY_NIL:
			puts("nil");
			break;
		case JAY_STRING:
			puts(value.as_string);
			break;
		case JAY_TRUE:
			puts("true");
			break;
		case JAY_FALSE:
			puts("false");
			break;
		case JAY_NUMBER:
			printf("%f\n", value.as_double);
			break;
		default:
			puts("<ref value>");
	}
}

jay_value
jay_add(jay_value a, jay_value b) {
	const char *message = "addition expects two numbers or two strings";
	if(a.tag == JAY_NUMBER) {
		double an = jay_as_number(a, message);
		double bn = jay_as_number(b, message);
		return jay_number(an + bn);
	}
	oops("string addition is TODO");
}

jay_value
jay_sub(jay_value a, jay_value b) {
	const char *message = "subtraction expects two numbers";
	double an = jay_as_number(a, message);
	double bn = jay_as_number(b, message);
	return jay_number(an - bn);
}

jay_value
jay_mul(jay_value a, jay_value b) {
	const char *message = "multiplication expects two numbers";
	double an = jay_as_number(a, message);
	double bn = jay_as_number(b, message);
	return jay_number(an * bn);
}

jay_value
jay_div(jay_value a, jay_value b) {
	const char *message = "division expects two numbers";
	double an = jay_as_number(a, message);
	double bn = jay_as_number(b, message);
	return jay_number(an / bn);
}

jay_value
jay_gt(jay_value a, jay_value b) {
	const char *message = "comparison (>) expects two numbers";
	double an = jay_as_number(a, message);
	double bn = jay_as_number(b, message);
	return jay_boolean(an > bn);
}

jay_value
jay_ge(jay_value a, jay_value b) {
	const char *message = "comparison (>=) expects two numbers";
	double an = jay_as_number(a, message);
	double bn = jay_as_number(b, message);
	return jay_boolean(an >= bn);
}

jay_value
jay_lt(jay_value a, jay_value b) {
	const char *message = "comparison (<) expects two numbers";
	double an = jay_as_number(a, message);
	double bn = jay_as_number(b, message);
	return jay_boolean(an < bn);
}

jay_value
jay_le(jay_value a, jay_value b) {
	const char *message = "comparison (<=) expects two numbers";
	double an = jay_as_number(a, message);
	double bn = jay_as_number(b, message);
	return jay_boolean(an <= bn);
}

bool
jay_truthy(jay_value value) {
	if(value.tag == JAY_FALSE || value.tag == JAY_NIL) return false;
	return true;
}

bool
jay_eq_impl(jay_value a, jay_value b) {
	if(a.tag != b.tag) {
		return false;
	}
	if(a.tag == JAY_NUMBER) {
		return a.as_double == b.as_double;
	}
	if(a.tag == JAY_STRING) {
		// TODO: Decide if we want string interning, etc
		return false;
	}
	// All other types can be compared by identity... I think
	return a.as_instance == b.as_instance;
}

jay_value
jay_neq(jay_value a, jay_value b) {
	return jay_boolean(!jay_eq_impl(a, b));
}

jay_value
jay_eq(jay_value a, jay_value b) {
	return jay_boolean(jay_eq_impl(a, b));
}

jay_value
jay_not(jay_value v) {
	return jay_boolean(!jay_truthy(v));
}

jay_value
jay_negate(jay_value v) {
	double vd = jay_as_number(v, "negation expects a number");
	return jay_number(-vd);
}

void*
jay_malloc(size_t size) {
	void *res = malloc(size);
	if(!res) {
		oops("out of memory");
	}
	return res;
}

/* --- Builtin Functions (e.g. clock) --- */

jay_value
jay_std_clock(jay_value *args, jay_instance *closure) {
	clock_t time = clock();
	double ms = (time * 1000.0 / CLOCKS_PER_SEC);
	return jay_number(ms);
}

#endif