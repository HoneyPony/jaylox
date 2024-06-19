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

typedef struct jay_value {
	uint64_t tag;
	union {
		double               as_double;
		struct jay_instance *as_instance;
		char                *as_string;
	};
} jay_value;

typedef struct {
	size_t name;
	struct jay_value value;
} jay_hash_entry;

typedef struct jay_instance {
	jay_hash_entry *members;
	size_t array_size;
	size_t used_entries;

	// The parent closure of this instance, if we're a callable.
	struct jay_instance *closure;

	// The class of this instance, or the superclass if we're a class.
	struct jay_instance *class;

	// For efficiency:
	// Callables are just instances. The closure is the jay_instance object,
	// And the function and arity are store here too.
	//
	// This makes it possible to make jay_value always take up 8 bytes for the
	// value (so with NaN-boxing, it could be 8 bytes always).
	//
	// For classes, this makes sense anyways, as the initializer *is* the
	// callable.
	int arity;
	jay_value (*callable)(jay_value *args, struct jay_instance *closure); 
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

/* --- Operators --- */

jay_value
jay_call(jay_value callee, size_t arity, jay_value *args) {
	jay_instance *instance = jay_as_callable(callee, "function calling expects a callable");
	
	if(instance->arity != arity) {
		oops("incorrect number of arguments");
	}

	return instance->callable(args, instance->closure);
}

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

// Creates a blank instance object with room for values in its table.
jay_instance*
jay_new_instance() {
	// TODO: GC considerations, etc.
	jay_instance *instance = jay_malloc(sizeof(*instance));
	instance->array_size = 8;
	instance->members = jay_malloc(sizeof(jay_hash_entry) * instance->array_size);
	for(size_t i = 0;i < instance->array_size; ++i) {
		// TODO: Use memset or something
		instance->members[i].name = JAY_NAME_TOMBSTONE;
	}
	instance->closure = NULL;
	instance->class = NULL;
	instance->callable = NULL;
	instance->arity = 0;

	instance->used_entries = 0;
}

jay_value
jay_new_class_instance(jay_value class_value) {
	jay_instance *class = jay_as_class(class_value, "only classes can be instantiated.");

	jay_value v;
	v.as_instance = jay_new_instance();
	v.as_instance->class = class;
	v.tag = JAY_INSTANCE;
	return v;
}

jay_instance*
jay_new_scope(jay_instance *closure) {
	jay_instance *instance = jay_new_instance();
	instance->closure = closure;
	return instance;
}

// Creates a new function object from a closure and a function pointer.
jay_value
jay_fun_from(jay_fn fn, size_t arity, jay_instance *closure) {
	jay_instance *instance = jay_new_instance();
	instance->callable = fn;
	instance->arity = arity;
	instance->closure = closure;

	jay_value result;
	result.tag = JAY_FUNCTION;
	result.as_instance = instance;
	return result;
}

jay_instance*
jay_instance_clone(jay_instance *other) {
	jay_instance *instance = jay_malloc(sizeof(*instance));

	instance->array_size = other->array_size;
	size_t bytes = instance->array_size * sizeof(*instance->members);
	// TODO: calloc...? I guess not here...
	instance->members = jay_malloc(bytes);
	memcpy(instance->members, other->members, bytes);

	instance->arity = other->arity;
	instance->callable = other->callable;

	instance->closure = other->closure;
	instance->class = other->class;
}

jay_value
jay_class_from(jay_instance *template, jay_instance *closure, jay_value superclass) {
	jay_instance *class = jay_instance_clone(template);

	// The two things not from the template are the closure and the superclass.
	class->closure = closure;
	
	if(jay_is_null(superclass)) {
		// Null superclass
		class->class = NULL;
	}
	else {
		class->class = jay_as_class(superclass, "superclass must be a class");
	}

	return jay_class(class);
}

// NOTE: This function is UNSAFE to call if you haven't guaranteed that there
// is an empty bucket
jay_hash_entry*
jay_find_empty_bucket_in(jay_hash_entry *array, size_t array_size, size_t name) {
	// TODO: Optimize to use &
	size_t i = name % array_size;
	for(;;) {
		i = (i + 1) % array_size;

		// We MUST find a tombstone to exit the loop.
		if(array[i].name == JAY_NAME_TOMBSTONE) {
			return &array[i];
		}
	}
}

// NOTE: This function is UNSAFE to call if you haven't guaranteed that there
// is an empty bucket
jay_hash_entry*
jay_find_empty_bucket(jay_instance *instance, size_t name) {
	return jay_find_empty_bucket_in(instance->members, instance->array_size, name);
}

jay_hash_entry*
jay_find_bucket_in(jay_hash_entry *array, size_t array_size, size_t name) {
	// TODO: Optimize to use &
	size_t i = name % array_size;
	size_t check = i;
	while(array[i].name != name) {
		// Linear probing
		i = (i + 1) % array_size;

		// Item doesn't exist
		if(i == check || array[i].name == JAY_NAME_TOMBSTONE) {
			return NULL;
		}
	}

	return &array[i];
}

jay_hash_entry*
jay_find_bucket(jay_instance *instance, size_t name) {
	return jay_find_bucket_in(instance->members, instance->array_size, name);
}

void
jay_rehash(jay_instance *instance) {
	size_t new_size = instance->array_size * 2;
	// TODO: calloc()
	jay_hash_entry *new_array = jay_malloc(sizeof(*new_array) * new_size);

	for(size_t i = 0; i < instance->array_size; ++i) {
		if(instance->members[i].name != JAY_NAME_TOMBSTONE) {
			// Note: here this should always return a tombstone, as all buckets
			// should be empty...
			jay_hash_entry *ptr = jay_find_empty_bucket_in(new_array, new_size, instance->members[i].name);
			ptr->name = instance->members[i].name;
			ptr->value = instance->members[i].value;
		}
	}

	free(instance->members);

	instance->members = new_array;
	instance->array_size = new_size;
}

jay_value
jay_put_new(jay_instance *scope, size_t name, jay_value value) {
	if((scope->used_entries + 1) > scope->array_size / 2) {
		jay_rehash(scope);
	}
	// We are guaranteed an empty bucket somewhere.
	// TODO: Consider Hopgood-Davenport probing
	jay_hash_entry *place = jay_find_empty_bucket(scope, name);
	place->name = name;
	place->value = value;

	scope->used_entries += 1;

	return value;
}

jay_value
jay_put_existing(jay_instance *scope, size_t name, jay_value value) {
	jay_hash_entry *place = jay_find_bucket(scope, name);
	if(!place) {
		if(scope->closure) {
			return jay_put_existing(scope->closure, name, value);
		}
		printf("when assigning <%zu>:\n", name);
		oops("assign: could not find the given name");
	}
	place->value = value;
	return value;
}

jay_value
jay_lookup(jay_instance *instance, size_t name) {
	jay_hash_entry *place = jay_find_bucket(instance, name);
	if(!place) {
		if(instance->closure) {
			return jay_lookup(instance->closure, name);
		}
		printf("when looking up <%zu>:\n", name);
		oops("lookup: could not find the given name");
	}
	return place->value;
}

/* --- Instance (of a class) related lookups --- */

jay_instance*
jay_find_method(jay_instance *class, size_t name) {
	jay_hash_entry *place = jay_find_bucket(class, name);
	if(place) {
		// Classes are guaranteed to only store functions.
		return jay_as_callable(place->value, "classes only store functions.");
	}

	if(class->class) {
		// Lookup on superclass if needed.
		return jay_find_method(class->class, name);
	}

	// No such method.
	return NULL;
}

jay_instance*
jay_bind_method(jay_instance *method, jay_instance *closure) {
	jay_instance *bound = jay_instance_clone(method);
	bound->closure = closure;
	return bound;
}

static size_t JAY_THIS;

jay_value
jay_get(jay_value object, size_t name) {
	jay_instance *instance = jay_as_instance(object, "can only look up fields on objects");

	jay_instance *method = jay_find_method(instance->class, name);
	if(method) {
		// Create a new closure with 'this'
		jay_instance *closure = jay_new_scope(instance->class->closure);
		jay_put_new(closure, JAY_THIS, object);
		return jay_function(jay_bind_method(method, closure));
	}

	// If there is no method, then look up the field... TODO, some kind of
	// static analysis..?
	// Note: This will oops when the value doesn't exist. But that's expected.
	return jay_lookup(instance, name);
}

jay_value
jay_set(jay_value object, size_t name, jay_value value) {
	jay_instance *instance = jay_as_instance(object, "can only set fields on objects");

	jay_hash_entry *place = jay_find_bucket(instance, name);
	if(!place) {
		jay_put_new(instance, name, value);
	}
	else {
		place->value = value;
	}
}

/* --- Builtin Functions (e.g. clock) --- */

jay_value
jay_std_clock(jay_value *args, jay_instance *closure) {
	clock_t time = clock();
	double ms = (time * 1000.0 / CLOCKS_PER_SEC);
	return jay_number(ms);
}

/*jay_value
jay_get(jay_value object, size_t name) {
	jay_instance *instance = jay_as_instance(object, "can only look up members on objects");

	name = name % instance->array_size;
	size_t check = name;
	while(instance->members[name].name != name) {
		name = (name + 1) % instance->array_size;
		if(name == check || instance->members[name].name == JAY_NAME_TOMBSTONE) {
			oops("tried to get non-existent member");
		}
	}

	return instance->members[name].value;
}*/

/*
fun example(param) {
	return param + 3;
}

fun make_cool(param) {
	fun inner(a, b) {
		return param + a + b;
	}

	return inner;
}

---

jay_value
example(jay_value *args, jay_instance *closure) {
	return jay_add(args[0], jay_number(3));
}

jay_value
inner(jay_value *args, jay_instance *closure) {
	return jay_add(
		jay_get(closure, NAME_PARAM),
		jay_add(args[0], args[1])
	);
}

jay_value
make_cool(jay_value *args, jay_instance *closure) {
	jay_instance *closure_a = jay_new_closure();
	jay_set(closure_a, NAME_PARAM, args[0]);

	return jay_callable_from(inner, 2, closure_a);
}

// Each top-level fun has a jay_instance so that there is no need for extra
// allocation
jay_instance make_cool_fn = {
	.members = NULL,
	.array_size = 0,

	.arity = 1,
	.callable = make_cool
};

So each instance might need both a class and a closure. In particular, consider:

fun example(param) {
	class Inner {
		fun init(x) {
			this.value = param + x;
		}
	}

	return Inner;
}

// The 'closure' passed here is the closure created by the class to bind the 'this'
// and 'super' properties. The closure will always contain a variable 'this', which
// points to the actual instance of the class. 
jay_value
init_blahblah(jay_value *args, jay_instance *closure) {
	jay_instance *scope = make_closure(closure);

	jay_set(
		jay_get(scope, NAME_this),
		NAME_value,
		jay_add(args[0], jay_get(scope, NAME_param))
	)
}

// So essentially:
// Each function is turned into a top-level C function that calls jay_new_scope()
// or something
//
// Each class is similarly turned into a top-level C function, one that calls
// the init method
//
// The code inside a function is 
*/

#endif