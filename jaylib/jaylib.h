#ifndef JAYLIB_H
#define JAYLIB_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

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
#define JAY_CALLABLE 4
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
jay_as_callable(jay_value value, size_t arity) {
	if(value.tag != JAY_CALLABLE && value.tag != JAY_CLASS) {
		oops("not a callable object");
	}

	if(value.as_instance->arity != arity) {
		oops("incorrect number of arguments");
	}

	return value.as_instance;
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

/* --- Operators --- */

jay_value
jay_add(jay_value a, jay_value b) {

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

jay_instance*
jay_new_scope(jay_instance *closure) {
	jay_instance *instance = jay_new_instance();
	instance->closure = closure;
	return instance;
}

// Creates a new function object from a closure and a function pointer.
jay_value
jay_fun_from(jay_fn fn, jay_instance *closure) {
	jay_instance *instance = jay_new_instance();
	instance->callable = fn;
	instance->closure = closure;

	jay_value result;
	result.tag = JAY_CALLABLE;
	result.as_instance = instance;
	return result;
}

// NOTE: This function is UNSAFE to call if you haven't guaranteed that there
// is an empty bucket
jay_hash_entry*
jay_find_empty_bucket_in(jay_hash_entry *array, size_t array_size, size_t name) {
	// TODO: Optimize to use &
	name = name % array_size;
	for(;;) {
		name = (name + 1) % array_size;

		// We MUST find a tombstone to exit the loop.
		if(array[name].name == JAY_NAME_TOMBSTONE) {
			return &array[name];
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
	name = name % array_size;
	size_t check = name;
	while(array[name].name != name) {
		// Linear probing
		name = (name + 1) % array_size;

		// Item doesn't exist
		if(name == check || array[name].name == JAY_NAME_TOMBSTONE) {
			return NULL;
		}
	}

	return &array[name];
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

	return value;
}

jay_value
jay_put_existing(jay_instance *scope, size_t name, jay_value value) {
	jay_hash_entry *place = jay_find_bucket(scope, name);
	if(!place) {
		oops("could not find the given name");
	}
	place->value = value;
	return value;
}

jay_value
jay_lookup(jay_instance *instance, size_t name) {
	jay_hash_entry *place = jay_find_bucket(instance, name);
	if(!place) {
		oops("could not find the given name");
	}
	return place->value;
}

jay_instance*
jay_find_method(jay_instance *class, size_t name) {

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