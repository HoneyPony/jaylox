#ifndef JAYLIB_H
#define JAYLIB_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

struct jay_value;

typedef struct {
	size_t name;
	struct jay_value value;
} jay_hash_entry;

typedef struct jay_instance {
	jay_hash_entry *members;
	size_t array_size;

	// The parent closure of this instance, if we're a callable.
	jay_instance *closure;

	// The class of this instance, or the superclass if we're a class.
	jay_instance *class;

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
	struct jay_value (*callable)(jay_value *args, jay_instance *closure); 
} jay_instance;

typedef struct jay_value (*jay_fn)(jay_value *args, jay_instance *closure);

#define JAY_NIL 0
#define JAY_TRUE 1
#define JAY_FALSE 2
#define JAY_NUMBER 3
#define JAY_CALLABLE 4
#define JAY_INSTANCE 5
#define JAY_STRING 6

#define JAY_NAME_TOMBSTONE ((size_t)0xFFFFFFFF)

typedef struct jay_value {
	uint64_t tag;
	union {
		double        as_double;
		jay_instance *as_instance;
		char         *as_string;
	};
} jay_value;

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

jay_value
jay_lookup(jay_instance *instance, size_t name) {

}

jay_instance*
jay_find_method(jay_instance *class, size_t name) {

}

jay_value
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
}

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
// 
*/

#endif