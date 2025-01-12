#ifndef JAYLIB_H
#define JAYLIB_H

// Options available for jaylib.h:
//
// #define JAY_NAN_BOXING
// - Switches the internal value representation from a 16-byte tag+value
//   to an 8-byte NaN boxed value. Generally improves code speed.
//
// #define JAY_GC_STRESS_TEST
// - Causes every allocation to perform a garbage collection. Useful for sniffing
//   out GC bugs.
//   (Note that most GC bugs are due to calling a possibly-allocating function
//   while holding on to a reference without push() or harbor()ing it)
//
// #define JAY_ASSERT_GC
// - Turns on some internal GC safety checks. Can be used to find some seriously
//   broken things.
//
// #define JAY_MAX_FIELD
// - Should be defined to be 1 more than the maximum number for a field entry.
//   E.g. if field names are x = 1, y = 2, z = 3, then JAY_MAX_FIELD should be 4,
//   so that (name < JAY_MAX_FIELD) will always return true for any name used
//   as a field name.

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <assert.h>

struct jay_value;
struct jay_instance;
struct jay_closure;
struct jay_function;
struct jay_method;
struct jay_string;
struct jay_class;

typedef uint32_t jay_name;

#ifndef JAY_MAX_FIELD
	// If this value hasn't been defined, just define a default so that IDEs are happier
	#define JAY_MAX_FIELD 0
#endif

#ifdef JAY_ASSERT_GC
	#define jay_gc_assert(...) assert(__VA_ARGS__)
#else
	#define jay_gc_assert(...)
#endif

#ifndef JAY_NAN_BOXING

typedef struct jay_value {
	uint64_t tag;
	union {
		double                   as_number;
		struct jay_instance     *as_instance;
		struct jay_function     *as_function;
		struct jay_bound_method *as_bound_method;
		struct jay_class        *as_class;
		struct jay_string       *as_string;
		struct jay_object       *as_object;    
	};
} jay_value;

#else

typedef uint64_t jay_value;

#endif

typedef jay_value (*jay_function_impl)(jay_value *args, struct jay_closure *closure);

// The "dispatcher" is the function that looks up methods inside a class. It
// essentially maps from NAME_ constants to offsets inside that classes stored
// jay_function array. 
typedef struct jay_method* (*jay_dispatcher_impl)(struct jay_class *class, jay_name name);

typedef struct jay_object {
	uint64_t gc;
} jay_object;

typedef struct jay_closure {
	jay_object object;
	struct jay_closure *parent;

	size_t count;
	jay_value values[]; // NOTE: Requires C11
} jay_closure;

typedef struct jay_stackframe {
	size_t count;
	jay_closure *gc_scope;
	jay_value values[];
} jay_stackframe;

typedef struct jay_function {
	jay_object object;
	jay_closure *closure;
	jay_function_impl implementation;
	size_t arity;
} jay_function;

typedef struct jay_method {
	// Unlike a function, a jay_method is not a jay_object

	// Hold on to the class for the garbage collector and to find the closure
	struct jay_class *parent;
	jay_function_impl implementation;
	size_t arity;
} jay_method;

typedef struct jay_class {
	jay_object object;

	struct jay_class *superclass;

	// The dispatcher is how a class actually looks up functions.
	jay_dispatcher_impl dispatcher;

	// The same closure is used for all methods inside a class
	jay_closure *closure;

	size_t methods_count;

	// The first method inside each class will be its init.
	jay_method methods[];
} jay_class;

typedef struct jay_bound_method {
	jay_object object;

	// The bound method is everything needed to call a method with a given
	// 'this'.
	// It does not need to hold on to the jay_class or the jay_method, because
	// all it actually needs is the method implementation and arity, as well
	// as the closure for that method.
	//
	// So, it is possible for a jay_bound_method to exist when both the associated
	// class and method have been garbage collected. Maybe at some point a demo
	// program showing this can be added.
	//
	// Of course, ideally, we just call the method directly without first allocating
	// a new jay_bound_method, which is just more allocation pressure (and GC pressure).
	// Note that this same optimization is in Crafting Interpreters, it is definitely
	// an important one.
	jay_function_impl implementation;
	size_t arity;

	jay_closure *closure;

	struct jay_instance *this;
} jay_bound_method;

typedef struct jay_table {
	jay_object object;
	size_t table_size;
	size_t used_entries;

	// The table format is as such:
	// First, we have all of the values packed tightly in an array.
	// Then, we have all of the names packed tightly in an array.
	//
	// We do it like this so that the alignment is simpler. (Because values
	// are guaranteed to have higher alignment than names).
	char table[];
} jay_table;

typedef struct jay_instance {
	jay_object object;

	jay_table *table;

	jay_class *class;
} jay_instance;

typedef struct jay_string {
	jay_object object;

	// Note: contents contains length + 1 bytes, for the NUL terminator.
	size_t length;
	uint32_t hash;

	char contents[];
} jay_string;

static struct {
	uintptr_t current_heap;
	size_t current_size;
	
	uintptr_t to_space;
	uintptr_t from_space;

	uintptr_t limit;
	uintptr_t high_ptr;

	bool flag_auto_recollect;
} jay_gc;

// Use 0 for the tombstone so that we can memset() the array to 0 / use calloc.
#define JAY_NAME_TOMBSTONE 0

#define JAY_STACK_SIZE 1048576

static jay_value jay_stack[JAY_STACK_SIZE];
static jay_value *jay_stack_ptr;

static jay_stackframe *jay_frames[4096];
static size_t jay_frames_ptr;

static void *jay_harbor_stack[4096];
static size_t jay_harbor_ptr;

static inline
void
jay_harbor(void *ptr) {
	jay_harbor_stack[jay_harbor_ptr++] = ptr;
}

static inline
void*
jay_unharbor() {
	jay_harbor_ptr -= 1;
	return jay_harbor_stack[jay_harbor_ptr];
}

// The GC tags are distinct from the normal tags, due to the fact that they
// kind of have to be for the NaN boxing version. We need to be careful to
// always use the correct GC_ or not tag.
#define JAY_GC_FUNCTION     1
#define JAY_GC_INSTANCE     2
#define JAY_GC_STRING       3
#define JAY_GC_CLASS        4
#define JAY_GC_BOUND_METHOD 5
// Tables are used internally by the jay_instance.
#define JAY_GC_TABLE        6
#define JAY_GC_CLOSURE      7

#ifndef JAY_NAN_BOXING

// NOTE: For no NAN_BOXING, we use 0 as the number tag.
// We don't define a constant for it, because it isn't always available.
// It's expected that, in a switch(), 'default' means number.

#define JAY_NIL          1
#define JAY_TRUE         2 // For true/false, note that they have convenient bit patterns of 0b01x
#define JAY_FALSE        3
#define JAY_FUNCTION     4
#define JAY_INSTANCE     5
#define JAY_STRING       6
#define JAY_CLASS        7
#define JAY_BOUND_METHOD 8

#define JAY_IS_NUMBER(v) ((v).tag == 0)
#define JAY_AS_NUMBER(v) ((v).as_number)

// Special bitwise trick to recognize 2/3
#define JAY_IS_BOOL(v) (((v).tag & ~1) == ~1)
#define JAY_AS_BOOL(v) (!!(v.tag == JAY_TRUE))

#define JAY_IS_NIL(v) ((v).tag == JAY_NIL)

#define JAY_IS_FUNCTION(v) ((v).tag == JAY_FUNCTION)
#define JAY_AS_FUNCTION(v) ((v).as_function)

#define JAY_IS_INSTANCE(v) ((v).tag == JAY_INSTANCE)
#define JAY_AS_INSTANCE(v) ((v).as_instance)

#define JAY_IS_STRING(v) ((v).tag == JAY_STRING)
#define JAY_AS_STRING(v) ((v).as_string)

#define JAY_IS_CLASS(v) ((v).tag == JAY_CLASS)
#define JAY_AS_CLASS(v) ((v).as_class)

#define JAY_IS_BOUND_METHOD(v) ((v).tag == JAY_BOUND_METHOD)
#define JAY_AS_BOUND_METHOD(v) ((v).as_bound_method)

#define JAY_TAG(v) ((v).tag)

#define JAY_MK_BOXER(ty, tag_val, as) \
static inline \
jay_value \
jay_box_ ## as(ty input) { \
	jay_value v; \
	v.tag = tag_val; \
	v.as_ ## as = input; \
	return v; \
}

JAY_MK_BOXER(double, 0, number)

static inline
jay_value
jay_box_bool(bool input) {
	jay_value v;
	v.tag = input ? JAY_TRUE : JAY_FALSE;
	return v;
}

static inline
jay_value
jay_box_nil() {
	jay_value v;
	v.tag = JAY_NIL;
	return v;
}

JAY_MK_BOXER(jay_function*, JAY_FUNCTION, function)
JAY_MK_BOXER(jay_class*, JAY_CLASS, class)
JAY_MK_BOXER(jay_instance*, JAY_INSTANCE, instance)
JAY_MK_BOXER(jay_bound_method*, JAY_BOUND_METHOD, bound_method)
JAY_MK_BOXER(jay_string*, JAY_STRING, string)

#undef JAY_MK_BOXER

#else


#define JAY_NAN_TAG_MASK ((uint64_t)0xfffc000000000007)
#define JAY_QNAN         ((uint64_t)0x7ffc000000000000)

#define JAY_NIL          ((uint64_t)0xfffc000000000000)  

#define JAY_TRUE         ((uint64_t)0xfffc000000000002)
#define JAY_FALSE        ((uint64_t)0xfffc000000000003)

// All reference types have high bit 0 so that we can mask more efficiently..?
#define JAY_FUNCTION     ((uint64_t)0x7ffc000000000000)
#define JAY_INSTANCE     ((uint64_t)0x7ffc000000000001)
#define JAY_STRING       ((uint64_t)0x7ffc000000000002)
#define JAY_CLASS        ((uint64_t)0x7ffc000000000003)
#define JAY_BOUND_METHOD ((uint64_t)0x7ffc000000000004)

// Note: For our version of NaN boxing, we assume that the 3 LSBs of a reference
// type will all be 0. This is because we align everything to 8-byte boundary.

static inline
double
jay_unbox_double(jay_value v) {
	double res;
	memcpy(&res, &v, sizeof(res));
	return res;
}

static inline
void*
jay_unbox_reference(jay_value v) {
	v = (v & ~JAY_NAN_TAG_MASK);
	void *ptr;
	memcpy(&ptr, &v, sizeof(ptr));
	return ptr;
}

#define JAY_IS_NUMBER(v) (((v) & JAY_QNAN) != JAY_QNAN)
#define JAY_AS_NUMBER(v) jay_unbox_double(v)

// Special bitwise trick to recognize 2/3
#define JAY_IS_BOOL(v) (((v) | 1) == JAY_FALSE)
#define JAY_AS_BOOL(v) (!!((v) == JAY_TRUE))

#define JAY_IS_NIL(v) ((v) == JAY_NIL)

#define JAY_TAG(v) ((v) & JAY_NAN_TAG_MASK)

#define JAY_IS_FUNCTION(v) (JAY_TAG(v) == JAY_FUNCTION)
#define JAY_AS_FUNCTION(v) ((jay_function*)jay_unbox_reference(v))

#define JAY_IS_INSTANCE(v) (JAY_TAG(v) == JAY_INSTANCE)
#define JAY_AS_INSTANCE(v) ((jay_instance*)jay_unbox_reference(v))

#define JAY_IS_STRING(v) (JAY_TAG(v) == JAY_STRING)
#define JAY_AS_STRING(v) ((jay_string*)jay_unbox_reference(v))

#define JAY_IS_CLASS(v) (JAY_TAG(v) == JAY_CLASS)
#define JAY_AS_CLASS(v) ((jay_class*)jay_unbox_reference(v))

#define JAY_IS_BOUND_METHOD(v) (JAY_TAG(v) == JAY_BOUND_METHOD)
#define JAY_AS_BOUND_METHOD(v) ((jay_bound_method*)jay_unbox_reference(v))

#define JAY_MK_BOXER(ty, tag_bits, as) \
static inline \
jay_value \
jay_box_ ## as(ty input) { \
	jay_value v; \
	memcpy(&v, &input, sizeof(v)); \
	v |= tag_bits; \
	return v; \
}

static inline
jay_value
jay_box_number(double input) {
	jay_value v;
	memcpy(&v, &input, sizeof(v));
	return v;
}

static inline
jay_value
jay_box_bool(bool input) {
	return input ? JAY_TRUE : JAY_FALSE;
}

static inline
jay_value
jay_box_nil() {
	return JAY_NIL;
}

JAY_MK_BOXER(jay_function*, JAY_FUNCTION, function)
JAY_MK_BOXER(jay_class*, JAY_CLASS, class)
JAY_MK_BOXER(jay_instance*, JAY_INSTANCE, instance)
JAY_MK_BOXER(jay_bound_method*, JAY_BOUND_METHOD, bound_method)
JAY_MK_BOXER(jay_string*, JAY_STRING, string)

#endif

// Define print early. op_print is defined much later...
static inline
void
jay_print(jay_value value) {
	switch(JAY_TAG(value)) {
		case JAY_NIL:
			puts("nil");
			break;
		case JAY_STRING:
			puts(JAY_AS_STRING(value)->contents);
			break;
		case JAY_TRUE:
			puts("true");
			break;
		case JAY_FALSE:
			puts("false");
			break;
		case JAY_INSTANCE:
			puts("<instance>");
			break;
		case JAY_CLASS:
			puts("<class>");
			break;
		case JAY_BOUND_METHOD:
			puts("<bound method>");
			break;
		case JAY_FUNCTION:
			puts("<function>");
			break;
		default:
			printf("%g\n", JAY_AS_NUMBER(value));
			break;
	}
}

#ifdef JAY_ASSUME_CORRECT

#define oops(fmt, ...) __builtin_unreachable()

#else

static inline
_Noreturn void 
oops(const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);

	printf("\n");

	// To match the lox reference implementation, use an exit code of 70.
	exit(70);
}

#endif

static inline
const char*
jay_gc_tag_name(uint32_t gc_tag) {
	switch (gc_tag) {
		case JAY_GC_STRING:
			return "string";

		case JAY_GC_BOUND_METHOD:
			return "bound method";

		case JAY_GC_CLASS:
			return "class";

		case JAY_GC_INSTANCE:
			return "instance";

		case JAY_GC_FUNCTION:
			return "function";

		case JAY_GC_TABLE:
			return "table";

		case JAY_GC_CLOSURE:
			return "closure";
	}

	return "<unknown>";
}

static inline
uintptr_t
jay_gc_align(uintptr_t val) {
	const uintptr_t alignment = 8;
	// This clears out the low bits of the value, after making sure it is high
	// enough (i.e. the initial + alignment - 1). Clearing out the bits makes
	// sure they're all zeroes.
	return (val + alignment - 1) & ~(alignment - 1);
}

static inline
size_t
jay_gc_find_size(jay_object *object) {
	uint32_t gc_tag = (object->gc >> 32ULL);

	//#define JAY_GC_FLEX(ty, LENGTH, WHAT) ( ((((ty*)object)->LENGTH) * (sizeof ((ty*)object)->WHAT[0])) + sizeof(WHAT) )
	#define JAY_GC_FLEX(ty, LENGTH, WHAT) ( sizeof((ty){0}.WHAT[0]) * ((ty*)object)->LENGTH ) + sizeof(ty);

	switch(gc_tag) {
		case JAY_GC_STRING:
			return JAY_GC_FLEX(jay_string, length, contents);

		case JAY_GC_BOUND_METHOD:
			return sizeof(jay_bound_method);

		case JAY_GC_CLASS:
			// TODO: Initialize methods_count
			return JAY_GC_FLEX(jay_class, methods_count, methods);

		case JAY_GC_INSTANCE:
			return sizeof(jay_instance);

		case JAY_GC_FUNCTION:
			return sizeof(jay_function);

		case JAY_GC_TABLE:
			jay_table *table = (jay_table*)object;
			return sizeof(jay_table) + table->table_size * (sizeof(jay_value) + sizeof(jay_name));

		case JAY_GC_CLOSURE:
			return JAY_GC_FLEX(jay_closure, count, values);

		default:
			oops("unknown garbage collection type");
	}
}

static inline
uint32_t
jay_gc_tag(void *object) {
	jay_object *obj = object;
	return ((uint64_t)obj->gc >> 32);
}

static inline
jay_object*
jay_gc_copy(jay_object *previous) {
	// Copies the object to a new location in to-space and updates its forwarding
	// pointer. Returns the new location of the object.
	//
	// This assumes we have enough space in to-space to copy the object. It should
	// therefore only be called during GC.

	// Allocate from high ptr
	jay_object *result = (void*)jay_gc.high_ptr;
	jay_gc_assert(jay_gc.high_ptr >= jay_gc.to_space);

	size_t size = jay_gc_find_size(previous);

	// Bump-allocate
	jay_gc.high_ptr = jay_gc_align(jay_gc.high_ptr + size);
	jay_gc_assert(jay_gc.high_ptr < jay_gc.limit);
	jay_gc_assert((jay_gc.high_ptr & 0x7) == 0x0);

	// Copy the old object over
	memcpy(result, previous, size);

	jay_gc_assert(((uint64_t)result->gc >> 32) == ((uint64_t)previous->gc >> 32));

	// Update forwarding pointer. Note that the LSB will always be 0 due to
	// our alignment of 8
	memcpy(&previous->gc, &result, sizeof(result));
	jay_gc_assert((previous->gc & 1) == 0);

	return result;
}

// GC Note: We use an LSB value of 0 to mean forwarding pointer and LSB value
// of 1 to mean not-yet-forwarded.
//
// This is so that we can quickly copy the forwarding pointer (it needs no change),
// and because we store the GC tag in the top 32 bits anyways so the shift will
// remove the bit. (Also, hopefully the compiler will just generate a 32 bit 
// load).

static inline
void*
jay_gc_copy_or_forward(void *prev) {
	if(!prev) return NULL;
	jay_object *previous = prev;

	if((previous->gc & 1) == 0) {
		jay_object* result;
		// Return the existing forwarding pointer.
		memcpy(&result, &previous->gc, sizeof(result));
		return result;
	}

	// Otherwise, copy the object, then update its forwarding pointer, and
	// finally return the same pointer.
	return jay_gc_copy(previous);
}

#define JAY_GC_VISIT_DIRECT(ptr) ptr = jay_gc_copy_or_forward(ptr)

static inline
void
jay_gc_visit(jay_value *field) {
	const uint64_t tag = JAY_TAG(*field);

#define FORWARD jay_gc_copy_or_forward(jay_unbox_reference(*field))

	switch(tag) {
#ifdef JAY_NAN_BOXING
		case JAY_INSTANCE:
			*field = jay_box_instance(FORWARD);
			break;
		case JAY_CLASS:
			*field = jay_box_class(FORWARD);
			break;
		case JAY_FUNCTION:
			*field = jay_box_function(FORWARD);
			break;
		case JAY_BOUND_METHOD:
			*field = jay_box_bound_method(FORWARD);
			break;
		case JAY_STRING:
			*field = jay_box_string(FORWARD);
			break;
#else
		case JAY_INSTANCE:
			field->as_instance = jay_gc_copy_or_forward(field->as_instance);
			break;
		case JAY_CLASS:
			field->as_class = jay_gc_copy_or_forward(field->as_class);
			break;
		case JAY_FUNCTION:
			field->as_function = jay_gc_copy_or_forward(field->as_function);
			break;
		case JAY_BOUND_METHOD:
			field->as_bound_method = jay_gc_copy_or_forward(field->as_bound_method);
			break;
		case JAY_STRING:
			field->as_string = jay_gc_copy_or_forward(field->as_string);
			break;
#endif
		default:
			// No action.
	}
}

// To be generated by the compiler.
static void
jay_gc_visit_globals();

static inline
jay_value*
jay_table_values(jay_table *table) {
	return (void*)(table->table);
}

static inline
jay_name*
jay_table_names(jay_table *table) {
	return (void*)(table->table + (table->table_size * sizeof(jay_value)));
}

// Returns the size of the object.
static
size_t
jay_gc_trace(jay_object *object) {
	uint32_t gc_tag = (object->gc >> 32ULL);

	switch(gc_tag) {
		case JAY_GC_STRING:
			/* no-op */
			break;

		case JAY_GC_BOUND_METHOD:
			jay_bound_method *bound_method = (jay_bound_method*)object;
			JAY_GC_VISIT_DIRECT(bound_method->closure);
			JAY_GC_VISIT_DIRECT(bound_method->this);
			break;

		case JAY_GC_CLASS:
			// TODO: Initialize methods_count
			jay_class *class = (jay_class*)object;
			JAY_GC_VISIT_DIRECT(class->closure);
			JAY_GC_VISIT_DIRECT(class->superclass);
			break;

		case JAY_GC_INSTANCE:
			jay_instance *instance = (jay_instance*)object;
			JAY_GC_VISIT_DIRECT(instance->class);
			JAY_GC_VISIT_DIRECT(instance->table);
			break;

		case JAY_GC_FUNCTION:
			jay_function *function = (jay_function*)object;
			JAY_GC_VISIT_DIRECT(function->closure);
			break;

		case JAY_GC_TABLE: {
			jay_table *table = (jay_table*)object;
			jay_name *names = jay_table_names(table);
			jay_value *values = jay_table_values(table);
			for(size_t i = 0; i < table->table_size; ++i) {
				if(names[i] != JAY_NAME_TOMBSTONE) {
					jay_gc_visit(&values[i]);
				}
			}
			break;
		}

		case JAY_GC_CLOSURE: {
			jay_closure *closure = (jay_closure*)object;
			JAY_GC_VISIT_DIRECT(closure->parent);
			for(size_t i = 0; i < closure->count; ++i) {
				jay_gc_visit(&closure->values[i]);
			}
			break;
		}

		default:
			oops("unknown garbage collection type");
	}

	return jay_gc_find_size(object);
}

// The core garbage-collection method, which performs scanning and copying
// from from-space to to-space.
static
void
jay_gc_go() {
	// We will scan through the new to-space to copy any more referenced objects.
	uintptr_t scan = jay_gc.high_ptr;

	// We visit all roots.
	for(jay_value *sp = jay_stack; sp != jay_stack_ptr; ++sp) {
		jay_gc_visit(sp);
	}

	for(size_t sf = 0; sf < jay_frames_ptr; ++sf) {
		jay_stackframe *frame = jay_frames[sf];
		JAY_GC_VISIT_DIRECT(frame->gc_scope);
		for(size_t i = 0; i < frame->count; ++i) {
			jay_gc_visit(&frame->values[i]);
		}

	}

	for(size_t hptr = 0; hptr < jay_harbor_ptr; ++hptr) {
		JAY_GC_VISIT_DIRECT(jay_harbor_stack[hptr]);
	}

	jay_gc_visit_globals();

	while(scan < jay_gc.high_ptr) {
		jay_object *to_scan = (void*)scan;
		scan = jay_gc_align(scan + jay_gc_trace(to_scan));
	}

	// Based on the proportion of collected memory, enable a flag to automatically
	// expand the memory next time. (E.g., if we think we're using 80% of the memory,
	// collection will likely be too often). The ratio can be set here...
	size_t used = (size_t)(jay_gc.high_ptr - jay_gc.to_space);
	size_t avail = (jay_gc.current_size / 2);

	const size_t flag_ratio = 2;

	// If we have used absolutely nothing, don't need the flag.
	if(used == 0) {
		jay_gc.flag_auto_recollect = false;
	}
	else {
		// Check ratio. Guaranteed to succeed because used > 0.
		size_t ratio = avail / used;

		// If we're using a lot of memory, automatically expand the heap next time.
		jay_gc.flag_auto_recollect = (ratio <= flag_ratio);
	}
}

static inline
void
jay_gc_recollect(size_t needed_size) {
	size_t half = jay_gc.current_size / 2;
	size_t taken = (size_t)(jay_gc.high_ptr - jay_gc.to_space);

	// Always double at least once.
	// This is for the following reasons:
	// - If we're calling from flag_auto_recollect, we're trying to expand the
	//   heap.
	// - If we're calling from gc_alloc_impl, then we know we didn't have enough
	//   space, so we might as well double it at least once.
	half *= 2;

	while((taken + needed_size) >= half) {
		// TODO: Overflow check?
		half *= 2;
	}

	size_t new_size = half * 2;
	void *new_mem = malloc(new_size); //gc_debugger

	if(!new_mem) {
		oops("out of memory: can't expand gc heap");
	}

	// NOTE: Should be totally unnecessary, as we don't actually use from_space
	// throughout all of jay_gc_go(), but this does make some sense. Can probably
	// be deleted if we really care.
	jay_gc.from_space = jay_gc.to_space;

	jay_gc.current_size = new_size;

	jay_gc.high_ptr = (uintptr_t)new_mem;
	jay_gc.to_space = (uintptr_t)new_mem;
	jay_gc.limit = jay_gc.to_space + (jay_gc.current_size / 2);

	// Note: When we recollect() after collect()ing, we do still need to e.g.
	// visit everything on the stack, in order to update its pointers. So
	// unfortunately recollect() can be slow, but we can add some tuning
	// to help the GC expand eagerly when it thinks it needs to.
	jay_gc_go();

	// Free the old heap.
	free((void*)jay_gc.current_heap);

	// Update the current heap pointer so we can track it for freeing later.
	jay_gc.current_heap = (uintptr_t)new_mem;

	// The actual from_space should be the top half of the heap like usual.
	jay_gc.from_space = jay_gc.limit;
}

static inline
void
jay_gc_collect(size_t desired_size) {
	if(jay_gc.flag_auto_recollect) {
		jay_gc_recollect(desired_size);
		return;
	}

	// Reset the high pointer and to_space to point to the from_space
	jay_gc.high_ptr = jay_gc.from_space;

	// And the from space points to the to-space
	jay_gc.from_space = jay_gc.to_space;

	// (to space points to old from space)
	jay_gc.to_space = jay_gc.high_ptr;

	// update limit
	jay_gc.limit = jay_gc.to_space + (jay_gc.current_size / 2);

	jay_gc_go();
}

static inline
void*
jay_gc_alloc_impl(size_t size) {
	void *result = (void*)jay_gc.high_ptr;
	uintptr_t next = jay_gc_align(jay_gc.high_ptr + size);

// When stress testing, skip the first option, always collect.
#ifndef JAY_GC_STRESS_TEST

	if(next < jay_gc.limit) {
		jay_gc.high_ptr = next;
		return result;
	}

#endif

	// If the allocation failed, then collect.
	jay_gc_collect(size);

	// Try the allocation again.
	result = (void*)jay_gc.high_ptr;
	next = jay_gc_align(jay_gc.high_ptr + size);

	if(next < jay_gc.limit) {
		jay_gc.high_ptr = next;
		return result;
	}

	// If it STILL failed, then collect to a new to space.
	jay_gc_recollect(size);

	// After recollect(), either our realloc failed, our we definitely have
	// enough space.
	result = (void*)jay_gc.high_ptr;
	jay_gc.high_ptr = jay_gc_align(jay_gc.high_ptr + size);
	return result;
}

// The size should include the size of the gc_obj.
static inline
void*
jay_gc_alloc(size_t size, uint32_t tag) {
	jay_object *obj = jay_gc_alloc_impl(size);
	// The gc pointer is initialized with 1 in the LSB, as well as the tag in
	// the high bits.
	obj->gc = 1 | ((uint64_t)tag << 32ULL);
	return obj;
}

static inline
void
jay_gc_init(size_t init_size) {
	void *mem = malloc(init_size);
	if(!mem) {
		oops("out of memory: can't create gc heap");
	}

	uintptr_t memcheck = (uintptr_t)mem;
	if((memcheck & 0x7) != 0) {
		// Malloc should be guaranteed to align to at least 8 bytes due to 
		// it being able to return long*, etc, so we might as well not try
		// to handle this case ourselves.
		oops("gc heap not aligned to 8 bytes--giving up");
	}

	jay_gc.current_heap = (uintptr_t)mem;
	jay_gc.current_size = init_size;

	jay_gc.to_space = (uintptr_t)mem;
	jay_gc.high_ptr = (uintptr_t)mem;

	jay_gc.from_space = jay_gc.to_space + (init_size / 2);
	jay_gc.limit = jay_gc.from_space; // Limit is halfway up initially

	// Don't automatically expand the heap, initially.
	jay_gc.flag_auto_recollect = false;
}

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

static inline bool
jay_truthy(jay_value value) {
	if(JAY_TAG(value) == JAY_FALSE || JAY_TAG(value) == JAY_NIL) return false;
	return true;
}

static inline bool
jay_pop_condition() {
	return jay_truthy(jay_pop());
}

/* --- Literals --- */

static inline
jay_string*
jay_new_string(size_t length) {
	// Add 1 for the NUL terminator
	jay_string *string = jay_gc_alloc(sizeof(jay_string) + length + 1, JAY_GC_STRING);
	string->length = length;
	// Set the NUL terminator
	string->contents[length] = '\0';
	
	return string;
}

// Note: this hash function is taken directly from clox.
// It is an implementation of FNV-1a.
static uint32_t jay_compute_string_hash(const char* key, size_t length) {
	uint32_t hash = 2166136261u;
	for (size_t i = 0; i < length; ++i) {
		hash ^= (uint8_t)key[i];
		hash *= 16777619;
	}
	return hash;
}

static inline
jay_string*
jay_string_from_literal(const char *literal) {
	size_t length = strlen(literal);
	jay_string *string = jay_new_string(length);

	memcpy(string->contents, literal, length);
	string->hash = jay_compute_string_hash(string->contents, length);

	return string;
}

/* --- Allocators --- */

jay_closure*
jay_new_scope(jay_closure *parent, size_t count) {
	// Harbor closure value for GC
	jay_harbor(parent);

	size_t bytes = sizeof(jay_closure) + (count * sizeof(jay_value));
	jay_closure *closure = jay_gc_alloc(bytes, JAY_GC_CLOSURE);
	closure->count = count;
	closure->parent = jay_unharbor();
	// Do we want to zero out the 'values' array..?

	return closure;
}

static inline
size_t
jay_table_max_usage(jay_table *table) {
	// For small tables, the cost of linear scan is small, so we can freely
	// use all 8 entries.
	if(table->table_size == 8) {
		return 8;
	}
	else if(table->table_size == 16) {
		return 12;
	}
	else {
		return table->table_size / 2;
	}
}

jay_table*
jay_new_table(size_t entries) {
	size_t flexible_bytes = (entries * sizeof(jay_value)) + (entries * sizeof(jay_name));
	size_t bytes = flexible_bytes + sizeof(jay_table);
	jay_table *result = jay_gc_alloc(bytes, JAY_GC_TABLE);

	result->table_size = entries;
	// Set the max allowed used entries to smaller numbers for smaller tables.
	
	result->used_entries = 0;

	// NOTE: Because we actually only ever look at non-TOMBSTONE values, we
	// very well could not memset this like this. But, this should be fine...
	memset(result->table, 0, flexible_bytes);

	return result;
}

jay_instance*
jay_new_instance() {
	jay_instance *instance = jay_gc_alloc(sizeof(*instance), JAY_GC_INSTANCE);

	// We can't set the class yet, because an allocation could result in the class
	// being invalid. So, instead, leave that to the caller. (The GC is happy with
	// NULL here).
	instance->class = NULL;
	instance->table = NULL;
	// So that the GC can find the instance while we're allocating the table, push it.
	jay_push(jay_box_instance(instance));

	// Allocate a table for the fields.
	jay_table *table = jay_new_table(8);

	// Clean up GC root. We have to reset the instance pointer as well in the
	// case that it is updated.
	instance = JAY_AS_INSTANCE(jay_pop());

	// Store table value.
	instance->table = table;

	return instance;
}

/* --- Instance Related "Hash" Map Stuff */

// NOTE: This function is UNSAFE to call if you haven't guaranteed that there
// is an empty bucket
// Returns the pointer to the value. Places name at the corresponding bucket.
jay_value*
jay_take_empty_bucket_in(jay_table *table, jay_name name) {
	// Modulo optimization: Only works for power-of-two sizes.
	size_t i = name & (table->table_size - 1);
	jay_name *names = jay_table_names(table);
	for(;;) {
		i = (i + 1) & (table->table_size - 1);

		// We MUST find a tombstone to exit the loop.
		if(names[i] == JAY_NAME_TOMBSTONE) {
			// Store name
			names[i] = name;
			return jay_table_values(table) + i;
		}
	}
}

// Finds the value bucket corresponding to the given name. Does not modify
// the table at all.
jay_value*
jay_find_bucket_in(jay_table *table, jay_name name) {
	// Modulo optimization: Only works for power-of-two sizes.
	size_t i = name & (table->table_size - 1);
	size_t check = i;

	jay_name *names = jay_table_names(table);

	while(names[i] != name) {
		// Linear probing
		i = (i + 1) & (table->table_size - 1);

		// Item doesn't exist
		if(i == check || names[i] == JAY_NAME_TOMBSTONE) {
			return NULL;
		}
	}

	return jay_table_values(table) + i;
}

static inline
jay_value*
jay_find_bucket(jay_instance *instance, jay_name name) {
	return jay_find_bucket_in(instance->table, name);
}

void
jay_rehash(jay_instance *instance) {
	size_t new_size = instance->table->table_size * 2;
	
	// TODO: Clean this up so we don't need redundant pushes
	jay_push(jay_box_instance(instance));
	jay_table *new_table = jay_new_table(new_size);
	instance = JAY_AS_INSTANCE(jay_pop());

	// Now that we're not allocating, it's safe to call names() and values()
	jay_name *old_names = jay_table_names(instance->table);
	jay_value *old_values = jay_table_values(instance->table);

	for(size_t i = 0; i < instance->table->table_size; ++i) {
		if(old_names[i] != JAY_NAME_TOMBSTONE) {
			// Note: here this should always return a tombstone, as all buckets
			// should be empty...
			jay_value *ptr = jay_take_empty_bucket_in(new_table, old_names[i]);
			*ptr = old_values[i];
		}
	}

	// We can't free() the old table, because it's managed by the GC. It will
	// eventually be recycled. TODO: Consider allocating the initial size
	// based on the estimated entries in init().

	new_table->used_entries = instance->table->used_entries;
	instance->table = new_table;
}

jay_value
jay_put_new(jay_instance *scope, size_t name, jay_value value) {
	if((scope->table->used_entries + 1) > jay_table_max_usage(scope->table)) {
		// Push and pop in case GC is invoked.
		jay_push(jay_box_instance(scope));
		// We also have to push the value, because it might also change.
		jay_push(value);
		jay_rehash(scope);
		value = jay_pop();
		scope = JAY_AS_INSTANCE(jay_pop());
	}
	// We are guaranteed an empty bucket somewhere.
	// TODO: Consider Hopgood-Davenport probing
	jay_value *ptr = jay_take_empty_bucket_in(scope->table, name);
	*ptr = value;

	scope->table->used_entries += 1;

	return value;
}


/* --- Instance Related Methods --- */

static inline
jay_value
jay_bind(jay_method method, jay_instance *this) {
	// IMPORTANT: We take method by value, because we can't really store it
	// for the GC, and we're going to copy everything out anyways.
	// But, we do need to push its class.

	jay_harbor(method.parent->closure);
	// Juggle with GC
	jay_push(jay_box_instance(this));
	jay_bound_method *result = jay_gc_alloc(sizeof(*result), JAY_GC_BOUND_METHOD);
	this = JAY_AS_INSTANCE(jay_pop());
	result->implementation = method.implementation;
	result->arity = method.arity;

	// We harborded the closure.
	result->closure = jay_unharbor();

	// TODO: Do we want to be able to have a non-jay_instance "this"?
	// probably not, as it might impede certain optimizations.. but for now,
	// it is a bit silly that we support it in all but name
	result->this = this;
	return jay_box_bound_method(result);
}

static inline
jay_value
jay_get_instance(jay_instance *instance, size_t name) {
	// In compat mode, we have to look up the field first.
	// Compare to JAY_MAX_FIELD so that we can skip this step for things that
	// are definitely not fields.
	if(name < JAY_MAX_FIELD) {
		jay_value *ptr = jay_find_bucket(instance, name);
		if(ptr) {
			return *ptr;
		}
	}

	jay_method *method = instance->class->dispatcher(instance->class, name);
	if(method) {
		return jay_bind(*method, instance);
	}

	oops("tried to get non-exist property");
}

static inline
void
jay_fence_get(jay_value v) {
	if(!JAY_IS_INSTANCE(v)) {
		oops("can only look up properties on an instance");
	}
}

static inline
jay_value
jay_get_super(jay_instance *instance, size_t name, jay_value static_class) {
	if(!JAY_IS_CLASS(static_class)) {
		oops("'super' must be used from a class");
	}

	// Superclass is "statically bound" so to speak -- see abc_super.lox. 
	// Essentially, the superclass does not change to match the superclass
	// of the class of the current instance (i.e. it is not instance->class->superclass),
	// but rather, it is always the same superclass.
	//
	// As such, we have to somehow explicitly track the superclass. I guess
	// this is why it is mandated to be a variable in lox -- so that that variable
	// can always be directly looked up.
	jay_class *superclass_real = JAY_AS_CLASS(static_class)->superclass;

	jay_method *method = superclass_real->dispatcher(superclass_real, name);
	if(method) {
		return jay_bind(*method, instance);
	}

	oops("superclass has no such method");
}

static inline
jay_value
jay_set_instance(jay_instance *instance, size_t name, jay_value value) {
	// Note: Do no JAY_MAX_FIELD check here for one simple reason:
	// The compiler should be generating correct code. In that case, the
	// jay_set command will only ever be called with a name that can legally
	// be a field.
	jay_value *ptr = jay_find_bucket(instance, name);
	if(!ptr) {
		// Slow path is creating a new value on the object. This shouldn't happen
		// too often.
		// TODO: One optimization is that jay_find_bucket() could return
		// the tombstone when it fails, and then, if we don't need to rehash,
		// we could just put the item in that bucket right here...
		//
		// I guess that could be a variant of jay_find_bucket()?
		// NOTE: Not gc-safe
		jay_put_new(instance, name, value);
	}
	else {
		*ptr = value;
	}

	return value;
}

static inline
void
jay_fence_set(jay_value v) {
	if(!JAY_IS_INSTANCE(v)) {
		oops("can only set fields on an instance");
	}
}

// Because the superclass must be (retrievable from) a variable, we have no need to do any stack
// machine shenanigans (even for GC purposes) -- so always pass it here.
// Same with 'this'. But, the super _op does push a value on to the stack.
static inline
void
jay_op_get_super(jay_instance *this, size_t name, jay_value static_class) {
	jay_push(jay_get_super(this, name, static_class));
}

/* --- Call Operators */

static inline
jay_value
jay_call_any(jay_function_impl fun, jay_closure *closure, size_t actual_arity, size_t tried_arity) {
	if(actual_arity != tried_arity) {
		oops("wrong arity");
	}

	jay_value result = fun(jay_stack_ptr - actual_arity, closure);

	jay_stack_ptr -= actual_arity;

	return result;
}

static inline
jay_value
jay_call(struct jay_function *fun, size_t arity) {
	if(arity != fun->arity) {
		oops("wrong arity");
	}

	// The arguments remain on the stack until the function returns.
	jay_value result = fun->implementation(jay_stack_ptr - arity, fun->closure);

	jay_stack_ptr -= arity;
	
	return result;
}

static inline
void
jay_op_call_direct(struct jay_function *fun, size_t arity) {
	jay_value result = jay_call(fun, arity);
	jay_push(result);
}

static inline
void
jay_op_call(size_t arity) {
	// We have to not pop the function pointer yet, as the GC might move it
	// when we allocate a new instance, etc.
	jay_value *fun_value = &jay_stack_ptr[-1];

	if(JAY_IS_FUNCTION(*fun_value)) {
		jay_function *fun = JAY_AS_FUNCTION(*fun_value);
		jay_pop();
		jay_value result = jay_call(fun, arity);
		jay_push(result);
	}
	else if(JAY_IS_BOUND_METHOD(*fun_value)) {
		jay_bound_method *method = JAY_AS_BOUND_METHOD(*fun_value);

		// For bound methods, push 'this' to the end of the args array
		// Note: An important semantic point with 'this' is that it can be
		// captured by a closure. So, it is easiest to treat it as 'another
		// local variable'. The compiler will have to be careful somehow.
		//
		// Maybe the compiler can literally just add a 'this' variable to
		// the array when parsing a function?

		// No possibility of allocation, so pop first.

		jay_pop();
		jay_push(jay_box_instance(method->this));

		// Note that we add 1 to the passed-in arity. That is, we're expecting
		// the compiler to NOT add the 'this' value at any point, and also,
		// if the compiler ever compares a call site to the method definition,
		// it should account for the call site arity == method arity - 1
		jay_value result = jay_call_any(
			method->implementation,
			method->closure,
			method->arity,
			arity + 1
		);
		jay_push(result);
	}
	else if(JAY_IS_CLASS(*fun_value)) {
		// We have to push the "this" as the last argument. But, this is calling
		// "init". So, we actually just create a new instance here. That is,
		// the caller of a class object is responsible for creating a new 'this'
		// and pushing it to the end of the array. This helps keep 'init' less
		// special-cased, as then it can easily be called again or bound, like
		// any other class method.

		// We have to allocate before popping.

		jay_instance *instance = jay_new_instance();

		// As soon as all allocation is done, we are allowed to pop from the stack.
		// (the class value may have changed since jay_new_instance).
		jay_class *class = JAY_AS_CLASS(jay_pop());

		// The caller of jay_new_instance() must initialize the class.
		instance->class = class;

		jay_value new_this = jay_box_instance(instance);
		
		jay_push(new_this);
		
		// Because 'init' might belong to the superclass or might not exist
		// at all, we have to look it up through the dispatcher for consistency
		// with normal method lookup.

		jay_method *method = class->dispatcher(class, NAME_init);
		if(method) {
			jay_value result = jay_call_any(
				method->implementation,
				class->closure,
				method->arity,
				arity + 1 // Same as above
			);
			
			// Originally, we just pushed new_this back on to the stack. But this
			// isn't gc-safe, because jay_call_any might allocate. Instead, the easiest
			// thing to do is to just trust the contract that the initializer will
			// return this, and push that value.
			jay_push(result);
		}
		else {
			// If there isn't an initializer, we simply return the new_this value,
			// which is already pushed onto the stack.
		}
	}
	else {
		oops("can only call callable objects");
	}
}

// Note: invoke_super and get_super both take a jay_value* because they MUST always
// be provided a 'this', and the compiler will always be able to generate the correct
// code for that.
static inline
void
jay_op_invoke_super(jay_instance *instance, size_t name, jay_value static_class, size_t arity) {
	if(!JAY_IS_CLASS(static_class)) {
		oops("'super' must be invoked from within a class");
	}

	// Superclass is "statically bound" so to speak -- see abc_super.lox. 
	// Essentially, the superclass does not change to match the superclass
	// of the class of the current instance (i.e. it is not instance->class->superclass),
	// but rather, it is always the same superclass. (More specifically, the superclass is
	// always the superclass of the jay_class containing the reference to "super." see
	// test/super/reassign_super.lox).
	//
	// As such, we have to somehow explicitly track that class.
	jay_class *superclass_real = JAY_AS_CLASS(static_class)->superclass;

	jay_method *method = superclass_real->dispatcher(superclass_real, name);
	if(method) {
		// Push 'this' on to the stack, then call the method.
		jay_push(jay_box_instance(instance));

		jay_value result = jay_call_any(
			method->implementation,
			superclass_real->closure,
			method->arity,
			arity + 1
		);

		jay_push(result);

		return;
	}

	oops("superclass has no such method");
}

static inline
void
jay_fence_invoke(jay_value target) {
	if(!JAY_IS_INSTANCE(target)) {
		oops("can only get properties on an instance");
	}
}

static inline
void
jay_op_invoke(size_t name, size_t arity, jay_instance *instance) {
	// Leave "this" on top in case we have to do a jay_op_get() and jay_op_call()
	// 'invoke' is a little bit weird in that we kind of need the 'this' pointer
	// on the stack at all times, but we also want to elide the JAY_AS_INSTANCE
	// check and such for 'this' calls. So, we both put it on the stack, and
	// pass it as a parameter.
	jay_value target = jay_stack_ptr[-1];

	// In full compat mode, we have to look up the field first.
	if(name < JAY_MAX_FIELD) {
		jay_value *ptr = jay_find_bucket(instance, name);
		if(ptr) {
			// In the case that we're calling a field, there are two cases
			// to consider:
			// 1. It's a regular function.
			// 2. It's a bound method.
			// In both cases, we do not want to have 'this' on the stack
			// anymore. In the first case, it is straight up the wrong
			// argument (see test/field_call.lox) and in the second case,
			// jay_op_call will push this for us so we end up with too
			// many.
			// Therefore, we have to pop(), then we can just push()
			// the actual function to call and jay_op_call.
			jay_pop();
			jay_push(*ptr);
			jay_op_call(arity);
			return;
		}
	}

	// Then, look up the method.
	jay_method *method = instance->class->dispatcher(instance->class, name);
	if(!method) {
		oops("tried to invoke non-existent property");
	}

	// Okay, we have a valid method, we can actually still leave "this" on top
	// as it's the last argument for the method...
	jay_value result = jay_call_any(
		method->implementation,
		instance->class->closure,
		method->arity,
		arity + 1
	);

	jay_push(result);
}

static inline
jay_value
jay_fun_from(jay_function_impl impl, size_t arity, jay_closure *closure) {
	// Harbor closure value for GC
	jay_harbor(closure);

	jay_function *f = jay_gc_alloc(sizeof(*f), JAY_GC_FUNCTION);
	f->arity = arity;
	f->closure = jay_unharbor();
	f->implementation = impl;

	return jay_box_function(f);
}

// Note that methods are weird in that they are not jay_objects nor can they
// be stored in jay_values.
static inline
jay_method
jay_method_from(jay_class *class, jay_function_impl impl, size_t arity) {
	return (jay_method) {
		.parent = class,
		.implementation = impl,
		.arity = arity
	};
}

/* --- Operators --- */

#define OP_TWO(name) \
static inline void \
jay_op_ ## name (void) { \
	jay_value b = jay_pop(); \
	jay_value a = jay_pop(); \
	jay_push(jay_ ## name(a, b)); \
}

static inline
void
jay_op_print(void) {
	jay_print(jay_pop());
}

static inline
jay_value
jay_add(jay_value aval, jay_value bval) {
	// For now, only add handles gc correctly..

	if(JAY_IS_NUMBER(aval)) {
		if(JAY_IS_NUMBER(bval)) {
			return jay_box_number(JAY_AS_NUMBER(aval) + JAY_AS_NUMBER(bval));
		}
	}

	if(JAY_IS_STRING(aval)) {
		if(JAY_IS_STRING(bval)) {
			jay_string *a = JAY_AS_STRING(aval);
			jay_string *b = JAY_AS_STRING(bval);
			size_t length = a->length + b->length;

			jay_harbor(b);
			jay_harbor(a);

			jay_string *cat = jay_new_string(length);

			// Now that we're done allocating, we can get back our references.
			a = jay_unharbor();
			b = jay_unharbor();

			memcpy(cat->contents,             a->contents, a->length);
			memcpy(cat->contents + a->length, b->contents, b->length);

			// Recompute hash
			cat->hash = jay_compute_string_hash(cat->contents, length);

			return jay_box_string(cat);
		}
	}

	oops("addition expects two numbers or two strings");
}

static inline
bool
jay_eq_impl(jay_value a, jay_value b) {
#ifdef JAY_NAN_BOXING

	if(JAY_IS_NUMBER(a) && JAY_IS_NUMBER(b)) {
		return JAY_AS_NUMBER(a) == JAY_AS_NUMBER(b);
	}

	return a == b;

#else

	uint64_t a_tag = JAY_TAG(a);
	if(a_tag != JAY_TAG(b)) {
		return false;
	}

	switch(a_tag) {
		case JAY_NIL:
		case JAY_TRUE:
		case JAY_FALSE:
			return true;

		case JAY_STRING:
			if(a.as_string == b.as_string) return true;
			if(a.as_string->hash != b.as_string->hash) return false;

			if(a.as_string->length != b.as_string->length) return false;

			return !memcmp(a.as_string, b.as_string, a.as_string->length);

		case JAY_INSTANCE:
			return JAY_AS_INSTANCE(a) == JAY_AS_INSTANCE(b);
		case JAY_FUNCTION:
			return JAY_AS_FUNCTION(a) == JAY_AS_FUNCTION(b);
		case JAY_BOUND_METHOD:
			return JAY_AS_BOUND_METHOD(a) == JAY_AS_BOUND_METHOD(b);
		case JAY_CLASS:
			return JAY_AS_CLASS(a) == JAY_AS_CLASS(b);

		default:
			return JAY_AS_NUMBER(a) == JAY_AS_NUMBER(b);
	}

#endif
}

static inline
jay_value
jay_neq(jay_value a, jay_value b) {
	return jay_box_bool(!jay_eq_impl(a, b));
}
OP_TWO(neq)

static inline
jay_value
jay_eq(jay_value a, jay_value b) {
	return jay_box_bool(jay_eq_impl(a, b));
}
OP_TWO(eq)

#ifdef JAY_ASSUME_CORRECT

#define jay_fence_number(v)

#else

static inline
void 
jay_fence_number(jay_value v) {
	if(!JAY_IS_NUMBER(v)) {
		oops("operation expects numerical arguments");
	}
}

#endif

/* --- Builtin Functions (e.g. clock) --- */

static
jay_value
jay_std_clock(jay_value *args, jay_closure *closure) {
	clock_t time = clock();
	double ms = (time * 1000.0 / CLOCKS_PER_SEC);
	return jay_box_number(ms);
}

#endif
