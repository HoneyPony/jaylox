#ifndef JAYLIB_H
#define JAYLIB_H

// TODO: Make this a compiler option or something
#define JAY_FULL_COMPAT

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
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
typedef struct jay_method* (*jay_dispatcher_impl)(struct jay_class *class, size_t name);

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

typedef struct {
	size_t name;
	struct jay_value value;
} jay_hash_entry;

typedef struct jay_table {
	jay_object object;
	size_t table_size;
	size_t used_entries;
	jay_hash_entry table[];
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
} jay_gc;

// Use 0 for the tombstone so that we can memset() the array to 0 / use calloc.
#define JAY_NAME_TOMBSTONE 0

#define JAY_STACK_SIZE 1048576

static jay_value jay_stack[JAY_STACK_SIZE];
static jay_value *jay_stack_ptr;

static jay_stackframe *jay_frames[4096];
static size_t jay_frames_ptr;

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

#define JAY_AS_OBJECT(v) ((v).as_object)

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
			printf("%f\n", JAY_AS_NUMBER(value));
			break;
	}
}

#ifdef JAY_ASSUME_CORRECT

#define oops(message) __builtin_unreachable()

#else

static inline
_Noreturn void 
oops(const char *message) {
	printf("runtime error: %s\n", message);
	exit(1);
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
			return JAY_GC_FLEX(jay_table, table_size, table);

		case JAY_GC_CLOSURE:
			return JAY_GC_FLEX(jay_closure, count, values);

		default:
			oops("unknown garbage collection type");
	}
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

	size_t size = jay_gc_find_size(previous);

	// Bump-allocate
	jay_gc.high_ptr = jay_gc_align(jay_gc.high_ptr + size);

#ifdef JAY_TRACE_GC
	printf("     v copy %s %p -> %p (%zu bytes)\n", jay_gc_tag_name((uint64_t)previous->gc >> 32), previous, result, size);
#endif

	// Copy the old object over
	memcpy(result, previous, size);

	assert(((uint64_t)result->gc >> 32) == ((uint64_t)previous->gc >> 32));

	// Update forwarding pointer. Note that the LSB will always be 0 due to
	// our alignment of 8
	memcpy(&previous->gc, &result, sizeof(result));
	assert((previous->gc & 1) == 0);

#ifdef JAY_TRACE_GC
	printf("     v (...which redirects %p -> %p)\n", previous, previous->gc);
#endif

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

	printf("     v forward %p -> %p\n", previous, previous->gc);

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

#ifdef JAY_TRACE_GC
	#define JAY_GC_VISIT_DIRECT(ptr) do { \
		void *tmp = jay_gc_copy_or_forward(ptr); \
		printf("\\-- %s: %p -> %p\n", #ptr, ptr, tmp); \
		ptr = tmp; \
	} while(0)
#else
	#define JAY_GC_VISIT_DIRECT(ptr) ptr = jay_gc_copy_or_forward(ptr)
#endif

static inline
void
jay_gc_visit(jay_value *field) {
	switch(JAY_TAG(*field)) {
		case JAY_INSTANCE:
		case JAY_CLASS:
		case JAY_FUNCTION:
		case JAY_BOUND_METHOD:
		case JAY_STRING:
#ifdef JAY_NAN_BOXING
#else
			field->as_object = jay_gc_copy_or_forward(field->as_object);
#endif
		default:
			// No action.
			;
	}
}

// To be generated by the compiler.
static void
jay_gc_visit_globals();

// Returns the size of the object.
static
size_t
jay_gc_trace(jay_object *object) {
	uint32_t gc_tag = (object->gc >> 32ULL);

#ifdef JAY_TRACE_GC
	printf("gc: trace %p = %s (%zd rel to high_ptr)\n", object, jay_gc_tag_name(gc_tag), (ssize_t)object - (ssize_t)jay_gc.high_ptr);
#endif

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
			for(size_t i = 0; i < table->table_size; ++i) {
				if(table->table[i].name != JAY_NAME_TOMBSTONE) {
					jay_gc_visit(&table->table[i].value);
#ifdef JAY_TRACE_GC
					printf("\\-- table entry %zu ", i);
					jay_print(table->table[i].value);
#endif
				}
			}
			break;
		}

		case JAY_GC_CLOSURE: {
			jay_closure *closure = (jay_closure*)object;
			JAY_GC_VISIT_DIRECT(closure->parent);
			for(size_t i = 0; i < closure->count; ++i) {
				jay_gc_visit(&closure->values[i]);
#ifdef JAY_TRACE_GC
					printf("\\-- closure entry %zu ", i);
					jay_print(closure->values[i]);
#endif
			}
			break;
		}

		default:
			oops("unknown garbage collection type");
	}

#ifdef JAY_TRACE_GC
	size_t result_size = jay_gc_find_size(object);
	printf("^^^ return size = %zu\n", result_size);
	return result_size;
#else
	return jay_gc_find_size(object);
#endif
}

// The core garbage-collection method, which performs scanning and copying
// from from-space to to-space.
static
void
jay_gc_go() {
	printf("GC: BEGIN COLLECT!\n");

	// We will scan through the new to-space to copy any more referenced objects.
	uintptr_t scan = jay_gc.high_ptr;

	// We visit all roots.
	for(jay_value *sp = jay_stack; sp != jay_stack_ptr; ++sp) {
#ifdef JAY_TRACE_GC
		printf("gc: visit stack ptr %p -> ", sp);
		jay_print(*sp);
#endif
		jay_gc_visit(sp);
	}

	for(size_t sf = 0; sf < jay_frames_ptr; ++sf) {
#ifdef JAY_TRACE_GC
		printf("gc: visit frame %zu\n", sf);
#endif
		jay_stackframe *frame = jay_frames[sf];
		JAY_GC_VISIT_DIRECT(frame->gc_scope);
		for(size_t i = 0; i < frame->count; ++i) {
#ifdef JAY_TRACE_GC
			printf("\\-- frame value %zu ", i);
			jay_print(frame->values[i]);
#endif
			jay_gc_visit(&frame->values[i]);
		}

	}

	jay_gc_visit_globals();

	while(scan < jay_gc.high_ptr) {
		jay_object *to_scan = (void*)scan;
		scan = jay_gc_align(scan + jay_gc_trace(to_scan));
	}

	printf("GC: COLLECT DONE! copied %zu bytes (out of %zu)\n", (size_t)(jay_gc.high_ptr - jay_gc.to_space), (jay_gc.current_size / 2));
}

static inline
void
jay_gc_recollect(size_t needed_size) {
	printf("GC: RECOLLECT (NEED %zu)\n", needed_size);
	printf("GC: REMAINING = %zu\n", (jay_gc.limit - jay_gc.high_ptr));

	size_t half = jay_gc.current_size / 2;
	size_t taken = (size_t)(jay_gc.high_ptr - jay_gc.to_space);
	printf("GC: TAKEN = %zu\n", taken);
	while((taken + needed_size) > half) {
		// TODO: Overflow check?
		half *= 2;
	}

	size_t new_size = half * 2;
	void *new_mem = malloc(new_size);

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
jay_gc_collect() {
	jay_gc_recollect(0);
	return;

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

	if(next < jay_gc.limit) {
		jay_gc.high_ptr = next;
		return result;
	}

	// If the allocation failed, then collect.
	jay_gc_collect();

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
#ifdef JAY_TRACE_GC
	printf("gc: alloc %zu bytes => %p (high ptr -> %p)\n", size, obj, jay_gc.high_ptr);
	printf("gc: tag = %lu %s\n", tag, jay_gc_tag_name(tag));
#endif
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

#ifdef JAY_TRACE_GC
	puts("--- gc init --- ");
	printf("current_heap = %p\n", jay_gc.current_heap);
	printf("current_size = %p\n", jay_gc.current_size);
	printf("to_space     = %p\n", jay_gc.to_space);
	printf("high_ptr     = %p\n", jay_gc.high_ptr);
	printf("limit        = %p\n", jay_gc.limit);
	printf("from_space   = %p\n", jay_gc.from_space);
	puts("---");
#endif
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

static inline
jay_value
jay_top() {
	return jay_stack_ptr[-1];
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

#define OP_LIT(name, param) \
static inline void \
jay_op_ ## name (param arg) { \
	jay_push(jay_box_ ## name (arg)); \
}

OP_LIT(number, double)
OP_LIT(bool, bool)

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
	size_t bytes = sizeof(jay_closure) + (count * sizeof(jay_value));
	jay_closure *closure = jay_gc_alloc(bytes, JAY_GC_CLOSURE);
	closure->count = count;
	closure->parent = parent;
	// Do we want to zero out the 'values' array..?

	return closure;
}

jay_table*
jay_new_table(size_t entries) {
	size_t entry_bytes = (entries * sizeof(jay_hash_entry));
	size_t bytes = entry_bytes + sizeof(jay_table);
	jay_table *result = jay_gc_alloc(bytes, JAY_GC_TABLE);

	result->table_size = entries;
	result->used_entries = 0;

	memset(result->table, 0, entry_bytes);

	return result;
}

jay_instance*
jay_new_instance(jay_class *class) {
	jay_instance *instance = jay_gc_alloc(sizeof(*instance), JAY_GC_INSTANCE);
	instance->class = class;
	instance->table = NULL;
	// So that the GC can find the instance while we're allocating the table, push it.
	jay_push(jay_box_instance(instance));

	// Allocate a table for the fields.
	instance->table = jay_new_table(8);

	// Clean up GC root.
	jay_pop();

	return instance;
}

/* --- Instance Related "Hash" Map Stuff */

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
	return jay_find_empty_bucket_in(instance->table->table, instance->table->table_size, name);
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
	return jay_find_bucket_in(instance->table->table, instance->table->table_size, name);
}

void
jay_rehash(jay_instance *instance) {
	size_t new_size = instance->table->table_size * 2;
	
	jay_table *new_table = jay_new_table(new_size);

	for(size_t i = 0; i < instance->table->table_size; ++i) {
		if(instance->table->table[i].name != JAY_NAME_TOMBSTONE) {
			// Note: here this should always return a tombstone, as all buckets
			// should be empty...
			jay_hash_entry *ptr = jay_find_empty_bucket_in(new_table->table, new_size, instance->table->table[i].name);
			ptr->name = instance->table->table[i].name;
			ptr->value = instance->table->table[i].value;
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
	if((scope->table->used_entries + 1) > scope->table->table_size / 2) {
		jay_rehash(scope);
	}
	// We are guaranteed an empty bucket somewhere.
	// TODO: Consider Hopgood-Davenport probing
	jay_hash_entry *place = jay_find_empty_bucket(scope, name);
	place->name = name;
	place->value = value;

	scope->table->used_entries += 1;

	return value;
}


/* --- Instance Related Methods --- */

static inline
jay_value
jay_bind(jay_method *method, jay_instance *this) {
	jay_bound_method *result = jay_gc_alloc(sizeof(*result), JAY_GC_BOUND_METHOD);
	result->implementation = method->implementation;
	result->arity = method->arity;
	result->closure = method->parent->closure;

	// TODO: Do we want to be able to have a non-jay_instance "this"?
	// probably not, as it might impede certain optimizations.. but for now,
	// it is a bit silly that we support it in all but name
	result->this = this;
	return jay_box_bound_method(result);
}

#ifdef JAY_FULL_COMPAT

static inline
jay_value
jay_get(jay_value v, size_t name) {
	if(!JAY_IS_INSTANCE(v)) {
		oops("can only look up properties on an instance");
	}
	jay_instance *instance = JAY_AS_INSTANCE(v);

	// In compat mode, we have to look up the field first.
	jay_hash_entry *place = jay_find_bucket(instance, name);
	if(place) {
		return place->value;
	}

	jay_method *method = instance->class->dispatcher(instance->class, name);
	if(method) {
		return jay_bind(method, instance);
	}

	oops("tried to get non-exist property");
}

#else

static inline
jay_value
jay_get(jay_value v, size_t name) {
	if(!JAY_IS_INSTANCE(v)) {
		oops("can only look up properties on an instance");
	}
	jay_instance *instance = JAY_AS_INSTANCE(v);

	jay_method *method = instance->class->dispatcher(instance->class, name);
	if(method) {
		return jay_bind(method, instance);
	}

	// Look up the field on the object.
	jay_hash_entry *place = jay_find_bucket(instance, name);
	if(!place) {
		oops("tried to look up non-existent property");
	}

	return place->value;
}

#endif

static inline
jay_value
jay_get_super(jay_value object, size_t name, jay_value superclass) {
	if(!JAY_IS_INSTANCE(object)) {
		oops("can only look up super properties on an instance");
	}
	if(!JAY_IS_CLASS(superclass)) {
		oops("superclass must be a class");
	}
	jay_instance *instance = JAY_AS_INSTANCE(object);

	// Superclass is "statically bound" so to speak -- see abc_super.lox. 
	// Essentially, the superclass does not change to match the superclass
	// of the class of the current instance (i.e. it is not instance->class->superclass),
	// but rather, it is always the same superclass.
	//
	// As such, we have to somehow explicitly track the superclass. I guess
	// this is why it is mandated to be a variable in lox -- so that that variable
	// can always be directly looked up.
	jay_class *superclass_real = JAY_AS_CLASS(superclass);

	jay_method *method = superclass_real->dispatcher(superclass_real, name);
	if(method) {
		return jay_bind(method, instance);
	}

	oops("superclass has no such method");
}

static inline
jay_value
jay_set(jay_value object, size_t name, jay_value value) {
	if(!JAY_IS_INSTANCE(object)) {
		oops("can only set fields on an instance");
	}
	jay_instance *instance = JAY_AS_INSTANCE(object);

	jay_hash_entry *place = jay_find_bucket(instance, name);
	if(!place) {
		// Slow path is creating a new value on the object. This shouldn't happen
		// too often.
		// TODO: One optimization is that jay_find_bucket() could return
		// the tombstone when it fails, and then, if we don't need to rehash,
		// we could just put the item in that bucket right here...
		//
		// I guess that could be a variant of jay_find_bucket()?
		jay_put_new(instance, name, value);
	}
	else {
		place->value = value;
	}

	return value;
}

static inline
void
jay_op_get(size_t name) {
	// TODO: Figure out whether we should be jay_pop'ing before or after evaluating
	// inner expressions...
	jay_push(jay_get(jay_pop(), name));
}

// Because the superclass must be a variable, we have no need to do any stack
// machine shenanigans (even for GC purposes) -- so always pass it here.
// Same with 'this'. But, the super _op does push a value on to the stack.
static inline
void
jay_op_get_super(jay_value this, size_t name, jay_value superclass) {
	jay_push(jay_get_super(this, name, superclass));
}

static inline
void
jay_op_set(size_t name) {
	// This is an expression, so it must leave a value on the stack. For efficiency,
	// just don't pop the value. I.e. the stack top = object, then value; just pop
	// object.
	jay_value object = jay_pop();
	jay_set(object, name, jay_top());
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

		jay_value new_this = jay_box_instance(jay_new_instance(JAY_AS_CLASS(*fun_value)));
		jay_class *class = JAY_AS_CLASS(jay_pop());
		jay_push(new_this);

		jay_call_any(
			class->methods[0].implementation,
			class->closure,
			class->methods[0].arity,
			arity + 1 // Same as above
		);
		
		// For initializers, we can simply ignore the return value...
		// That said, we do need to implement the actual returning of 'this'
		// inside initializers in the compiler, otherwise the semantic won't
		// be right when we re-call an initializer
		jay_push(new_this);
	}
	else {
		oops("can only call callable objects");
	}
}

static inline
void
jay_op_invoke_super(jay_value object, size_t name, jay_value superclass, size_t arity) {
	if(!JAY_IS_INSTANCE(object)) {
		oops("can only look up super properties on an instance");
	}
	if(!JAY_IS_CLASS(superclass)) {
		oops("superclass must be a class");
	}
	jay_instance *instance = JAY_AS_INSTANCE(object);

	// Superclass is "statically bound" so to speak -- see abc_super.lox. 
	// Essentially, the superclass does not change to match the superclass
	// of the class of the current instance (i.e. it is not instance->class->superclass),
	// but rather, it is always the same superclass.
	//
	// As such, we have to somehow explicitly track the superclass. I guess
	// this is why it is mandated to be a variable in lox -- so that that variable
	// can always be directly looked up.
	jay_class *superclass_real = JAY_AS_CLASS(superclass);

	jay_method *method = superclass_real->dispatcher(superclass_real, name);
	if(method) {
		// Push 'this' on to the stack, then call the method.
		jay_push(object);

		jay_value result = jay_call_any(
			method->implementation,
			instance->class->closure,
			method->arity,
			arity + 1
		);

		jay_push(result);

		return;
	}

	oops("superclass has no such method");
}

#ifdef JAY_FULL_COMPAT

static inline
void
jay_op_invoke(size_t name, size_t arity) {
	// Leave "this" on top in case we have to do a jay_op_get() and jay_op_call()
	jay_value target = jay_top();

	if(!JAY_IS_INSTANCE(target)) {
		oops("can only get properties on an instance");
	}

	jay_instance *instance = JAY_AS_INSTANCE(target);

	// In full compat mode, we have to look up the field first.
	jay_hash_entry *field = jay_find_bucket(instance, name);
	if(field) {
		jay_push(field->value);
		jay_op_call(arity);
		return;
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

#else

static inline
void
jay_op_invoke(size_t name, size_t arity) {
	// Leave "this" on top in case we have to do a jay_op_get() and jay_op_call()
	jay_value target = jay_top();

	if(!JAY_IS_INSTANCE(target)) {
		oops("can only get properties on an instance");
	}

	jay_instance *instance = JAY_AS_INSTANCE(target);

	jay_method *method = instance->class->dispatcher(instance->class, name);
	if(!method) {
		// If the method doesn't exist, it still might be a field, in which case
		// we look it up that way.
		//
		// I believe this differs from clox... so we could try looking up the
		// field first, but that seems a little annoying.
		jay_op_get(name);
		jay_op_call(arity);
		return;
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

#endif

static inline
jay_value
jay_fun_from(jay_function_impl impl, size_t arity, jay_closure *closure) {
	jay_function *f = jay_gc_alloc(sizeof(*f), JAY_GC_FUNCTION);
	f->arity = arity;
	f->closure = closure;
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

#define OP_ONE(name) \
static inline void \
jay_op_ ## name (void) { \
	jay_push(jay_ ## name(jay_pop())); \
}

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
void
jay_dbg_stack(const char *message) {
	if(jay_stack_ptr == jay_stack) {
		printf("%s\t[empty]\n", message);
		return;
	}

	printf("%s\t%llu : ", message, (jay_stack_ptr - jay_stack));
	jay_print(jay_top());
}

static inline
jay_value
jay_add(jay_value a, jay_value b) {
	const char *message = "addition expects two numbers or two strings";
	if(JAY_IS_NUMBER(a)) {
		if(JAY_IS_NUMBER(b)) {
			return jay_box_number(JAY_AS_NUMBER(a) + JAY_AS_NUMBER(b));
		}
	}
	if(a.tag == JAY_STRING) {
		if(b.tag == JAY_STRING) {
			jay_string *as = JAY_AS_STRING(a);
			jay_string *bs = JAY_AS_STRING(b);

			size_t length = as->length + bs->length;
			jay_string *cat = jay_new_string(length);

			memcpy(cat->contents,              as->contents, as->length);
			memcpy(cat->contents + as->length, bs->contents, bs->length);

			// Recompute hash
			cat->hash = jay_compute_string_hash(cat->contents, length);

			return jay_box_string(cat);
		}	
	}
	oops(message);
}
OP_TWO(add)

static inline
jay_value
jay_sub(jay_value a, jay_value b) {
	if(!JAY_IS_NUMBER(a) || !JAY_IS_NUMBER(b)) {
		oops("subtraction expects two numbers");
	}
	return jay_box_number(JAY_AS_NUMBER(a) - JAY_AS_NUMBER(b));
}
OP_TWO(sub)

static inline
jay_value
jay_mul(jay_value a, jay_value b) {
	if(!JAY_IS_NUMBER(a) || !JAY_IS_NUMBER(b)) {
		oops("multiplication expects two numbers");
	}
	return jay_box_number(JAY_AS_NUMBER(a) * JAY_AS_NUMBER(b));
}
OP_TWO(mul)

static inline
jay_value
jay_div(jay_value a, jay_value b) {
	if(!JAY_IS_NUMBER(a) || !JAY_IS_NUMBER(b)) {
		oops("division expects two numbers");
	}
	return jay_box_number(JAY_AS_NUMBER(a) / JAY_AS_NUMBER(b));
}
OP_TWO(div)

static inline
jay_value
jay_gt(jay_value a, jay_value b) {
	if(!JAY_IS_NUMBER(a) || !JAY_IS_NUMBER(b)) {
		oops("comparison (>) expects two numbers");
	}
	return jay_box_bool(JAY_AS_NUMBER(a) > JAY_AS_NUMBER(b));
}
OP_TWO(gt)

static inline
jay_value
jay_ge(jay_value a, jay_value b) {
	if(!JAY_IS_NUMBER(a) || !JAY_IS_NUMBER(b)) {
		oops("comparison (>=) expects two numbers");
	}
	return jay_box_bool(JAY_AS_NUMBER(a) >= JAY_AS_NUMBER(b));
}
OP_TWO(ge)

static inline
jay_value
jay_lt(jay_value a, jay_value b) {
	if(!JAY_IS_NUMBER(a) || !JAY_IS_NUMBER(b)) {
		oops("comparison (<) expects two numbers");
	}
	return jay_box_bool(JAY_AS_NUMBER(a) < JAY_AS_NUMBER(b));
}
OP_TWO(lt)

static inline
jay_value
jay_le(jay_value a, jay_value b) {
	if(!JAY_IS_NUMBER(a) || !JAY_IS_NUMBER(b)) {
		oops("comparison (<=) expects two numbers");
	}
	return jay_box_bool(JAY_AS_NUMBER(a) <= JAY_AS_NUMBER(b));
}
OP_TWO(le)

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

static inline
jay_value
jay_not(jay_value v) {
	return jay_box_bool(!jay_truthy(v));
}
OP_ONE(not)

static inline
jay_value
jay_negate(jay_value v) {
	if(!JAY_IS_NUMBER(v)) {
		oops("negation expects a number");
	}
	return jay_box_number(-JAY_AS_NUMBER(v));
}
OP_ONE(negate)

/* --- Builtin Functions (e.g. clock) --- */

static
jay_value
jay_std_clock(jay_value *args, jay_closure *closure) {
	clock_t time = clock();
	double ms = (time * 1000.0 / CLOCKS_PER_SEC);
	return jay_box_number(ms);
}

#endif