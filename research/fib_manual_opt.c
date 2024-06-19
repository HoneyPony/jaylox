/*** This C file created by jaylox https://github.com/HoneyPony/jaylox ***/
#include "../jaylib/jaylib.h"

static jay_value globals[2];
jay_value fib(jay_value *arguments, jay_closure *closure);

/* --- NAME Definitions --- */

#define NAME_init ((size_t)1)
#define NAME_this ((size_t)2)
#define NAME_super ((size_t)3)

/* --- Function Definitions --- */

jay_value
fib(jay_value *args, jay_closure *closure) {
	jay_instance *scope = NULL;

	if(jay_truthy(jay_le(args[0], jay_number(1)))) {
		return args[0];
	}
	jay_push(jay_sub(args[0], jay_number(2)));
	jay_op_call_direct(globals[1].as_function, 1);
	jay_push(jay_sub(args[0], jay_number(1)));
	jay_op_call_direct(globals[1].as_function, 1);
	jay_op_add();
	return jay_pop();
	return jay_null();
}

/* --- main() --- */

#ifndef ITERS
	#define ITERS 30
#endif

int
main(void) {
	JAY_THIS = NAME_this;

	jay_stack_ptr = jay_stack;

	jay_frames_ptr = 0;

	jay_closure *scope = NULL;
	globals[1] = jay_fun_from(fib, 1, scope);
	{
		jay_op_number(0);
		globals[0] = jay_pop();
		for(;;) {
			if(!jay_truthy(jay_lt(globals[0], jay_number(ITERS)))) { break; }
			{
				{
					jay_push(globals[0]);
					jay_op_call_direct(globals[1].as_function, 1);
					jay_op_print();
				}
				globals[0] = jay_add(globals[0], jay_number(1));
			}
		}
	}
}
