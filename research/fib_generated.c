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

	jay_push(args[0]);
	jay_op_number(1);
	jay_op_le();
	if(jay_pop_condition()) {
		jay_push(args[0]);
		return jay_pop();
	}
	jay_push(args[0]);
	jay_op_number(2);
	jay_op_sub();
	jay_push(globals[1]);
	jay_op_call(1);
	jay_push(args[0]);
	jay_op_number(1);
	jay_op_sub();
	jay_push(globals[1]);
	jay_op_call(1);
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
			jay_push(globals[0]);
			jay_op_number(ITERS);
			jay_op_lt();
			if(!jay_pop_condition()) { break; }
			{
				{
					jay_push(globals[0]);
					jay_push(globals[1]);
					jay_op_call(1);
					jay_op_print();
				}
				jay_push(globals[0]);
				jay_op_number(1);
				jay_op_add();
				jay_push(globals[0] = jay_pop());
				jay_pop();
			}
		}
	}
}
