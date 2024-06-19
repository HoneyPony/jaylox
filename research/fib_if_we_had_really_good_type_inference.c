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

	if(args[0].as_double <= 1) {
		return args[0];
	}
	jay_push(jay_number(args[0].as_double - 2));
	double a = jay_call(globals[1].as_function, 1).as_double;
	jay_push(jay_number(args[0].as_double - 1));
	double b = jay_call(globals[1].as_function, 1).as_double;

	return jay_number(a + b);
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
		globals[0] = jay_number(0);
		for(;;) {
			if(!(globals[0].as_double < ITERS)) { break; }
			{
				{
					jay_push(globals[0]);
					jay_op_call_direct(globals[1].as_function, 1);
					jay_op_print();
				}
				globals[0].as_double += 1;
			}
		}
	}
}
