all: fib_gen fib_man

fib_gen: fib_generated.c
	gcc $^ -o $@ $(CFLAGS)

fib_man: fib_manual_opt.c
	gcc $^ -o $@ $(CFLAGS)

clean:
	rm fib_gen fib_man