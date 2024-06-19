all: fib_gen fib_man fib_c

fib_gen: fib_generated.c
	gcc $^ -o $@ $(CFLAGS)

fib_man: fib_manual_opt.c
	gcc $^ -o $@ $(CFLAGS)

fib_c: in_c.c
	gcc $^ -o $@ $(CFLAGS)

clean:
	rm -f fib_gen fib_man fib_c