all: fib_gen fib_man fib_c fib_unreal

fib_gen: fib_generated.c
	gcc $^ -o $@ $(CFLAGS)

fib_man: fib_manual_opt.c
	gcc $^ -o $@ $(CFLAGS)

fib_c: in_c.c
	gcc $^ -o $@ $(CFLAGS)

fib_unreal: fib_if_we_had_really_good_type_inference.c
	gcc $^ -o $@ $(CFLAGS)

clean:
	rm -f fib_gen fib_man fib_c fib_unreal