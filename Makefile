include .config

all: jaylox-debug

jaylox-debug: .config
	RUSTFLAGS=$(RUSTFLAGS) cargo build $(FEATURES)
	cp -f target/debug/jlox jaylox

jaylox-release: .config
	RUSTFLAGS=$(RUSTFLAGS) cargo build --release $(FEATURES)
	cp -f target/release/jlox jaylox

.config:
	if ! [ -f .config ]; then printf "RUSTFLAGS=\nFEATURES=\n" > .config; fi

clean:
	rm -f jaylox
	cargo clean

.PHONY: all jaylox-debug jaylox-release