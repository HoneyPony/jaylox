include .config

all: jaylox-debug

jaylox-debug: .config
	RUSTFLAGS=$(RUSTFLAGS) cargo build $(FEATURES)
	cp -f target/debug/jlox jaylox-debug

jaylox-release: .config
	RUSTFLAGS=$(RUSTFLAGS) cargo build --release $(FEATURES)
	cp -f target/release/jlox jaylox-release

.config:
	if ! [ -f .config ]; then printf "RUSTFLAGS=\nFEATURES=\n" > .config; fi

clean:
	cargo clean

.PHONY: all jaylox-debug jaylox-release clean