if [ -d extern/craftinginterpreters ]; then
    # Repo is already cloned, just setup our own tests.
    # Delete the directory first in case we deleted some tests.
    rm -rf extern/craftinginterpreters/test/jaylox
    cp -r -T tests extern/craftinginterpreters/test/jaylox
else
    git clone https://github.com/munificent/craftinginterpreters extern/craftinginterpreters --depth 1
fi

# Prepare the tester in case it wasn't prepared yet.
cd extern/craftinginterpreters
make get