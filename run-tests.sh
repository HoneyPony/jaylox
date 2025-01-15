if ! [ -d extern/craftinginterpreters ]; then
    echo "Run './fetch-tester.sh' first -- this downloads the craftinginterpreters repo to access the test cases.";
    exit 1;
fi
cd extern/craftinginterpreters

echo "--- Running Normal ---"
dart tool/bin/test.dart jlox --interpreter ../../run-jaylox.sh
echo "--- Running NanBox ---"
dart tool/bin/test.dart jlox --interpreter ../../run-jaylox.sh --arguments "-nanbox"
echo "--- Running Full Conform ---"
dart tool/bin/test.dart jlox --interpreter ../../run-jaylox.sh --arguments "-conformance"
echo "--- Running NanBox + Full Conform ---"
dart tool/bin/test.dart jlox --interpreter ../../run-jaylox.sh --arguments "-nanbox -conformance"
cd ../..
rm -f run-jaylox-out
rm -f run-jaylox-out.c