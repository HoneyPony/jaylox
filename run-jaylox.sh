extra_opts=""
test_given_path=""
if [ "$#" -gt 1 ]; then
	extra_opts="$1"
	test_given_path="$2"
else
	test_given_path="$1"
fi
opts="-backtrace -enablenames $extra_opts"
cc="tcc"
c_name="./run-jaylox-out.c"
exe_name="./run-jaylox-out"

# For a lot of tests we need "full conformance."
# Basically, the tester is expecting things like runtime errors for numerical
# operations that, when constant folded, simply have an error at compile time;
# or for undeclared global variables that we produce compile time errors for.
#
# Enabling the full conformance mode allows these tests to pass, but we want
# to keep it off when possible to find other errors (e.g. several errors in our
# code were caused by optimizations).
#
# Ideally we also run as many tests possible in both conformance and non-conformance
# mode, but we haven't set that up quite yet. (We could also test both with
# and without nanbox).
list_needing_conform=$(cat <<EOF

# A lot of operator tests require full conformance due to constant folding.
test/operator/divide_num_nonnum.lox
test/operator/add_num_nil.lox
test/operator/less_nonnum_num.lox
test/operator/add_string_nil.lox
test/operator/add_nil_nil.lox
test/operator/multiply_nonnum_num.lox
test/operator/subtract_nonnum_num.lox
test/operator/greater_or_equal_nonnum_num.lox
test/operator/greater_or_equal_num_nonnum.lox
test/operator/greater_num_nonnum.lox
test/operator/less_or_equal_nonnum_num.lox
test/operator/less_or_equal_num_nonnum.lox
test/operator/add_bool_nil.lox
test/operator/add_bool_string.lox
test/operator/subtract_num_nonnum.lox
test/operator/multiply_num_nonnum.lox
test/operator/less_num_nonnum.lox
test/operator/divide_nonnum_num.lox
test/operator/add_bool_num.lox
test/operator/greater_nonnum_num.lox

# These other tests require full conformance for other reasons, normally due to the
# requirement for runtime-error global variable failures.
test/function/local_mutual_recursion.lox
test/string/error_after_multiline.lox
test/variable/unreached_undefined.lox
test/variable/undefined_global.lox
test/variable/undefined_local.lox
test/for/statement_initializer.lox
test/assignment/undefined.lox
test/field/set_evaluation_order.lox
test/method/refer_to_name.lox
test/string/error_after_multiline.lox
test/return/at_top_level.lox
test/unexpected_character.lox
test/inheritance/inherit_from_nil.lox
EOF
)

conform=""
if echo "$list_needing_conform" | grep -w -q "$test_given_path"; then
	conform="-conformance"
fi
test_realpath=$(realpath "$test_given_path")
cd $(dirname "$0")
rm -f "$c_name" "$exe_name"

jaylox=""
# Note: This file will be created by running either 'make jaylox-debug'
# or 'make jaylox-release'
if [ -f ./jaylox ]; then
	jaylox="./jaylox"
else
	exit 10
fi

"$jaylox" -oc "$c_name" $opts $conform "$test_realpath"
err="$?"

if [ $err -ne 0 ]; then exit $err; fi
"$cc" "$c_name" -o "$exe_name" -w
if [ -f "$exe_name" ]; then
	"$exe_name"
	err="$?"
	exit $err
fi

exit 20