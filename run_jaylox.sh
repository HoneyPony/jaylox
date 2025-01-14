list_needing_conform=$(cat <<EOF

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

#conform=""
#if echo "$1" | grep "operator/" >/dev/null; then
	# If the test contains "operator" in the path and "nonnum", it's
	# testing a runtime error for some operator.
	# (regex: "operator/.*nonnum")
	#
	# TODO: This also applies to the operator/add_num_nil and add_bool_num,
	# etc... for now just -conformance in all tests in operator/.
	#
	# But we usually have a compile-time error for those operators,
	# as they get constant folded. So we have to explicitly turn off
	# that folding and get a runtime error.
#	conform="-conformance"
#fi
if echo "$list_needing_conform" | grep -w -q "$1"; then
	conform="-conformance"
fi
test_realpath=$(realpath "$1")
cd $(dirname "$0")
rm -f ./out ./out.c
./target/debug/jlox -oc out.c -nanbox -backtrace -enablenames $conform $test_realpath
err="$?"
if [ $err -ne 0 ]; then exit $err; fi
tcc out.c -o out -w
if [ -f ./out ]; then
	./out
	err="$?"
	exit $err
fi
