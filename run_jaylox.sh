opts="-nanbox -backtrace -enablenames"
cc="tcc"
c_name="./run-jaylox-out.c"
exe_name="./run-jaylox-out"
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

conform=""
if echo "$list_needing_conform" | grep -w -q "$1"; then
	conform="-conformance"
fi
test_realpath=$(realpath "$1")
cd $(dirname "$0")
rm -f "$c_name" "$exe_name"
./target/debug/jlox -oc "$c_name" $opts $conform "$test_realpath"
err="$?"
if [ $err -ne 0 ]; then exit $err; fi
"$cc" "$c_name" -o "$exe_name" -w
if [ -f "$exe_name" ]; then
	"$exe_name"
	err="$?"
	exit $err
fi