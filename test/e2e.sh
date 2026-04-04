#!/bin/bash

set -e

HJQ="cabal run hjq --verbose=0 --"
PASS=0
FAIL=0

assert_eq() {
  local description="$1"
  local expected="$2"
  local actual="$3"
  if [ "$expected" = "$actual" ]; then
    echo "[PASS] $description"
    PASS=$((PASS + 1))
  else
    echo "[FAIL] $description"
    echo "  expected: $expected"
    echo "  actual:   $actual"
    FAIL=$((FAIL + 1))
  fi
}

assert_exit_code() {
  local description="$1"
  local expected="$2"
  local actual="$3"
  if [ "$expected" = "$actual" ]; then
    echo "[PASS] $description"
    PASS=$((PASS + 1))
  else
    echo "[FAIL] $description"
    echo "  expected exit code: $expected"
    echo "  actual exit code:   $actual"
    FAIL=$((FAIL + 1))
  fi
}

# identity
actual=$(echo '{"foo":"bar"}' | $HJQ '.')
expected=$(printf '{\n    "foo": "bar"\n}')
assert_eq "identity (.)" "$expected" "$actual"

# field access
actual=$(echo '{"foo":"bar"}' | $HJQ '.foo')
expected='"bar"'
assert_eq "field access (.foo)" "$expected" "$actual"

# nested field
actual=$(echo '{"foo":{"bar":"baz"}}' | $HJQ '.foo.bar')
expected='"baz"'
assert_eq "nested field (.foo.bar)" "$expected" "$actual"

# array index
actual=$(echo '{"arr":["a","b","c"]}' | $HJQ '.arr[1]')
expected='"b"'
assert_eq "array index (.arr[1])" "$expected" "$actual"

# invalid JSON exits with code 1
set +e
echo 'not json' | $HJQ '.' > /dev/null 2>&1
assert_exit_code "invalid JSON exits with code 1" "1" "$?"
set -e

# invalid query exits with code 1
set +e
echo '{"foo":"bar"}' | $HJQ 'invalid' > /dev/null 2>&1
assert_exit_code "invalid query exits with code 1" "1" "$?"
set -e

echo ""
echo "Results: $PASS passed, $FAIL failed"
[ $FAIL -eq 0 ]
