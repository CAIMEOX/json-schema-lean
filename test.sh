#!/usr/bin/env bash
set -eo pipefail

# Completed Parts - Tests that pass 100%
OUTPUT=$(mktemp)
trap "rm -f $OUTPUT" EXIT

FAILED=0

run_test() {
    local suite=$1
    if ! bowtie suite -i localhost/lean-jsonschema:latest "$suite" | tee -a "$OUTPUT" | bowtie summary --show failures; then
        FAILED=1
    fi
    # Also check JSON output for failures
    if grep -q '"failed": true' "$OUTPUT"; then
        FAILED=1
    fi
}

run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/const.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/enum.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/maximum.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/minimum.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/exclusiveMaximum.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/exclusiveMinimum.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/maxLength.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/minLength.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/required.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/multipleOf.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/type.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/boolean_schema.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/allOf.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/anyOf.json
run_test https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/oneOf.json

if [ $FAILED -eq 1 ]; then
    echo "Some tests failed!" >&2
    exit 1
fi

echo "Running Full Test Suite (with other features)"
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7 | tee "$OUTPUT" | bowtie summary --show failures
