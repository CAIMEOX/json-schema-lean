#!/usr/bin/env fish

# Completed Parts
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/const.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/enum.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/maximum.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/minimum.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/exclusiveMaximum.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/exclusiveMinimum.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/maxLength.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/minLength.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/required.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/multipleOf.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/type.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/boolean_schema.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/allOf.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/anyOf.json | bowtie summary --show failures
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7/oneOf.json | bowtie summary --show failures

# Runall
bowtie suite -i localhost/lean-jsonschema:latest https://github.com/json-schema-org/JSON-Schema-Test-Suite/blob/main/tests/draft7 | bowtie summary --show failures
