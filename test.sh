bowtie run --dialect 7 -i localhost/lean-jsonschema:latest -V <<EOF
{"description": "test case 1", "schema": {}, "tests": [{"description": "a test", "instance": {}}] }
{"description": "test case 2", "schema": {"const": 37}, "tests": [{"description": "not 37", "instance": {}}, {"description": "is 37", "instance": 37}] }
EOF