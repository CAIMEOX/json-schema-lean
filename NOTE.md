# Notes About Json Schema and Bowtie
> This note refers to the `json-schema` and `bowtie` documents.

## Terminology
- validation API: functions that evaluate a schema against a specific instance
- test harness / test runner: a small program which accepts **test cases** sent from Bowtie, passes them through a validation API and returns the results to Bowtie
- test case: JSON schema and instance which Bowtie will pass to implementation
- IHOP: the input → harness → output protocol.

