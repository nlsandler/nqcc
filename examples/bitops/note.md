The following cases are undefined, so we don't test them:

* Bit shift count is negative (e.g. `1>>-2`).
* Bit shift count is greater than width of type (e.g. `1>>33`; width of type is 32).