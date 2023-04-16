# Todo

- type
- match
- impl
- typeclass
- print compile errors (codespan)
- *type checking*
- resolve field accesses (after type checking)

## Minor

- generally always recover from parsing within a token tree
  e.g. if there is an error within a function's arguments,
  don't stop parsing the function
- compare floats better (`ApproxFloat` struct?)
- pretty-print token kinds
