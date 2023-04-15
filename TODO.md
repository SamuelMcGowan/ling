# Todo

## Next

## Later

- match, type, enum, data, typeclass, impl
- resolve types
- print compile errors (codespan)
- allow "forward referencing" functions and types

## Minor

- generally always recover from parsing within a token tree
  e.g. if there is an error within a function's arguments,
  don't stop parsing the function
- compare floats better (`ApproxFloat` struct?)
- pretty-print token kinds
