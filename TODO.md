# Todo

- type
- match
- impl
- typeclass
- print compile errors (codespan)
- *type checking*
- resolve field accesses (after type checking)

## Name/module resolution

- after parsing each module, fetch all submodules and parse them
- global forward declaration pass goes over all modules
- then it's easy to resolve names between modules as they are stored in a flat list with resolved globals

## Minor

- generally always recover from parsing within a token tree
  e.g. if there is an error within a function's arguments,
  don't stop parsing the function
- compare floats better (`ApproxFloat` struct?)
- pretty-print token kinds
