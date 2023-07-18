/*!
Overview of compilation process:
- parse all modules and store in module tree
- declare all globals
- run passes for each module:
    - resolve names
    - typecheck
*/

mod module_tree;
