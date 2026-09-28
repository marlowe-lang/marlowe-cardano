# Refactor cardanoCli.ts

* I just introduced execCardanoCli and execCardanoCliJsonTyped to the cardanoCli.ts

* We want to migrate all the existing functions **from this module** so they use that new implementation.

* All the functions should start accepting `debug` argument as well.

* Please do the refactoring without testing but only checking if the code compiles.
