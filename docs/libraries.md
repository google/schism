Schism has basic support for library imports. Eventually, it should support the
whole R6RS library system. Currently, we support a subset of this that helps us
 organize the compiler code much better.

 Schism compiles libraries as more of a preprocessor step. It starts by
 transitively reading all the libraries referenced by a program. This gives us
 a module list, which contains the bodies of all the imported libraries.

 From there, we visit each module, constructing an environment based on what
 each module imports. Things are sufficiently alpha renamed so we can respect
 namespace. After this resolution step, we flatten all the modules into a
 single list of functions, and compilation proceeds as normal.
 