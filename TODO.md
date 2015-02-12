* Write a program that generates the repetitive code?
* Does the code accept `opts.AddOption("", [] (int const) { ... });`?
* Does the code accept `opts.AddOption("", [] (int &&) { ... });`?
* Does the code accept `opts.AddOption("", [] (int &) { ... });`?
* Does the code accept `opts.AddOption("", [] (int const &) { ... });`?
* Check that code works when either/both `LambdaOpts` and `ParseEnv` are passed to different scopes. Especially when their option functions leave scope (e.g. the functions should rely on internal copies, rather than references).
* Test ALL public interfaces.
* Write docs.
* Do I want to supply a default `RawParser` for `bool`? Probably not because what should it accept? `"True"`, `"true"`, `"TRUE"`, `"1"`, `"t"`, etc. Probably best left for the consumer and/or in a seperate parser-extension header.
