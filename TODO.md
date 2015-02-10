



* Write a program that generates the repetitive code?
* Does the code accept `opts.AddOption("", [] (int const) { ... });`?
* Does the code accept `opts.AddOption("", [] (int &&) { ... });`?
* Does the code accept `opts.AddOption("", [] (int &) { ... });`?
* Does the code accept `opts.AddOption("", [] (int const &) { ... });`?
* Check that code works when either/both `LambdaOpts` and `ParseEnv` are passed to different scopes. Especially when their option functions leave scope (e.g. the functions should rely on internal copies, rather than references).
* Test ALL public interfaces.
* Write docs.
* Test `std::array` support. Do nested arrays work as well?
* Lift the restriction that custom parse types need to be default constructible.
