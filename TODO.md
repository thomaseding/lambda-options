



* Write a program that generates the repetitive code?
* Make a `LambdaOptsImpl` that is stored in a `std::shared_ptr`
* Does the code accept `opts.AddOption("", [] (int const) { ... });`?
* Does the code accept `opts.AddOption("", [] (int &&) { ... });`?
* Does the code accept `opts.AddOption("", [] (int &) { ... });`?
* Does the code accept `opts.AddOption("", [] (int const &) { ... });`?
* Check that code works when either/both `LambdaOpts` and `ParseEnv` are passed to different scopes. Especially when their option functions leave scope (e.g. the functions should rely on internal copies, rather than references).
* Test ALL public interfaces.
* Write docs.
* Test `std::array` support. Do nested arrays work as well?
* Add in support for custom type parsers? The best way to do this would probably be to expose a class to specialize. This class would have parse logic as well as some sort of type kind.
* Can probabaly generate a type id programatically via a template like this
```
typedef void const * TypeId;
template <typename T>
static TypeId GetTypeId ()
{
    static char const uniqueMemLoc;
    return &uniqueMemLoc;
}
```
