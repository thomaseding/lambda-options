



* Write a program that generates the repetitive code?
* Make a `LambdaOptsImpl` that is stored in a `std::shared_ptr`
* Does the code accept functions with const arguments such as `opts.AddOption("", [] (int const) { ... });`
* Check that the code does not compile when given a pointer or a reference as a parameter for an option callback.
* Check that code works when either/both `LambdaOpts` and `ParseEnv` are passed to different scopes. Especially when their option functions leave scope (e.g. the functions should rely on internal copies, rather than references).
* Test ALL public interfaces.
* Write docs.
* Add in `std::array` support
* Add in support for custom `TypeTag`s. Might need to be of the following macro API
```cpp
#ifdef LAMBDA_OPTS_CUSTOM_TAG_CLASSES
#	include LAMBDA_OPTS_CUSTOM_TAG_CLASSES
#endif
#ifdef LAMBDA_OPTS_CUSTOM_TAG_CASE_STATEMENTS
#	include LAMDBA_OPTS_CUSTOM_TAG_CASE_STATEMENTS
#endif
```
Can probably avoid that and store TypeKind's into an `std::map<TypeKind, Parser>`,
where `Parser` might be 
```cpp
typedef std::function<xxx> OpaqueParser;
OpaqueParser parser = [] (String const & str) {
	return UniqueOpaque(TypeTag<int>::Parse(str).release(), TypeTag<int>::Delete);
};
```


