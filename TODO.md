* Write a program that generates the repetitive code?
* Write documentation.
* Write tutorial (Beyond the existing iterative examples; e.g. Match name of cpp files but with *.md)
* Add more examples.
* Flesh out `Keyword::desc`/`LambdaOptions<Char>::HelpDescription` a little more.
* Keyword match modes: Case-insensitive, relaxed dashes, relaxed underscores.
* Keyword name styles: Exact (no prefixes on long or short), GNU (long "--" & short "-"), Windows (long "/" & short "/"). If added prefixes exist (or exist in part), add only as much as needed.
* Optional short option cluster parsing.
* Create a Haskell port.
* Store callback functions uniformly, such that the engine is not limited to hard coded arities. This will improve the design of the library and make the change to vararg template easy (when I ditch support for compilers that don't support vararg templates).
* May want to augment lambda_options::char_typedefs. (With a template class where one can grab non-templated typedefs.)
