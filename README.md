# lambda-opts

A declarative command line parser for C++.

* Easy to use. The API is expressive.
* Easy to learn. The API is tiny and simple.
* Easy to integrate. Contained in a single [header file](cpp/src/LambdaOpts.h?ts=4).

Quick installation: Download the required header here: [cpp/src/LambdaOpts.h](cpp/src/LambdaOpts.h?ts=4)

Examples directory: [cpp/example](cpp/example)

###Docs as well as more examples coming soon!

--------------

Basic example:
```cpp
#include "LambdaOpts.h"
#include <iostream>

int main (int argc, char ** argv)
{
	LambdaOpts<char> opts;

	opts.AddOption("--help", [] () {
		std::cout << "--user NAME [AGE]" << std::endl;
	});
	opts.AddOption("--user", [] (std::string name) {
		std::cout << "Name:" << name << std::endl;
	});
	opts.AddOption("--user", [] (std::string name, unsigned int age) {
		std::cout << "Name:" << name << " Age:" << age << std::endl;
	});

	auto parseEnv = opts.CreateParseEnv(argv + 1, argv + argc);

	try {
		parseEnv.Run()
	}
	catch (lambda_opts::ParseFailedException const &) {
		std::cout << "Bad arguments.";
		return 1;
	}

	return 0;
}
```


