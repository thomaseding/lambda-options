# lambda-options

A declarative command-line parser for C++.

* Easy to use. The API is expressive.
* Easy to learn. The API is tiny and simple.
* Easy to integrate. Contained in a single [header file](cpp/src/LambdaOptions.h?ts=4).

This library is designed to be an intuitive alternative to other C++ command-line libraries such as: Boost.Program_Options, GNU Getopt, TCLAP, and others.

Permissive licensing (BSD) allows you download, use, and modify for your own use... commercial, proprietary, or otherwise.

Quick installation: Download the required header here: [cpp/src/LambdaOptions.h](cpp/src/LambdaOptions.h?ts=4)

* Examples: [cpp/example](cpp/example)
* Documentation: [wiki/API-Index](../../wiki/API-Index)

--------------

Basic example:
```cpp
#include "LambdaOptions.h"
#include <iostream>

int main (int argc, char ** argv)
{
	LambdaOptions<char> opts;

	opts.AddOption("--help", [] () {
		std::cout << "--user NAME [AGE]" << std::endl;
	});
	opts.AddOption("--user", [] (std::string name) {
		std::cout << "Name:" << name << std::endl;
	});
	opts.AddOption("--user", [] (std::string name, unsigned int age) {
		std::cout << "Name:" << name << " Age:" << age << std::endl;
	});

	auto parseContext = opts.CreateParseContext(argv + 1, argv + argc);

	try {
		parseContext.Run();
	}
	catch (lambda_options::ParseFailedException const &) {
		return 1;
	}

	return 0;
}
```


