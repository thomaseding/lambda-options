# lambda-options

A modern command-line parser for C++.

* Easy to use. The API is expressive.
* Easy to learn. The API is tiny and simple.
* Easy to integrate. Contained in a single [header file](src/LambdaOptions.h?ts=4).
* BSD 1-Clause License

This library is designed to be an intuitive alternative to other C++ command-line libraries such as: Boost.Program_Options, GNU Getopt, TCLAP, and others.

Quick installation: Download the required header here: [src/LambdaOptions.h](src/LambdaOptions.h?ts=4)

* Examples: [example](example)
* Documentation: [API-Index](https://github.com/thomaseding/wiki-lambda-options-cpp/wiki/API-Index)

--------------

Basic example:
```cpp
#include "LambdaOptions.h"
#include <iostream>

int main (int argc, char ** argv)
{
	using namespace lambda_options::with_char;

	bool doHelp = false;

	Options opts;

	opts.AddOption(Keyword("help", "h")	
		.Text("Display this help text."),
	[&] () {
		doHelp = true;
	});
	opts.AddOption(Keyword("user")
		.ArgText("USER")
		.Text("Prints name."),
	[] (std::string name) {
		std::cout << "Name:" << name << std::endl;
	});
	opts.AddOption(Keyword("user")
		.ArgText("USER AGE")
		.Text("Prints name and age."),
	[] (std::string name, unsigned int age) {
		std::cout << "Name:" << name << " Age:" << age << std::endl;
	});

	auto help = [&] {
		std::cout << "Usage: example.exe [OPTIONS...]" << std::endl;
		std::cout << options.HelpDescription() << std::endl;
	};
	auto badArgs = [&](std::string const & message) {
		std::cout << message << std::endl;
		help();
		return 1;
	};

	auto parseContext = options.CreateParseContext(argv + 1, argv + argc);
	try {
		parseContext.Run();
	}
	catch (ParseFailedException const & e) {
		return badArgs(e.message);
	}

	if (helpRequested) {
		help();
	}

	return 0;
}
```

```
$ example.exe --user HaskellCurry 81 --user GraceHopper
Name:HaskellCurry Age:81
Name:GraceHopper
$ example.exe -h
Usage:
 -h, --help                  Display this help text.
     --user NAME             Prints name.
     --user NAME AGE         Prints name and age.
$ example.exe --user Pythagoras LXXV
Unknown option at index 2: `LXXV'
Usage:
 -h, --help                  Display this help text.
     --user NAME             Prints name.
     --user NAME AGE         Prints name and age.
```
