# lambda-opts
lambda-opts


A declarative command line parser for C++.

* Easy to use. The API is expressive.
* Easy to learn. The API is tiny and simple.
* Easy to integrate. Contained in a single [header file](cpp/src/LambdaOpts.h).
* ~~Robust.~~ (TODO: Write unit tests.)


Quick installation: Download the required header here: [cpp/src/LambdaOpts.h](cpp/src/LambdaOpts.h)


Example:
```cpp
#include "LambdaOpts.h"
#include <iostream>

int main (int argc, char ** argv)
{
	LambdaOpts<char> opts;

	opts.Add("--help", [] () {
		std::cout << "--user NAME [AGE]" << std::endl;
	});
	opts.Add("--user", [] (std::string name) {
		std::cout << "Name:" << name << std::endl;
	});
	opts.Add("--user", [] (std::string name, unsigned int age) {
		std::cout << "Name:" << name << " Age:" << age << std::endl;
	});

	auto parseEnv = opts.NewParseEnv(std::vector<std::string>(argv + 1, argv + argc));

	int parseFailureIndex;
	if (!parseEnv.Parse(parseFailureIndex)) {
		std::cout << "Parse failed at index " << parseFailureIndex << std::endl;
		return 1;
	}

	return 0;
}
```


