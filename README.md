# lambda-opts
lambda-opts


A declarative command line parser for C++.


Example:
```
#include "LambdaOpts.h"
#include <iostream>

int main (int argc, char ** argv)
{
	LambdaOpts<char> opts;

	opts.Add("--help", [] () {
		std::cout << "--user NAME AGE" << std::endl;
	});
	opts.Add("--num", [] (int num) {
		std::cout << "Num:" << num << std::endl;
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


