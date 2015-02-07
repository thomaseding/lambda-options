#include "../src/LambdaOpts.h"

#include <iostream>


//////////////////////////////////////////////////////////////////////////


int main (int argc, char ** argv)
{
	typedef LambdaOpts<char> Opts;
	Opts opts;

	opts.Add("--help", [] () {
		std::cout << "--user NAME [AGE]" << std::endl;
		return Opts::ParseResult::Accept;
	});
	opts.Add("--user", [] (std::string name) {
		std::cout << "Name:" << name << std::endl;
		return Opts::ParseResult::Accept;
	});
	opts.Add("--user", [] (std::string name, unsigned int age) {
		std::cout << "Name:" << name << " Age:" << age << std::endl;
		return Opts::ParseResult::Accept;
	});

	auto parseEnv = opts.NewParseEnv(argv + 1, argv + argc);

	int parseFailureIndex;
	if (!parseEnv.Parse(parseFailureIndex)) {
		std::cout << "Parse failed at index " << parseFailureIndex << std::endl;
		return 1;
	}

	return 0;
}




