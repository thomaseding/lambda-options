#include "../src/LambdaOpts.h"

#include <iostream>


//////////////////////////////////////////////////////////////////////////


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




