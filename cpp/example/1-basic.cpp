#include "../src/LambdaOptions.h"
#include <iostream>


//////////////////////////////////////////////////////////////////////////


int main (int argc, char ** argv)
{
	lambda_options::Options<char> opts;

	opts.AddOption("--help", [] () {
		std::cout << "--user NAME [AGE]\n";
	});
	opts.AddOption("--user", [] (std::string name) {
		std::cout << "Name:" << name << "\n";
	});
	opts.AddOption("--user", [] (std::string name, unsigned int age) {
		std::cout << "Name:" << name << " Age:" << age << "\n";
	});

	auto parseContext = opts.CreateParseContext(argv + 1, argv + argc);

	try {
		parseContext.Run();
	}
	catch (lambda_options::ParseFailedException const &) {
		std::cout << "Bad arguments.";
		return 1;
	}

	return 0;
}




