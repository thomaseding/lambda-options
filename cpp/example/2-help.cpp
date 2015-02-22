#include "../src/LambdaOptions.h"
#include <iostream>


//////////////////////////////////////////////////////////////////////////


int main (int argc, char ** argv)
{
	using namespace lambda_options::char_typedefs;

	Options opts;

	auto printHelp = [&] () {
		std::cout << "Usage: prog.exe [OPTIONS]\n\n";
		std::cout << opts.HelpDescription();
	};

	Keyword kwHelp("--help", 'h');
	kwHelp.desc = "Display this help message.";
	opts.AddOption(kwHelp, printHelp);

	Keyword kwUser1("--user");
	kwUser1.args = "NAME";
	kwUser1.desc = "Prints user's name.";
	opts.AddOption(kwUser1, [] (std::string name) {
		std::cout << "Name:" << name << "\n";
	});

	Keyword kwUser2("--user");
	kwUser2.args = "NAME AGE";
	kwUser2.desc = "Prints user's name and age.";
	opts.AddOption(kwUser2, [] (std::string name, unsigned int age) {
		std::cout << "Name:" << name << " Age:" << age << "\n";
	});

	auto parseContext = opts.CreateParseContext(argv + 1, argv + argc);

	try {
		parseContext.Run();
	}
	catch (lambda_options::ParseFailedException const &) {
		std::cout << "Bad arguments.\n";
		printHelp();
		return 1;
	}

	return 0;
}




