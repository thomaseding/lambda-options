#include "../src/LambdaOpts.h"

#include <iostream>


//////////////////////////////////////////////////////////////////////////


int main (int argc, char ** argv)
{
	LambdaOpts<char> parser;

	parser.Add("--help", [] () {
		std::cout << "--user NAME AGE" << std::endl;
	});
	parser.Add("--num", [] (int num) {
		std::cout << "Num:" << num << std::endl;
	});
	parser.Add("--user", [] (std::string name, unsigned int age) {
		std::cout << "Name:" << name << " Age:" << age << std::endl;
	});

	LambdaOpts<char>::ParseEnv parseEnv(parser, std::vector<std::string>(argv + 1, argv + argc));

	int parseFailureIndex;
	if (!parseEnv.Parse(parseFailureIndex)) {
		std::cout << "Parse failed at index " << parseFailureIndex << std::endl;
		return 1;
	}

	return 0;
}




