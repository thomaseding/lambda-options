#include "../src/LambdaOpts.h"

#include <iostream>


//////////////////////////////////////////////////////////////////////////


int main (int argc, char ** argv)
{
	LambdaOpts<char> opts(argc, const_cast<char const * const *>(argv));

	opts.Add("--help", [] () {
		std::cout << "--user NAME AGE" << std::endl;
	});
	opts.Add("--user", [] (std::string name, unsigned int age) {
		std::cout << "Name:" << name << " Age:" << age << std::endl;
	});

	int parseFailedIndex;
	if (!opts.Parse(parseFailedIndex)) {
		std::cout << "Parse failed on index " << parseFailedIndex << std::endl;
		return 1;
	}
	return 0;
}




