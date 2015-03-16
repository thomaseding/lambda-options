#include "../src/LambdaOptions.h"
#include <iostream>
#include <set>


//////////////////////////////////////////////////////////////////////////


using namespace lambda_options::with_char;


//////////////////////////////////////////////////////////////////////////


class User {
public:
	User (std::string name, unsigned int age)
		: name(name)
		, age(age)
	{}

	bool operator< (User const & other) const
	{
		if (name < other.name) return true;
		if (name > other.name) return false;
		return age < other.age;
	}

	std::string name;
	unsigned int age;
};


int main (int argc, char ** argv)
{
	using namespace lambda_options::with_char;

	Options opts;

	bool helpRequested = false;
	std::set<User> users;

	Keyword kwHelp("help", "h");
	kwHelp.text = "Display this help message.";
	opts.AddOption(kwHelp, [&] () {
		helpRequested = true;
	});

	Keyword kwUser1("user");
	kwUser1.argText = "NAME";
	kwUser1.text = "Prints user's name with a default age of 0.";
	opts.AddOption(kwUser1, [&] (std::string name) {
		users.insert(User(name, 0));
	});

	Keyword kwUser2("user");
	kwUser2.argText = "NAME AGE";
	kwUser2.text = "Prints user's name and age.";
	opts.AddOption(kwUser2, [&] (std::string name, unsigned int age) {
		users.insert(User(name, age));
	});

	auto printHelp = [&] () {
		std::cout << "Usage: prog.exe [OPTIONS]\n\n";
		std::cout << opts.HelpDescription();
	};

	auto parseContext = opts.CreateParseContext(argv + 1, argv + argc);

	try {
		parseContext.Run();
	}
	catch (ParseFailedException const &) {
		std::cout << "Bad arguments.\n";
		printHelp();
		return 1;
	}

	if (helpRequested) {
		printHelp();
		return 0;
	}

	for (User const & user : users) {
		std::cout << "Name:" << user.name << " Age:" << user.age << "\n";
	}

	return 0;
}




