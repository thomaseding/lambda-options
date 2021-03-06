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


namespace lambda_options
{
	template <typename Char>
	struct RawParser<Char, User> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			std::string name = *parseState.iter++;
			unsigned int age = 0;
			Maybe<unsigned int> maybeAge;
			if (Parse<Char, unsigned int>(parseState, maybeAge)) {
				age = *maybeAge;
			}
			new (rawMemory) User(name, age);
			return true;
		}
	};
}


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

	Keyword kwUsers("users");
	kwUsers.argText = "NAME [AGE=0]...";
	kwUsers.text = "Prints user names and ages.";
	opts.AddOption(kwUsers, [&] (ParseState parseState) {
		lambda_options::Maybe<User> maybeUser;
		while (lambda_options::Parse(parseState, maybeUser)) {
			users.insert(*maybeUser);
		}
	});

	auto printHelp = [&] () {
		std::cout << "Usage: prog.exe [OPTIONS]\n\n";
		std::cout << opts.HelpDescription();
	};

	auto parseContext = opts.CreateParseContext(argv + 1, argv + argc);

	try {
		parseContext.Run();
	}
	catch (ParseFailedException const & e) {
		std::cout << e.message << "\n";
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




