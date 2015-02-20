#include "../src/LambdaOptions.h"
#include <iostream>
#include <set>


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
		bool operator() (ParseState<Char> parseState, void * rawMemory)
		{
			std::string name = *parseState.iter++;
			unsigned int age = 0;
			if (parseState.iter != parseState.end) {
				Maybe<unsigned int> maybeAge;
				if (Parse<Char, unsigned int>(parseState, maybeAge)) {
					age = *maybeAge;
				}
			}
			new (rawMemory) User(name, age);
			return true;
		}
	};
}


int main (int argc, char ** argv)
{
	typedef LambdaOptions<char>::Keyword Keyword;

	LambdaOptions<char> opts;

	bool helpRequested = false;
	std::set<User> users;

	Keyword kwHelp("--help", 'h');
	kwHelp.help = "Display this help message.";
	opts.AddOption(kwHelp, [&] () {
		helpRequested = true;
	});

	Keyword kwUser("--user");
	kwUser.args = "NAME [AGE=0]";
	kwUser.help = "Prints user's name and age.";
	opts.AddOption(kwUser, [&] (User user) {
		if (user.name == "Kelly") {
			return lambda_options::ParseResult::Reject;
		}
		users.insert(user);
		return lambda_options::ParseResult::Accept;
	});

	auto printHelp = [&] () {
		std::cout << "Usage: prog.exe [OPTIONS]\n\n";
		std::cout << opts.HelpDescription();
	};

	auto parseContext = opts.CreateParseContext(argv + 1, argv + argc);

	try {
		parseContext.Run();
	}
	catch (lambda_options::ParseFailedException const & e) {
		auto const & args = parseContext.Args();
		if (e.beginIndex == e.endIndex) {
			std::cout << "Unknown option at index " << e.beginIndex << ": " << args[e.beginIndex] << "\n";
		}
		else if (e.endIndex < args.size()) {
			std::cout << "Bad input for " << args[e.beginIndex] << " at index " << e.endIndex << ": " << args[e.endIndex] << "\n";
		}
		else {
			std::cout << "Bad input for " << args[e.beginIndex] << " at index " << e.endIndex << ".\n";
		}
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




