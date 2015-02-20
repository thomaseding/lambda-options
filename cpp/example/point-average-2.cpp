#include "../src/LambdaOptions.h"

#include <iostream>


//////////////////////////////////////////////////////////////////////////


class Point {
public:
	Point (float x, float y, float z)
		: x(x)
		, y(y)
		, z(z)
	{}

public:
	float x;
	float y;
	float z;
};


namespace lambda_options
{
	template <typename Char>
	struct RawParser<Char, Point> {
		bool operator() (ParseState<Char> parseState, void * rawMemory)
		{
			Maybe<std::array<float, 3>> maybeVals;
			if (Parse<Char, std::array<float, 3>>(parseState, maybeVals)) {
				auto & vals = *maybeVals;
				new (rawMemory) Point(vals[0], vals[1], vals[2]);
				return true;
			}
			return false;
		}
	};
}


struct ParseResults {
	bool failed;
	bool help;
	std::vector<Point> points;
};


class Parser {
public:
	Parser ()
	{
		opts.AddOption("--help", [&] () {
			results.help = true;
		});
		opts.AddOption("--point", [&] (Point p) {
			results.points.push_back(p);
		});
	}

	ParseResults Parse (std::vector<std::string> const & args)
	{
		results.failed = false;
		results.help = false;
		results.points.clear();

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		try {
			parseContext.Run();
		}
		catch (lambda_options::ParseFailedException const &) {
			results.failed = true;
		}
		if (results.points.empty()) {
			results.failed = true;
		}
		return results;
	}

private:
	LambdaOptions<char> opts;
	ParseResults results;
};


Point ComputeAverage (std::vector<Point> const & points)
{
	Point average(0, 0, 0);
	for (Point const & p : points) {
		average.x += p.x;
		average.y += p.y;
		average.z += p.z;
	}
	if (!points.empty()) {
		float n = static_cast<float>(points.size());
		average.x /= n;
		average.y /= n;
		average.z /= n;
	}
	return average;
}


void PrintAverage (std::vector<Point> const & points)
{
	Point p = ComputeAverage(points);
	std::cout << "Point average is (" << p.x << "," << p.y << "," << p.z << ").\n";
}


void PrintHelp ()
{
	std::cout << "USAGE\n";
	std::cout << "Specify points via multiple '--point x y z'.\n";
	std::cout << "Program returns their average.\n";
}


void PrintFailed ()
{
	std::cout << "Bad command line arguments.\n";
	PrintHelp();
}


int main (int argc, char ** argv)
{
	std::vector<std::string> args(argv + 1, argv + argc);
	Parser parser;
	ParseResults results = parser.Parse(args);
	if (results.failed) {
		PrintFailed();
		return 1;
	}
	if (results.help) {
		PrintHelp();
		return 0;
	}
	PrintAverage(results.points);
	return 0;
}




