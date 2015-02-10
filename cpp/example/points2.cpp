#include "../src/LambdaOpts.h"

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


namespace lambda_opts
{
	template <typename Char>
	struct Parser<Char, Point> {
		static bool Parse (typename ArgsIter<Char>::type & iter, typename ArgsIter<Char>::type end, void * memory)
		{
			std::array<float, 3> p;
			if (Parser<Char, std::array<float, 3>>::Parse(iter, end, &p)) {
				new (memory) Point(p[0], p[1], p[2]);
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

		auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
		if (!parseEnv.Run()) {
			results.failed = true;
		}
		if (results.points.empty()) {
			results.failed = true;
		}
		return results;
	}

private:
	LambdaOpts<char> opts;
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




