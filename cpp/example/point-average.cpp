#include "../src/LambdaOptions.h"
#include <iostream>


//////////////////////////////////////////////////////////////////////////


using namespace lambda_options::with_char;


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
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
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


class OptionsParser {
public:
	OptionsParser ()
	{
		Keyword kwHelp("help", "h");
		kwHelp.text = "Display this help message.";
		opts.AddOption(kwHelp, [&] () {
			doHelp = true;
		});

		Keyword kwPoint("point", "p");
		kwPoint.argText = "x y z";
		kwPoint.text = "Add a point to contribute to displayed point average.";
		opts.AddOption(kwPoint, [&] (Point p) {
			points.emplace_back(p.x, p.y, p.z);
		});

		Keyword kwWeightedPoint("point", "p");
		kwWeightedPoint.argText = "w x y z";
		kwWeightedPoint.text = "Add a weighted point to contribute to displayed point average.";
		opts.AddOption(kwWeightedPoint, [&] (float weight, Point p) {
			points.emplace_back(weight * p.x, weight * p.y, weight * p.z);
		});
	}


	bool Run (std::vector<std::string> const & args)
	{
		doHelp = false;
		points.clear();

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		try {
			parseContext.Run();
		}
		catch (ParseFailedException const & e) {
			PrintBadArgs(e);
			return false;
		}
		if (points.empty()) {
			PrintHelp();
			return false;
		}
		if (doHelp) {
			PrintHelp();
			return true;
		}
		PrintAverage();
		return true;
	}


	void PrintAverage () const
	{
		Point p = ComputeAverage(points);
		std::cout << "Point average is (" << p.x << "," << p.y << "," << p.z << ").\n";
	}


	void PrintHelp () const
	{
		std::cout << "Usage: prog [OPTION]\n";
		std::cout << "Computes the average of supplied points.\n";
		std::cout << opts.HelpDescription();
	}


	void PrintBadArgs (ParseFailedException const & e) const
	{
		std::cout << e.message << "\n";
		PrintHelp();
	}


private:
	Options opts;
	bool doHelp;
	std::vector<Point> points;
};


int main (int argc, char ** argv)
{
	std::vector<std::string> args(argv + 1, argv + argc);
	OptionsParser parser;
	if (parser.Run(args)) {
		return 0;
	}
	return 1;
}




