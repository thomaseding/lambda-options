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
		typedef LambdaOptions<char>::Keyword Keyword;

		Keyword kwHelp("--help", 'h');
		kwHelp.desc = "Display this help message.";
		opts.AddOption(kwHelp, [&] () {
			doHelp = true;
		});

		Keyword kwPoint("--point", 'p');
		kwPoint.args = "x y z";
		kwPoint.desc = "Add a point to contribute to displayed point average.";
		opts.AddOption(kwPoint, [&] (float x, float y, float z) {
			points.emplace_back(x, y, z);
		});

		Keyword kwWeightedPoint("--point", 'p');
		kwWeightedPoint.args = "w x y z";
		kwWeightedPoint.desc = "Add a weighted point to contribute to displayed point average.";
		opts.AddOption(kwWeightedPoint, [&] (float weight, float x, float y, float z) {
			points.emplace_back(weight * x, weight * y, weight * z);
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
		catch (lambda_options::ParseFailedException const & e) {
			PrintBadArgs(e, args);
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


	void PrintBadArgs (lambda_options::ParseFailedException const & e, std::vector<std::string> const & args) const
	{
		if (e.beginIndex == e.endIndex) {
			std::cout << "Unknown option at index " << e.beginIndex << ": " << args[e.beginIndex] << "\n";
		}
		else if (e.endIndex < args.size()) {
			std::cout << "Bad input for " << args[e.beginIndex] << " at index " << e.endIndex << ": " << args[e.endIndex] << "\n";
		}
		else {
			std::cout << "Bad input for " << args[e.beginIndex] << " at index " << e.endIndex << ": End of input.\n";
		}
		PrintHelp();
	}


private:
	LambdaOptions<char> opts;
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




