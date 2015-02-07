#include "../src/LambdaOpts.h"

#include <algorithm>
#include <iostream>


//////////////////////////////////////////////////////////////////////////


typedef LambdaOpts<char> Opts;
typedef LambdaOpts<wchar_t> WOpts;


typedef Opts::ParseResult PR;
typedef WOpts::ParseResult WPR;


//////////////////////////////////////////////////////////////////////////


class FailException : public std::exception {
public:
	FailException (char const * file, unsigned int line) {
		char buff[1024];
		sprintf(buff, "FailException thrown in '%s' at %u.", file, line);
		msg = buff;
	}
	virtual char const * what () const throw() override {
		return msg.c_str();
	}
private:
	std::string msg;
};


#define FAIL \
	do \
		throw FailException(__FILE__, __LINE__); \
	while (false)


//////////////////////////////////////////////////////////////////////////


void TestCompileTypes ()
{
	Opts opts;
	opts.Add("x", [] (int) { return PR::Accept; });
	opts.Add("x", [] (unsigned int) { return PR::Accept; });
	opts.Add("x", [] (float) { return PR::Accept; });
	opts.Add("x", [] (double) { return PR::Accept; });
	opts.Add("x", [] (char) { return PR::Accept; });
	opts.Add("x", [] (std::string) { return PR::Accept; });

	std::vector<std::string> args;
	auto parseEnv = opts.NewParseEnv(args.begin(), args.end());
	int failIdx;
	parseEnv.Parse(failIdx);
}


void TestCompileWTypes ()
{
	WOpts opts;
	opts.Add(L"x", [] (int) { return WPR::Accept; });
	opts.Add(L"x", [] (unsigned int) { return WPR::Accept; });
	opts.Add(L"x", [] (float) { return WPR::Accept; });
	opts.Add(L"x", [] (double) { return WPR::Accept; });
	opts.Add(L"x", [] (wchar_t) { return WPR::Accept; });
	opts.Add(L"x", [] (std::wstring) { return WPR::Accept; });

	std::vector<std::wstring> args;
	auto parseEnv = opts.NewParseEnv(args.begin(), args.end());
	int failIdx;
	parseEnv.Parse(failIdx);
}


void TestCompileArities ()
{
	typedef int I;

	Opts opts;
	opts.Add("x", [] () { return PR::Accept; });
	opts.Add("x", [] (I) { return PR::Accept; });
	opts.Add("x", [] (I,I) { return PR::Accept; });
	opts.Add("x", [] (I,I,I) { return PR::Accept; });
	opts.Add("x", [] (I,I,I,I) { return PR::Accept; });
	opts.Add("x", [] (I,I,I,I,I) { return PR::Accept; });

	std::vector<std::string> args;
	auto parseEnv = opts.NewParseEnv(args.begin(), args.end());
	int failIdx;
	parseEnv.Parse(failIdx);
}


void TestRejectEmptyRule ()
{
	try {
		Opts opts;
		opts.Add("", [] () { return PR::Accept; });
	}
	catch (...) {	// TODO: Use the actual exception type when it is implemented.
		return;
	}
	FAIL;
}


void TestArityPrecedence1 ()
{
	typedef std::string S;
	std::vector<int> calls;

	Opts opts;
	opts.Add("", [&] (S) { calls.push_back(1); return PR::Accept; });
	opts.Add("", [&] (S,S) { calls.push_back(2); return PR::Accept; });
	opts.Add("", [&] (S,S,S) { calls.push_back(3); return PR::Accept; });
	opts.Add("", [&] (S,S,S,S) { calls.push_back(4); return PR::Accept; });
	opts.Add("", [&] (S,S,S,S,S) { calls.push_back(5); return PR::Accept; });

	auto parseCount = [&] (size_t n) {
		std::vector<std::string> args(n);
		auto parseEnv = opts.NewParseEnv(args.begin(), args.end());
		int failIdx;
		if (!parseEnv.Parse(failIdx)) {
			FAIL;
		}
	};
	parseCount(0);
	parseCount(1);
	parseCount(2);
	parseCount(3);
	parseCount(4);
	parseCount(5);

	int const expectedCalls[] = { 1,2,3,4,5 };
	if (!std::equal(calls.begin(), calls.end(), expectedCalls)) {
		FAIL;
	}
}


void TestArityPrecedence2 ()
{
	typedef std::string S;
	std::vector<int> calls;
	Opts opts;
	opts.Add("x", [&] () { calls.push_back(0); return PR::Accept; });
	opts.Add("x", [&] (S) { calls.push_back(1); return PR::Accept; });
	opts.Add("x", [&] (S,S) { calls.push_back(2); return PR::Accept; });
	opts.Add("x", [&] (S,S,S) { calls.push_back(3); return PR::Accept; });
	opts.Add("x", [&] (S,S,S,S) { calls.push_back(4); return PR::Accept; });
	opts.Add("x", [&] (S,S,S,S,S) { calls.push_back(5); return PR::Accept; });

	auto parseCount = [&] (size_t n) {
		std::vector<std::string> args(n + 1);
		args.front() = "x";
		auto parseEnv = opts.NewParseEnv(args.begin(), args.end());
		int failIdx;
		if (!parseEnv.Parse(failIdx)) {
			FAIL;
		}
	};
	parseCount(0);
	parseCount(1);
	parseCount(2);
	parseCount(3);
	parseCount(4);
	parseCount(5);

	int const expectedCalls[] = { 0,1,2,3,4,5 };
	if (!std::equal(calls.begin(), calls.end(), expectedCalls)) {
		FAIL;
	}
}


void TestEmptyPrecedence1 ()
{
	typedef std::string S;
	std::vector<int> calls;
	Opts opts;
	opts.Add("", [&] (S) { calls.push_back(0); return PR::Accept; });
	opts.Add("x", [&] (S) { calls.push_back(1); return PR::Accept; });

	std::vector<std::string> args(3);
	args.front() = "x";
	auto parseEnv = opts.NewParseEnv(args.begin(), args.end());
	int failIdx;
	if (!parseEnv.Parse(failIdx)) {
		FAIL;
	}

	int const expectedCalls[] = { 1,0 };
	if (!std::equal(calls.begin(), calls.end(), expectedCalls)) {
		FAIL;
	}
}


void TestEmptyPrecedence2 ()
{
	typedef std::string S;
	std::vector<int> calls;
	Opts opts;
	opts.Add("x", [&] (S) { calls.push_back(1); return PR::Accept; });
	opts.Add("", [&] (S) { calls.push_back(0); return PR::Accept; });

	std::vector<std::string> args(3);
	args.front() = "x";
	auto parseEnv = opts.NewParseEnv(args.begin(), args.end());
	int failIdx;
	if (!parseEnv.Parse(failIdx)) {
		FAIL;
	}

	int const expectedCalls[] = { 1,0 };
	if (!std::equal(calls.begin(), calls.end(), expectedCalls)) {
		FAIL;
	}
}


bool RunTests ()
{
	typedef void (*TestFunc)();

	TestFunc tests[] = {
		TestCompileTypes,
		TestCompileArities,
		TestRejectEmptyRule,
		TestArityPrecedence1,
		TestEmptyPrecedence1,
		TestEmptyPrecedence2,
	};

	try {
		for (auto test : tests) {
			test();
		}
	}
	catch (std::exception const & e) {
		std::cout << e.what() << std::endl;
		return false;
	}

	std::cout << "All tests succeeded." << std::endl;
	return true;
}


int main (int argc, char ** argv)
{
	if (!RunTests()) {
		return 1;
	}
	return 0;
}




