#include "../src/LambdaOpts.h"

#include <algorithm>
#include <clocale>
#include <iostream>
#include <sstream>


//////////////////////////////////////////////////////////////////////////


typedef LambdaOpts<char> Opts;
typedef LambdaOpts<wchar_t> WOpts;


typedef Opts::ParseResult PR;
typedef WOpts::ParseResult WPR;


//////////////////////////////////////////////////////////////////////////


#define UNREFERENCED(x) \
	((void) (x))


namespace
{
	class FailException : public std::exception {
	public:
		FailException (char const * file, unsigned int line) {
			std::stringstream ss;
			ss << "FailException thrown in '" << file << "' at " << line << ".";
			msg = ss.str();
		}
		virtual char const * what () const throw() override {
			return msg.c_str();
		}
	private:
		std::string msg;
	};
}


#define FAIL \
	do \
		throw FailException(__FILE__, __LINE__); \
	while (false)


//////////////////////////////////////////////////////////////////////////


template <typename T>
static bool Equal (std::vector<T> const & xs, std::vector<T> const & ys)
{
	if (xs.size() != ys.size()) {
		return false;
	}
	return std::equal(xs.begin(), xs.end(), ys.begin());
}


template <typename T, size_t N>
static bool Equal (std::vector<T> const & xs, T const (&ys)[N])
{
	if (xs.size() != N) {
		return false;
	}
	return std::equal(xs.begin(), xs.end(), ys);
}


//////////////////////////////////////////////////////////////////////////


static void Escape (std::ostream & os, unsigned char c, char quote)
{
	if (c == '\0') {
		os << "\\0";
	}
	else if (c == '\r') {
		os << "\\r";
	}
	else if (c == '\n') {
		os << "\\n";
	}
	else if (c == '\t') {
		os << "\\t";
	}
	else if (c == '\'' && c == quote) {
		os << "\\'";
	}
	else if (c == '\"' && c == quote) {
		os << "\\\"";
	}
	else if (c == '\\') {
		os << "\\\\";
	}
	else if (32 <= c && c <= 126) {
		os << c;
	}
	else {
		char buff[8];
		sprintf(buff, "\\x%x", c);
		os << buff;
	}
}

static void Pretty (std::ostream & os, char c)
{
	os << '\'';
	Escape(os, static_cast<unsigned char>(c), '\'');
	os << '\'';
}


static void Pretty (std::ostream & os, std::string const & str)
{
	os << '"';
	for (char c : str) {
		Escape(os, static_cast<unsigned char>(c), '"');
	}
	os << '"';
}


static void Dump (std::ostream & os, int x)
{
	os << "int(" << x << ")\n";
}


static void Dump (std::ostream & os, unsigned int x)
{
	os << "uint(" << x << ")\n";
}


static void Dump (std::ostream & os, float x)
{
	os << "float(" << x << ")\n";
}


static void Dump (std::ostream & os, double x)
{
	os << "double(" << x << ")\n";
}


static void Dump (std::ostream & os, char x)
{
	os << "char(";
	Pretty(os, x);
	os << ")\n";
}


static void Dump (std::ostream & os, std::string const & x)
{
	os << "string(";
	Pretty(os, x);
	os << ")\n";
}


template <typename T>
static void Dump (T const & x)
{
	Dump(std::cout, x);
	std::cout.flush();
}


static void DumpMemo (std::ostream & os, std::string const & x)
{
	os << x << "\n";
}


static void UNREFERENCED_FUNCS ()
{
	std::stringstream ss;
	Dump(ss, 0);
	Dump(ss, 0u);
	Dump(ss, 0.0f);
	Dump(ss, 0.0);
	Dump(ss, '0');
	Dump(ss, "0");
	UNREFERENCED_FUNCS();
}


//////////////////////////////////////////////////////////////////////////


static void TestCompileTypes ()
{
	Opts opts;

	opts.AddOption("x", [] (int) {
		return PR::Accept;
	});
	opts.AddOption("x", [] (unsigned int) {
		return PR::Accept;
	});
	opts.AddOption("x", [] (float) {
		return PR::Accept;
	});
	opts.AddOption("x", [] (double) {
		return PR::Accept;
	});
	opts.AddOption("x", [] (char) {
		return PR::Accept;
	});
	opts.AddOption("x", [] (std::string) {
		return PR::Accept;
	});

	std::vector<std::string> args;
	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	parseEnv.Run(failIdx);
	if (failIdx != -1) {
		FAIL;
	}
}


static void TestCompileWTypes ()
{
	WOpts opts;
	opts.AddOption(L"x", [] (int) {
		return WPR::Accept;
	});
	opts.AddOption(L"x", [] (unsigned int) {
		return WPR::Accept;
	});
	opts.AddOption(L"x", [] (float) {
		return WPR::Accept;
	});
	opts.AddOption(L"x", [] (double) {
		return WPR::Accept;
	});
	opts.AddOption(L"x", [] (wchar_t) {
		return WPR::Accept;
	});
	opts.AddOption(L"x", [] (std::wstring) {
		return WPR::Accept;
	});

	std::vector<std::wstring> args;
	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	parseEnv.Run(failIdx);
	if (failIdx != -1) {
		FAIL;
	}
}


static void TestCompileArities ()
{
	typedef int I;

	Opts opts;

	opts.AddOption("x", [] () {});
	opts.AddOption("x", [] (I) {});
	opts.AddOption("x", [] (I,I) {});
	opts.AddOption("x", [] (I,I,I) {});
	opts.AddOption("x", [] (I,I,I,I) {});
	opts.AddOption("x", [] (I,I,I,I,I) {});

	opts.AddOption("x", [] () {
		return PR::Accept;
	});
	opts.AddOption("x", [] (I) {
		return PR::Accept;
	});
	opts.AddOption("x", [] (I,I) {
		return PR::Accept;
	});
	opts.AddOption("x", [] (I,I,I) {
		return PR::Accept;
	});
	opts.AddOption("x", [] (I,I,I,I) {
		return PR::Accept;
	});
	opts.AddOption("x", [] (I,I,I,I,I) {
		return PR::Accept;
	});

	std::vector<std::string> args;
	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	parseEnv.Run(failIdx);
	if (failIdx != -1) {
		FAIL;
	}
}


static void TestRejectEmptyKeyword ()
{
	try {
		Opts opts;
		opts.AddOption("", [] () { return PR::Accept; });
	}
	catch (lambda_opts::Exception const & e) {
		return;
	}
	FAIL;
}


static void TestArityPrecedence1 ()
{
	typedef std::string S;
	std::vector<int> calls;

	Opts opts;
	opts.AddOption("", [&] (S) {
		calls.push_back(1);
		return PR::Accept;
	});
	opts.AddOption("", [&] (S,S) {
		calls.push_back(2);
		return PR::Accept;
	});
	opts.AddOption("", [&] (S,S,S) {
		calls.push_back(3);
		return PR::Accept;
	});
	opts.AddOption("", [&] (S,S,S,S) {
		calls.push_back(4);
		return PR::Accept;
	});
	opts.AddOption("", [&] (S,S,S,S,S) {
		calls.push_back(5);
		return PR::Accept;
	});

	auto parseCount = [&] (size_t n) {
		std::vector<std::string> args(n);
		auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
		int failIdx;
		if (!parseEnv.Run(failIdx)) {
			FAIL;
		}
		if (failIdx != -1) {
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
	if (!Equal(calls, expectedCalls)) {
		FAIL;
	}
}


static void TestArityPrecedence2 ()
{
	typedef std::string S;
	std::vector<int> calls;

	Opts opts;
	opts.AddOption("x", [&] () {
		calls.push_back(0);
		return PR::Accept;
	});
	opts.AddOption("x", [&] (S) {
		calls.push_back(1);
		return PR::Accept;
	});
	opts.AddOption("x", [&] (S,S) {
		calls.push_back(2);
		return PR::Accept;
	});
	opts.AddOption("x", [&] (S,S,S) {
		calls.push_back(3);
		return PR::Accept;
	});
	opts.AddOption("x", [&] (S,S,S,S) {
		calls.push_back(4);
		return PR::Accept;
	});
	opts.AddOption("x", [&] (S,S,S,S,S) {
		calls.push_back(5);
		return PR::Accept;
	});

	auto parseCount = [&] (size_t n) {
		std::vector<std::string> args(n + 1);
		args.front() = "x";
		auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
		int failIdx;
		if (!parseEnv.Run(failIdx)) {
			FAIL;
		}
		if (failIdx != -1) {
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
	if (!Equal(calls, expectedCalls)) {
		FAIL;
	}
}


static void TestEmptyPrecedence1 ()
{
	typedef std::string S;
	std::vector<int> calls;

	Opts opts;
	opts.AddOption("", [&] (S) {
		calls.push_back(0);
		return PR::Accept;
	});
	opts.AddOption("x", [&] () {
		calls.push_back(1);
		return PR::Accept;
	});
	opts.AddOption("x", [&] (S) {
		calls.push_back(2);
		return PR::Accept;
	});

	std::vector<std::string> args;
	args.push_back("");
	args.push_back("x");
	args.push_back("");
	args.push_back("x");


	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	if (!parseEnv.Run(failIdx)) {
		FAIL;
	}
	if (failIdx != -1) {
		FAIL;
	}

	int const expectedCalls[] = { 0,2,1 };
	if (!Equal(calls, expectedCalls)) {
		FAIL;
	}
}


static void TestEmptyPrecedence2 ()
{
	typedef std::string S;
	std::vector<int> calls;

	Opts opts;
	opts.AddOption("x", [&] (S) {
		calls.push_back(0);
		return PR::Accept;
	});
	opts.AddOption("x", [&] () {
		calls.push_back(1);
		return PR::Accept;
	});
	opts.AddOption("", [&] (S) {
		calls.push_back(2); return
		PR::Accept;
	});

	std::vector<std::string> args;
	args.push_back("");
	args.push_back("x");
	args.push_back("");
	args.push_back("x");

	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	if (!parseEnv.Run(failIdx)) {
		FAIL;
	}
	if (failIdx != -1) {
		FAIL;
	}

	int const expectedCalls[] = { 2,0,1 };
	if (!Equal(calls, expectedCalls)) {
		FAIL;
	}
}


static void TestObtainedValues ()
{
	std::stringstream ss;

	Opts opts;

	opts.AddOption("", [&] (unsigned int x) {
		Dump(ss, x);
		return PR::Accept;
	});
	opts.AddOption("", [&] (int x) {
		Dump(ss, x);
		return PR::Accept;
	});
	opts.AddOption("", [&] (float x) {
		Dump(ss, x);
		if (x == 0.0f) {
			DumpMemo(ss, "REJECTED");
			return PR::Reject;
		}
		return PR::Accept;
	});
	opts.AddOption("", [&] (double x) {
		Dump(ss, x);
		return PR::Accept;
	});
	opts.AddOption("", [&] (char x) {
		Dump(ss, x);
		return PR::Accept;
	});
	opts.AddOption("", [&] (std::string x) {
		Dump(ss, x);
		return PR::Accept;
	});

	std::vector<std::string> args;
	std::stringstream expected;

	args.push_back("");
	Dump(expected, "");

	args.push_back("-4");
	Dump(expected, -4);

	args.push_back("0");
	Dump(expected, 0u);

	args.push_back("+4");
	Dump(expected, 4u);

	args.push_back("5.1e-9");
	Dump(expected, 5.1e-9f);

	args.push_back("-5.1e-100");
	Dump(expected, -0.0f);
	DumpMemo(expected, "REJECTED");
	Dump(expected, -5.1e-100);

	args.push_back("0.5");
	Dump(expected, 0.5f);

	args.push_back("-0.5");
	Dump(expected, -0.5f);

	args.push_back("+0.5");
	Dump(expected, 0.5f);

	args.push_back(".5");
	Dump(expected, 0.5f);

	args.push_back("5e-1");
	Dump(expected, 0.5f);

	args.push_back(" ");
	Dump(expected, ' ');

	args.push_back("-");
	Dump(expected, '-');

	args.push_back("+");
	Dump(expected, '+');

	args.push_back(" 0");
	Dump(expected, " 0");

	args.push_back("08");
	Dump(expected, 8u);

	args.push_back("0111");
	Dump(expected, 111u);

	args.push_back("0x");
	Dump(expected, "0x");

	args.push_back("0x111");
	Dump(expected, "0x111");

	args.push_back("0X111");
	Dump(expected, "0X111");

	args.push_back("0xa");
	Dump(expected, "0xa");

	args.push_back("0XA");
	Dump(expected, "0XA");

	args.push_back("0Xa");
	Dump(expected, "0Xa");

	args.push_back("0xg");
	Dump(expected, "0xg");

	args.push_back("\n0");
	Dump(expected, "\n0");

	args.push_back("\t0");
	Dump(expected, "\t0");

	args.push_back(std::string(1, '\0'));
	Dump(expected, '\0');

	args.push_back(std::string(2, '\0'));
	Dump(expected, std::string(2, '\0'));

	std::string weird = "123";
	weird.push_back('\0');
	args.push_back(weird);
	Dump(expected, weird);

	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	if (!parseEnv.Run(failIdx)) {
		FAIL;
	}
	if (failIdx != -1) {
		FAIL;
	}

	if (ss.str() != expected.str()) {
		FAIL;
	}
}


static void TestReject1 ()
{
	std::stringstream ss;

	Opts opts;
	opts.AddOption("", [&] (int x) {
		Dump(ss, x);
		return PR::Reject;
	});
	opts.AddOption("", [&] (std::string x) {
		Dump(ss, x);
		return PR::Accept;
	});

	std::vector<std::string> args;
	std::stringstream expected;

	args.push_back("1");
	Dump(expected, 1);
	Dump(expected, "1");

	args.push_back("x");
	Dump(expected, "x");

	args.push_back("2");
	Dump(expected, 2);
	Dump(expected, "2");

	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	if (!parseEnv.Run(failIdx)) {
		FAIL;
	}
	if (failIdx != -1) {
		FAIL;
	}

	if (ss.str() != expected.str()) {
		FAIL;
	}
}


static void TestReject2 ()
{
	std::stringstream ss;

	Opts opts;
	opts.AddOption("", [&] (int x) {
		Dump(ss, x);
		return PR::Reject;
	});
	opts.AddOption("", [&] (char x) {
		Dump(ss, x);
		return PR::Accept;
	});

	std::vector<std::string> args;
	std::stringstream expected;

	args.push_back("1");
	Dump(expected, 1);
	Dump(expected, '1');

	args.push_back("x");
	Dump(expected, 'x');

	args.push_back("22");
	Dump(expected, 22);

	args.push_back("3");

	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	if (parseEnv.Run(failIdx)) {
		FAIL;
	}
	if (failIdx != 2) {
		FAIL;
	}

	if (ss.str() != expected.str()) {
		FAIL;
	}
}


static void TestFatal ()
{
	std::stringstream ss;

	Opts opts;
	opts.AddOption("", [&] (int x) {
		Dump(ss, x);
		return PR::Fatal;
	});
	opts.AddOption("", [&] (std::string x) {
		Dump(ss, x);
		return PR::Accept;
	});

	std::vector<std::string> args;
	args.push_back("x");
	args.push_back("1");
	args.push_back("y");
	args.push_back("2");

	std::stringstream expected;
	Dump(expected, "x");
	Dump(expected, 1);

	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	if (parseEnv.Run(failIdx)) {
		FAIL;
	}
	if (failIdx != 1) {
		FAIL;
	}

	if (ss.str() != expected.str()) {
		FAIL;
	}
}


static void TestNoMatch ()
{
	Opts opts;
	opts.AddOption("", [] (int x) {
		return PR::Accept;
	});
	opts.AddOption("", [] (char x) {
		return PR::Accept;
	});

	std::vector<std::string> args;
	args.push_back("11");
	args.push_back("22");
	args.push_back("x");
	args.push_back("yy");
	args.push_back("z");

	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	if (parseEnv.Run(failIdx)) {
		FAIL;
	}
	if (failIdx != 3) {
		FAIL;
	}
}


static void TestKeyword1 ()
{
	std::stringstream ss;

	Opts opts;
	opts.AddOption("", [&] (int x) {
		DumpMemo(ss, "int");
		Dump(ss, x);
		return PR::Accept;
	});
	opts.AddOption("", [&] (char x) {
		DumpMemo(ss, "char");
		Dump(ss, x);
		return PR::Accept;
	});
	opts.AddOption("", [&] (std::string x) {
		DumpMemo(ss, "string");
		Dump(ss, x);
		return PR::Accept;
	});
	opts.AddOption("x", [&] () {
		DumpMemo(ss, "x");
		return PR::Accept;
	});
	opts.AddOption("xx", [&] () {
		DumpMemo(ss, "xx");
		return PR::Accept;
	});
	opts.AddOption("yy", [&] () {
		DumpMemo(ss, "yy");
		return PR::Accept;
	});
	opts.AddOption("y", [&] () {
		DumpMemo(ss, "y");
		return PR::Accept;
	});
	opts.AddOption("z", [&] (int x) {
		DumpMemo(ss, "z");
		Dump(ss, x);
		return PR::Accept;
	});
	opts.AddOption("zz", [&] (int x) {
		DumpMemo(ss, "zz");
		Dump(ss, x);
		return PR::Accept;
	});

	std::vector<std::string> args;
	std::stringstream expected;

	args.push_back("x");
	DumpMemo(expected, "x");

	args.push_back("xx");
	DumpMemo(expected, "xx");

	args.push_back("0");
	DumpMemo(expected, "int");
	Dump(expected, 0);

	args.push_back("y");
	DumpMemo(expected, "y");

	args.push_back("yy");
	DumpMemo(expected, "yy");

	args.push_back("1");
	DumpMemo(expected, "int");
	Dump(expected, 1);

	args.push_back("z");
	DumpMemo(expected, "char");
	Dump(expected, 'z');

	args.push_back("zz");
	DumpMemo(expected, "string");
	Dump(expected, "zz");

	args.push_back("z");
	args.push_back("2");
	DumpMemo(expected, "z");
	Dump(expected, 2);

	args.push_back("zz");
	args.push_back("3");
	DumpMemo(expected, "zz");
	Dump(expected, 3);

	args.push_back("w");
	DumpMemo(expected, "char");
	Dump(expected, 'w');

	args.push_back("ww");
	DumpMemo(expected, "string");
	Dump(expected, "ww");

	auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
	int failIdx;
	if (!parseEnv.Run(failIdx)) {
		FAIL;
	}
	if (failIdx != -1) {
		FAIL;
	}

	if (ss.str() != expected.str()) {
		FAIL;
	}
}


static bool RunTests ()
{
	typedef void (*TestFunc)();

	TestFunc tests[] = {
		TestCompileTypes,
		TestCompileWTypes,
		TestCompileArities,
		TestRejectEmptyKeyword,
		TestArityPrecedence1,
		TestArityPrecedence2,
		TestEmptyPrecedence1,
		TestEmptyPrecedence2,
		TestObtainedValues,
		TestReject1,
		TestReject2,
		TestFatal,
		TestNoMatch,
		TestKeyword1,
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
	std::setlocale(LC_ALL, "C");
	if (!RunTests()) {
		return 1;
	}
	return 0;
}













