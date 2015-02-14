#include "../src/LambdaOpts.h"

#include <algorithm>
#include <clocale>
#include <iostream>
#include <sstream>


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


class Bit {
private:
	Bit ();	// Intentionally disable to ensure LambdaOpts can handle values it cannot default instantiate.

public:
	Bit (bool value)
		: value(value)
	{}

public:
	bool value;
};


namespace lambda_opts
{
	template <typename Char>
	struct RawParser<Char, Bit> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			Maybe<int> mNum;
			if (Parse<Char, int>(parseState, mNum)) {
				int num = *mNum;
				if (num == 0 || num == 1) {
					new (rawMemory) Bit(num != 0);
					return true;
				}
			}
			return false;
		}
	};
}


//////////////////////////////////////////////////////////////////////////


class TestMaybeLifetimeHelper {
public:
	static int const P1 = 7907;
	static int const P2 = 7919;

	static int value;

	TestMaybeLifetimeHelper ()
	{
		value += P1;
	}

	~TestMaybeLifetimeHelper ()
	{
		value += P2;
	}
};


int TestMaybeLifetimeHelper::value = 666;


namespace lambda_opts
{
	template <typename Char>
	struct RawParser<Char, TestMaybeLifetimeHelper> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			new (rawMemory) TestMaybeLifetimeHelper();
			++parseState.iter;
			return true;
		}
	};
}


//////////////////////////////////////////////////////////////////////////


__declspec(align(8192)) class TestMaybeLifetimeHelperSuperAligned : public TestMaybeLifetimeHelper {};


namespace lambda_opts
{
	template <typename Char>
	struct RawParser<Char, TestMaybeLifetimeHelperSuperAligned> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			new (rawMemory) TestMaybeLifetimeHelperSuperAligned();
			++parseState.iter;
			return true;
		}
	};
}


//////////////////////////////////////////////////////////////////////////


template <typename T, size_t N>
static bool Equal (std::vector<T> const & xs, T const (&ys)[N])
{
	if (xs.size() != N) {
		return false;
	}
	return std::equal(xs.begin(), xs.end(), ys);
}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
class Tests {
private:
	typedef std::basic_string<Char> String;
	typedef LambdaOpts<Char> Opts;
	typedef typename lambda_opts::ParseResult PR;
	typedef typename Opts::Keyword Keyword;


	static std::wstring L (std::string const & str)
	{
		std::wstring wstr;
		for (char c : str) {
			wstr.push_back(c);
		}
		return wstr;
	}


	static std::wstring L (std::wstring const & wstr)
	{
		return wstr;
	}


	static String Q (std::string const & str)
	{
		String qstr;
		for (char c : str) {
			qstr.push_back(static_cast<Char>(c));
		}
		return qstr;
	}


	static String Q (std::wstring const & wstr)
	{
		String qstr;
		for (char c : wstr) {
			qstr.push_back(static_cast<Char>(c));
		}
		return qstr;
	}


	static void Escape (std::wostream & os, wchar_t c, wchar_t quote)
	{
		if (c == L'\0') {
			os << L"\\0";
		}
		else if (c == L'\r') {
			os << L"\\r";
		}
		else if (c == L'\n') {
			os << L"\\n";
		}
		else if (c == L'\t') {
			os << "\\t";
		}
		else if (c == L'\'' && c == quote) {
			os << L"\\'";
		}
		else if (c == L'\"' && c == quote) {
			os << L"\\\"";
		}
		else if (c == L'\\') {
			os << L"\\\\";
		}
		else if (32 <= c && c <= 126) {
			os << c;
		}
		else {
			wchar_t buff[8];
			swprintf(buff, L"\\x%x", c);
			os << buff;
		}
	}


	static void Pretty (std::wostream & os, wchar_t c)
	{
		os << L'\'';
		Escape(os, c, L'\'');
		os << L'\'';
	}


	static void Pretty (std::wostream & os, char c)
	{
		Pretty(os, static_cast<wchar_t>(c));
	}


	static void Pretty (std::wostream & os, std::string const & str)
	{
		os << L'"';
		for (char c : str) {
			Escape(os, static_cast<wchar_t>(c), L'"');
		}
		os << L'"';
	}


	static void Pretty (std::wostream & os, std::wstring const & str)
	{
		os << L'"';
		for (wchar_t c : str) {
			Escape(os, c, L'"');
		}
		os << L'"';
	}


	static void Dump (std::wostream & os, bool x)
	{
		os << L"bool(" << (x ? L"true" : L"false") << L")\n";
	}


	static void Dump (std::wostream & os, int x)
	{
		os << L"int(" << x << L")\n";
	}


	static void Dump (std::wostream & os, unsigned int x)
	{
		os << L"uint(" << x << L")\n";
	}


	static void Dump (std::wostream & os, float x)
	{
		os << L"float(" << x << L")\n";
	}


	static void Dump (std::wostream & os, double x)
	{
		os << L"double(" << x << L")\n";
	}


	static void Dump (std::wostream & os, wchar_t x)
	{
		os << L"char(";
		Pretty(os, x);
		os << L")\n";
	}


	static void Dump (std::wostream & os, char x)
	{
		Dump(os, static_cast<wchar_t>(x));
	}


	static void Dump (std::wostream & os, std::string const & x)
	{
		os << L"string(";
		Pretty(os, x);
		os << L")\n";
	}


	static void Dump (std::wostream & os, std::wstring const & x)
	{
		os << L"string(";
		Pretty(os, x);
		os << L")\n";
	}


	static void Dump (std::wostream & os, char const * x)
	{
		Dump(os, std::string(x));
	}


	static void Dump (std::wostream & os, wchar_t const * x)
	{
		Dump(os, std::wstring(x));
	}


	static void DumpMemo (std::wostream & os, std::wstring const & x)
	{
		os << x << L"\n";
	}
	

public:
	static Keyword const empty;

	static void TestCompileTypes ()
	{
		Opts opts;
	
		opts.AddOption(Q("x"), [] (bool) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (int) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (unsigned int) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (float) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (double) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (Char) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (String) {
			return PR::Accept;
		});
	
		std::vector<String> args;
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
	
		opts.AddOption(Q("x"), [] () {});
		opts.AddOption(Q("x"), [] (I) {});
		opts.AddOption(Q("x"), [] (I,I) {});
		opts.AddOption(Q("x"), [] (I,I,I) {});
		opts.AddOption(Q("x"), [] (I,I,I,I) {});
		opts.AddOption(Q("x"), [] (I,I,I,I,I) {});
	
		opts.AddOption(Q("x"), [] () {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (I) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (I,I) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (I,I,I) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (I,I,I,I) {
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [] (I,I,I,I,I) {
			return PR::Accept;
		});
	
		std::vector<String> args;
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
			opts.AddOption(empty, [] () { return PR::Accept; });
		}
		catch (lambda_opts::Exception const &) {
			return;
		}
		FAIL;
	}
	
	
	static void TestArityPrecedence1 ()
	{
		typedef String S;
		std::vector<int> calls;
	
		Opts opts;
		opts.AddOption(empty, [&] (S) {
			calls.push_back(1);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (S,S) {
			calls.push_back(2);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (S,S,S) {
			calls.push_back(3);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (S,S,S,S) {
			calls.push_back(4);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (S,S,S,S,S) {
			calls.push_back(5);
			return PR::Accept;
		});
	
		auto parseCount = [&] (size_t n) {
			std::vector<String> args(n);
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
		typedef String S;
		std::vector<int> calls;
	
		Opts opts;
		opts.AddOption(Q("x"), [&] () {
			calls.push_back(0);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (S) {
			calls.push_back(1);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (S,S) {
			calls.push_back(2);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (S,S,S) {
			calls.push_back(3);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (S,S,S,S) {
			calls.push_back(4);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (S,S,S,S,S) {
			calls.push_back(5);
			return PR::Accept;
		});
	
		auto parseCount = [&] (size_t n) {
			std::vector<String> args(n + 1);
			args.front() = Q("x");
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
		typedef String S;
		std::vector<int> calls;
	
		Opts opts;
		opts.AddOption(empty, [&] (S) {
			calls.push_back(0);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] () {
			calls.push_back(1);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (S) {
			calls.push_back(2);
			return PR::Accept;
		});
	
		std::vector<String> args;
		args.push_back(Q(""));
		args.push_back(Q("x"));
		args.push_back(Q(""));
		args.push_back(Q("x"));
	
	
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
		typedef String S;
		std::vector<int> calls;
	
		Opts opts;
		opts.AddOption(Q("x"), [&] (S) {
			calls.push_back(0);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] () {
			calls.push_back(1);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (S) {
			calls.push_back(2); return
			PR::Accept;
		});
	
		std::vector<String> args;
		args.push_back(Q(""));
		args.push_back(Q("x"));
		args.push_back(Q(""));
		args.push_back(Q("x"));
	
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
		std::wstringstream ss;
	
		Opts opts;
	
		opts.AddOption(empty, [&] (bool x) {
			Dump(ss, x);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (unsigned int x) {
			Dump(ss, x);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (int x) {
			Dump(ss, x);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (float x) {
			Dump(ss, x);
			if (x == 0.0f) {
				DumpMemo(ss, L"REJECTED");
				return PR::Reject;
			}
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (double x) {
			Dump(ss, x);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (Char x) {
			Dump(ss, x);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (String x) {
			Dump(ss, x);
			return PR::Accept;
		});
	
		std::vector<String> args;
		std::wstringstream expected;
	
		args.push_back(Q(""));
		Dump(expected, "");

		args.push_back(Q("true"));
		Dump(expected, true);

		args.push_back(Q("false"));
		Dump(expected, false);

		args.push_back(Q(" false"));
		Dump(expected, L" false");

		args.push_back(Q("false "));
		Dump(expected, L"false ");
	
		args.push_back(Q("-4"));
		Dump(expected, -4);
	
		args.push_back(Q("0"));
		Dump(expected, 0u);
	
		args.push_back(Q("+4"));
		Dump(expected, 4u);
	
		args.push_back(Q("5.1e-9"));
		Dump(expected, 5.1e-9f);
	
		args.push_back(Q("-5.1e-100"));
		Dump(expected, -0.0f);
		DumpMemo(expected, L"REJECTED");
		Dump(expected, -5.1e-100);
	
		args.push_back(Q("0.5"));
		Dump(expected, 0.5f);
	
		args.push_back(Q("-0.5"));
		Dump(expected, -0.5f);
	
		args.push_back(Q("+0.5"));
		Dump(expected, 0.5f);
	
		args.push_back(Q(".5"));
		Dump(expected, 0.5f);
	
		args.push_back(Q("5e-1"));
		Dump(expected, 0.5f);
	
		args.push_back(Q(" "));
		Dump(expected, L' ');
	
		args.push_back(Q("-"));
		Dump(expected, L'-');
	
		args.push_back(Q("+"));
		Dump(expected, L'+');
	
		args.push_back(Q(" 0"));
		Dump(expected, L" 0");

		args.push_back(Q("0 "));
		Dump(expected, L"0 ");
	
		args.push_back(Q("08"));
		Dump(expected, 8u);
	
		args.push_back(Q("0111"));
		Dump(expected, 111u);
	
		args.push_back(Q("0x"));
		Dump(expected, L"0x");
	
		args.push_back(Q("0x111"));
		Dump(expected, L"0x111");
	
		args.push_back(Q("0X111"));
		Dump(expected, L"0X111");
	
		args.push_back(Q("0xa"));
		Dump(expected, L"0xa");
	
		args.push_back(Q("0XA"));
		Dump(expected, L"0XA");
	
		args.push_back(Q("0Xa"));
		Dump(expected, L"0Xa");
	
		args.push_back(Q("0xg"));
		Dump(expected, L"0xg");
	
		args.push_back(Q("\n0"));
		Dump(expected, L"\n0");
	
		args.push_back(Q("\t0"));
		Dump(expected, L"\t0");
	
		args.push_back(String(1, '\0'));
		Dump(expected, L'\0');
	
		args.push_back(String(2, '\0'));
		Dump(expected, std::wstring(2, '\0'));
	
		String weird = Q("123");
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
		std::wstringstream ss;
	
		Opts opts;
		opts.AddOption(empty, [&] (int x) {
			Dump(ss, x);
			return PR::Reject;
		});
		opts.AddOption(empty, [&] (String x) {
			Dump(ss, x);
			return PR::Accept;
		});
	
		std::vector<String> args;
		std::wstringstream expected;
	
		args.push_back(Q("1"));
		Dump(expected, 1);
		Dump(expected, L"1");
	
		args.push_back(Q("x"));
		Dump(expected, L"x");
	
		args.push_back(Q("2"));
		Dump(expected, 2);
		Dump(expected, L"2");
	
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
		std::wstringstream ss;
	
		Opts opts;
		opts.AddOption(empty, [&] (int x) {
			Dump(ss, x);
			return PR::Reject;
		});
		opts.AddOption(empty, [&] (Char x) {
			Dump(ss, x);
			return PR::Accept;
		});
	
		std::vector<String> args;
		std::wstringstream expected;
	
		args.push_back(Q("1"));
		Dump(expected, 1);
		Dump(expected, L'1');
	
		args.push_back(Q("x"));
		Dump(expected, L'x');
	
		args.push_back(Q("22"));
		Dump(expected, 22);
	
		args.push_back(Q("3"));
	
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
	
	
	static void TestFatal1 ()
	{
		std::wstringstream ss;
	
		Opts opts;
		opts.AddOption(empty, [&] (String x) {
			Dump(ss, x);
			return PR::Fatal;
		});
	
		std::vector<String> args;
		args.push_back(Q("x"));
	
		std::wstringstream expected;
		Dump(expected, L"x");
	
		auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
		int failIdx;
		if (parseEnv.Run(failIdx)) {
			FAIL;
		}
		if (failIdx != 0) {
			FAIL;
		}
	
		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


	static void TestFatal2 ()
	{
		std::wstringstream ss;

		Opts opts;
		opts.AddOption(empty, [&] (int x) {
			Dump(ss, x);
			return PR::Fatal;
		});
		opts.AddOption(empty, [&] (String x) {
			Dump(ss, x);
			return PR::Accept;
		});

		std::vector<String> args;
		args.push_back(Q("x"));
		args.push_back(Q("1"));
		args.push_back(Q("y"));
		args.push_back(Q("2"));

		std::wstringstream expected;
		Dump(expected, L"x");
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
		opts.AddOption(empty, [] (int) {
			return PR::Accept;
		});
		opts.AddOption(empty, [] (Char) {
			return PR::Accept;
		});
	
		std::vector<String> args;
		args.push_back(Q("11"));
		args.push_back(Q("22"));
		args.push_back(Q("x"));
		args.push_back(Q("yy"));
		args.push_back(Q("z"));
	
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
		std::wstringstream ss;
	
		Opts opts;
		opts.AddOption(empty, [&] (int x) {
			DumpMemo(ss, L"int");
			Dump(ss, x);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (Char x) {
			DumpMemo(ss, L"char");
			Dump(ss, x);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (String x) {
			DumpMemo(ss, L"string");
			Dump(ss, x);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] () {
			DumpMemo(ss, L"x");
			return PR::Accept;
		});
		opts.AddOption(Q("xx"), [&] () {
			DumpMemo(ss, L"xx");
			return PR::Accept;
		});
		opts.AddOption(Q("yy"), [&] () {
			DumpMemo(ss, L"yy");
			return PR::Accept;
		});
		opts.AddOption(Q("y"), [&] () {
			DumpMemo(ss, L"y");
			return PR::Accept;
		});
		opts.AddOption(Q("z"), [&] (int x) {
			DumpMemo(ss, L"z");
			Dump(ss, x);
			return PR::Accept;
		});
		opts.AddOption(Q("zz"), [&] (int x) {
			DumpMemo(ss, L"zz");
			Dump(ss, x);
			return PR::Accept;
		});
	
		std::vector<String> args;
		std::wstringstream expected;
	
		args.push_back(Q("x"));
		DumpMemo(expected, L"x");
	
		args.push_back(Q("xx"));
		DumpMemo(expected, L"xx");
	
		args.push_back(Q("0"));
		DumpMemo(expected, L"int");
		Dump(expected, 0);
	
		args.push_back(Q("y"));
		DumpMemo(expected, L"y");
	
		args.push_back(Q("yy"));
		DumpMemo(expected, L"yy");
	
		args.push_back(Q("1"));
		DumpMemo(expected, L"int");
		Dump(expected, 1);
	
		args.push_back(Q("z"));
		DumpMemo(expected, L"char");
		Dump(expected, L'z');
	
		args.push_back(Q("zz"));
		DumpMemo(expected, L"string");
		Dump(expected, L"zz");
	
		args.push_back(Q("z"));
		args.push_back(Q("2"));
		DumpMemo(expected, L"z");
		Dump(expected, 2);
	
		args.push_back(Q("zz"));
		args.push_back(Q("3"));
		DumpMemo(expected, L"zz");
		Dump(expected, 3);
	
		args.push_back(Q("w"));
		DumpMemo(expected, L"char");
		Dump(expected, L'w');
	
		args.push_back(Q("ww"));
		DumpMemo(expected, L"string");
		Dump(expected, L"ww");
	
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


	static void TestArrays ()
	{
		std::wstringstream ss;

		Opts opts;
		opts.AddOption(empty, [&] (std::array<std::array<int, 2>, 2> xs) {
			DumpMemo(ss, L"<array>");
			Dump(ss, xs[0][0]);
			Dump(ss, xs[0][1]);
			Dump(ss, xs[1][0]);
			Dump(ss, xs[1][1]);
			DumpMemo(ss, L"</array>");
		});
		opts.AddOption(empty, [&] (std::array<int, 3> xs) {
			DumpMemo(ss, L"<array>");
			Dump(ss, xs[0]);
			Dump(ss, xs[1]);
			Dump(ss, xs[2]);
			DumpMemo(ss, L"</array>");
		});
		opts.AddOption(empty, [&] (int x) {
			Dump(ss, x);
		});
		opts.AddOption(empty, [&] (int a, int b, int c, int d, int e) {
			DumpMemo(ss, L"<not-array>");
			Dump(ss, a);
			Dump(ss, b);
			Dump(ss, c);
			Dump(ss, d);
			Dump(ss, e);
			DumpMemo(ss, L"</not-array>");
		});
		opts.AddOption(empty, [&] (Char x) {
			Dump(ss, x);
		});

		std::vector<String> args;
		std::wstringstream expected;

		args.push_back(Q("-1"));
		args.push_back(Q("-2"));
		args.push_back(Q("-3"));
		args.push_back(Q("-4"));
		args.push_back(Q("-5"));
		DumpMemo(expected, L"<not-array>");
		Dump(expected, -1);
		Dump(expected, -2);
		Dump(expected, -3);
		Dump(expected, -4);
		Dump(expected, -5);
		DumpMemo(expected, L"</not-array>");

		args.push_back(Q("x"));
		Dump(expected, L'x');

		args.push_back(Q("1"));
		args.push_back(Q("2"));
		args.push_back(Q("3"));
		args.push_back(Q("4"));
		DumpMemo(expected, L"<array>");
		Dump(expected, 1);
		Dump(expected, 2);
		Dump(expected, 3);
		Dump(expected, 4);
		DumpMemo(expected, L"</array>");

		args.push_back(Q("x"));
		Dump(expected, L'x');

		args.push_back(Q("5"));
		args.push_back(Q("6"));
		args.push_back(Q("7"));
		DumpMemo(expected, L"<array>");
		Dump(expected, 5);
		Dump(expected, 6);
		Dump(expected, 7);
		DumpMemo(expected, L"</array>");

		args.push_back(Q("x"));
		Dump(expected, L'x');

		args.push_back(Q("8"));
		Dump(expected, 8);

		args.push_back(Q("9"));
		Dump(expected, 9);

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


	static void TestCustomParser ()
	{
		std::wstringstream ss;

		Opts opts;
		opts.AddOption(empty, [&] (Bit x) {
			DumpMemo(ss, L"bit");
			Dump(ss, x.value);
		});
		opts.AddOption(empty, [&] (int x) {
			Dump(ss, x);
		});
	
		std::vector<String> args;
		std::wstringstream expected;

		args.push_back(Q("-1"));
		Dump(expected, -1);

		args.push_back(Q("0"));
		DumpMemo(expected, L"bit");
		Dump(expected, false);

		args.push_back(Q("1"));
		DumpMemo(expected, L"bit");
		Dump(expected, true);

		args.push_back(Q("2"));
		Dump(expected, 2);

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


	static void TestParseState ()
	{
		std::wstringstream ss;

		Opts opts;
		opts.AddOption(empty, [&] (int x) {
			Dump(ss, x);
		});
		opts.AddOption(empty, [&] (lambda_opts::ParseState<Char> parseState) {
			using namespace lambda_opts;

			DumpMemo(ss, L"<parse-state>");

			Dump(ss, *parseState.iter);
			++parseState.iter;

			Dump(ss, *parseState.iter);
			++parseState.iter;

			lambda_opts::Maybe<unsigned int> mUint;
			if (Parse<Char, unsigned int>(parseState, mUint)) {
				FAIL;
			}
			lambda_opts::Maybe<int> mInt;
			if (!Parse<Char, int>(parseState, mInt)) {
				FAIL;
			}
			Dump(ss, *mInt);

			if (!Parse<Char, unsigned int>(parseState, mUint)) {
				FAIL;
			}
			Dump(ss, *mUint);

			Dump(ss, *parseState.iter);
			++parseState.iter;

			DumpMemo(ss, L"</parse-state>");
		});
	
		std::vector<String> args;
		std::wstringstream expected;

		args.push_back(Q("0"));
		Dump(expected, 0);

		args.push_back(Q("1"));
		Dump(expected, 1);

		args.push_back(Q("x"));
		args.push_back(Q("y"));
		args.push_back(Q("-1"));
		args.push_back(Q("2"));
		args.push_back(Q("z"));
		DumpMemo(expected, L"<parse-state>");
		Dump(expected, L"x");
		Dump(expected, L"y");
		Dump(expected, -1);
		Dump(expected, 2u);
		Dump(expected, L"z");
		DumpMemo(expected, L"</parse-state>");

		args.push_back(Q("3"));
		Dump(expected, 3);

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


	template <typename Helper>
	static void TestMaybeLifetime ()
	{
		int const & P1 = Helper::P1;
		int const & P2 = Helper::P2;
		int & value = Helper::value;

		value = 0;

		Opts opts;
		opts.AddOption(empty, [&] (lambda_opts::ParseState<Char> parseState) {
			{
				lambda_opts::Maybe<Helper> mObject;
				if (value != 0) {
					FAIL;
				}
			}
			if (value != 0) {
				FAIL;
			}
			{
				lambda_opts::Maybe<Helper> mObject;
				if (!lambda_opts::Parse<Char, Helper>(parseState, mObject)) {
					FAIL;
				}
				if (value != P1) {
					FAIL;
				}
			}
			if (value != P1 + P2) {
				FAIL;
			}
			value = 0;
			{
				typedef std::array<Helper, 5> Array;
				lambda_opts::Maybe<Array> mObject;
				if (lambda_opts::Parse<Char, Array>(parseState, mObject)) {
					FAIL;
				}
				if (value != 3 * P1 + 3 * P2) {
					FAIL;
				}
			}
			if (value != 3 * P1 + 3 * P2) {
				FAIL;
			}
			value = 0;
			{
				typedef std::array<Helper, 3> Array;
				lambda_opts::Maybe<Array> mObject;
				if (!lambda_opts::Parse<Char, Array>(parseState, mObject)) {
					FAIL;
				}
				if (value != 3 * P1) {
					FAIL;
				}
			}
			if (value != 3 * P1 + 3 * P2) {
				FAIL;
			}
		});

		std::vector<String> args(4);

		auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
		if (!parseEnv.Run()) {
			FAIL;
		}
	}


	static void TestSubKeywords1 ()
	{
		std::wostringstream ss;

		Opts opts;
		{
			opts.AddOption(Q("foo"), [&] (int x) {
				Dump(ss, x);
			});
		}
		{
			Keyword foo(Q("foo"));
			foo.AddSubKeyword(Keyword(Q("bar")));
			opts.AddOption(foo, [&] (Char x) {
				Dump(ss, x);
			});
		}
		{
			Keyword foo(Q("foo"));
			foo.AddSubKeyword(Keyword(Q("baz")));
			foo.AddSubKeyword(Keyword(Q("baaz")));
			opts.AddOption(foo, [&] (bool x) {
				Dump(ss, x);
			});
		}
		opts.AddOption(empty, [&] (String x) {
			Dump(ss, x);
		});

		std::vector<String> args;
		std::wostringstream expected;

		args.push_back(Q("foo"));
		args.push_back(Q("666"));
		Dump(expected, 666);
		
		args.push_back(Q("foo"));
		args.push_back(Q("miss1"));
		Dump(expected, L"foo");
		Dump(expected, L"miss1");

		args.push_back(Q("foo"));
		args.push_back(Q("bar"));
		args.push_back(Q("@"));
		Dump(expected, '@');

		args.push_back(Q("foo"));
		args.push_back(Q("bar"));
		args.push_back(Q("miss2"));
		Dump(expected, L"foo");
		Dump(expected, L"bar");
		Dump(expected, L"miss2");

		args.push_back(Q("foo"));
		args.push_back(Q("baz"));
		args.push_back(Q("true"));
		Dump(expected, true);

		args.push_back(Q("foo"));
		args.push_back(Q("baaz"));
		args.push_back(Q("false"));
		Dump(expected, false);

		args.push_back(Q("foo"));
		args.push_back(Q("baaaz"));
		args.push_back(Q("false"));
		Dump(expected, L"foo");
		Dump(expected, L"baaaz");
		Dump(expected, L"false");

		args.push_back(Q("foo"));
		args.push_back(Q("baz"));
		args.push_back(Q("miss3"));
		Dump(expected, L"foo");
		Dump(expected, L"baz");
		Dump(expected, L"miss3");

		args.push_back(Q("foo"));
		args.push_back(Q("baaz"));
		args.push_back(Q("miss4"));
		Dump(expected, L"foo");
		Dump(expected, L"baaz");
		Dump(expected, L"miss4");

		auto parseEnv = opts.CreateParseEnv(args.begin(), args.end());
		if (!parseEnv.Run()) {
			FAIL;
		}
		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


	static void TestHelpDescription ()
	{
		auto nop = [] () {};

		Opts opts;

		opts.AddOption(Keyword(Q("foo"), Q("Foo does shtuff.")), nop);
		opts.AddOption(Keyword(Q("bar"), 'b', Q("Bar does even moare shtuff!!!")), nop);
		opts.AddOption(Keyword(Q("foflcopter"), Q("0123456789 01234567890123456789 012345678901234567890123456789 0123456789012345678901234567890123456789")), nop);

		Opts::FormatConfig config;
		config.maxWidth = 0;
		String desc = opts.HelpDescription(config);

		std::cout << "\n<HELP>\n";
		for (Char c : desc) {
			std::cout << c;
		}
		std::cout << "\n</HELP>\n";
	}
};


template <typename Char>
typename LambdaOpts<Char>::Keyword const Tests<Char>::empty;


template <typename Char>
static bool RunCharTests ()
{
	typedef void (*TestFunc)();

	TestFunc tests[] = {
		Tests<Char>::TestCompileTypes,
		Tests<Char>::TestCompileArities,
		Tests<Char>::TestRejectEmptyKeyword,
		Tests<Char>::TestArityPrecedence1,
		Tests<Char>::TestArityPrecedence2,
		Tests<Char>::TestEmptyPrecedence1,
		Tests<Char>::TestEmptyPrecedence2,
		Tests<Char>::TestObtainedValues,
		Tests<Char>::TestReject1,
		Tests<Char>::TestReject2,
		Tests<Char>::TestFatal1,
		Tests<Char>::TestFatal2,
		Tests<Char>::TestNoMatch,
		Tests<Char>::TestKeyword1,
		Tests<Char>::TestArrays,
		Tests<Char>::TestCustomParser,
		Tests<Char>::TestParseState,
		Tests<Char>::TestMaybeLifetime<TestMaybeLifetimeHelper>,
		Tests<Char>::TestMaybeLifetime<TestMaybeLifetimeHelperSuperAligned>,
		Tests<Char>::TestSubKeywords1,
		Tests<Char>::TestHelpDescription,
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

	return true;
}


static bool RunTests ()
{
	static_assert(std::is_same<LambdaOpts<>::char_type, LambdaOpts<char>::char_type>::value, "Default [Char] type is not [char].");

	if (!RunCharTests<char>()) {
		return false;
	}
	if (!RunCharTests<wchar_t>()) {
		return false;
	}
	return true;
}


int main (int, char **)
{
	std::setlocale(LC_ALL, "C");

	if (RunTests()) {
		std::cout << "All tests succeeded." << std::endl;
		return 0;
	}

	return 1;
}













