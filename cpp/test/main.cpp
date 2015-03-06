#include "../src/LambdaOptions.h"

#include <algorithm>
#include <clocale>
#include <cwchar>
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
	Bit ();	// Intentionally disable to ensure LambdaOptions can handle values it cannot default instantiate.

public:
	Bit (bool value)
		: value(value)
	{}

public:
	bool value;
};


namespace lambda_options
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


namespace lambda_options
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


template <typename T, size_t N>
static bool Equal (std::vector<T> const & xs, T const (&ys)[N])
{
	if (xs.size() != N) {
		return false;
	}
	return std::equal(xs.begin(), xs.end(), ys);
}


using lambda_options::Any;


static void nop0 () {}
static void nop1 (Any) {}
static void nop2 (Any, Any) {}
static void nop3 (Any, Any, Any) {}
static void nop4 (Any, Any, Any, Any) {}
static void nop5 (Any, Any, Any, Any, Any) {}

auto const & nop = nop0;


//////////////////////////////////////////////////////////////////////////


template <typename Char>
class Tests {
private:
	typedef std::basic_string<Char> String;
	typedef lambda_options::Options<Char> Opts;
	typedef lambda_options::ParseResult PR;
	typedef lambda_options::Keyword<Char> Keyword;
	typedef lambda_options::FormattingConfig<Char> FormattingConfig;
	typedef lambda_options::OptionsConfig OptionsConfig;
	typedef lambda_options::KeywordStyle KeywordStyle;
	typedef lambda_options::MatchFlags MatchFlags;


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


	static String Q (char c)
	{
		return String(1, c);
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
			std::swprintf(buff, 8, L"\\x%x", c);
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
	static OptionsConfig const testConfig;

	static void TestCompileTypes ()
	{
		Opts opts(testConfig);
	
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
		opts.AddOption(Q("xc"), [] (String const) {
			return PR::Accept;
		});
		opts.AddOption(Q("xr"), [] (String &&) {
			return PR::Accept;
		});
		opts.AddOption(Q("xcr"), [] (String const &&) {
			return PR::Accept;
		});
		opts.AddOption(Q("xl"), [] (String &) {
			return PR::Accept;
		});
		opts.AddOption(Q("xcl"), [] (String const &) {
			return PR::Accept;
		});
		
	
		std::vector<String> args;
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
	}
	
	
	static void TestCompileArities ()
	{
		Opts opts(testConfig);
	
		opts.AddOption(Q("x"), nop0);
		opts.AddOption(Q("x"), nop1);
		opts.AddOption(Q("x"), nop2);
		opts.AddOption(Q("x"), nop3);
		opts.AddOption(Q("x"), nop4);
		opts.AddOption(Q("x"), nop5);
	
		opts.AddOption(Q("xx"), [] () {
			return PR::Accept;
		});
		opts.AddOption(Q("xx"), [] (Any) {
			return PR::Accept;
		});
		opts.AddOption(Q("xx"), [] (Any,Any) {
			return PR::Accept;
		});
		opts.AddOption(Q("xx"), [] (Any,Any,Any) {
			return PR::Accept;
		});
		opts.AddOption(Q("xx"), [] (Any,Any,Any,Any) {
			return PR::Accept;
		});
		opts.AddOption(Q("xx"), [] (Any,Any,Any,Any,Any) {
			return PR::Accept;
		});
	
		std::vector<String> args;
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
	}
	
	
	static void TestRejectEmptyOption ()
	{
		try {
			Opts opts(testConfig);
			opts.AddOption(empty, [] () { return PR::Accept; });
		}
		catch (lambda_options::EmptyOptionException const &) {
			return;
		}
		FAIL;
	}
	
	
	static void TestArityPrecedence1 ()
	{
		std::vector<int> calls;
	
		Opts opts(testConfig);
		opts.AddOption(empty, [&] (String) {
			calls.push_back(1);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (String,String) {
			calls.push_back(2);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (String,String,String) {
			calls.push_back(3);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (String,String,String,String) {
			calls.push_back(4);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (String,String,String,String,String) {
			calls.push_back(5);
			return PR::Accept;
		});
	
		auto parseCount = [&] (size_t n) {
			std::vector<String> args(n);
			auto parseContext = opts.CreateParseContext(args.begin(), args.end());
			parseContext.Run();
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
		std::vector<int> calls;
	
		Opts opts(testConfig);
		opts.AddOption(Q("x"), [&] () {
			calls.push_back(0);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (String) {
			calls.push_back(1);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (String,String) {
			calls.push_back(2);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (String,String,String) {
			calls.push_back(3);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (String,String,String,String) {
			calls.push_back(4);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (String,String,String,String,String) {
			calls.push_back(5);
			return PR::Accept;
		});
	
		auto parseCount = [&] (size_t n) {
			std::vector<String> args(n + 1);
			args.front() = Q("x");
			auto parseContext = opts.CreateParseContext(args.begin(), args.end());
			parseContext.Run();
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
		std::vector<int> calls;
	
		Opts opts(testConfig);
		opts.AddOption(empty, [&] (String) {
			calls.push_back(0);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] () {
			calls.push_back(1);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] (String) {
			calls.push_back(2);
			return PR::Accept;
		});
	
		std::vector<String> args;
		args.push_back(Q(""));
		args.push_back(Q("x"));
		args.push_back(Q(""));
		args.push_back(Q("x"));
	
	
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
	
		int const expectedCalls[] = { 0,2,1 };
		if (!Equal(calls, expectedCalls)) {
			FAIL;
		}
	}
	
	
	static void TestEmptyPrecedence2 ()
	{
		std::vector<int> calls;
	
		Opts opts(testConfig);
		opts.AddOption(Q("x"), [&] (String) {
			calls.push_back(0);
			return PR::Accept;
		});
		opts.AddOption(Q("x"), [&] () {
			calls.push_back(1);
			return PR::Accept;
		});
		opts.AddOption(empty, [&] (String) {
			calls.push_back(2); return
			PR::Accept;
		});
	
		std::vector<String> args;
		args.push_back(Q(""));
		args.push_back(Q("x"));
		args.push_back(Q(""));
		args.push_back(Q("x"));
	
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
	
		int const expectedCalls[] = { 2,0,1 };
		if (!Equal(calls, expectedCalls)) {
			FAIL;
		}
	}
	
	
	static void TestObtainedValues ()
	{
		std::wstringstream ss;
	
		Opts opts(testConfig);
	
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
	
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
	
		if (ss.str() != expected.str()) {
			FAIL;
		}
	}
	
	
	static void TestReject1 ()
	{
		std::wstringstream ss;
	
		Opts opts(testConfig);
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
	
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
	
		if (ss.str() != expected.str()) {
			FAIL;
		}
	}
	
	
	static void TestReject2 ()
	{
		std::wstringstream ss;
	
		Opts opts(testConfig);
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
	
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		try {
			parseContext.Run();
			FAIL;
		}
		catch (lambda_options::ParseFailedException const & e) {
			if (e.endIndex != 3) {
				FAIL;
			}
		}
	
		if (ss.str() != expected.str()) {
			FAIL;
		}
	}
	
	
	static void TestFatal1 ()
	{
		std::wstringstream ss;
	
		Opts opts(testConfig);
		opts.AddOption(empty, [&] (String x) {
			Dump(ss, x);
			return PR::Fatal;
		});
	
		std::vector<String> args;
		args.push_back(Q("x"));
	
		std::wstringstream expected;
		Dump(expected, L"x");
	
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		try {
			parseContext.Run();
			FAIL;
		}
		catch (lambda_options::ParseFailedException const & e) {
			if (e.endIndex != 1) {
				FAIL;
			}
		}
		
		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


	static void TestFatal2 ()
	{
		std::wstringstream ss;

		Opts opts(testConfig);
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

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		try {
			parseContext.Run();
			FAIL;
		}
		catch (lambda_options::ParseFailedException const & e) {
			if (e.endIndex != 2) {
				FAIL;
			}
		}

		if (ss.str() != expected.str()) {
			FAIL;
		}
	}
	
	
	static void TestNoMatch ()
	{
		Opts opts(testConfig);
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
	
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		try {
			parseContext.Run();
			FAIL;
		}
		catch (lambda_options::ParseFailedException const & e) {
			if (e.endIndex != 3) {
				FAIL;
			}
		}
	}
	
	
	static void TestKeyword1 ()
	{
		std::wstringstream ss;
	
		Opts opts(testConfig);
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
	
		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
	
		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


	static void TestArrays ()
	{
		std::wstringstream ss;

		Opts opts(testConfig);
		opts.AddOption(Q("arr0"), [&] (std::array<int, 0>) {
			DumpMemo(ss, L"<array/>");
		});
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

		args.push_back(Q("arr0"));
		DumpMemo(expected, L"<array/>");

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

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();

		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


	static void TestCustomParser ()
	{
		std::wstringstream ss;

		Opts opts(testConfig);
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

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();

		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


	static void TestParseState ()
	{
		std::wstringstream ss;

		Opts opts(testConfig);
		opts.AddOption(empty, [&] (int x) {
			Dump(ss, x);
		});
		opts.AddOption(empty, [&] (lambda_options::ParseState<Char> parseState) {
			using namespace lambda_options;

			DumpMemo(ss, L"<parse-state>");

			Dump(ss, *parseState.iter);
			++parseState.iter;

			Dump(ss, *parseState.iter);
			++parseState.iter;

			lambda_options::Maybe<unsigned int> mUint;
			if (Parse<Char, unsigned int>(parseState, mUint)) {
				FAIL;
			}
			lambda_options::Maybe<int> mInt;
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

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();

		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


	static void TestMaybeLifetime ()
	{
		typedef TestMaybeLifetimeHelper Helper;

		int const P1 = Helper::P1;
		int const P2 = Helper::P2;
		int & value = Helper::value;

		value = 0;

		Opts opts(testConfig);
		opts.AddOption(empty, [&] (lambda_options::ParseState<Char> parseState) {
			{
				lambda_options::Maybe<Helper> mObject;
				if (value != 0) {
					FAIL;
				}
			}
			if (value != 0) {
				FAIL;
			}
			{
				lambda_options::Maybe<Helper> mObject;
				if (!lambda_options::Parse<Char, Helper>(parseState, mObject)) {
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
				lambda_options::Maybe<Array> mObject;
				if (lambda_options::Parse<Char, Array>(parseState, mObject)) {
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
				lambda_options::Maybe<Array> mObject;
				if (!lambda_options::Parse<Char, Array>(parseState, mObject)) {
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

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
	}


	static void TestHelpDescription ()
	{
		Opts opts(testConfig);

		Keyword kwFoo(Q("foo"));
		kwFoo.desc = Q("Foo does shtuff.");
		opts.AddOption(kwFoo, nop);

		Keyword kwBar(Q("bar"), Q('b'));
		kwBar.desc = Q("Bar does even moare shtuff!!!");
		opts.AddOption(kwBar, nop);

		Keyword kwRofl(Q("foflcopter"));
		kwRofl.desc = Q("0123456789 01234567890123456789 012345678901234567890123456789 0123456789012345678901234567890123456789");
		opts.AddOption(kwRofl, nop);

		FormattingConfig config;
		config.maxWidth = 0;
		String desc = opts.HelpDescription(config);

		auto printString = [] (String const & str) {
			std::cout << "\n<String>\n";
			for (Char c : str) {
				std::cout << static_cast<char>(c);
			}
			std::cout << "\n</String>\n";
		};

		printString(desc);
	}

	static void TestHelpGroups ()
	{
		Opts opts(testConfig);

		Keyword kwFoo(Q("foo"));
		kwFoo.group = Q("cake");
		opts.AddOption(kwFoo, nop);

		Keyword kwBar(Q("bar"));
		kwBar.group = Q("lie");
		opts.AddOption(kwBar, nop);

		Keyword kwBaz(Q("baz"));
		kwBaz.group = Q("cake");
		opts.AddOption(kwBaz, nop);

		auto printString = [] (String const & str) {
			std::cout << "\n<String>\n";
			for (Char c : str) {
				std::cout << static_cast<char>(c);
			}
			std::cout << "\n</String>\n";
		};

		{
			String desc = opts.HelpDescription();
			printString(desc);
		}
		opts.SetGroupPriority(Q("cake"), 0);
		opts.SetGroupPriority(Q("lie"), 1);
		{
			String desc = opts.HelpDescription();
			printString(desc);
		}
		opts.SetGroupPriority(Q("cake"), 10);
		opts.SetGroupPriority(Q("lie"), 5);
		{
			FormattingConfig config;
			config.groupFilter.push_back(Q("cake"));
			String desc = opts.HelpDescription(config);
			printString(desc);
		}
		{
			FormattingConfig config;
			config.groupFilter.push_back(Q("cake"));
			config.groupFilter.push_back(Q("lie"));
			String desc = opts.HelpDescription(config);
			printString(desc);
		}
		{
			FormattingConfig config;
			config.groupFilter.push_back(Q("lie"));
			config.groupFilter.push_back(Q("cake"));
			String desc = opts.HelpDescription(config);
			printString(desc);
		}
		{
			FormattingConfig config;
			config.groupFilter.push_back(Q("where"));
			config.groupFilter.push_back(Q("lie"));
			config.groupFilter.push_back(Q("waldo"));
			String desc = opts.HelpDescription(config);
			printString(desc);
		}
	}


	static void TestKeywordStyle ()
	{
		static_assert(KeywordStyle::Default == KeywordStyle::Gnu, "whoops");

		auto testStyle = [&] (KeywordStyle style, String const & inputName, String const expectedName) {
			std::wstringstream ss;

			OptionsConfig config;
			config.keywordStyle = style;
			Opts opts(config);

			opts.AddOption(inputName, [&] () {
				DumpMemo(ss, L(inputName));
			});
			opts.AddOption(empty, [&] (String s) {
				Dump(ss, s);
			});

			std::vector<String> args;
			std::wstringstream expected;

			auto pushArg = [&] (String const & arg) {
				args.push_back(arg);
				if (arg == expectedName) {
					DumpMemo(expected, L(inputName));
				}
				else {
					Dump(expected, arg);
				}
			};

			pushArg(Q("foo"));
			pushArg(Q("-foo"));
			pushArg(Q("--foo"));
			pushArg(Q("---foo"));
			pushArg(Q("/foo"));
			pushArg(Q("//foo"));
			pushArg(Q(""));
			pushArg(Q("f"));
			pushArg(Q("-"));
			pushArg(Q("/"));
			pushArg(Q("-f"));
			pushArg(Q("/f"));
			pushArg(Q("--f"));
			pushArg(Q("//f"));

			auto parseContext = opts.CreateParseContext(args.begin(), args.end());
			parseContext.Run();
		};

		testStyle(KeywordStyle::Exact, Q(""), Q(""));
		testStyle(KeywordStyle::Exact, Q("f"), Q("f"));
		testStyle(KeywordStyle::Exact, Q("-f"), Q("-f"));
		testStyle(KeywordStyle::Exact, Q("--f"), Q("--f"));
		testStyle(KeywordStyle::Exact, Q("/f"), Q("/f"));
		testStyle(KeywordStyle::Exact, Q("//f"), Q("//f"));
		testStyle(KeywordStyle::Exact, Q("foo"), Q("foo"));
		testStyle(KeywordStyle::Exact, Q("-foo"), Q("-foo"));
		testStyle(KeywordStyle::Exact, Q("--foo"), Q("--foo"));
		testStyle(KeywordStyle::Exact, Q("---foo"), Q("---foo"));
		testStyle(KeywordStyle::Exact, Q("/foo"), Q("/foo"));
		testStyle(KeywordStyle::Exact, Q("//foo"), Q("//foo"));

		testStyle(KeywordStyle::Gnu, Q(""), Q(""));
		testStyle(KeywordStyle::Gnu, Q("f"), Q("-f"));
		testStyle(KeywordStyle::Gnu, Q("-f"), Q("-f"));
		testStyle(KeywordStyle::Gnu, Q("--f"), Q("--f"));
		testStyle(KeywordStyle::Gnu, Q("/f"), Q("--/f"));
		testStyle(KeywordStyle::Gnu, Q("//f"), Q("--//f"));
		testStyle(KeywordStyle::Gnu, Q("foo"), Q("--foo"));
		testStyle(KeywordStyle::Gnu, Q("-foo"), Q("--foo"));
		testStyle(KeywordStyle::Gnu, Q("--foo"), Q("--foo"));
		testStyle(KeywordStyle::Gnu, Q("---foo"), Q("---foo"));
		testStyle(KeywordStyle::Gnu, Q("/foo"), Q("--/foo"));
		testStyle(KeywordStyle::Gnu, Q("//foo"), Q("--//foo"));

		testStyle(KeywordStyle::Windows, Q(""), Q(""));
		testStyle(KeywordStyle::Windows, Q("f"), Q("/f"));
		testStyle(KeywordStyle::Windows, Q("-f"), Q("/-f"));
		testStyle(KeywordStyle::Windows, Q("--f"), Q("/--f"));
		testStyle(KeywordStyle::Windows, Q("/f"), Q("/f"));
		testStyle(KeywordStyle::Windows, Q("//f"), Q("//f"));
		testStyle(KeywordStyle::Windows, Q("foo"), Q("/foo"));
		testStyle(KeywordStyle::Windows, Q("-foo"), Q("/-foo"));
		testStyle(KeywordStyle::Windows, Q("--foo"), Q("/--foo"));
		testStyle(KeywordStyle::Windows, Q("---foo"), Q("/---foo"));
		testStyle(KeywordStyle::Windows, Q("/foo"), Q("/foo"));
		testStyle(KeywordStyle::Windows, Q("//foo"), Q("//foo"));
	}


	static void TestMatchFlags1 ()
	{
		OptionsConfig config;
		config.keywordStyle = KeywordStyle::Exact;
		config.matchFlags = MatchFlags::IgnoreAsciiCase;
		Opts opts(config);

		auto addOption = [&] (char const * name) {
			opts.AddOption(Q(name), nop);
		};

		auto dupOption = [&] (char const * name, char const * existingName) {
			try {
				addOption(name);
			}
			catch (lambda_options::OptionConflictException<Char> const & e) {
				if (e.conflictingNames.first != Q(name)) {
					FAIL;
				}
				if (e.conflictingNames.second != Q(existingName)) {
					FAIL;
				}
				return;
			}
			FAIL;
		};

		addOption("abc");
		dupOption("Abc", "abc");
		dupOption("aBc", "abc");
		dupOption("abC", "abc");
		dupOption("aBC", "abc");
		dupOption("AbC", "abc");
		dupOption("ABc", "abc");
		dupOption("ABC", "abc");

		addOption("FOO");
		dupOption("foo", "FOO");

		addOption("BaR");
		dupOption("bAr", "BaR");
	}


	static void TestMatchFlags2 ()
	{
		std::wstringstream ss;
		
		OptionsConfig config;
		config.keywordStyle = KeywordStyle::Exact;
		config.matchFlags = MatchFlags::RelaxedDashes;
		Opts opts(config);

		opts.AddOption(empty, [&] (String str) {
			Dump(ss, str);
		});

		auto addOption = [&] (char const * name) {
			opts.AddOption(Q(name), [&, name] () {
				DumpMemo(ss, L(name));
			});
		};

		auto dupOption = [&] (char const * name, char const * existingName) {
			try {
				addOption(name);
			}
			catch (lambda_options::OptionConflictException<Char> const & e) {
				if (e.conflictingNames.first != Q(name)) {
					FAIL;
				}
				if (e.conflictingNames.second != Q(existingName)) {
					FAIL;
				}
				return;
			}
			FAIL;
		};
		
		addOption("a");
		addOption("-a");
		addOption("--a");
		addOption("---a");

		dupOption("a", "a");
		dupOption("a-", "a");
		dupOption("a--", "a");
		dupOption("a---", "a");
		dupOption("-a", "-a");
		dupOption("--a", "--a");
		dupOption("---a", "---a");

		addOption("aa");
		addOption("-aa");
		addOption("--aa");
		addOption("---aa");

		dupOption("a-a", "aa");
		dupOption("a--a", "aa");
		dupOption("a---a", "aa");
		dupOption("a-a-", "aa");
		dupOption("-a-a", "-aa");
		dupOption("-a-a-", "-aa");
		dupOption("--a--a--", "--aa");
		dupOption("---a---a---", "---aa");
		
		std::wstringstream expected;
		std::vector<String> args;

		auto yesArg = [&] (char const * cstrIn, char const * cstrExpected) {
			args.push_back(Q(cstrIn));
			DumpMemo(expected, L(cstrExpected));
		};

		auto badArg = [&] (char const * arg) {
			args.push_back(Q(arg));
			Dump(expected, arg);
		};

		yesArg("a", "a");
		yesArg("-a", "-a");
		yesArg("--a", "--a");
		yesArg("---a", "---a");
		yesArg("a-", "a");
		yesArg("a-a", "aa");
		yesArg("a--a", "aa");
		yesArg("a---a", "aa");
		yesArg("-a-", "-a");
		yesArg("-a--", "-a");
		yesArg("-a---", "-a");
		yesArg("--a-", "--a");
		yesArg("--a--", "--a");
		yesArg("--a---", "--a");
		yesArg("---a-", "---a");
		yesArg("---a--", "---a");
		yesArg("---a---", "---a");
		yesArg("-aa-", "-aa");
		yesArg("-aa--", "-aa");
		yesArg("-aa---", "-aa");
		yesArg("-a-a-", "-aa");
		yesArg("-a-a--", "-aa");
		yesArg("-a-a---", "-aa");
		yesArg("-a--a-", "-aa");
		yesArg("-a--a--", "-aa");
		yesArg("-a--a---", "-aa");
		yesArg("--aa-", "--aa");
		yesArg("--aa--", "--aa");
		yesArg("--aa---", "--aa");
		yesArg("---aa-", "---aa");
		yesArg("---aa--", "---aa");
		yesArg("---aa---", "---aa");
		yesArg("--a-a-", "--aa");
		yesArg("--a-a--", "--aa");
		yesArg("--a-a---", "--aa");
		yesArg("---a-a-", "---aa");
		yesArg("---a-a--", "---aa");
		yesArg("---a-a---", "---aa");
		yesArg("--a--a-", "--aa");
		yesArg("--a--a--", "--aa");
		yesArg("--a--a---", "--aa");
		yesArg("---a--a-", "---aa");
		yesArg("---a--a--", "---aa");
		yesArg("---a--a---", "---aa");
		yesArg("-a---a-", "-aa");
		yesArg("-a---a--", "-aa");
		yesArg("-a---a---", "-aa");
		yesArg("--a---a-", "--aa");
		yesArg("--a---a--", "--aa");
		yesArg("--a---a---", "--aa");
		yesArg("---a---a-", "---aa");
		yesArg("---a---a--", "---aa");
		yesArg("---a---a---", "---aa");

		badArg("");
		badArg("A");
		badArg("_a");
		badArg("a_");

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
		
		auto const ssStr = ss.str();
		auto const expectedStr = expected.str();
		if (ssStr != expectedStr) {
			FAIL;
		}
	}


	static void TestMatchFlags3 ()
	{
		std::wstringstream ss;
		
		OptionsConfig config;
		config.keywordStyle = KeywordStyle::Exact;
		config.matchFlags = MatchFlags::RelaxedUnderscores;
		Opts opts(config);

		opts.AddOption(empty, [&] (String str) {
			Dump(ss, str);
		});

		auto addOption = [&] (char const * name) {
			opts.AddOption(Q(name), [&, name] () {
				DumpMemo(ss, L(name));
			});
		};

		auto dupOption = [&] (char const * name, char const * existingName) {
			try {
				addOption(name);
			}
			catch (lambda_options::OptionConflictException<Char> const & e) {
				if (e.conflictingNames.first != Q(name)) {
					FAIL;
				}
				if (e.conflictingNames.second != Q(existingName)) {
					FAIL;
				}
				return;
			}
			FAIL;
		};
		
		addOption("a");
		addOption("-a");
		addOption("--a");
		addOption("---a");

		dupOption("a", "a");
		dupOption("a_", "a");
		dupOption("a__", "a");
		dupOption("a___", "a");
		dupOption("-a", "-a");
		dupOption("--a", "--a");
		dupOption("---a", "---a");

		addOption("aa");
		addOption("-aa");
		addOption("--aa");
		addOption("---aa");

		dupOption("a_a", "aa");
		dupOption("a__a", "aa");
		dupOption("a___a", "aa");
		dupOption("a_a_", "aa");
		dupOption("-a_a", "-aa");
		dupOption("-a_a_", "-aa");
		dupOption("--a__a__", "--aa");
		dupOption("---a___a___", "---aa");
		
		std::wstringstream expected;
		std::vector<String> args;

		auto yesArg = [&] (char const * cstrIn, char const * cstrExpected) {
			args.push_back(Q(cstrIn));
			DumpMemo(expected, L(cstrExpected));
		};

		auto badArg = [&] (char const * arg) {
			args.push_back(Q(arg));
			Dump(expected, arg);
		};

		yesArg("a", "a");
		yesArg("-a", "-a");
		yesArg("--a", "--a");
		yesArg("---a", "---a");
		yesArg("a_", "a");
		yesArg("a_a", "aa");
		yesArg("a__a", "aa");
		yesArg("a___a", "aa");
		yesArg("-a_", "-a");
		yesArg("-a__", "-a");
		yesArg("-a___", "-a");
		yesArg("--a_", "--a");
		yesArg("--a__", "--a");
		yesArg("--a___", "--a");
		yesArg("---a_", "---a");
		yesArg("---a__", "---a");
		yesArg("---a___", "---a");
		yesArg("-aa_", "-aa");
		yesArg("-aa__", "-aa");
		yesArg("-aa___", "-aa");
		yesArg("-a_a_", "-aa");
		yesArg("-a_a__", "-aa");
		yesArg("-a_a___", "-aa");
		yesArg("-a__a_", "-aa");
		yesArg("-a__a__", "-aa");
		yesArg("-a__a___", "-aa");
		yesArg("--aa_", "--aa");
		yesArg("--aa__", "--aa");
		yesArg("--aa___", "--aa");
		yesArg("---aa_", "---aa");
		yesArg("---aa__", "---aa");
		yesArg("---aa___", "---aa");
		yesArg("--a_a_", "--aa");
		yesArg("--a_a__", "--aa");
		yesArg("--a_a___", "--aa");
		yesArg("---a_a_", "---aa");
		yesArg("---a_a__", "---aa");
		yesArg("---a_a___", "---aa");
		yesArg("--a__a_", "--aa");
		yesArg("--a__a__", "--aa");
		yesArg("--a__a___", "--aa");
		yesArg("---a__a_", "---aa");
		yesArg("---a__a__", "---aa");
		yesArg("---a__a___", "---aa");
		yesArg("-a___a_", "-aa");
		yesArg("-a___a__", "-aa");
		yesArg("-a___a___", "-aa");
		yesArg("--a___a_", "--aa");
		yesArg("--a___a__", "--aa");
		yesArg("--a___a___", "--aa");
		yesArg("---a___a_", "---aa");
		yesArg("---a___a__", "---aa");
		yesArg("---a___a___", "---aa");

		yesArg("-_a", "-a");
		yesArg("--__a", "--a");
		yesArg("---___a", "---a");

		badArg("");
		badArg("A");
		badArg("-a-");
		badArg("_-a");

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();
		
		auto const ssStr = ss.str();
		auto const expectedStr = expected.str();
		if (ssStr != expectedStr) {
			FAIL;
		}
	}


	static void TestMatchFlags4 ()
	{
		OptionsConfig config;
		config.keywordStyle = KeywordStyle::Exact;
		config.matchFlags = MatchFlags::IgnoreAsciiCase | MatchFlags::RelaxedDashes | MatchFlags::RelaxedUnderscores;
		Opts opts(config);

		auto addOption = [&] (char const * name) {
			opts.AddOption(Q(name), nop);
		};

		auto dupOption = [&] (char const * name, char const * existingName) {
			try {
				addOption(name);
			}
			catch (lambda_options::OptionConflictException<Char> const & e) {
				if (e.conflictingNames.first != Q(name)) {
					FAIL;
				}
				if (e.conflictingNames.second != Q(existingName)) {
					FAIL;
				}
				return;
			}
			FAIL;
		};

		addOption("");
		dupOption("_", "");
		dupOption("_", "");
		dupOption("_-", "");
		dupOption("_-_", "");
		dupOption("__", "");

		addOption("-");
		dupOption("-_", "-");
		dupOption("-__--__--", "-");

		addOption("a");
		addOption("-a");
		addOption("--A");
		addOption("---a");

		dupOption("a", "a");
		dupOption("a_", "a");
		dupOption("a__", "a");
		dupOption("a___", "a");
		dupOption("-a", "-a");
		dupOption("--a", "--A");
		dupOption("---a", "---a");
		dupOption("-_A", "-a");
		dupOption("--_A_", "--A");
		dupOption("---A_", "---a");

		addOption("aa");
		addOption("-aa");
		addOption("--aa");
		addOption("---aa");

		dupOption("AA", "aa");
		dupOption("Aa", "aa");
		dupOption("aA", "aa");
		dupOption("Aa--__-_", "aa");
		dupOption("a_a", "aa");
		dupOption("a__a", "aa");
		dupOption("a___a", "aa");
		dupOption("a_a_", "aa");
		dupOption("-a_a", "-aa");
		dupOption("-a_a_", "-aa");
		dupOption("--a__a__", "--aa");
		dupOption("---a___a___", "---aa");

		dupOption("a_A", "aa");
		dupOption("A_-a", "aa");
		dupOption("A--_A", "aa");
		dupOption("a_A-", "aa");
		dupOption("-A---__---__-_A", "-aa");
		dupOption("---A---__---__-_A", "---aa");
	}


	static void TestVectors1 ()
	{
		std::wstringstream ss;

		Opts opts(testConfig);

		opts.AddOption(empty, [&] (String str) {
			Dump(ss, str);
		});
		opts.AddOption(Q("x"), [&] (std::vector<String> && strs) {
			DumpMemo(ss, L"<vector>");
			for (String const & str : strs) {
				Dump(ss, str);
			}
			DumpMemo(ss, L"</vector>");
		});

		std::wstringstream expected;
		std::vector<String> args;

		args.push_back(Q("a"));
		Dump(expected, L"a");

		args.push_back(Q("b"));
		Dump(expected, L"b");

		args.push_back(Q("x"));
		DumpMemo(expected, L"<vector>");{
			args.push_back(Q("x"));
			Dump(expected, L"x");
	
			args.push_back(Q("b"));
			Dump(expected, L"b");

			args.push_back(Q("c"));
			Dump(expected, L"c");

			args.push_back(Q("x"));
			Dump(expected, L"x");
		}DumpMemo(expected, L"</vector>");

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();

		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


	static void TestVectors2 ()
	{
		std::wstringstream ss;

		Opts opts(testConfig);

		opts.AddOption(empty, [&] (String str) {
			Dump(ss, str);
		});
		opts.AddOption(Q("x"), [&] (std::vector<String> && strs) {
			DumpMemo(ss, L"<vector>");
			for (String const & str : strs) {
				Dump(ss, str);
			}
			DumpMemo(ss, L"</vector>");
		});

		std::wstringstream expected;
		std::vector<String> args;

		args.push_back(Q("a"));
		Dump(expected, L"a");

		args.push_back(Q("b"));
		Dump(expected, L"b");

		args.push_back(Q("x"));
		DumpMemo(expected, L"<vector>");
		DumpMemo(expected, L"</vector>");

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();

		if (ss.str() != expected.str()) {
			FAIL;
		}
	}

	
	static void TestConsumeRest ()
	{
		std::wstringstream ss;

		Opts opts(testConfig);

		opts.AddOption(empty, [&] (String str) {
			Dump(ss, str);
		});
		opts.AddOption(Q(";"), lambda_options::ConsumeRest<Char>);

		std::wstringstream expected;
		std::vector<String> args;

		args.push_back(Q("a"));
		Dump(expected, L"a");

		args.push_back(Q(";"));
		args.push_back(Q("b"));
		args.push_back(Q("c"));


		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();

		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


	static void Test_TEMPLATE ()
	{
		std::wstringstream ss;

		Opts opts(testConfig);

		Keyword kw(Q(""));
		opts.AddOption(kw, nop);

		std::wstringstream expected;
		std::vector<String> args;

		args.push_back(Q(""));
		DumpMemo(expected, L"");

		auto parseContext = opts.CreateParseContext(args.begin(), args.end());
		parseContext.Run();

		if (ss.str() != expected.str()) {
			FAIL;
		}
	}


};


template <typename Char>
typename lambda_options::Keyword<Char> const Tests<Char>::empty;


template <typename Char>
typename lambda_options::OptionsConfig const Tests<Char>::testConfig = ([] {
	lambda_options::OptionsConfig config;
	config.keywordStyle = lambda_options::KeywordStyle::Exact;
	return config;
})();


template <typename Char>
static bool RunCharTests ()
{
	typedef void (*TestFunc)();

	TestFunc tests[] = {
		Tests<Char>::TestCompileTypes,
		Tests<Char>::TestCompileArities,
		Tests<Char>::TestRejectEmptyOption,
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
		Tests<Char>::TestMaybeLifetime,
		Tests<Char>::TestHelpDescription,
		Tests<Char>::TestHelpGroups,
		Tests<Char>::TestKeywordStyle,
		Tests<Char>::TestMatchFlags1,
		Tests<Char>::TestMatchFlags2,
		Tests<Char>::TestMatchFlags3,
		Tests<Char>::TestMatchFlags4,
		Tests<Char>::TestVectors1,
		Tests<Char>::TestVectors2,
		Tests<Char>::TestConsumeRest,
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













