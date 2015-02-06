// Copyright (c) 2015, Thomas Eding
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer. 
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
// (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
// LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
// ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
// 
// The views and conclusions contained in the software and documentation are those
// of the authors and should not be interpreted as representing official policies, 
// either expressed or implied, of the FreeBSD Project.


#pragma once

#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>


//////////////////////////////////////////////////////////////////////////


template <typename Char>
class LambdaOpts {
	typedef Char const * CString;
	typedef std::basic_string<Char> String;

public:
	~LambdaOpts ();
	LambdaOpts (int argc, Char const * const argv[]);

	template <typename Func>
	void Add (String option, Func f);

	bool Parse (int & parseFailureIndex);
	String const & Program () const;
	bool Peek (String & outArg) const ;
	bool Next ();

//////////////////////////////////////////////////////////////////////////

private:
	static void ASSERT (bool truth)
	{
#ifndef NDEBUG
		if (!truth) {
			throw std::logic_error("LambdaOpts<Char>::ASSERT failed.");
		}
#endif
	}

//////////////////////////////////////////////////////////////////////////

	template <typename Char>
	static size_t StrLen (Char const * str)
	{
		size_t size = 0;
		while (*str++) {
			++size;
		}
		return size;
	}

	static bool Scan (std::string const & str, char const * format, void * dest)
	{
		char dummy;
		return sscanf(str.c_str(), format, dest, &dummy) == 1;
	}

	static bool Scan (std::wstring const & str, char const * format, void * dest)
	{
		wchar_t wformat[8];
		size_t len = strlen(format) + 1;
		ASSERT(len <= (sizeof(wformat) / sizeof(wchar_t)));
		for (size_t i = 0; i < len; ++i) {
			wformat[i] = format[i];
		}
		wchar_t dummy;
		return swscanf(str.c_str(), wformat, dest, &dummy) == 1;
	}

//////////////////////////////////////////////////////////////////////////

	typedef void const * V;
	typedef std::vector<V> OpaqueArgs;

	enum TypeKind { T_Int, T_Uint, T_Float, T_Double, T_Char, T_CString };

	static TypeKind GetTypeKind (int *) { return T_Int; }
	static TypeKind GetTypeKind (unsigned int *) { return T_Uint; }
	static TypeKind GetTypeKind (float *) { return T_Float; }
	static TypeKind GetTypeKind (double *) { return T_Double; }
	static TypeKind GetTypeKind (Char *) { return T_Char; }
	static TypeKind GetTypeKind (CString *) { return T_CString; }
	static TypeKind GetTypeKind (String *) { return T_CString; }

	template <typename FuncSig>
	struct OptInfo {
		String option;
		std::vector<TypeKind> types;
		std::function<FuncSig> callback;
	};


	// This code is suboptimal compared to struct template specialization, but VC11 is buggy.
	template <typename T>
	static TypeKind GetTypeKind ()
	{
		return GetTypeKind(static_cast<T *>(nullptr));
	}

//////////////////////////////////////////////////////////////////////////

	static void Reify (void const * p, int & out) { out = *static_cast<int const *>(p); }
	static void Reify (void const * p, unsigned int & out) { out = *static_cast<unsigned int const *>(p); }
	static void Reify (void const * p, float & out) { out = *static_cast<float const *>(p); }
	static void Reify (void const * p, double & out) { out = *static_cast<double const *>(p); }
	static void Reify (void const * p, Char & out) { out = *static_cast<Char const *>(p); }
	static void Reify (void const * p, CString & out) { out = static_cast<CString>(p); }
	static void Reify (void const * p, String & out) { out = static_cast<CString>(p); }

	// This code is suboptimal compared to struct template specialization, but VC11 is buggy.
	template <typename T>
	static T Reify (void const * opaque)
	{
		T actual;
		Reify(opaque, actual);
		return actual;
	}

//////////////////////////////////////////////////////////////////////////

	template <typename Func>
	struct FuncTraits : public FuncTraits<decltype(&Func::operator())> {};

	template <typename X, typename R>
	struct FuncTraits<R(X::*)() const> {
		enum { arity = 0 };
		typedef R ReturnType;
	};

	template <typename X, typename R, typename A>
	struct FuncTraits<R(X::*)(A) const> {
		enum { arity = 1 };
		typedef R ReturnType;
		struct Arg0 { typedef A type; };
	};

	template <typename X, typename R, typename A, typename B>
	struct FuncTraits<R(X::*)(A, B) const> {
		enum { arity = 2 };
		typedef R ReturnType;
		struct Arg0 { typedef A type; };
		struct Arg1 { typedef B type; };
	};

	template <typename X, typename R, typename A, typename B, typename C>
	struct FuncTraits<R(X::*)(A, B, C) const> {
		enum { arity = 3 };
		typedef R ReturnType;
		struct Arg0 { typedef A type; };
		struct Arg1 { typedef B type; };
		struct Arg2 { typedef C type; };
	};

	template <typename X, typename R, typename A, typename B, typename C, typename D>
	struct FuncTraits<R(X::*)(A, B, C, D) const> {
		enum { arity = 4 };
		typedef R ReturnType;
		struct Arg0 { typedef A type; };
		struct Arg1 { typedef B type; };
		struct Arg2 { typedef C type; };
		struct Arg3 { typedef D type; };
	};

	template <typename X, typename R, typename A, typename B, typename C, typename D, typename E>
	struct FuncTraits<R(X::*)(A, B, C, D, E) const> {
		enum { arity = 5 };
		typedef R ReturnType;
		struct Arg0 { typedef A type; };
		struct Arg1 { typedef B type; };
		struct Arg2 { typedef C type; };
		struct Arg3 { typedef D type; };
		struct Arg4 { typedef E type; };
	};

//////////////////////////////////////////////////////////////////////////

	template <typename Func, size_t>
	friend struct Adder;

	template <typename Func, size_t>
	struct Adder {};

	template <typename Func>
	struct Adder<Func, 0> {
		static void Add (LambdaOpts<Char> & parser, String option, Func f) {
			parser.AddImpl(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 1> {
		static void Add (LambdaOpts<Char> & parser, String option, Func f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			parser.AddImpl<A>(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 2> {
		static void Add (LambdaOpts<Char> & parser, String option, Func f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			parser.AddImpl<A, B>(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 3> {
		static void Add (LambdaOpts<Char> & parser, String option, Func f) {
			typedef typename FuncTraits<Func>::arg0::type A;
			typedef typename FuncTraits<Func>::arg1::type B;
			typedef typename FuncTraits<Func>::arg2::type C;
			parser.AddImpl<A, B, C>(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 4> {
		static void Add (LambdaOpts<Char> & parser, String option, Func f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			typedef typename FuncTraits<Func>::Arg2::type C;
			typedef typename FuncTraits<Func>::Arg3::type D;
			parser.AddImpl<A, B, C, D>(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 5> {
		static void Add (LambdaOpts<Char> & parser, String option, Func f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			typedef typename FuncTraits<Func>::Arg2::type C;
			typedef typename FuncTraits<Func>::Arg3::type D;
			typedef typename FuncTraits<Func>::Arg4::type E;
			parser.AddImpl<A, B, C, D, E>(option, f);
		}
	};

//////////////////////////////////////////////////////////////////////////

	void AddImpl (String option, std::function<void()> f) {
		OptInfo<void()> info;
		info.option = option;
		info.callback = f;
		infos0.push_back(info);
	}

	template <typename A>
	void AddImpl (String option, std::function<void(A)> f) {
		auto g = [f] (V va) {
			auto a = Reify<A>(va);
			f(a);
		};
		OptInfo<void(V)> info;
		info.option = option;
		info.types.push_back(GetTypeKind<A>());
		info.callback = g;
		infos1.push_back(info);
	}

	template <typename A, typename B>
	void AddImpl (String option, std::function<void(A,B)> f) {
		auto g = [f] (V va, V vb) {
			auto a = Reify<A>(va);
			auto b = Reify<B>(vb);
			f(a, b);
		};
		OptInfo<void(V,V)> info;
		info.option = option;
		info.types.push_back(GetTypeKind<A>());
		info.types.push_back(GetTypeKind<B>());
		info.callback = g;
		infos2.push_back(info);
	}

	template <typename A, typename B, typename C>
	void AddImpl (String option, std::function<void(A,B,C)> f) {
		auto g = [f] (V va, V vb, V vc) {
			auto a = Reify<A>(va);
			auto b = Reify<B>(vb);
			auto c = Reify<C>(vc);
			f(a, b, c);
		};
		OptInfo<void(V,V,V)> info;
		info.option = option;
		info.types.push_back(GetTypeKind<A>());
		info.types.push_back(GetTypeKind<B>());
		info.types.push_back(GetTypeKind<C>());
		info.callback = g;
		infos3.push_back(info);
	}

	template <typename A, typename B, typename C, typename D>
	void AddImpl (String option, std::function<void(A,B,C,D)> f) {
		auto g = [f] (V va, V vb, V vc, V vd) {
			auto a = Reify<A>(va);
			auto b = Reify<B>(vb);
			auto c = Reify<C>(vc);
			auto d = Reify<D>(vd);
			f(a, b, c, d);
		};
		OptInfo<void(V,V,V,V)> info;
		info.option = option;
		info.types.push_back(GetTypeKind<A>());
		info.types.push_back(GetTypeKind<B>());
		info.types.push_back(GetTypeKind<C>());
		info.types.push_back(GetTypeKind<D>());
		info.callback = g;
		infos4.push_back(info);
	}

	template <typename A, typename B, typename C, typename D, typename E>
	void AddImpl (String option, std::function<void(A,B,C,D,E)> f) {
		auto g = [f](V va, V vb, V vc, V vd, V ve) {
			auto a = Reify<A>(va);
			auto b = Reify<B>(vb);
			auto c = Reify<C>(vc);
			auto d = Reify<D>(vd);
			auto e = Reify<E>(ve);
			f(a, b, c, d, e);
		};
		OptInfo<void(V,V,V,V,V)> info;
		info.option = option;
		info.types.push_back(GetTypeKind<A>());
		info.types.push_back(GetTypeKind<B>());
		info.types.push_back(GetTypeKind<C>());
		info.types.push_back(GetTypeKind<D>());
		info.types.push_back(GetTypeKind<E>());
		info.callback = g;
		infos5.push_back(info);
	}

//////////////////////////////////////////////////////////////////////////

	static void Apply (std::function<void()> const & func, OpaqueArgs const & args)
	{
		(void) args;
		func();
	}


	static void Apply (std::function<void(V)> const & func, OpaqueArgs const & args)
	{
		func(args[0]);
	}


	static void Apply (std::function<void(V,V)> const & func, OpaqueArgs const & args)
	{
		func(args[0], args[1]);
	}


	static void Apply (std::function<void(V,V,V)> const & func, OpaqueArgs const & args)
	{
		func(args[0], args[1], args[2]);
	}


	static void Apply (std::function<void(V,V,V,V)> const & func, OpaqueArgs const & args)
	{
		func(args[0], args[1], args[2], args[3]);
	}


	static void Apply (std::function<void(V,V,V,V,V)> const & func, OpaqueArgs const & args)
	{
		func(args[0], args[1], args[2], args[3], args[4]);
	}

//////////////////////////////////////////////////////////////////////////

	size_t RemainingArgs () const
	{
		ASSERT(argPos <= args.size());
		return args.size() - argPos;
	}

	void * Parse_int (String const & arg)
	{
		int x;
		if (Scan(arg, "%d%c", &x)) {
			return Allocate<int>(x);
		}
		return nullptr;
	}

	void * Parse_unsigned_int (String const & arg)
	{
		unsigned int x;
		if (Scan(arg, "%u%c", &x)) {
			return Allocate<unsigned int>(x);
		}
		return nullptr;
	}

	void * Parse_float (String const & arg)
	{
		float x;
		if (Scan(arg, "%f%c", &x)) {
			return Allocate<float>(x);
		}
		return nullptr;
	}

	void * Parse_double (String const & arg)
	{
		double x;
		if (Scan(arg, "%lf%c", &x)) {
			return Allocate<double>(x);
		}
		return nullptr;
	}

	void * Parse_Char (String const & arg)
	{
		Char x;
		if (Scan(arg, "%c%c", &x)) {
			return Allocate<Char>(x);
		}
		return nullptr;
	}

	void * Parse_CString (String const & arg)
	{
		return Allocate_CString(arg.c_str());
	}

	void * Parse (TypeKind type, String const & arg)
	{
		switch (type) {
			case T_Int: return Parse_int(arg);
			case T_Uint: return Parse_unsigned_int(arg);
			case T_Float: return Parse_float(arg);
			case T_Double: return Parse_double(arg);
			case T_Char: return Parse_Char(arg);
			case T_CString: return Parse_CString(arg);
		}
		ASSERT(false);
		return nullptr;
	}

	template <typename GenericOptInfo>
	size_t TryParse (std::vector<GenericOptInfo> const & infos)
	{
		if (infos.empty()) {
			return 0;
		}
		size_t const arity = infos.front().types.size();
		if (RemainingArgs() < arity + 1) {
			return 0;
		}
		for (auto const & info : infos) {
			FreeParseAllocations();
			if (args[argPos] == info.option) {
				OpaqueArgs parsedArgs;
				bool success = true;
				for (size_t i = 0; i < arity; ++i) {
					TypeKind type = info.types[i];
					String const & rawArg = args[argPos + i + 1];
					void * parsedArg = Parse(type, rawArg);
					if (parsedArg == nullptr) {
						success = false;
						break;
					}
					parsedArgs.push_back(parsedArg);
				}
				if (success) {
					Apply(info.callback, parsedArgs);
					return arity + 1;
				}
			}
		}
		return 0;
	}

	bool TryParse ()
	{
		size_t parseCount = 0;
		if (parseCount == 0) {
			parseCount = TryParse(infos5);
		}
		if (parseCount == 0) {
			parseCount = TryParse(infos4);
		}
		if (parseCount == 0) {
			parseCount = TryParse(infos3);
		}
		if (parseCount == 0) {
			parseCount = TryParse(infos2);
		}
		if (parseCount == 0) {
			parseCount = TryParse(infos1);
		}
		if (parseCount == 0) {
			parseCount = TryParse(infos0);
		}
		argPos += parseCount;
		return parseCount > 0;
	}

//////////////////////////////////////////////////////////////////////////

	template <typename T>
	void * Allocate (T value)
	{
		char * p = new char[sizeof(T)];
		memcpy(p, &value, sizeof(T));
		parseAllocations.emplace_back(p);
		return p;
	}

	void * Allocate_CString (CString str)
	{
		size_t size = sizeof(Char) * (StrLen(str) + 1);
		char * p = new char[size];
		memcpy(p, str, size);
		parseAllocations.emplace_back(p);
		return p;
	}

	void FreeParseAllocations ()
	{
		parseAllocations.clear();
	}

//////////////////////////////////////////////////////////////////////////

private:
	String program;
	std::vector<String> args;
	size_t argPos;

	std::vector<std::unique_ptr<char const>> parseAllocations;

	std::vector<OptInfo<void()>> infos0;
	std::vector<OptInfo<void(V)>> infos1;
	std::vector<OptInfo<void(V,V)>> infos2;
	std::vector<OptInfo<void(V,V,V)>> infos3;
	std::vector<OptInfo<void(V,V,V,V)>> infos4;
	std::vector<OptInfo<void(V,V,V,V,V)>> infos5;
};


//////////////////////////////////////////////////////////////////////////


template <typename Char>
LambdaOpts<Char>::~LambdaOpts ()
{
	FreeParseAllocations();
}


template <typename Char>
LambdaOpts<Char>::LambdaOpts (int argc, Char const * const argv[])
	: program(argv[0])
	, argPos(0)
{
	for (int i = 1; i < argc; ++i) {
		args.push_back(argv[i]);
	}
}


template <typename Char>
template <typename Func>
void LambdaOpts<Char>::Add (String option, Func f)
{
	Adder<Func, FuncTraits<Func>::arity>::Add(*this, option, f);
}


template <typename Char>
bool LambdaOpts<Char>::Parse (int & parseFailureIndex)
{
	FreeParseAllocations();
	argPos = 0;
	while (TryParse()) {
		continue;
	}
	if (RemainingArgs() == 0) {
		parseFailureIndex = -1;
		return true;
	}
	parseFailureIndex = static_cast<int>(argPos);
	return false;
}


template <typename Char>
typename LambdaOpts<Char>::String const & LambdaOpts<Char>::Program () const
{
	return program;
}


template <typename Char>
bool LambdaOpts<Char>::Peek (String & outArg) const
{
	if (argPos < args.size()) {
		outArg = args[argPos];
		return true;
	}
	return false;
}


template <typename Char>
bool LambdaOpts<Char>::Next ()
{
	if (argPos < args.size()) {
		++argPos;
		return true;
	}
	return false;
}











