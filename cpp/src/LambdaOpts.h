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
#include <utility>
#include <vector>


//////////////////////////////////////////////////////////////////////////


template <typename Char>
class LambdaOpts {
	typedef Char const * CString;
	typedef std::basic_string<Char> String;
	class ParseEnv;
	class ParseEnvImpl;

public:
	enum class ParseResult {
		Accept,
		Reject,
		Fatal,
	};

	template <typename Func>
	void Add (String option, Func f);

	ParseEnv NewParseEnv (std::vector<String> args);

	class ParseEnv {
		friend class LambdaOpts;

	public:
		ParseEnv (ParseEnv && other);
		ParseEnv & operator= (ParseEnv && other);

		bool Parse (int & outParseFailureIndex);

		template <typename T>
		bool Peek (T & outArg);

		bool Next ();

	private:
		ParseEnv (LambdaOpts const & opts, std::vector<String> && args);
		ParseEnv (ParseEnv const & other);       // disable
		void operator= (ParseEnv const & other); // disable

	private:
		std::unique_ptr<ParseEnvImpl> impl;
	};

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

	typedef void const * V;

	typedef std::vector<V> OpaqueArgs;

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
		static void Add (LambdaOpts & opts, String option, Func f);
	};

	template <typename Func>
	struct Adder<Func, 1> {
		static void Add (LambdaOpts & opts, String option, Func f);
	};

	template <typename Func>
	struct Adder<Func, 2> {
		static void Add (LambdaOpts & opts, String option, Func f);
	};

	template <typename Func>
	struct Adder<Func, 3> {
		static void Add (LambdaOpts & opts, String option, Func f);
	};

	template <typename Func>
	struct Adder<Func, 4> {
		static void Add (LambdaOpts & opts, String option, Func f);
	};

	template <typename Func>
	struct Adder<Func, 5> {
		static void Add (LambdaOpts & opts, String option, Func f);
	};

//////////////////////////////////////////////////////////////////////////

	void AddImpl (String option, std::function<ParseResult()> f);

	template <typename A>
	void AddImpl (String option, std::function<ParseResult(A)> f);

	template <typename A, typename B>
	void AddImpl (String option, std::function<ParseResult(A,B)> f);

	template <typename A, typename B, typename C>
	void AddImpl (String option, std::function<ParseResult(A,B,C)> f);

	template <typename A, typename B, typename C, typename D>
	void AddImpl (String option, std::function<ParseResult(A,B,C,D)> f);

	template <typename A, typename B, typename C, typename D, typename E>
	void AddImpl (String option, std::function<ParseResult(A,B,C,D,E)> f);

//////////////////////////////////////////////////////////////////////////

	static ParseResult Apply (std::function<ParseResult()> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V)> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V,V)> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V,V,V)> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V,V,V,V)> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V,V,V,V,V)> const & func, OpaqueArgs const & args);

//////////////////////////////////////////////////////////////////////////

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

	// TODO: TypeTag base class
	enum TypeKind { T_Int, T_Uint, T_Float, T_Double, T_Char, T_String };

	static TypeKind GetTypeKind (int *) { return T_Int; }
	static TypeKind GetTypeKind (unsigned int *) { return T_Uint; }
	static TypeKind GetTypeKind (float *) { return T_Float; }
	static TypeKind GetTypeKind (double *) { return T_Double; }
	static TypeKind GetTypeKind (Char *) { return T_Char; }
	static TypeKind GetTypeKind (String *) { return T_String; }

	// TODO: Can I now use template specialization instead of this?
	template <typename T>
	static TypeKind GetTypeKind ()
	{
		return GetTypeKind(static_cast<T *>(nullptr));
	}

	template <typename FuncSig>
	struct OptInfo {
		String option;
		std::vector<TypeKind> types;
		std::function<FuncSig> callback;
	};

//////////////////////////////////////////////////////////////////////////

	class ParseEnvImpl {
	public:
		ParseEnvImpl (LambdaOpts const & opts, std::vector<String> && args);

		bool Parse (int & outParseFailureIndex);

		template <typename T>
		bool Peek (T & outArg);

		bool Next ();

		size_t RemainingArgs () const;

		void * Parse_int (String const & arg);
		void * Parse_unsigned_int (String const & arg);
		void * Parse_float (String const & arg);
		void * Parse_double (String const & arg);
		void * Parse_Char (String const & arg);
		void * Parse_String (String const & arg);
		void * Parse (TypeKind type, String const & arg);

		template <typename GenericOptInfo>
		int TryParse (std::vector<GenericOptInfo> const & infos);

		bool TryParse ();

		template <typename T>
		void * Allocate (T value);

		void * Allocate_String (String const & str);
		void FreeParseAllocations ();

	public:
		LambdaOpts const & opts;
		std::vector<std::unique_ptr<char const>> parseAllocations;
		std::vector<String> args;
		size_t argIndex;
	};

//////////////////////////////////////////////////////////////////////////

	static void Reify (void const * opaque, int & out);
	static void Reify (void const * opaque, unsigned int & out);
	static void Reify (void const * opaque, float & out);
	static void Reify (void const * opaque, double & out);
	static void Reify (void const * opaque, Char & out);
	static void Reify (void const * opaque, String & out);

	// TODO: Can I now use template specialization instead of this?
	template <typename T>
	static T Reify (void const * opaque);

//////////////////////////////////////////////////////////////////////////

private:
	std::vector<OptInfo<ParseResult()>> infos0;
	std::vector<OptInfo<ParseResult(V)>> infos1;
	std::vector<OptInfo<ParseResult(V,V)>> infos2;
	std::vector<OptInfo<ParseResult(V,V,V)>> infos3;
	std::vector<OptInfo<ParseResult(V,V,V,V)>> infos4;
	std::vector<OptInfo<ParseResult(V,V,V,V,V)>> infos5;
};


//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////


template <typename Char>
template <typename Func>
void LambdaOpts<Char>::Adder<Func, 0>::Add (LambdaOpts & opts, String option, Func f)
{
	opts.AddImpl(option, f);
}


template <typename Char>
template <typename Func>
void LambdaOpts<Char>::Adder<Func, 1>::Add (LambdaOpts & opts, String option, Func f)
{
	typedef typename FuncTraits<Func>::Arg0::type A;
	opts.AddImpl<A>(option, f);
}


template <typename Char>
template <typename Func>
void LambdaOpts<Char>::Adder<Func, 2>::Add (LambdaOpts & opts, String option, Func f)
{
	typedef typename FuncTraits<Func>::Arg0::type A;
	typedef typename FuncTraits<Func>::Arg1::type B;
	opts.AddImpl<A,B>(option, f);
}


template <typename Char>
template <typename Func>
void LambdaOpts<Char>::Adder<Func, 3>::Add (LambdaOpts & opts, String option, Func f)
{
	typedef typename FuncTraits<Func>::Arg0::type A;
	typedef typename FuncTraits<Func>::Arg1::type B;
	typedef typename FuncTraits<Func>::Arg2::type C;
	opts.AddImpl<A,B,C>(option, f);
}


template <typename Char>
template <typename Func>
void LambdaOpts<Char>::Adder<Func, 4>::Add (LambdaOpts & opts, String option, Func f)
{
	typedef typename FuncTraits<Func>::Arg0::type A;
	typedef typename FuncTraits<Func>::Arg1::type B;
	typedef typename FuncTraits<Func>::Arg2::type C;
	typedef typename FuncTraits<Func>::Arg3::type D;
	opts.AddImpl<A,B,C,D>(option, f);
}


template <typename Char>
template <typename Func>
void LambdaOpts<Char>::Adder<Func, 5>::Add (LambdaOpts & opts, String option, Func f)
{
	typedef typename FuncTraits<Func>::Arg0::type A;
	typedef typename FuncTraits<Func>::Arg1::type B;
	typedef typename FuncTraits<Func>::Arg2::type C;
	typedef typename FuncTraits<Func>::Arg3::type D;
	typedef typename FuncTraits<Func>::Arg4::type E;
	opts.AddImpl<A,B,C,D,E>(option, f);
}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
void LambdaOpts<Char>::AddImpl (String option, std::function<ParseResult()> func)
{
	OptInfo<ParseResult()> info;
	info.option = option;
	info.callback = func;
	infos0.push_back(info);
}


template <typename Char>
template <typename A>
void LambdaOpts<Char>::AddImpl (String option, std::function<ParseResult(A)> func)
{
	auto wrapper = [=] (V va) {
		auto a = Reify<A>(va);
		return func(a);
	};
	OptInfo<ParseResult(V)> info;
	info.option = option;
	info.types.push_back(GetTypeKind<A>());
	info.callback = wrapper;
	infos1.push_back(info);
}


template <typename Char>
template <typename A, typename B>
void LambdaOpts<Char>::AddImpl (String option, std::function<ParseResult(A,B)> func)
{
	auto wrapper = [=] (V va, V vb) {
		auto a = Reify<A>(va);
		auto b = Reify<B>(vb);
		return func(a, b);
	};
	OptInfo<ParseResult(V,V)> info;
	info.option = option;
	info.types.push_back(GetTypeKind<A>());
	info.types.push_back(GetTypeKind<B>());
	info.callback = wrapper;
	infos2.push_back(info);
}


template <typename Char>
template <typename A, typename B, typename C>
void LambdaOpts<Char>::AddImpl (String option, std::function<ParseResult(A,B,C)> func)
{
	auto wrapper = [=] (V va, V vb, V vc) {
		auto a = Reify<A>(va);
		auto b = Reify<B>(vb);
		auto c = Reify<C>(vc);
		return func(a, b, c);
	};
	OptInfo<ParseResult(V,V,V)> info;
	info.option = option;
	info.types.push_back(GetTypeKind<A>());
	info.types.push_back(GetTypeKind<B>());
	info.types.push_back(GetTypeKind<C>());
	info.callback = wrapper;
	infos3.push_back(info);
}


template <typename Char>
template <typename A, typename B, typename C, typename D>
void LambdaOpts<Char>::AddImpl (String option, std::function<ParseResult(A,B,C,D)> func)
{
	auto wrapper = [=] (V va, V vb, V vc, V vd) {
		auto a = Reify<A>(va);
		auto b = Reify<B>(vb);
		auto c = Reify<C>(vc);
		auto d = Reify<D>(vd);
		return func(a, b, c, d);
	};
	OptInfo<ParseResult(V,V,V,V)> info;
	info.option = option;
	info.types.push_back(GetTypeKind<A>());
	info.types.push_back(GetTypeKind<B>());
	info.types.push_back(GetTypeKind<C>());
	info.types.push_back(GetTypeKind<D>());
	info.callback = wrapper;
	infos4.push_back(info);
}


template <typename Char>
template <typename A, typename B, typename C, typename D, typename E>
void LambdaOpts<Char>::AddImpl (String option, std::function<ParseResult(A,B,C,D,E)> func)
{
	auto wrapper = [=] (V va, V vb, V vc, V vd, V ve) {
		auto a = Reify<A>(va);
		auto b = Reify<B>(vb);
		auto c = Reify<C>(vc);
		auto d = Reify<D>(vd);
		auto e = Reify<E>(ve);
		return func(a, b, c, d, e);
	};
	OptInfo<ParseResult(V,V,V,V,V)> info;
	info.option = option;
	info.types.push_back(GetTypeKind<A>());
	info.types.push_back(GetTypeKind<B>());
	info.types.push_back(GetTypeKind<C>());
	info.types.push_back(GetTypeKind<D>());
	info.types.push_back(GetTypeKind<E>());
	info.callback = wrapper;
	infos5.push_back(info);
}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult()> const & func, OpaqueArgs const & args)
{
	(void) args;
	return func();
}


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult(V)> const & func, OpaqueArgs const & args)
{
	return func(args[0]);
}


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult(V,V)> const & func, OpaqueArgs const & args)
{
	return func(args[0], args[1]);
}


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult(V,V,V)> const & func, OpaqueArgs const & args)
{
	return func(args[0], args[1], args[2]);
}


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult(V,V,V,V)> const & func, OpaqueArgs const & args)
{
	return func(args[0], args[1], args[2], args[3]);
}


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult(V,V,V,V,V)> const & func, OpaqueArgs const & args)
{
	return func(args[0], args[1], args[2], args[3], args[4]);
}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
void LambdaOpts<Char>::Reify (void const * opaque, int & out)
{
	out = *static_cast<int const *>(opaque);
}


template <typename Char>
void LambdaOpts<Char>::Reify (void const * opaque, unsigned int & out)
{
	out = *static_cast<unsigned int const *>(opaque);
}


template <typename Char>
void LambdaOpts<Char>::Reify (void const * opaque, float & out)
{
	out = *static_cast<float const *>(opaque);
}


template <typename Char>
void LambdaOpts<Char>::Reify (void const * opaque, double & out)
{
	out = *static_cast<double const *>(opaque);
}


template <typename Char>
void LambdaOpts<Char>::Reify (void const * opaque, Char & out)
{
	out = *static_cast<Char const *>(opaque);
}


template <typename Char>
void LambdaOpts<Char>::Reify (void const * opaque, String & out)
{
	out = static_cast<CString>(opaque);
}


template <typename Char>
template <typename T>
static T LambdaOpts<Char>::Reify (void const * opaque)
{
	T actual;
	Reify(opaque, actual);
	return actual;
}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
LambdaOpts<Char>::ParseEnvImpl::ParseEnvImpl (LambdaOpts const & opts, std::vector<String> && args)
	: opts(opts)
	, parseAllocations()
	, args(std::move(args))
	, argIndex(0)
{}


template <typename Char>
void * LambdaOpts<Char>::ParseEnvImpl::Parse_int (String const & arg)
{
	int x;
	if (Scan(arg, "%d%c", &x)) {
		return Allocate<int>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::ParseEnvImpl::Parse_unsigned_int (String const & arg)
{
	unsigned int x;
	if (Scan(arg, "%u%c", &x)) {
		return Allocate<unsigned int>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::ParseEnvImpl::Parse_float (String const & arg)
{
	float x;
	if (Scan(arg, "%f%c", &x)) {
		return Allocate<float>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::ParseEnvImpl::Parse_double (String const & arg)
{
	double x;
	if (Scan(arg, "%lf%c", &x)) {
		return Allocate<double>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::ParseEnvImpl::Parse_Char (String const & arg)
{
	Char x;
	if (Scan(arg, "%c%c", &x)) {
		return Allocate<Char>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::ParseEnvImpl::Parse_String (String const & arg)
{
	return Allocate_String(arg);
}


template <typename Char>
void * LambdaOpts<Char>::ParseEnvImpl::Parse (TypeKind type, String const & arg)
{
	switch (type) {
		case T_Int: return Parse_int(arg);
		case T_Uint: return Parse_unsigned_int(arg);
		case T_Float: return Parse_float(arg);
		case T_Double: return Parse_double(arg);
		case T_Char: return Parse_Char(arg);
		case T_String: return Parse_String(arg);
	}
	ASSERT(false);
	return nullptr;
}


template <typename Char>
template <typename GenericOptInfo>
int LambdaOpts<Char>::ParseEnvImpl::TryParse (std::vector<GenericOptInfo> const & infos)
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
		if (args[argIndex] == info.option) {
			OpaqueArgs parsedArgs;
			bool success = true;
			for (size_t i = 0; i < arity; ++i) {
				TypeKind type = info.types[i];
				String const & rawArg = args[argIndex + i + 1];
				void * parsedArg = Parse(type, rawArg);
				if (parsedArg == nullptr) {
					success = false;
					break;
				}
				parsedArgs.push_back(parsedArg);
			}
			if (success) {
				ParseResult res = Apply(info.callback, parsedArgs);
				switch (res) {
					case ParseResult::Accept: {
						return static_cast<int>(arity + 1);
					} break;
					case ParseResult::Reject: {
						continue;
					} break;
					case ParseResult::Fatal: {
						return -1;
					} break;
					default: {
						ASSERT(false);
					}
				}
			}
		}
	}
	return 0;
}


template <typename Char>
bool LambdaOpts<Char>::ParseEnvImpl::TryParse ()
{
	size_t parseCount = 0;

	if (parseCount >= 0) {
		parseCount = TryParse(opts.infos5);
	}
	if (parseCount >= 0) {
		parseCount = TryParse(opts.infos4);
	}
	if (parseCount >= 0) {
		parseCount = TryParse(opts.infos3);
	}
	if (parseCount >= 0) {
		parseCount = TryParse(opts.infos2);
	}
	if (parseCount >= 0) {
		parseCount = TryParse(opts.infos1);
	}
	if (parseCount >= 0) {
		parseCount = TryParse(opts.infos0);
	}

	if (parseCount <= 0) {
		return false;
	}

	argIndex += parseCount;
	return true;
}


template <typename Char>
bool LambdaOpts<Char>::ParseEnvImpl::Parse (int & outParseFailureIndex)
{
	FreeParseAllocations();
	argIndex = 0;
	while (TryParse()) {
		continue;
	}
	if (RemainingArgs() > 0) {
		outParseFailureIndex = static_cast<int>(argIndex);
		return false;
	}
	outParseFailureIndex = -1;
	return true;
}


template <typename Char>
size_t LambdaOpts<Char>::ParseEnvImpl::RemainingArgs () const
{
	ASSERT(argIndex <= args.size());
	return args.size() - argIndex;
}


template <typename Char>
template <typename T>
bool LambdaOpts<Char>::ParseEnvImpl::Peek (T & outArg)
{
	if (argIndex < args.size()) {
		FreeParseAllocations();
		TypeKind const kind = GetTypeKind(outArg);
		String const & arg = args[argIndex];
		void const * p = Parse(kind, arg);
		if (p != nullptr) {
			Reify(p, outArg);
			return true;
		}
	}
	return false;
}


template <typename Char>
bool LambdaOpts<Char>::ParseEnvImpl::Next ()
{
	if (argIndex < args.size()) {
		++argIndex;
		return true;
	}
	return false;
}


template <typename Char>
template <typename T>
void * LambdaOpts<Char>::ParseEnvImpl::Allocate (T value)
{
	char * p = new char[sizeof(T)];
	memcpy(p, &value, sizeof(T));
	parseAllocations.emplace_back(p);
	return p;
}


template <typename Char>
void * LambdaOpts<Char>::ParseEnvImpl::Allocate_String (String const & str)
{
	size_t size = sizeof(Char) * (str.size() + 1);
	char * p = new char[size];
	memcpy(p, str.c_str(), size);
	parseAllocations.emplace_back(p);
	return p;
}


template <typename Char>
void LambdaOpts<Char>::ParseEnvImpl::FreeParseAllocations ()
{
	parseAllocations.clear();
}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
LambdaOpts<Char>::ParseEnv::ParseEnv (LambdaOpts const & opts, std::vector<String> && args)
	: impl(new ParseEnvImpl(opts, std::move(args)))
{}


template <typename Char>
LambdaOpts<Char>::ParseEnv::ParseEnv (ParseEnv && other)
	: impl(std::move(other.impl))
{}


template <typename Char>
typename LambdaOpts<Char>::ParseEnv & LambdaOpts<Char>::ParseEnv::operator= (ParseEnv && other)
{
	impl = std::move(other.impl);
}


template <typename Char>
bool LambdaOpts<Char>::ParseEnv::Parse (int & outParseFailureIndex)
{
	return impl->Parse(outParseFailureIndex);
}


template <typename Char>
template <typename T>
bool LambdaOpts<Char>::ParseEnv::Peek (T & outArg)
{
	return impl->Peek(outArg);
}


template <typename Char>
bool LambdaOpts<Char>::ParseEnv::Next ()
{
	return impl->Next();
}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
template <typename Func>
void LambdaOpts<Char>::Add (String option, Func f)
{
	Adder<Func, FuncTraits<Func>::arity>::Add(*this, option, f);
}


template <typename Char>
typename LambdaOpts<Char>::ParseEnv LambdaOpts<Char>::NewParseEnv (std::vector<String> args)
{
	return ParseEnv(*this, std::move(args));
}








