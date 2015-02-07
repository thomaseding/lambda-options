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

#include <cstring>
#include <functional>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#ifndef NDEBUG
#	include <stdexcept>
#endif


//////////////////////////////////////////////////////////////////////////


template <typename Char>
class LambdaOpts {
	typedef Char const * CString;
	typedef std::basic_string<Char> String;
	class ParseEnvImpl;

public:
	class ParseEnv;

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
#ifdef NDEBUG
		(void) truth;
#else
		if (!truth) {
			throw std::logic_error("LambdaOpts<Char>::ASSERT failed.");
		}
#endif
	}

//////////////////////////////////////////////////////////////////////////

	typedef void (*OpaqueDeleter)(void const *);
	typedef std::unique_ptr<void const, OpaqueDeleter> UniqueOpaque;
	typedef std::vector<UniqueOpaque> OpaqueArgs;
	typedef void const * V;

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
		static void Add (LambdaOpts & opts, String option, Func f) {
			opts.AddImpl(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 1> {
		static void Add (LambdaOpts & opts, String option, Func f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			opts.AddImpl<A>(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 2> {
		static void Add (LambdaOpts & opts, String option, Func f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			opts.AddImpl<A,B>(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 3> {
		static void Add (LambdaOpts & opts, String option, Func f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			typedef typename FuncTraits<Func>::Arg2::type C;
			opts.AddImpl<A,B,C>(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 4> {
		static void Add (LambdaOpts & opts, String option, Func f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			typedef typename FuncTraits<Func>::Arg2::type C;
			typedef typename FuncTraits<Func>::Arg3::type D;
			opts.AddImpl<A,B,C,D>(option, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 5> {
		static void Add (LambdaOpts & opts, String option, Func f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			typedef typename FuncTraits<Func>::Arg2::type C;
			typedef typename FuncTraits<Func>::Arg3::type D;
			typedef typename FuncTraits<Func>::Arg4::type E;
			opts.AddImpl<A,B,C,D,E>(option, f);
		}
	};


//////////////////////////////////////////////////////////////////////////


	void AddImpl (String option, std::function<ParseResult()> func)
	{
		OptInfo<ParseResult()> info;
		info.option = option;
		info.callback = func;
		infos0.push_back(info);
	}

	template <typename A>
	void AddImpl (String option, std::function<ParseResult(A)> func)
	{
		auto wrapper = [=] (V va) {
			auto const & a = TypeTag<A>::ReifyOpaque(va);
			return func(a);
		};
		OptInfo<ParseResult(V)> info;
		info.option = option;
		info.types.push_back(TypeTag<A>::Kind);
		info.callback = wrapper;
		infos1.push_back(info);
	}

	template <typename A, typename B>
	void AddImpl (String option, std::function<ParseResult(A,B)> func)
	{
		auto wrapper = [=] (V va, V vb) {
			auto const & a = TypeTag<A>::ReifyOpaque(va);
			auto const & b = TypeTag<B>::ReifyOpaque(vb);
			return func(a, b);
		};
		OptInfo<ParseResult(V,V)> info;
		info.option = option;
		info.types.push_back(TypeTag<A>::Kind);
		info.types.push_back(TypeTag<B>::Kind);
		info.callback = wrapper;
		infos2.push_back(info);
	}

	template <typename A, typename B, typename C>
	void AddImpl (String option, std::function<ParseResult(A,B,C)> func)
	{
		auto wrapper = [=] (V va, V vb, V vc) {
			auto const & a = TypeTag<A>::ReifyOpaque(va);
			auto const & b = TypeTag<B>::ReifyOpaque(vb);
			auto const & c = TypeTag<C>::ReifyOpaque(vc);
			return func(a, b, c);
		};
		OptInfo<ParseResult(V,V,V)> info;
		info.option = option;
		info.types.push_back(TypeTag<A>::Kind);
		info.types.push_back(TypeTag<B>::Kind);
		info.types.push_back(TypeTag<C>::Kind);
		info.callback = wrapper;
		infos3.push_back(info);
	}

	template <typename A, typename B, typename C, typename D>
	void AddImpl (String option, std::function<ParseResult(A,B,C,D)> func)
	{
		auto wrapper = [=] (V va, V vb, V vc, V vd) {
			auto const & a = TypeTag<A>::ReifyOpaque(va);
			auto const & b = TypeTag<B>::ReifyOpaque(vb);
			auto const & c = TypeTag<C>::ReifyOpaque(vc);
			auto const & d = TypeTag<D>::ReifyOpaque(vd);
			return func(a, b, c, d);
		};
		OptInfo<ParseResult(V,V,V,V)> info;
		info.option = option;
		info.types.push_back(TypeTag<A>::Kind);
		info.types.push_back(TypeTag<B>::Kind);
		info.types.push_back(TypeTag<C>::Kind);
		info.types.push_back(TypeTag<D>::Kind);
		info.callback = wrapper;
		infos4.push_back(info);
	}

	template <typename A, typename B, typename C, typename D, typename E>
	void AddImpl (String option, std::function<ParseResult(A,B,C,D,E)> func)
	{
		auto wrapper = [=] (V va, V vb, V vc, V vd, V ve) {
			auto a = TypeTag<A>::ReifyOpaque(va);
			auto b = TypeTag<B>::ReifyOpaque(vb);
			auto c = TypeTag<C>::ReifyOpaque(vc);
			auto d = TypeTag<D>::ReifyOpaque(vd);
			auto e = TypeTag<E>::ReifyOpaque(ve);
			return func(a, b, c, d, e);
		};
		OptInfo<ParseResult(V,V,V,V,V)> info;
		info.option = option;
		info.types.push_back(TypeTag<A>::Kind);
		info.types.push_back(TypeTag<B>::Kind);
		info.types.push_back(TypeTag<C>::Kind);
		info.types.push_back(TypeTag<D>::Kind);
		info.types.push_back(TypeTag<E>::Kind);
		info.callback = wrapper;
		infos5.push_back(info);
	}

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
		size_t len = std::strlen(format) + 1;
		ASSERT(len <= (sizeof(wformat) / sizeof(wchar_t)));
		for (size_t i = 0; i < len; ++i) {
			wformat[i] = format[i];
		}
		wchar_t dummy;
		return swscanf(str.c_str(), wformat, dest, &dummy) == 1;
	}

//////////////////////////////////////////////////////////////////////////

	template <typename T>
	static std::unique_ptr<T> AllocateCopy (T const & source)
	{
		return std::unique_ptr<T>(new T(source));
	}

	typedef size_t TypeKind;

	template <typename T>
	struct TypeTagBase {
		typedef T Type;

		static char const * const ScanDescription ();

		static Type const & ReifyOpaque (void const * p) {
			return *static_cast<Type const *>(p);
		}

		static void Delete (void const * p) {
			delete static_cast<T const *>(p);
		}

		static std::unique_ptr<T const> Parse (std::string const & str) {
			T item;
			if (Scan(str, TypeTag<T>::ScanDescription(), &item)) {
				return AllocateCopy(item);
			}
			return nullptr;
		}
	};

	template <typename T, typename Z=void>
	struct TypeTag {};

	template <typename Z>
	struct TypeTag<int, Z> : public TypeTagBase<int> {
	public:
		enum : TypeKind { Kind = __LINE__ };
		static char const * const ScanDescription () { return "%d%c"; }
	};

	template <typename Z>
	struct TypeTag<unsigned int, Z> : public TypeTagBase<unsigned int> {
	public:
		enum : TypeKind { Kind = __LINE__ };
		static char const * const ScanDescription () { return "%u%c"; }
	};

	template <typename Z>
	struct TypeTag<float, Z> : public TypeTagBase<float> {
	public:
		enum : TypeKind { Kind = __LINE__ };
		static char const * const ScanDescription () { return "%f%c"; }
	};

	template <typename Z>
	struct TypeTag<double, Z> : public TypeTagBase<double> {
	public:
		enum : TypeKind { Kind = __LINE__ };
		static char const * const ScanDescription () { return "%lf%c"; }
	};

	template <typename Z>
	struct TypeTag<Char, Z> : public TypeTagBase<Char> {
	public:
		enum : TypeKind { Kind = __LINE__ };
		static char const * const ScanDescription () { return "%cf%c"; }
	};

	template <typename Z>
	struct TypeTag<String, Z> : public TypeTagBase<String> {
	public:
		enum : TypeKind { Kind = __LINE__ };
		static std::unique_ptr<std::string const> Parse (std::string const & str) {
			return AllocateCopy(str);
		}
	};

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

		UniqueOpaque OpaqueParse (TypeKind type, String const & arg);

		template <typename GenericOptInfo>
		int TryParse (std::vector<GenericOptInfo> const & infos);

		bool TryParse ();

		template <typename T>
		void * Allocate (T value);

		void * Allocate_String (String const & str);
		void FreeParseAllocations ();

	public:
		LambdaOpts const & opts;
		std::vector<UniqueOpaque> parseAllocations;
		std::vector<String> args;
		size_t argIndex;
	};

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
	return func(args[0].get());
}


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult(V,V)> const & func, OpaqueArgs const & args)
{
	return func(args[0].get(), args[1].get());
}


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult(V,V,V)> const & func, OpaqueArgs const & args)
{
	return func(args[0].get(), args[1].get(), args[2].get());
}


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult(V,V,V,V)> const & func, OpaqueArgs const & args)
{
	return func(args[0].get(), args[1].get(), args[2].get(), args[3].get());
}


template <typename Char>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::Apply (std::function<ParseResult(V,V,V,V,V)> const & func, OpaqueArgs const & args)
{
	return func(args[0].get(), args[1].get(), args[2].get(), args[3].get(), args[4].get());
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
typename LambdaOpts<Char>::UniqueOpaque LambdaOpts<Char>::ParseEnvImpl::OpaqueParse (TypeKind type, String const & arg)
{
	switch (type) {
		case TypeTag<int>::Kind:			return UniqueOpaque(TypeTag<int>::Parse(arg).release(),				TypeTag<int>::Delete);
		case TypeTag<unsigned int>::Kind:	return UniqueOpaque(TypeTag<unsigned int>::Parse(arg).release(),	TypeTag<unsigned int>::Delete);
		case TypeTag<float>::Kind:			return UniqueOpaque(TypeTag<float>::Parse(arg).release(),			TypeTag<float>::Delete);
		case TypeTag<double>::Kind:			return UniqueOpaque(TypeTag<double>::Parse(arg).release(),			TypeTag<double>::Delete);
		case TypeTag<Char>::Kind:			return UniqueOpaque(TypeTag<Char>::Parse(arg).release(),			TypeTag<Char>::Delete);
		case TypeTag<String>::Kind:			return UniqueOpaque(TypeTag<String>::Parse(arg).release(),			TypeTag<String>::Delete);
	}
	ASSERT(false);
	return UniqueOpaque(static_cast<char *>(nullptr), [](void const *){});
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
				UniqueOpaque parsedArg = OpaqueParse(type, rawArg);
				if (parsedArg == nullptr) {
					success = false;
					break;
				}
				parsedArgs.emplace_back(std::move(parsedArg));
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
	int parseCount = 0;

	if (parseCount == 0) {
		parseCount = TryParse(opts.infos5);
	}
	if (parseCount == 0) {
		parseCount = TryParse(opts.infos4);
	}
	if (parseCount == 0) {
		parseCount = TryParse(opts.infos3);
	}
	if (parseCount == 0) {
		parseCount = TryParse(opts.infos2);
	}
	if (parseCount == 0) {
		parseCount = TryParse(opts.infos1);
	}
	if (parseCount == 0) {
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








