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

#include <array>
#include <cctype>
#include <cstdio>
#include <exception>
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
	typedef std::basic_string<Char> String;
	typedef std::vector<String> Args;
	class ParseEnvImpl;

public:
	class Exception : std::exception {
	public:
		Exception (std::string const & message)
			: message(message)
		{}
		virtual char const * what () const throw() override {
			return message.c_str();
		}
	private:
		std::string message;
	};

	class ParseEnv;

	enum class ParseResult {
		Accept,
		Reject,
		Fatal,
	};

	template <typename Func>
	void AddOption (String const & keyword, Func const & f);

	template <typename StringIter>
	ParseEnv CreateParseEnv (StringIter begin, StringIter end);

	class ParseEnv {
		friend class LambdaOpts;

	public:
		ParseEnv (ParseEnv && other);
		ParseEnv & operator= (ParseEnv && other);

		bool Run (int & outParseFailureIndex);

		template <typename T>
		bool Peek (T & outArg);

		bool Next ();

	private:
		ParseEnv (LambdaOpts const & opts, Args && args);
		ParseEnv (ParseEnv const & other);       // disable
		void operator= (ParseEnv const & other); // disable

	private:
		std::unique_ptr<ParseEnvImpl> impl;
	};

//////////////////////////////////////////////////////////////////////////

private:
	static void ASSERT (unsigned int line, bool truth)
	{
#ifdef NDEBUG
		(void) truth;
#else
		if (!truth) {
			char msg[1024];
			sprintf(msg, "LambdaOpts::ASSERT failed in '%s' on line %u", __FILE__, line);
			throw std::logic_error(msg);
		}
#endif
	}

//////////////////////////////////////////////////////////////////////////

	template <typename T>
	struct Tag {
		typedef T type;
	};

//////////////////////////////////////////////////////////////////////////

	typedef typename Args::const_iterator ArgsIter;

	typedef void * V;
	typedef void (*OpaqueDeleter)(void *);
	typedef std::unique_ptr<void, OpaqueDeleter> UniqueOpaque;
	typedef std::vector<UniqueOpaque> OpaqueArgs;

	class TypeKind;

	typedef UniqueOpaque (*OpaqueParser)(ArgsIter &, ArgsIter);
	typedef std::vector<std::pair<TypeKind, OpaqueParser>> DynamicParsers;

//////////////////////////////////////////////////////////////////////////

	class TypeKind {
	public:
		TypeKind (Tag<int>) { InitSimple(__LINE__); }
		TypeKind (Tag<unsigned int>) { InitSimple(__LINE__); }
		TypeKind (Tag<float>) { InitSimple(__LINE__); }
		TypeKind (Tag<double>) { InitSimple(__LINE__); }
		TypeKind (Tag<Char>) { InitSimple(__LINE__); }
		TypeKind (Tag<String>) { InitSimple(__LINE__); }

		template <typename T, size_t N>
		TypeKind (Tag<std::array<T, N>>)
			: typeId(__LINE__)
			, arrayCount(N)
			, arrayElem(new TypeKind(Tag<T>()))
		{}

		bool operator== (TypeKind const & other) const
		{
			if (!arrayElem && other.arrayElem) {
				return false;
			}
			if (arrayElem) {
				if (!other.arrayElem) {
					return false;
				}
				if (arrayCount != other.arrayCount) {
					return false;
				}
				if (!(*arrayElem == *other.arrayElem)) {
					return false;
				}
			}
			return typeId == other.typeId;
		}

	private:
		void InitSimple (size_t typeId)
		{
			this->typeId = typeId;
			arrayCount = 0;
		}

	private:
		size_t typeId;
		size_t arrayCount;
		std::unique_ptr<TypeKind> arrayElem;
	};

///////////////////////////////////////////////////////////////////////////

	template <typename Func>
	struct FuncTraits : public FuncTraits<decltype(&Func::operator())> {};

	template <typename X, typename R>
	struct FuncTraits<R(X::*)() const> {
		enum { arity = 0 };
		struct Return { typedef R type; };
	};

	template <typename X, typename R, typename A>
	struct FuncTraits<R(X::*)(A) const> {
		enum { arity = 1 };
		struct Return { typedef R type; };
		struct Arg0 { typedef A type; };
	};

	template <typename X, typename R, typename A, typename B>
	struct FuncTraits<R(X::*)(A, B) const> {
		enum { arity = 2 };
		struct Return { typedef R type; };
		struct Arg0 { typedef A type; };
		struct Arg1 { typedef B type; };
	};

	template <typename X, typename R, typename A, typename B, typename C>
	struct FuncTraits<R(X::*)(A, B, C) const> {
		enum { arity = 3 };
		struct Return { typedef R type; };
		struct Arg0 { typedef A type; };
		struct Arg1 { typedef B type; };
		struct Arg2 { typedef C type; };
	};

	template <typename X, typename R, typename A, typename B, typename C, typename D>
	struct FuncTraits<R(X::*)(A, B, C, D) const> {
		enum { arity = 4 };
		struct Return { typedef R type; };
		struct Arg0 { typedef A type; };
		struct Arg1 { typedef B type; };
		struct Arg2 { typedef C type; };
		struct Arg3 { typedef D type; };
	};

	template <typename X, typename R, typename A, typename B, typename C, typename D, typename E>
	struct FuncTraits<R(X::*)(A, B, C, D, E) const> {
		enum { arity = 5 };
		struct Return { typedef R type; };
		struct Arg0 { typedef A type; };
		struct Arg1 { typedef B type; };
		struct Arg2 { typedef C type; };
		struct Arg3 { typedef D type; };
		struct Arg4 { typedef E type; };
	};

//////////////////////////////////////////////////////////////////////////

	template <typename T, typename Dummy=void>
	struct ReturnType {
		static bool const allowed = false;
	};

	template <typename Dummy>
	struct ReturnType<void, Dummy> {
		static bool const allowed = true;
	};

	template <typename Dummy>
	struct ReturnType<ParseResult, Dummy> {
		static bool const allowed = true;
	};

	template <typename Func, size_t>
	friend struct Adder;

	template <typename Func, size_t>
	struct Adder {};

	template <typename Func>
	struct Adder<Func, 0> {
		static void Add (LambdaOpts & opts, String const & keyword, Func const & f) {
			typedef typename FuncTraits<Func>::Return::type R;
			static_assert(ReturnType<R>::allowed, "Illegal return type.");
			opts.AddImpl(Tag<R>(), keyword, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 1> {
		static void Add (LambdaOpts & opts, String const & keyword, Func const & f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Return::type R;
			static_assert(ReturnType<R>::allowed, "Illegal return type.");
			opts.AddImpl<A>(Tag<R>(), keyword, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 2> {
		static void Add (LambdaOpts & opts, String const & keyword, Func const & f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			typedef typename FuncTraits<Func>::Return::type R;
			static_assert(ReturnType<R>::allowed, "Illegal return type.");
			opts.AddImpl<A,B>(Tag<R>(), keyword, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 3> {
		static void Add (LambdaOpts & opts, String const & keyword, Func const & f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			typedef typename FuncTraits<Func>::Arg2::type C;
			typedef typename FuncTraits<Func>::Return::type R;
			static_assert(ReturnType<R>::allowed, "Illegal return type.");
			opts.AddImpl<A,B,C>(Tag<R>(), keyword, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 4> {
		static void Add (LambdaOpts & opts, String const & keyword, Func const & f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			typedef typename FuncTraits<Func>::Arg2::type C;
			typedef typename FuncTraits<Func>::Arg3::type D;
			typedef typename FuncTraits<Func>::Return::type R;
			static_assert(ReturnType<R>::allowed, "Illegal return type.");
			opts.AddImpl<A,B,C,D>(Tag<R>(), keyword, f);
		}
	};

	template <typename Func>
	struct Adder<Func, 5> {
		static void Add (LambdaOpts & opts, String const & keyword, Func const & f) {
			typedef typename FuncTraits<Func>::Arg0::type A;
			typedef typename FuncTraits<Func>::Arg1::type B;
			typedef typename FuncTraits<Func>::Arg2::type C;
			typedef typename FuncTraits<Func>::Arg3::type D;
			typedef typename FuncTraits<Func>::Arg4::type E;
			typedef typename FuncTraits<Func>::Return::type R;
			static_assert(ReturnType<R>::allowed, "Illegal return type.");
			opts.AddImpl<A,B,C,D,E>(Tag<R>(), keyword, f);
		}
	};


//////////////////////////////////////////////////////////////////////////

	template <typename T, typename Dummy=void>
	struct AddDynamicParserExtra {
		static void Exec (DynamicParsers & dynamicParsers) {}
	};

	template <typename T, size_t N, typename Dummy>
	struct AddDynamicParserExtra<std::array<T, N>, Dummy> {
		static void Exec (DynamicParsers & dynamicParsers)
		{
			AddDynamicParser<T>(dynamicParsers);
		}
	};

	template <typename T>
	void AddDynamicParser ()
	{
		TypeKind typeKind{ Tag<T>() };
		for (auto const & key_value : dynamicParsers) {
			TypeKind const & key = key_value.first;
			if (key == typeKind) {
				return;
			}
		}
		AddDynamicParserExtra<T>::Exec(dynamicParsers);
		OpaqueParser parser = TypeTag<T>::OpaqueParse;
		dynamicParsers.emplace_back(std::move(typeKind), parser);
	}

	OpaqueParser LookupDynamicParser (TypeKind const & k) const
	{
		for (auto const & key_value : dynamicParsers) {
			TypeKind const & key = key_value.first;
			OpaqueParser p = key_value.second;
			if (key == k) {
				return p;
			}
		}
		ASSERT(__LINE__, false);
		return nullptr;
	}

//////////////////////////////////////////////////////////////////////////

	template <typename T>
	struct SimplifyType {
		typedef typename std::remove_cv<typename std::remove_reference<T>::type>::type type;
	};

	template <typename T>
	void PushTypeKind (std::vector<TypeKind> & kinds)
	{
		kinds.push_back(TypeKind(Tag<T>()));
		AddDynamicParser<T>();
	}

	void AddImpl (Tag<void>, String const & keyword, std::function<void()> const & func)
	{
		AddImpl(Tag<ParseResult>(), keyword, [=] () {
			func();
			return ParseResult::Accept;
		});
	}

	void AddImpl (Tag<ParseResult>, String const & keyword, std::function<ParseResult()> const & func)
	{
		if (keyword.empty()) {
			throw Exception("Cannot add an empty rule.");
		}
		infos0.emplace_back(keyword, func);
	}

	template <typename A>
	void AddImpl (Tag<void>, String const & keyword, std::function<void(A)> const & func)
	{
		AddImpl<A>(Tag<ParseResult>(), keyword, [=] (A && a) {
			func(std::forward<A>(a));
			return ParseResult::Accept;
		});
	}

	template <typename A>
	void AddImpl (Tag<ParseResult>, String const & keyword, std::function<ParseResult(A)> const & func)
	{
		typedef typename SimplifyType<A>::type A2;
		auto wrapper = [=] (V va) {
			A2 && a = TypeTag<A2>::ReifyOpaque(va);
			return func(std::forward<A>(a));
		};
		infos1.emplace_back(keyword, wrapper);
		auto & info = infos1.back();
		PushTypeKind<A2>(info.types);
	}

	template <typename A, typename B>
	void AddImpl (Tag<void>, String const & keyword, std::function<void(A,B)> const & func)
	{
		AddImpl<A,B>(Tag<ParseResult>(), keyword, [=] (A && a, B && b) {
			func(std::forward<A>(a), std::forward<B>(b));
			return ParseResult::Accept;
		});
	}

	template <typename A, typename B>
	void AddImpl (Tag<ParseResult>, String const & keyword, std::function<ParseResult(A,B)> const & func)
	{
		typedef typename SimplifyType<A>::type A2;
		typedef typename SimplifyType<B>::type B2;
		auto wrapper = [=] (V va, V vb) {
			A2 && a = TypeTag<A2>::ReifyOpaque(va);
			B2 && b = TypeTag<B2>::ReifyOpaque(vb);
			return func(std::forward<A>(a), std::forward<B>(b));
		};
		infos2.emplace_back(keyword, wrapper);
		auto & info = infos2.back();
		PushTypeKind<A2>(info.types);
		PushTypeKind<B2>(info.types);
	}

	template <typename A, typename B, typename C>
	void AddImpl (Tag<void>, String const & keyword, std::function<void(A,B,C)> const & func)
	{
		AddImpl<A,B,C>(Tag<ParseResult>(), keyword, [=] (A && a, B && b, C && c) {
			func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c));
			return ParseResult::Accept;
		});
	}

	template <typename A, typename B, typename C>
	void AddImpl (Tag<ParseResult>, String const & keyword, std::function<ParseResult(A,B,C)> const & func)
	{
		typedef typename SimplifyType<A>::type A2;
		typedef typename SimplifyType<B>::type B2;
		typedef typename SimplifyType<C>::type C2;
		auto wrapper = [=] (V va, V vb, V vc) {
			A2 && a = TypeTag<A2>::ReifyOpaque(va);
			B2 && b = TypeTag<B2>::ReifyOpaque(vb);
			C2 && c = TypeTag<C2>::ReifyOpaque(vc);
			return func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c));
		};
		infos3.emplace_back(keyword, wrapper);
		auto & info = infos3.back();
		PushTypeKind<A2>(info.types);
		PushTypeKind<B2>(info.types);
		PushTypeKind<C2>(info.types);
	}

	template <typename A, typename B, typename C, typename D>
	void AddImpl (Tag<void>, String const & keyword, std::function<void(A,B,C,D)> const & func)
	{
		AddImpl<A,B,C,D>(Tag<ParseResult>(), keyword, [=] (A && a, B && b, C && c, D && d) {
			func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d));
			return ParseResult::Accept;
		});
	}

	template <typename A, typename B, typename C, typename D>
	void AddImpl (Tag<ParseResult>, String const & keyword, std::function<ParseResult(A,B,C,D)> const & func)
	{
		typedef typename SimplifyType<A>::type A2;
		typedef typename SimplifyType<B>::type B2;
		typedef typename SimplifyType<C>::type C2;
		typedef typename SimplifyType<D>::type D2;
		auto wrapper = [=] (V va, V vb, V vc, V vd) {
			A2 && a = TypeTag<A2>::ReifyOpaque(va);
			B2 && b = TypeTag<B2>::ReifyOpaque(vb);
			C2 && c = TypeTag<C2>::ReifyOpaque(vc);
			D2 && d = TypeTag<D2>::ReifyOpaque(vd);
			return func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d));
		};
		infos4.emplace_back(keyword, wrapper);
		auto & info = infos4.back();
		PushTypeKind<A2>(info.types);
		PushTypeKind<B2>(info.types);
		PushTypeKind<C2>(info.types);
		PushTypeKind<D2>(info.types);
	}

	template <typename A, typename B, typename C, typename D, typename E>
	void AddImpl (Tag<void>, String const & keyword, std::function<void(A,B,C,D,E)> const & func)
	{
		AddImpl<A,B,C,D,E>(Tag<ParseResult>(), keyword, [=] (A && a, B && b, C && c, D && d, E && e) {
			func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d), std::forward<E>(e));
			return ParseResult::Accept;
		});
	}

	template <typename A, typename B, typename C, typename D, typename E>
	void AddImpl (Tag<ParseResult>, String const & keyword, std::function<ParseResult(A,B,C,D,E)> const & func)
	{
		typedef typename SimplifyType<A>::type A2;
		typedef typename SimplifyType<B>::type B2;
		typedef typename SimplifyType<C>::type C2;
		typedef typename SimplifyType<D>::type D2;
		typedef typename SimplifyType<E>::type E2;
		auto wrapper = [=] (V va, V vb, V vc, V vd, V ve) {
			A2 && a = TypeTag<A2>::ReifyOpaque(va);
			B2 && b = TypeTag<B2>::ReifyOpaque(vb);
			C2 && c = TypeTag<C2>::ReifyOpaque(vc);
			D2 && d = TypeTag<D2>::ReifyOpaque(vd);
			E2 && e = TypeTag<E2>::ReifyOpaque(ve);
			return func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d), std::forward<E>(e));
		};
		infos5.emplace_back(keyword, wrapper);
		auto & info = infos5.back();
		PushTypeKind<A2>(info.types);
		PushTypeKind<B2>(info.types);
		PushTypeKind<C2>(info.types);
		PushTypeKind<D2>(info.types);
		PushTypeKind<E2>(info.types);
	}

//////////////////////////////////////////////////////////////////////////

	static ParseResult Apply (std::function<ParseResult()> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V)> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V,V)> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V,V,V)> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V,V,V,V)> const & func, OpaqueArgs const & args);
	static ParseResult Apply (std::function<ParseResult(V,V,V,V,V)> const & func, OpaqueArgs const & args);

//////////////////////////////////////////////////////////////////////////

	template <typename C>
	static size_t StrLen (C const * str)
	{
		size_t size = 0;
		while (*str++) ++size;
		return size;
	}

	static bool Scan (std::string const & str, char const * format, void * dest)
	{
		char dummy;
		return std::sscanf(str.c_str(), format, dest, &dummy) == 1;
	}

	static bool Scan (std::wstring const & str, char const * format, void * dest)
	{
		wchar_t wformat[8];
		size_t len = StrLen(format) + 1;
		ASSERT(__LINE__, len <= (sizeof(wformat) / sizeof(wchar_t)));
		for (size_t i = 0; i < len; ++i) {
			wformat[i] = format[i];
		}
		wchar_t dummy;
		return std::swscanf(str.c_str(), wformat, dest, &dummy) == 1;
	}

//////////////////////////////////////////////////////////////////////////

	template <typename C, typename Dummy=void>
	struct StringLiteral {};

	template <typename Dummy>
	struct StringLiteral<char, Dummy> {
		static char const * xX () { return "xX"; };
	};

	template <typename Dummy>
	struct StringLiteral<wchar_t, Dummy> {
		static wchar_t const * xX () { return L"xX"; };
	};

//////////////////////////////////////////////////////////////////////////

	template <typename T>
	static std::unique_ptr<typename std::remove_reference<T>::type> AllocateCopy (T && source)
	{
		typedef typename std::remove_reference<T>::type T2;
		T2 * p = new T2(std::forward<T>(source));
		return std::unique_ptr<T2>(p);
	}

	template <typename T>
	struct TypeTagNumberBase {
		static bool Parse (ArgsIter & iter, ArgsIter end, T & out) {
			ASSERT(__LINE__, iter != end);
			String const & str = *iter;
			if (str.size() > 1 && std::isspace(str.front())) {
				return false;
			}
			if (Scan(*iter, TypeTag<T>::ScanDescription(), &out)) {
				if (str.size() == StrLen(str.c_str())) {
					if (str.find_first_of(StringLiteral<Char>::xX()) == std::string::npos) {
						++iter;
						return true;
					}
				}
			}
			return false;
		}
	};

	template <typename T, typename Dummy=void>
	struct TypeTagImpl {};

	template <typename Dummy>
	struct TypeTagImpl<int, Dummy> : public TypeTagNumberBase<int> {
	public:
		static char const * const ScanDescription () { return "%d%c"; }
	};

	template <typename Dummy>
	struct TypeTagImpl<unsigned int, Dummy> : public TypeTagNumberBase<unsigned int> {
	public:
		static char const * const ScanDescription () { return "%u%c"; }
		static bool Parse (ArgsIter & iter, ArgsIter end, unsigned int & out) {
			ASSERT(__LINE__, iter != end);
			if (!iter->empty() && iter->front() == '-') {
				return false;
			}
			return TypeTagNumberBase<unsigned int>::Parse(iter, end, out);
		}
	};

	template <typename Dummy>
	struct TypeTagImpl<float, Dummy> : public TypeTagNumberBase<float> {
	public:
		static char const * const ScanDescription () { return "%f%c"; }
	};

	template <typename Dummy>
	struct TypeTagImpl<double, Dummy> : public TypeTagNumberBase<double> {
	public:
		static char const * const ScanDescription () { return "%lf%c"; }
	};

	template <typename Dummy>
	struct TypeTagImpl<Char, Dummy> {
	public:
		static bool Parse (ArgsIter & iter, ArgsIter end, Char & out) {
			ASSERT(__LINE__, iter != end);
			if (iter->size() == 1) {
				out = iter->front();
				++iter;
				return true;
			}
			return false;
		}
	};

	template <typename Dummy>
	struct TypeTagImpl<String, Dummy> {
	public:
		static bool Parse (ArgsIter & iter, ArgsIter end, String & out) {
			ASSERT(__LINE__, iter != end);
			out = *iter;
			++iter;
			return true;
		}
	};

	template <typename T, size_t N, typename Dummy>
	struct TypeTagImpl<std::array<T, N>, Dummy> {
		static bool Parse (ArgsIter & iter, ArgsIter end, std::array<T, N> & out) {
			static_assert(N > 0, "Parsing a 0-sized array is not well-defined.");
			ASSERT(__LINE__, iter != end);
			for (size_t i = 0; i < N; ++i) {
				if (iter == end) {
					return false;
				}
				if (!TypeTagImpl<T>::Parse(iter, end, out[i])) {
					return false;
				}
			}
			return true;
		}
	};

	template <typename T>
	struct TypeTag : public TypeTagImpl<T> {
		typedef T Type;
		typedef TypeTagImpl<Type> Base;

		static Type && ReifyOpaque (void * p) {
			static_assert(std::is_same<Type, typename std::remove_reference<Type>::type>::value, "Internal error.");
			Type & val = *static_cast<Type *>(p);
			return std::move(val);
		}

		static void Delete (void * p) {
			delete static_cast<Type *>(p);
		}

		using Base::Parse;

		static UniqueOpaque OpaqueParse (ArgsIter & iter, ArgsIter end) {
			T x;
			if (Parse(iter, end, x)) {
				return UniqueOpaque(AllocateCopy(std::move(x)).release(), Delete);
			}
			return UniqueOpaque(static_cast<T *>(nullptr), Delete);
		}
	};

	template <typename FuncSig>
	class OptInfo {
	public:
		OptInfo (String const & keyword, std::function<FuncSig> const & callback)
			: keyword(keyword)
			, callback(callback)
		{}

		OptInfo (OptInfo && other)
			: keyword(std::move(other.keyword))
			, callback(std::move(other.callback))
			, types(std::move(other.types))
		{}

	public:
		String keyword;
		std::function<FuncSig> callback;
		std::vector<TypeKind> types;
	};

//////////////////////////////////////////////////////////////////////////

	class ParseEnvImpl {
	public:
		ParseEnvImpl (LambdaOpts const & opts, Args && args);

		bool Run (int & outParseFailureIndex);

		template <typename T>
		bool Peek (T & outArg);

		bool Next ();

		UniqueOpaque OpaqueParse (TypeKind const & typeKind, ArgsIter & iter, ArgsIter end);

		template <typename GenericOptInfo>
		ParseResult TryParse (bool useKeyword, std::vector<GenericOptInfo> const & infos);

		bool TryParse ();

	public:
		LambdaOpts const & opts;
		Args args;
		ArgsIter currArg;
	};

//////////////////////////////////////////////////////////////////////////

private:
	DynamicParsers dynamicParsers;
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
	, args(std::move(args))
	, currArg(args.begin())
{}


template <typename Char>
typename LambdaOpts<Char>::UniqueOpaque LambdaOpts<Char>::ParseEnvImpl::OpaqueParse (TypeKind const & typeKind, ArgsIter & iter, ArgsIter end)
{
	ArgsIter const begin = iter;

	OpaqueParser parser = opts.LookupDynamicParser(typeKind);
	UniqueOpaque p = parser(iter, end);

	if (p) {
		ASSERT(__LINE__, iter > begin);
	}
	else {
		iter = begin;
	}

	return p;
}


template <typename Char>
template <typename GenericOptInfo>
typename LambdaOpts<Char>::ParseResult LambdaOpts<Char>::ParseEnvImpl::TryParse (bool useKeyword, std::vector<GenericOptInfo> const & infos)
{
	if (infos.empty()) {
		return ParseResult::Reject;
	}
	size_t const arity = infos.front().types.size();

	ArgsIter const startArg = currArg;
	ASSERT(__LINE__, startArg != args.end());

	for (auto const & info : infos) {
		if (info.keyword.empty() == useKeyword) {
			continue;
		}
		currArg = startArg;
		if (!useKeyword || *currArg++ == info.keyword) {
			OpaqueArgs parsedArgs;
			bool parsedFullArity = true;
			for (size_t i = 0; i < arity; ++i) {
				if (currArg == args.end()) {
					parsedFullArity = false;
					break;
				}
				TypeKind const & typeKind = info.types[i];
				UniqueOpaque parsedArg = OpaqueParse(typeKind, currArg, args.end());
				if (parsedArg == nullptr) {
					parsedFullArity = false;
					break;
				}
				parsedArgs.emplace_back(std::move(parsedArg));
			}
			if (parsedFullArity) {
				ParseResult res = Apply(info.callback, parsedArgs);
				switch (res) {
					case ParseResult::Accept: {
						return ParseResult::Accept;
					} break;
					case ParseResult::Reject: {
						continue;
					} break;
					case ParseResult::Fatal: {
						currArg = startArg;
						return ParseResult::Fatal;
					} break;
					default: {
						ASSERT(__LINE__, false);
					}
				}
			}
		}
	}

	currArg = startArg;
	return ParseResult::Reject;
}


template <typename Char>
bool LambdaOpts<Char>::ParseEnvImpl::TryParse ()
{
	if (currArg == args.end()) {
		return false;
	}

	ParseResult res = ParseResult::Reject;

	bool useKeywordState[] = { true, false };

	for (bool useKeyword : useKeywordState) {
		if (res == ParseResult::Reject) {
			res = TryParse(useKeyword, opts.infos5);
		}
		if (res == ParseResult::Reject) {
			res = TryParse(useKeyword, opts.infos4);
		}
		if (res == ParseResult::Reject) {
			res = TryParse(useKeyword, opts.infos3);
		}
		if (res == ParseResult::Reject) {
			res = TryParse(useKeyword, opts.infos2);
		}
		if (res == ParseResult::Reject) {
			res = TryParse(useKeyword, opts.infos1);
		}
		if (res == ParseResult::Reject) {
			res = TryParse(useKeyword, opts.infos0);
		}
	}

	switch (res) {
		case ParseResult::Accept: return true;
		case ParseResult::Reject: return false;
		case ParseResult::Fatal: return false;
		default: ASSERT(__LINE__, false); return false;
	}
}


template <typename Char>
bool LambdaOpts<Char>::ParseEnvImpl::Run (int & outParseFailureIndex)
{
	currArg = args.begin();
	while (TryParse()) {
		continue;
	}
	if (currArg == args.end()) {
		outParseFailureIndex = -1;
		return true;
	}
	size_t argIndex = currArg - args.begin();
	outParseFailureIndex = static_cast<int>(argIndex);
	return false;
}


template <typename Char>
template <typename T>
bool LambdaOpts<Char>::ParseEnvImpl::Peek (T & outArg)
{
	if (currArg != args.end()) {
		ArgsIter startArg = currArg;
		bool res = TypeTag<T>::Parse(currArg, args.end(), outArg);
		currArg = startArg;
		return res;
	}
	return false;
}


template <typename Char>
bool LambdaOpts<Char>::ParseEnvImpl::Next ()
{
	if (currArg != args.end()) {
		++currArg;
		return true;
	}
	return false;
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
bool LambdaOpts<Char>::ParseEnv::Run (int & outParseFailureIndex)
{
	return impl->Run(outParseFailureIndex);
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
void LambdaOpts<Char>::AddOption (String const & keyword, Func const & f)
{
	Adder<Func, FuncTraits<Func>::arity>::Add(*this, keyword, f);
}


template <typename Char>
template <typename StringIter>
typename LambdaOpts<Char>::ParseEnv LambdaOpts<Char>::CreateParseEnv (StringIter begin, StringIter end)
{
	return ParseEnv(*this, Args(begin, end));
}








