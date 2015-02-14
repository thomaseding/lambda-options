// Copyright (c) 2015, Thomas Eding
// All rights reserved.
// 
// Homepage: https://github.com/thomaseding/lambda-opts
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

#include <algorithm>
#include <array>
#include <cctype>
#include <cstdio>
#include <exception>
#include <functional>
#include <limits>
#include <memory>
#include <new>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>


//////////////////////////////////////////////////////////////////////////


template <typename Char>
class LambdaOpts;


namespace lambda_opts
{
	class Exception : public std::exception {
	public:
		Exception (std::string const & message)
			: message(message)
		{}

		virtual char const * what () const throw() override
		{
			return message.c_str();
		}

	private:
		std::string message;
	};

	template <typename Char>
	class ArgsIter {
	public:
		typedef std::basic_string<Char> String;

	private:
		friend class LambdaOpts<Char>;

		typedef std::vector<String> Args;
		typedef typename Args::const_iterator Iter;

	public:
		ArgsIter & operator++ ()
		{
			if (iter == end) {
				throw Exception("lambda_opts::ArgsIter<Char>::operator++: Cannot increment past end iterator.");
			}
			++iter;
			return *this;
		}

		ArgsIter operator++ (int)
		{
			ArgsIter old = *this;
			++*this;
			return old;
		}

		bool operator== (ArgsIter const & other) const
		{
			EnsureSameBacking(other);
			return iter == other.iter;
		}

		bool operator!= (ArgsIter const & other) const
		{
			return !(*this == other);
		}

		bool operator< (ArgsIter const & other) const
		{
			EnsureSameBacking(other);
			return iter < other.iter;
		}

		bool operator> (ArgsIter const & other) const
		{
			return other < *this;
		}

		bool operator<= (ArgsIter const & other) const
		{
			return *this < other || *this == other;
		}

		bool operator>= (ArgsIter const & other) const
		{
			return *this > other || *this == other;
		}

		String const & operator* () const
		{
			return *iter;
		}

		String const * operator-> () const
		{
			return iter.operator->();
		}

	private:
		ArgsIter (Iter iter, Iter end)
			: iter(iter)
			, end(end)
		{}

		void EnsureSameBacking (ArgsIter const & other) const
		{
			if (end != other.end) {
				throw Exception("Iterators do not correspond to the same data.");
			}
		}

	private:
		Iter iter;
		Iter end;
	};

	template <typename Char = char>
	class ParseState {
		friend class LambdaOpts<Char>;

	private:
		ParseState (ArgsIter<Char> & iter, ArgsIter<Char> end)
			: iter(iter)
			, end(end)
		{}

	private:
		void operator= (ParseState const &); // disable

	public:
		ArgsIter<Char> & iter;
		ArgsIter<Char> const end;
	};

	namespace unstable_dont_use
	{
		inline void ASSERT (unsigned int line, bool truth)
		{
			if (!truth) {
				char msg[1024];
				sprintf(msg, "ASSERT failed in '%s' on line %u.", __FILE__, line);
				throw std::logic_error(msg);
			}
		}

		template <typename K, typename V>
		static V const * Lookup (std::vector<std::pair<K, V>> const & assocs, K const & key)
		{
			for (auto const & assoc : assocs) {
				if (assoc.first == key) {
					return &assoc.second;
				}
			}
			return nullptr;
		}

		inline void DeleteCharArray (void * p)
		{
			delete [] static_cast<char *>(p);
		}

		template <typename Char>
		inline size_t StrLen (Char const * str)
		{
			size_t size = 0;
			while (*str++) ++size;
			return size;
		}

		inline bool Scan (std::string const & str, char const * format, void * dest)
		{
			char dummy;
			return std::sscanf(str.c_str(), format, dest, &dummy) == 1;
		}

		inline bool Scan (std::wstring const & str, char const * format, void * dest)
		{
			wchar_t wformat[8];
			size_t len = StrLen(format) + 1;
			for (size_t i = 0; i < len; ++i) {
				wformat[i] = format[i];
			}
			wchar_t dummy;
			return std::swscanf(str.c_str(), wformat, dest, &dummy) == 1;
		}

		template <typename Char>
		struct StringLiteral {};

		template <>
		struct StringLiteral<char> {
			static char const * xX () { return "xX"; };
		};

		template <>
		struct StringLiteral<wchar_t> {
			static wchar_t const * xX () { return L"xX"; };
		};

		template <typename Char>
		inline bool ScanNumber (
			ParseState<Char> & parseState,
			void * rawMemory,
			char const * format)
		{
			auto const & str = *parseState.iter;
			if (str.size() > 1 && std::isspace(str.front())) {
				return false;
			}
			if (Scan(str, format, rawMemory)) {
				if (str.size() == StrLen(str.c_str())) {
					if (str.find_first_of(StringLiteral<Char>::xX()) == std::string::npos) {
						++parseState.iter;
						return true;
					}
				}
			}
			return false;
		}
	}

	template <typename T>
	class Maybe;

	template <typename Char, typename T>
	struct RawParser {};

	template <typename Char, typename T>
	inline bool RawParse (ParseState<Char> & parseState, void * rawMemory)
	{
		return RawParser<Char, T>()(parseState, rawMemory);
	}

	template <typename Char, typename T>
	inline bool Parse (ParseState<Char> & parseState, Maybe<T> & out)
	{
		out.ReleaseObjectIfValid();
		ArgsIter<Char> const startIter = parseState.iter;
		if (RawParse<Char, T>(parseState, out.ObjectAddress())) {
			out.alive = true;
			return true;
		}
		parseState.iter = startIter;
		return false;
	}

	template <typename Char>
	struct RawParser<Char, ParseState<Char>> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			new (rawMemory) ParseState<Char>(parseState);
			return true;
		}
	};

	template <typename Char>
	struct RawParser<Char, bool> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			std::basic_string<Char> const & s = *parseState.iter;
			if (s.size() == 4 && s[0] == 't' && s[1] == 'r' && s[2] == 'u' && s[3] == 'e') {
				new (rawMemory) bool(true);
				++parseState.iter;
				return true;
			}
			if (s.size() == 5 && s[0] == 'f' && s[1] == 'a' && s[2] == 'l' && s[3] == 's' && s[4] == 'e') {
				new (rawMemory) bool(false);
				++parseState.iter;
				return true;
			}
			return false;
		}
	};

	template <typename Char>
	struct RawParser<Char, int> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			return unstable_dont_use::ScanNumber<Char>(parseState, rawMemory, "%d%c");
		}
	};

	template <typename Char>
	struct RawParser<Char, unsigned int> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			if (!parseState.iter->empty() && parseState.iter->front() == '-') {
				return false;
			}
			return unstable_dont_use::ScanNumber<Char>(parseState, rawMemory, "%u%c");
		}
	};

	template <typename Char>
	struct RawParser<Char, float> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			return unstable_dont_use::ScanNumber<Char>(parseState, rawMemory, "%f%c");
		}
	};

	template <typename Char>
	struct RawParser<Char, double> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			return unstable_dont_use::ScanNumber<Char>(parseState, rawMemory, "%lf%c");
		}
	};

	template <typename Char>
	struct RawParser<Char, Char> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			if (parseState.iter->size() == 1) {
				new (rawMemory) Char(parseState.iter->front());
				++parseState.iter;
				return true;
			}
			return false;
		}
	};

	template <typename Char>
	struct RawParser<Char, std::basic_string<Char>> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			new (rawMemory) std::basic_string<Char>(*parseState.iter);
			++parseState.iter;
			return true;
		}
	};

	template <typename Char, typename T, size_t N>
	struct RawParser<Char, std::array<T, N>> {
	private:
		typedef std::array<T, N> Array;

	public:
		~RawParser ()
		{
			if (!success) {
				while (currIndex-- > 0) {
					(*pArray)[currIndex].~T();
				}
			}
		}

		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			static_assert(N > 0, "Parsing a zero-sized array is not well-defined.");
			success = false;
			pArray = reinterpret_cast<Array *>(rawMemory);
			Array & array = *pArray;
			currIndex = 0;
			for (; currIndex < N; ++currIndex) {
				if (parseState.iter == parseState.end) {
					return false;
				}
				T & elem = array[currIndex];
				char * rawElem = reinterpret_cast<char *>(&elem);
				if (!RawParse<Char, T>(parseState, rawElem)) {
					return false;
				}
			}
			success = true;
			return true;
		}

	private:
		bool success;
		size_t currIndex;
		Array * pArray;
	};

	template <typename T>
	class Maybe {
		template <typename Char, typename T2>
		friend bool Parse (ParseState<Char> & parseState, Maybe<T2> & out);

	public:
		Maybe ()
			: outsidePtr(static_cast<char *>(nullptr), free)
			, alignedPtr(innerBuffer)
			, alive(false)
		{
			size_t space = sizeof(innerBuffer);
			alignedPtr = std::align(std::alignment_of<T>::value, sizeof(T), alignedPtr, space);
		}

		~Maybe ()
		{
			ReleaseObjectIfValid();
		}

		bool HasValidObject () const
		{
			return alive;
		}

		T & Get ()
		{
			return *ObjectAddress();
		}

		T & operator* ()
		{
			return Get();
		}

		T * operator-> ()
		{
			return &Get();
		}

	private:
		T * ObjectAddress ()
		{
			if (alignedPtr != nullptr) {
				return reinterpret_cast<T *>(alignedPtr);
			}
			unstable_dont_use::ASSERT(__LINE__, !outsidePtr);
			outsidePtr = std::unique_ptr<char, void(*)(void *)>(new char[sizeof(T)], unstable_dont_use::DeleteCharArray);
			alignedPtr = outsidePtr.get();
			return ObjectAddress();
		}

		void ReleaseObjectIfValid ()
		{
			if (alive) {
				Get().~T();
				alive = false;
			}
		}

	private:
		Maybe (Maybe &&);               // disable
		Maybe (Maybe const &);          // disable
		void operator= (Maybe &&);      // disable
		void operator= (Maybe const &); // disable

	private:
		char innerBuffer[2 * sizeof(T) + 64];	// Normally a type would sit within 2x its size. Add in 64 for typical cache alignment.
		std::unique_ptr<char, void(*)(void*)> outsidePtr;
		void * alignedPtr;
		bool alive;
	};

	enum class ParseResult {
		Accept,
		Reject,
		Fatal,
	};
}


//////////////////////////////////////////////////////////////////////////


template <typename Char = char>
class LambdaOpts {
public:
	typedef std::basic_string<Char> String;

	class SubKeyword;

private:
	typedef std::vector<String> Args;
	typedef lambda_opts::ParseResult ParseResult;
	typedef int Priority;

	class LambdaOptsImpl;
	class ParseEnvImpl;

	class KeywordBase {
	public:
		virtual ~KeywordBase () {}
		virtual std::vector<std::shared_ptr<SubKeyword const>> const & SubKeywords () const = 0;

	public:
		std::vector<String> names;
	};

public:
	typedef Char char_type;

	class ParseEnv;

	class FormatConfig {
	public:
		FormatConfig ();
	public:
		size_t maxWidth;
	};

	class SubKeyword : private KeywordBase {
		friend class ParseEnvImpl;

	private:
		virtual std::vector<std::shared_ptr<SubKeyword const>> const & SubKeywords () const override
		{
			static std::vector<std::shared_ptr<SubKeyword const>> const subs;
			return subs;
		}

	public:
		SubKeyword ();
		explicit SubKeyword (Char shortName);
		explicit SubKeyword (String const & longName);
		SubKeyword (String const & longName, Char shortName);

	public:
		using KeywordBase::names;
	};

	class Keyword : private KeywordBase {
		friend class ParseEnvImpl;

	public:
		Keyword ();
		explicit Keyword (Char shortName);
		explicit Keyword (String const & longName);
		Keyword (String const & longName, Char shortName);
		Keyword (Char shortName, String const & help);
		Keyword (String const & longName, String const & help);
		Keyword (String const & longName, Char shortName, String const & help);
		Keyword (String const & longName, String const & group, String const & help);
		Keyword (Char shortName, String const & group, String const & help);
		Keyword (String const & longName, Char shortName, String const & group, String const & help);

		void AddSubKeyword (SubKeyword const & subKeyword);
		virtual std::vector<std::shared_ptr<SubKeyword const>> const & SubKeywords () const override;

	private:
		void Init (String const * longName, Char * shortName, String const * group, String const * help);

		void Validate () const;

	private:
		std::vector<std::shared_ptr<SubKeyword const>> subKeywords;
	public:
		using KeywordBase::names;
		String help;
		String group;
	};

	LambdaOpts ();

	template <typename Func>
	void AddOption (String const & keyword, Func const & f)
	{
		Keyword kw(keyword);
		impl->AddOption<Func>(kw, f);
	}

	template <typename Func>
	void AddOption (Keyword const & keyword, Func const & f)
	{
		impl->AddOption<Func>(keyword, f);
	}

	String HelpDescription (FormatConfig const & config) const
	{
		return impl->HelpDescription(config);
	}

	template <typename StringIter>
	ParseEnv CreateParseEnv (StringIter begin, StringIter end) const;

	class ParseEnv {
		friend class LambdaOpts;

	public:
		ParseEnv (ParseEnv && other);
		ParseEnv & operator= (ParseEnv && other);

		bool Run ()
		{
			int dummy;
			return Run(dummy);
		}

		bool Run (int & outParseFailureIndex)
		{
			return impl->Run(outParseFailureIndex);
		}

	private:
		ParseEnv (std::shared_ptr<LambdaOptsImpl const> opts, Args && args);
		ParseEnv (ParseEnv const & other);       // disable
		void operator= (ParseEnv const & other); // disable

	private:
		std::unique_ptr<ParseEnvImpl> impl;
	};


//////////////////////////////////////////////////////////////////////////

private:
	static void ASSERT (unsigned int line, bool truth)
	{
		namespace my = ::lambda_opts::unstable_dont_use;
		my::ASSERT(line, truth);
	}


//////////////////////////////////////////////////////////////////////////


	template <typename T>
	struct Tag {
		typedef T type;
	};

	template <typename T>
	struct SimplifyType {
		typedef typename std::remove_cv<typename std::remove_reference<T>::type>::type type;
	};


//////////////////////////////////////////////////////////////////////////


	typedef void * V;
	typedef void (*OpaqueDeleter)(void *);
	typedef std::unique_ptr<void, OpaqueDeleter> UniqueOpaque;
	typedef std::vector<UniqueOpaque> OpaqueValues;

	class TypeKind;

	typedef UniqueOpaque (*OpaqueParser)(lambda_opts::ParseState<Char> &);
	typedef std::vector<std::pair<TypeKind, OpaqueParser>> DynamicParserMap;


//////////////////////////////////////////////////////////////////////////


	class TypeKind {
	public:
		bool operator== (TypeKind const & other) const
		{
			return id == other.id;
		}

#ifdef RTTI_DISABLED
	private:
		TypeKind (void const * id)
			: id(id)
		{}

	public:
		template <typename T>
		static TypeKind Get ()
		{
			static char const uniqueMemLoc = 0;
			return TypeKind(&uniqueMemLoc);
		}

	private:
		void const * id;
#else
	private:
		TypeKind (std::type_info const & id)
			: id(id)
		{}

		void operator= (TypeKind const &); // disable

	public:
		template <typename T>
		static TypeKind Get ()
		{
			return TypeKind(typeid(T));
		}

	private:
		std::type_info const & id;
#endif
	};


///////////////////////////////////////////////////////////////////////////


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


	static ParseResult Apply (std::function<ParseResult()> const & func, OpaqueValues const & vals)
	{
		(void) vals;
		return func();
	}

	static ParseResult Apply (std::function<ParseResult(V)> const & func, OpaqueValues const & vals)
	{
		return func(vals[0].get());
	}

	static ParseResult Apply (std::function<ParseResult(V,V)> const & func, OpaqueValues const & vals)
	{
		return func(vals[0].get(), vals[1].get());
	}

	static ParseResult Apply (std::function<ParseResult(V,V,V)> const & func, OpaqueValues const & vals)
	{
		return func(vals[0].get(), vals[1].get(), vals[2].get());
	}

	static ParseResult Apply (std::function<ParseResult(V,V,V,V)> const & func, OpaqueValues const & vals)
	{
		return func(vals[0].get(), vals[1].get(), vals[2].get(), vals[3].get());
	}

	static ParseResult Apply (std::function<ParseResult(V,V,V,V,V)> const & func, OpaqueValues const & vals)
	{
		return func(vals[0].get(), vals[1].get(), vals[2].get(), vals[3].get(), vals[4].get());
	}


//////////////////////////////////////////////////////////////////////////


	template <typename T>
	static T && ReifyOpaque (void * p)
	{
		T & val = *static_cast<T *>(p);
		return std::move(val);
	}

	template <typename T>
	static void Delete (void * p)
	{
		delete static_cast<T *>(p);
	}

	template <typename T>
	static std::unique_ptr<typename std::remove_reference<T>::type> AllocateCopy (T && source)
	{
		typedef typename std::remove_reference<T>::type T2;
		T2 * p = new T2(std::forward<T>(source));
		return std::unique_ptr<T2>(p);
	}

	template <typename T>
	static UniqueOpaque OpaqueParse (lambda_opts::ParseState<Char> & parseState)
	{
		lambda_opts::Maybe<T> maybe;
		if (lambda_opts::Parse<Char, T>(parseState, maybe)) {
			return UniqueOpaque(AllocateCopy(std::move(*maybe)).release(), Delete<T>);
		}
		return UniqueOpaque(static_cast<T *>(nullptr), Delete<T>);
	}


///////////////////////////////////////////////////////////////////////////


	struct LambdaOptsImpl {


//////////////////////////////////////////////////////////////////////////


		String HelpDescription (FormatConfig const & config) const;


//////////////////////////////////////////////////////////////////////////


		template <typename Func, size_t>
		friend struct Adder;

		template <typename Func, size_t>
		struct Adder {};

		template <typename Func>
		void AddOption (Keyword const & keyword, Func const & f)
		{
			Adder<Func, FuncTraits<Func>::arity>::Add(*this, keyword, f);
		}

		template <typename Func>
		struct Adder<Func, 0> {
			static void Add (LambdaOptsImpl & opts, Keyword const & keyword, Func const & f)
			{
				typedef typename FuncTraits<Func>::Return::type R;
				static_assert(ReturnType<R>::allowed, "Illegal return type.");
				opts.AddImpl(Tag<R>(), keyword, f);
			}
		};

		template <typename Func>
		struct Adder<Func, 1> {
			static void Add (LambdaOptsImpl & opts, Keyword const & keyword, Func const & f)
			{
				typedef typename FuncTraits<Func>::Arg0::type A;
				typedef typename FuncTraits<Func>::Return::type R;
				static_assert(ReturnType<R>::allowed, "Illegal return type.");
				opts.AddImpl<A>(Tag<R>(), keyword, f);
			}
		};

		template <typename Func>
		struct Adder<Func, 2> {
			static void Add (LambdaOptsImpl & opts, Keyword const & keyword, Func const & f)
			{
				typedef typename FuncTraits<Func>::Arg0::type A;
				typedef typename FuncTraits<Func>::Arg1::type B;
				typedef typename FuncTraits<Func>::Return::type R;
				static_assert(ReturnType<R>::allowed, "Illegal return type.");
				opts.AddImpl<A,B>(Tag<R>(), keyword, f);
			}
		};

		template <typename Func>
		struct Adder<Func, 3> {
			static void Add (LambdaOptsImpl & opts, Keyword const & keyword, Func const & f)
			{
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
			static void Add (LambdaOptsImpl & opts, Keyword const & keyword, Func const & f)
			{
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
			static void Add (LambdaOptsImpl & opts, Keyword const & keyword, Func const & f)
			{
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

		void AddImpl (Tag<void>, Keyword const & keyword, std::function<void()> const & func)
		{
			AddImpl(Tag<ParseResult>(), keyword, [=] () {
				func();
				return ParseResult::Accept;
			});
		}

		void AddImpl (Tag<ParseResult>, Keyword const & keyword, std::function<ParseResult()> const & func)
		{
			if (keyword.names.empty()) {
				throw lambda_opts::Exception("Cannot add an empty rule.");
			}
			infos0.emplace_back(keyword, func);
		}

		template <typename A>
		void AddImpl (Tag<void>, Keyword const & keyword, std::function<void(A)> const & func)
		{
			AddImpl<A>(Tag<ParseResult>(), keyword, [=] (A && a) {
				func(std::forward<A>(a));
				return ParseResult::Accept;
			});
		}

		template <typename A>
		void AddImpl (Tag<ParseResult>, Keyword const & keyword, std::function<ParseResult(A)> const & func)
		{
			typedef typename SimplifyType<A>::type A2;
			auto wrapper = [=] (V va) {
				A2 && a = ReifyOpaque<A2>(va);
				return func(std::forward<A>(a));
			};
			infos1.emplace_back(keyword, wrapper);
			auto & info = infos1.back();
			PushTypeKind<A2>(info.typeKinds);
		}

		template <typename A, typename B>
		void AddImpl (Tag<void>, Keyword const & keyword, std::function<void(A,B)> const & func)
		{
			AddImpl<A,B>(Tag<ParseResult>(), keyword, [=] (A && a, B && b) {
				func(std::forward<A>(a), std::forward<B>(b));
				return ParseResult::Accept;
			});
		}

		template <typename A, typename B>
		void AddImpl (Tag<ParseResult>, Keyword const & keyword, std::function<ParseResult(A,B)> const & func)
		{
			typedef typename SimplifyType<A>::type A2;
			typedef typename SimplifyType<B>::type B2;
			auto wrapper = [=] (V va, V vb) {
				A2 && a = ReifyOpaque<A2>(va);
				B2 && b = ReifyOpaque<B2>(vb);
				return func(std::forward<A>(a), std::forward<B>(b));
			};
			infos2.emplace_back(keyword, wrapper);
			auto & info = infos2.back();
			PushTypeKind<A2>(info.typeKinds);
			PushTypeKind<B2>(info.typeKinds);
		}

		template <typename A, typename B, typename C>
		void AddImpl (Tag<void>, Keyword const & keyword, std::function<void(A,B,C)> const & func)
		{
			AddImpl<A,B,C>(Tag<ParseResult>(), keyword, [=] (A && a, B && b, C && c) {
				func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c));
				return ParseResult::Accept;
			});
		}

		template <typename A, typename B, typename C>
		void AddImpl (Tag<ParseResult>, Keyword const & keyword, std::function<ParseResult(A,B,C)> const & func)
		{
			typedef typename SimplifyType<A>::type A2;
			typedef typename SimplifyType<B>::type B2;
			typedef typename SimplifyType<C>::type C2;
			auto wrapper = [=] (V va, V vb, V vc) {
				A2 && a = ReifyOpaque<A2>(va);
				B2 && b = ReifyOpaque<B2>(vb);
				C2 && c = ReifyOpaque<C2>(vc);
				return func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c));
			};
			infos3.emplace_back(keyword, wrapper);
			auto & info = infos3.back();
			PushTypeKind<A2>(info.typeKinds);
			PushTypeKind<B2>(info.typeKinds);
			PushTypeKind<C2>(info.typeKinds);
		}

		template <typename A, typename B, typename C, typename D>
		void AddImpl (Tag<void>, Keyword const & keyword, std::function<void(A,B,C,D)> const & func)
		{
			AddImpl<A,B,C,D>(Tag<ParseResult>(), keyword, [=] (A && a, B && b, C && c, D && d) {
				func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d));
				return ParseResult::Accept;
			});
		}

		template <typename A, typename B, typename C, typename D>
		void AddImpl (Tag<ParseResult>, Keyword const & keyword, std::function<ParseResult(A,B,C,D)> const & func)
		{
			typedef typename SimplifyType<A>::type A2;
			typedef typename SimplifyType<B>::type B2;
			typedef typename SimplifyType<C>::type C2;
			typedef typename SimplifyType<D>::type D2;
			auto wrapper = [=] (V va, V vb, V vc, V vd) {
				A2 && a = ReifyOpaque<A2>(va);
				B2 && b = ReifyOpaque<B2>(vb);
				C2 && c = ReifyOpaque<C2>(vc);
				D2 && d = ReifyOpaque<D2>(vd);
				return func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d));
			};
			infos4.emplace_back(keyword, wrapper);
			auto & info = infos4.back();
			PushTypeKind<A2>(info.typeKinds);
			PushTypeKind<B2>(info.typeKinds);
			PushTypeKind<C2>(info.typeKinds);
			PushTypeKind<D2>(info.typeKinds);
		}

		template <typename A, typename B, typename C, typename D, typename E>
		void AddImpl (Tag<void>, Keyword const & keyword, std::function<void(A,B,C,D,E)> const & func)
		{
			AddImpl<A,B,C,D,E>(Tag<ParseResult>(), keyword, [=] (A && a, B && b, C && c, D && d, E && e) {
				func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d), std::forward<E>(e));
				return ParseResult::Accept;
			});
		}

		template <typename A, typename B, typename C, typename D, typename E>
		void AddImpl (Tag<ParseResult>, Keyword const & keyword, std::function<ParseResult(A,B,C,D,E)> const & func)
		{
			typedef typename SimplifyType<A>::type A2;
			typedef typename SimplifyType<B>::type B2;
			typedef typename SimplifyType<C>::type C2;
			typedef typename SimplifyType<D>::type D2;
			typedef typename SimplifyType<E>::type E2;
			auto wrapper = [=] (V va, V vb, V vc, V vd, V ve) {
				A2 && a = ReifyOpaque<A2>(va);
				B2 && b = ReifyOpaque<B2>(vb);
				C2 && c = ReifyOpaque<C2>(vc);
				D2 && d = ReifyOpaque<D2>(vd);
				E2 && e = ReifyOpaque<E2>(ve);
				return func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d), std::forward<E>(e));
			};
			infos5.emplace_back(keyword, wrapper);
			auto & info = infos5.back();
			PushTypeKind<A2>(info.typeKinds);
			PushTypeKind<B2>(info.typeKinds);
			PushTypeKind<C2>(info.typeKinds);
			PushTypeKind<D2>(info.typeKinds);
			PushTypeKind<E2>(info.typeKinds);
		}


//////////////////////////////////////////////////////////////////////////


		template <typename T>
		void AddDynamicParser ()
		{
			namespace my = ::lambda_opts::unstable_dont_use;
			TypeKind typeKind = TypeKind::Get<T>();
			if (my::Lookup(dynamicParserMap, typeKind) == nullptr) {
				OpaqueParser parser = OpaqueParse<T>;
				dynamicParserMap.emplace_back(std::move(typeKind), parser);
			}
		}

		OpaqueParser LookupDynamicParser (TypeKind const & k) const
		{
			namespace my = ::lambda_opts::unstable_dont_use;
			OpaqueParser const * pParser = my::Lookup(dynamicParserMap, k);
			ASSERT(__LINE__, pParser != nullptr);
			return *pParser;
		}

		template <typename T>
		void PushTypeKind (std::vector<TypeKind> & kinds)
		{
			kinds.push_back(TypeKind::Get<T>());
			AddDynamicParser<T>();
		}


//////////////////////////////////////////////////////////////////////////


		template <typename FuncSig>
		class OptInfo {
		public:
			OptInfo (Keyword const & keyword, std::function<FuncSig> const & callback)
				: keyword(keyword)
				, callback(callback)
			{}

			OptInfo (OptInfo && other)
				: keyword(std::move(other.keyword))
				, callback(std::move(other.callback))
				, typeKinds(std::move(other.typeKinds))
			{}

		public:
			Keyword keyword;
			std::function<FuncSig> callback;
			std::vector<TypeKind> typeKinds;
		};


//////////////////////////////////////////////////////////////////////////


	public:
		DynamicParserMap dynamicParserMap;
		std::vector<std::pair<String, Priority>> groupPriorities;
		std::vector<OptInfo<ParseResult()>> infos0;
		std::vector<OptInfo<ParseResult(V)>> infos1;
		std::vector<OptInfo<ParseResult(V,V)>> infos2;
		std::vector<OptInfo<ParseResult(V,V,V)>> infos3;
		std::vector<OptInfo<ParseResult(V,V,V,V)>> infos4;
		std::vector<OptInfo<ParseResult(V,V,V,V,V)>> infos5;
	};


//////////////////////////////////////////////////////////////////////////


	class ParseEnvImpl {
	public:
		ParseEnvImpl (std::shared_ptr<LambdaOptsImpl const> opts, Args && args)
			: opts(opts)
			, args(std::move(args))
			, begin(this->args.begin(), this->args.end())
			, end(this->args.end(), this->args.end())
			, iter(begin)
			, parseState(iter, end)
		{}

		bool Run (int & outParseFailureIndex)
		{
			iter = begin;
			while (TryParse()) {
				continue;
			}
			if (iter == end) {
				outParseFailureIndex = -1;
				return true;
			}
			outParseFailureIndex = static_cast<int>(iter.iter - begin.iter);
			return false;
		}

	private:
		UniqueOpaque OpaqueParse (TypeKind const & typeKind)
		{
			OpaqueParser parser = opts->LookupDynamicParser(typeKind);
			return parser(parseState);
		}

		OpaqueValues ParseArgs (std::vector<TypeKind> const & typeKinds)
		{
			size_t const N = typeKinds.size();
			OpaqueValues parsedArgs;
			for (size_t i = 0; i < N; ++i) {
				if (iter == end) {
					break;
				}
				TypeKind const & typeKind = typeKinds[i];
				UniqueOpaque parsedArg = OpaqueParse(typeKind);
				if (parsedArg == nullptr) {
					break;
				}
				parsedArgs.emplace_back(std::move(parsedArg));
			}
			return std::move(parsedArgs);
		}

		bool MatchKeyword (KeywordBase const & keyword)
		{
			if (keyword.names.empty()) {
				return true;
			}

			auto const startIter1 = iter;

			for (String const & name : keyword.names) {
				iter = startIter1;
				if (*iter == name) {
					++iter;
					if (keyword.SubKeywords().empty()) {
						return true;
					}
					auto const startIter2 = iter;
					for (auto const & subKeyword : keyword.SubKeywords()) {
						iter = startIter2;
						if (MatchKeyword(*subKeyword)) {
							return true;
						}
					}
					return false;
				}
			}
			return false;
		}

		template <typename GenericOptInfo>
		ParseResult TryParse (bool useKeyword, std::vector<GenericOptInfo> const & infos)
		{
			if (infos.empty()) {
				return ParseResult::Reject;
			}
			size_t const arity = infos.front().typeKinds.size();

			auto const startIter = iter;

			for (auto const & info : infos) {
				if (info.keyword.names.empty() == useKeyword) {
					continue;
				}
				iter = startIter;
				if (MatchKeyword(info.keyword)) {
					auto const & typeKinds = info.typeKinds;
					ASSERT(__LINE__, typeKinds.size() == arity);
					OpaqueValues parsedArgs = ParseArgs(typeKinds);
					if (parsedArgs.size() == arity) {
						ParseResult res = Apply(info.callback, parsedArgs);
						switch (res) {
							case ParseResult::Accept: {
								return ParseResult::Accept;
							} break;
							case ParseResult::Reject: {
								continue;
							} break;
							case ParseResult::Fatal: {
								iter = startIter;
								return ParseResult::Fatal;
							} break;
							default: {
								ASSERT(__LINE__, false);
							}
						}
					}
				}
			}

			iter = startIter;
			return ParseResult::Reject;
		}

		bool TryParse ()
		{
			if (iter == end) {
				return false;
			}

			ParseResult res = ParseResult::Reject;

			bool const useKeywordState[] = { true, false };
			for (bool useKeyword : useKeywordState) {
				if (res == ParseResult::Reject) {
					res = TryParse(useKeyword, opts->infos5);
				}
				if (res == ParseResult::Reject) {
					res = TryParse(useKeyword, opts->infos4);
				}
				if (res == ParseResult::Reject) {
					res = TryParse(useKeyword, opts->infos3);
				}
				if (res == ParseResult::Reject) {
					res = TryParse(useKeyword, opts->infos2);
				}
				if (res == ParseResult::Reject) {
					res = TryParse(useKeyword, opts->infos1);
				}
				if (res == ParseResult::Reject) {
					res = TryParse(useKeyword, opts->infos0);
				}
			}

			switch (res) {
				case ParseResult::Accept: return true;
				case ParseResult::Reject: return false;
				case ParseResult::Fatal: return false;
				default: {
					ASSERT(__LINE__, false);
					return false;
				}
			}
		}

	public:
		std::shared_ptr<LambdaOptsImpl const> opts;
		Args args;
		lambda_opts::ArgsIter<Char> const begin;
		lambda_opts::ArgsIter<Char> const end;
		lambda_opts::ArgsIter<Char> iter;
		lambda_opts::ParseState<Char> parseState;
	};


///////////////////////////////////////////////////////////////////////////


	class Formatter {
		enum Phase { EmitName, EmitDesc };

	public:
		Formatter (FormatConfig const & config)
			: config(config)
		{
			this->config.maxWidth = std::max<size_t>(config.maxWidth, 30);
		}

		void FormatKeyword (Keyword const & keyword)
		{
			width = 0;
			ChangeIndentation(0);
			NewLine();
			FormatKeywordNames(keyword);
			FormatKeywordHelp(keyword);
			FlushWord();
		}

		String ToString () const
		{
			return String(emittedChars.begin(), emittedChars.end());
		}

	private:
		void FormatKeywordNames (Keyword const & keyword)
		{
			std::vector<String> names = keyword.names;
			std::sort(names.begin(), names.end());

			if (!names.empty()) {
				size_t idx = 0;
				if (IsShort(names[idx])) {
					ChangeIndentation(1);
					Emit(names[idx]);
					++idx;
				}
				for ( ; idx < names.size(); ++idx) {
					if (idx > 0) {
						Emit(',');
					}
					ChangeIndentation(5);
					Emit(names[idx]);
				}
			}
		}

		void FormatKeywordHelp (Keyword const & keyword)
		{
			ChangeIndentation(29);
			Emit(keyword.help);
		}

		static bool IsShort (String const & name)
		{
			return name.size() == 2 && name.front() == '-';
		}

		bool FlushWord ()
		{
			if (word.empty()) {
				return false;
			}
			if (!(width == indentation || word.size() + width <= config.maxWidth)) {
				NewLine(false);
			}
			emittedChars.insert(emittedChars.end(), word.begin(), word.end());
			width += word.size();
			word.clear();
			return true;
		}

		void ChangeIndentation (size_t newAmount)
		{
			indentation = newAmount;
			Indent();
		}

		void Indent (bool flushWord = true)
		{
			if (flushWord) {
				FlushWord();
			}
			if (width >= indentation) {
				return;
			}
			size_t amount = indentation - width;
			emittedChars.insert(emittedChars.end(), amount, ' ');
			width = indentation;
		}

		void NewLine (bool flushWord = true)
		{
			if (!emittedChars.empty()) {
				emittedChars.push_back('\n');
			}
			width = 0;
			Indent(flushWord);
		}

		void Emit (Char c)
		{
			switch (c) {
				case ' ': {
					if (FlushWord()) {
						if (width < config.maxWidth) {
							emittedChars.push_back(' ');
						}
						else {
							NewLine();
						}
					}
				} break;
				default: {
					word.push_back(c);
				}
			}
		}

		void Emit (String const & str)
		{
			for (Char c : str) {
				Emit(c);
			}
		}

		template <typename C>
		void Emit (C const * str)
		{
			while (*str) {
				Emit(*str++);
			}
		}

	private:
		FormatConfig config;
		std::vector<Char> emittedChars;
		std::vector<Char> word;
		Phase phase;
		size_t width;
		size_t indentation;
	};


///////////////////////////////////////////////////////////////////////////


private:
	std::shared_ptr<LambdaOptsImpl> impl;
};


//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////


template <typename Char>
LambdaOpts<Char>::LambdaOpts ()
	: impl(new LambdaOptsImpl())
{}


template <typename Char>
template <typename StringIter>
typename LambdaOpts<Char>::ParseEnv LambdaOpts<Char>::CreateParseEnv (StringIter begin, StringIter end) const
{
	return ParseEnv(impl, Args(begin, end));
}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
LambdaOpts<Char>::FormatConfig::FormatConfig ()
	: maxWidth(80)
{}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
LambdaOpts<Char>::SubKeyword::SubKeyword () {}


template <typename Char>
LambdaOpts<Char>::SubKeyword::SubKeyword (Char shortName)
{
	names.emplace_back(1, '-');
	names.back().push_back(shortName);
}


template <typename Char>
LambdaOpts<Char>::SubKeyword::SubKeyword (String const & longName)
{
	names.emplace_back(longName);
}


template <typename Char>
LambdaOpts<Char>::SubKeyword::SubKeyword (String const & longName, Char shortName)
{
	names.emplace_back(longName);
	names.emplace_back(1, '-');
	names.back().push_back(shortName);
}


//////////////////////////////////////////////////////////////////////////



template <typename Char>
LambdaOpts<Char>::Keyword::Keyword ()
{
	Init(nullptr, nullptr, nullptr, nullptr);
}


template <typename Char>
LambdaOpts<Char>::Keyword::Keyword (Char shortName)
{
	Init(nullptr, &shortName, nullptr, nullptr);
}


template <typename Char>
LambdaOpts<Char>::Keyword::Keyword (String const & longName)
{
	Init(&longName, nullptr, nullptr, nullptr);
}


template <typename Char>
LambdaOpts<Char>::Keyword::Keyword (String const & longName, Char shortName)
{
	Init(&longName, &shortName, nullptr, nullptr);
}


template <typename Char>
LambdaOpts<Char>::Keyword::Keyword (Char shortName, String const & help)
{
	Init(nullptr, &shortName, nullptr, &help);
}


template <typename Char>
LambdaOpts<Char>::Keyword::Keyword (String const & longName, String const & help)
{
	Init(&longName, nullptr, nullptr, &help);
}


template <typename Char>
LambdaOpts<Char>::Keyword::Keyword (String const & longName, Char shortName, String const & help)
{
	Init(&longName, &shortName, nullptr, &help);
}


template <typename Char>
LambdaOpts<Char>::Keyword::Keyword (String const & longName, String const & group, String const & help)
{
	Init(&longName, &shortName, &group, &help);
}


template <typename Char>
LambdaOpts<Char>::Keyword::Keyword (Char shortName, String const & group, String const & help)
{
	Init(nullptr, &shortName, &group, &help);
}


template <typename Char>
LambdaOpts<Char>::Keyword::Keyword (String const & longName, Char shortName, String const & group, String const & help)
{
	Init(&longName, &shortName, &group, &help);
}


template <typename Char>
void LambdaOpts<Char>::Keyword::Init (String const * longName, Char * shortName, String const * group, String const * help)
{
	if (shortName != nullptr) {
		std::basic_string<Char> shortNameStr;
		shortNameStr.push_back('-');
		shortNameStr.push_back(*shortName);
		names.push_back(shortNameStr);
	}
	if (longName != nullptr) {
		names.push_back(*longName);
	}
	if (group != nullptr) {
		this->group = *group;
	}
	if (help != nullptr) {
		this->help = *help;
	}
	Validate();
}


template <typename Char>
void LambdaOpts<Char>::Keyword::AddSubKeyword (SubKeyword const & subKeyword)
{
	subKeywords.push_back(std::shared_ptr<SubKeyword>(new SubKeyword(subKeyword)));
	try {
		Validate();
	}
	catch (lambda_opts::Exception const & e) {
		subKeywords.pop_back();
		throw e;
	}
}


template <typename Char>
auto LambdaOpts<Char>::Keyword::SubKeywords () const -> std::vector<std::shared_ptr<SubKeyword const>> const &
{
	return subKeywords;
}


template <typename Char>
void LambdaOpts<Char>::Keyword::Validate () const
{
	if (names.empty() && !subKeywords.empty()) {
		throw lambda_opts::Exception("Empty keyword cannot have sub-keywords.");
	}
}


//////////////////////////////////////////////////////////////////////////


template <typename Char>
LambdaOpts<Char>::ParseEnv::ParseEnv (std::shared_ptr<LambdaOptsImpl const> opts, std::vector<String> && args)
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
auto LambdaOpts<Char>::LambdaOptsImpl::HelpDescription (FormatConfig const & config) const -> String
{
	namespace my = ::lambda_opts::unstable_dont_use;

	std::vector<Keyword const *> keywords;
	for (auto const & info : infos0) {
		keywords.push_back(&info.keyword);
	};
	for (auto const & info : infos1) {
		keywords.push_back(&info.keyword);
	};
	for (auto const & info : infos2) {
		keywords.push_back(&info.keyword);
	};
	for (auto const & info : infos3) {
		keywords.push_back(&info.keyword);
	};
	for (auto const & info : infos4) {
		keywords.push_back(&info.keyword);
	};
	for (auto const & info : infos5) {
		keywords.push_back(&info.keyword);
	};

	auto getPriority = [&] (String const & group) {
		Priority const * pPriority = my::Lookup(groupPriorities, group);
		if (pPriority) {
			return *pPriority;
		}
		return std::numeric_limits<Priority>::max();
	};

	std::sort(keywords.begin(), keywords.end(), [&] (Keyword const * kw1, Keyword const * kw2) {
		String const & g1 = kw1->group;
		String const & g2 = kw2->group;
		Priority const p1 = getPriority(g1);
		Priority const p2 = getPriority(g2);
		if (p1 < p2) {
			return true;
		}
		if (p1 > p2) {
			return false;
		}
		return g1 < g2;
	});

	Formatter formatter(config);
	for (Keyword const * keyword : keywords) {
		formatter.FormatKeyword(*keyword);
	}
	return formatter.ToString();
}










