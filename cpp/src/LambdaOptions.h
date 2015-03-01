// Copyright (c) 2015, Thomas Eding
// All rights reserved.
// 
// Homepage: https://github.com/thomaseding/lambda-options
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer. 
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
#include <cstring>
#include <cwchar>
#include <cwctype>
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


namespace lambda_options
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
	class Options;


	enum class ParseResult {
		Accept,
		Reject,
		Fatal,
	};


	namespace _private
	{
		inline void ASSERT (unsigned int line, bool truth)
		{
			if (!truth) {
				char msg[1024];
				sprintf(msg, "ASSERT failed in '%s' on line %u.", __FILE__, line);
				throw Exception(msg);
			}
		}


		template <typename T>
		struct SimplifyType {
			typedef typename std::remove_cv<typename std::remove_reference<T>::type>::type type;
		};


		template <typename T>
		struct Tag {
			typedef T type;
		};


		template <typename Char>
		inline bool IsPrefixOf (char const * prefix, std::basic_string<Char> const & str)
		{
			auto it = str.begin();
			auto const end = str.end();
			while (*prefix != '\0') {
				if (it == end) {
					return false;
				}
				if (*it != *prefix) {
					return false;
				}
				++it;
				++prefix;
			}
			return true;
		}


		template <typename Iter, typename T>
		inline bool Contains (Iter begin, Iter end, T const & val)
		{
			return std::find(begin, end, val) != end;
		}


		template <typename K, typename V>
		inline V * Lookup (std::vector<std::pair<K, V>> & assocs, K const & key)
		{
			for (auto & assoc : assocs) {
				if (assoc.first == key) {
					return &assoc.second;
				}
			}
			return nullptr;
		}


		template <typename K, typename V>
		inline V const * Lookup (std::vector<std::pair<K, V>> const & assocs, K const & key)
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


		inline size_t StrLen (char const * str)
		{
			return std::strlen(str);
		}


		inline size_t StrLen (wchar_t const * str)
		{
			return std::wcslen(str);
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


		typedef void (*OpaqueDeleter)(void *);
		typedef std::unique_ptr<void, OpaqueDeleter> UniqueOpaque;
		typedef std::vector<UniqueOpaque> OpaqueValues;
	}


	template <typename Char>
	class ParseState;


	template <typename T>
	class Maybe;


	template <typename Char, typename T>
	bool Parse (ParseState<Char> & parseState, Maybe<T> & out);


	template <typename T>
	class Maybe {
		template <typename Char, typename T2>
		friend bool Parse (ParseState<Char> & parseState, Maybe<T2> & out);

	public:
		Maybe ()
			: outsidePtr(static_cast<char *>(nullptr), free)
#if _MSC_VER
			, alignedPtr(innerBuffer)
#else
			, alignedPtr(u.rawView)
#endif
			, alive(false)
		{
#if _MSC_VER
			size_t space = sizeof(innerBuffer);
			alignedPtr = std::align(std::alignment_of<T>::value, sizeof(T), alignedPtr, space);
#endif
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
			_private::ASSERT(__LINE__, !outsidePtr);
			outsidePtr = std::unique_ptr<char, void(*)(void *)>(new char[sizeof(T)], _private::DeleteCharArray);
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
#if _MSC_VER
		char innerBuffer[2 * sizeof(T) + 64];	// Normally a type would sit within 2x its size. Add in 64 for typical cache alignment.
#else
		union U {
			char rawView[sizeof(T)];
			T objectView;
			U () : rawView() {}
			~U () {}
		} u;
#endif
		std::unique_ptr<char, void(*)(void*)> outsidePtr;
		void * alignedPtr;
		bool alive;
	};


	class OptionException : public Exception {
	public:
		OptionException (std::string const & message)
			: Exception(message)
		{}
	};


	template <typename Char>
	class OptionConflictException : public OptionException {
	public:
		typedef std::basic_string<Char> String;

		OptionConflictException (String const & name1, String const & name2, size_t arity)
			: OptionException("Cannot add an option that has the same keyword and function signature of another option.")
			, conflictingNames(name1, name2)
			, conflictingArity(arity)
		{}

	public:
		std::pair<String, String> conflictingNames;
		size_t conflictingArity;
	};


	class EmptyOptionException : public OptionException {
	public:
		EmptyOptionException ()
			: OptionException("Cannot add an empty option.")
		{}
	};


	class IteratorException : public Exception {
	public:
		IteratorException (std::string const & message)
			: Exception(message)
		{}
	};


	class ParseFailedException : public Exception {
	public:
		ParseFailedException (size_t beginIndex, size_t endIndex)
			: Exception("")
			, beginIndex(beginIndex)
			, endIndex(endIndex)
		{
			sprintf(message, "Parse failed in argument range [%u, %u].",
				static_cast<unsigned int>(beginIndex),
				static_cast<unsigned int>(endIndex));
		}

		virtual char const * what () const throw() override
		{
			return message;
		}

	public:
		size_t beginIndex;
		size_t endIndex;
	private:
		char message[128];
	};


	namespace _private
	{
		template <typename Char>
		class ParseContextImpl;
	}


	template <typename Char = char>
	class ArgsIter {
	public:
		typedef std::basic_string<Char> String;

	private:
		friend class _private::ParseContextImpl<Char>;

		typedef typename std::vector<String>::const_iterator Iter;

	public:
		size_t Index () const;

		ArgsIter & operator++ ();

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
		ArgsIter (Iter iter, Iter end, void * opaqueParseContext)
			: iter(iter)
			, end(end)
			, opaqueParseContext(opaqueParseContext)
		{}

		void EnsureSameBacking (ArgsIter const & other) const
		{
			if (end != other.end) {
				throw IteratorException("Iterators do not correspond to the same data.");
			}
		}

	private:
		Iter iter;
		Iter end;
		void * opaqueParseContext;
	};


	namespace _private
	{
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


		template <typename T>
		static T && ReifyOpaque (void * p)
		{
			T & val = *static_cast<T *>(p);
			return std::move(val);
		}

		template <typename T>
		static T && ReifyOpaque (UniqueOpaque & p)
		{
			return ReifyOpaque<T>(p.get());
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

		template <typename Char, typename T>
		static UniqueOpaque OpaqueParse (ParseState<Char> & parseState)
		{
			Maybe<T> maybe;
			if (Parse<Char, T>(parseState, maybe)) {
				return UniqueOpaque(AllocateCopy(std::move(*maybe)).release(), Delete<T>);
			}
			return UniqueOpaque(static_cast<T *>(nullptr), Delete<T>);
		}
	}


	template <typename Char = char>
	class ParseState {
		friend class _private::ParseContextImpl<Char>;

	public:
		ParseState (ParseState const & other)
			: iter(other.iter)
			, end(other.end)
		{}

	private:
		ParseState (ArgsIter<Char> & iter, ArgsIter<Char> end)
			: iter(iter)
			, end(end)
		{}

	private:
		void operator= (ParseState &&); // disable
		void operator= (ParseState const &); // disable

	public:
		ArgsIter<Char> & iter;
		ArgsIter<Char> const end;
	};


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
		if (parseState.iter == parseState.end) {
			return false;
		}
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
			return _private::ScanNumber<Char>(parseState, rawMemory, "%d%c");
		}
	};


	template <typename Char>
	struct RawParser<Char, unsigned int> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			if (!parseState.iter->empty() && parseState.iter->front() == '-') {
				return false;
			}
			return _private::ScanNumber<Char>(parseState, rawMemory, "%u%c");
		}
	};


	template <typename Char>
	struct RawParser<Char, float> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			return _private::ScanNumber<Char>(parseState, rawMemory, "%f%c");
		}
	};


	template <typename Char>
	struct RawParser<Char, double> {
		bool operator() (ParseState<Char> & parseState, void * rawMemory)
		{
			return _private::ScanNumber<Char>(parseState, rawMemory, "%lf%c");
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


	typedef int Priority;


	enum class KeywordStyle {
		Exact,
		Gnu,
		Windows,

		Default = Gnu
	};


	enum class MatchFlags : size_t {
		Empty              = 0,
		IgnoreAsciiCase    = 1 << 0,
		RelaxedDashes      = 1 << 1,
		RelaxedUnderscores = 1 << 2,
		//GnuShortGrouping   = 1 << 3,

		Default = Empty
	};

	inline MatchFlags operator& (MatchFlags a, MatchFlags b)
	{
		return static_cast<MatchFlags>(static_cast<size_t>(a) & static_cast<size_t>(b));
	}

	inline MatchFlags operator| (MatchFlags a, MatchFlags b)
	{
		return static_cast<MatchFlags>(static_cast<size_t>(a) | static_cast<size_t>(b));
	}


	class OptionsConfig {
	public:
		OptionsConfig ()
			: keywordStyle(KeywordStyle::Default)
			, matchFlags(MatchFlags::Default)
		{}

	public:
		KeywordStyle keywordStyle;
		MatchFlags matchFlags;
	};


	template <typename Char>
	class FormattingConfig {
		typedef std::basic_string<Char> String;
	public:
		FormattingConfig ();
	public:
		size_t maxWidth;
		std::vector<String> groupFilter;
	};


	template <typename Char>
	class Keyword {
		typedef std::basic_string<Char> String;
		static String const nil;

	public:
		explicit Keyword (
			String const & name1 = nil,
			String const & name2 = nil,
			String const & name3 = nil,
			String const & name4 = nil);

	public:
		std::vector<String> names;
		std::vector<String> exactNames;
		String desc;
		String args;
		String group;
	};


	template <typename Char>
	typename Keyword<Char>::String const Keyword<Char>::nil;


	namespace _private
	{
		static bool FitsAscii (wchar_t c)
		{
			return 0 <= c && c < 0x80;
		}

		static bool EqualsCI (char a, char b)
		{
			return std::tolower(a) == std::tolower(b);
		}

		static bool EqualsCI (wchar_t a, wchar_t b)
		{
			if (FitsAscii(a) && FitsAscii(b)) {
				return std::towlower(a) == std::towlower(b);
			}
			return a == b;
		}

		template <typename String>
		static bool EqualsCI (String const & a, String const & b)
		{
			size_t const N = a.size();
			if (N != b.size()) {
				return false;
			}
			for (size_t i = 0; i < N; ++i) {
				if (!EqualsCI(a[i], b[i])) {
					return false;
				}
			}
			return true;
		}

		template <typename StringIter, typename Char>
		static size_t SkipAll (StringIter & it, StringIter const & end, Char c)
		{
			size_t n = 0;
			while (it != end && *it == c) {
				++it;
				++n;
			}
			return n;
		}

		template <typename String>
		static bool MatchesName (MatchFlags flags, String const & arg, String const & name)
		{
			if (arg == name) {
				return true;
			}
			if (arg.empty() || name.empty()) {
				return false;
			}

			if (flags == MatchFlags::Empty) {
				return false;
			}
			if (flags == MatchFlags::IgnoreAsciiCase) {
				return EqualsCI(arg, name);
			}

			auto testFlags = [flags] (MatchFlags other) {
				return (flags & other) == other;
			};

			bool const ci = testFlags(MatchFlags::IgnoreAsciiCase);
			bool const rd = testFlags(MatchFlags::RelaxedDashes);
			bool const ru = testFlags(MatchFlags::RelaxedUnderscores);

			auto argIter = arg.begin();
			auto nameIter = name.begin();

			auto const argEnd = arg.end();
			auto const nameEnd = name.end();

			size_t const argDashes = SkipAll(argIter, argEnd, '-');
			size_t const nameDashes = SkipAll(nameIter, nameEnd, '-');

			if (argDashes != nameDashes) {
				if (argDashes < 2 || nameDashes < 2) {
					return false;
				}
			}

			if (argDashes == 1) {
				if (arg.size() <= 2 || name.size() <= 2) {
					if (ci) {
						return EqualsCI(arg, name);
					}
					return false;
				}
			}

			while (true) {
				if (rd) {
					SkipAll(argIter, argEnd, '-');
					SkipAll(nameIter, nameEnd, '-');
				}
				if (ru) {
					SkipAll(argIter, argEnd, '_');
					SkipAll(nameIter, nameEnd, '_');
				}
				if (argIter == argEnd || nameIter == nameEnd) {
					return argIter == argEnd && nameIter == nameEnd;
				}
				if (ci) {
					if (!EqualsCI(*argIter, *nameIter)) {
						return false;
					}
				}
				else {
					if (*argIter != *nameIter) {
						return false;
					}
				}
				++argIter;
				++nameIter;
			}
		}

		template <typename Char>
		class OptionsImpl;

		template <typename Char>
		class ParseContextImpl;
	}

	
	template <typename Char = char>
	class ParseContext {
		friend class Options<Char>;
	
		typedef std::basic_string<Char> String;
		typedef _private::OptionsImpl<Char> OptionsImpl;
		typedef _private::ParseContextImpl<Char> ParseContextImpl;
	
	public:
		ParseContext (ParseContext && other);
		ParseContext & operator= (ParseContext && other);
	
		void Run ()
		{
			impl->Run();
		}
	
		std::vector<String> const & Args () const
		{
			return impl->Args();
		}
	
	private:
		ParseContext (std::shared_ptr<OptionsImpl const> opts, std::vector<String> && args);
	
		ParseContext (ParseContext const & other);   // disable
		void operator= (ParseContext const & other); // disable
	
	private:
		std::unique_ptr<ParseContextImpl> impl;
	};


	namespace _private
	{
		template <typename Char>
		inline bool IsShort (std::basic_string<Char> const & name)
		{
			if (name.size() <= 1) {
				return true;
			}
			if (name.size() != 2) {
				return false;
			}
			Char const c = name.front();
			return c == '-' || c == '/';
		}


		class TypeKind {
		public:
			bool operator== (TypeKind const & other) const
			{
				return id == other.id;
			}

#ifdef RTTI_ENABLED
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
#else
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
#endif
		};
	}


	template <typename Char>
	class OptInfo {
	public:
		typedef std::function<ParseResult(_private::OpaqueValues &)> Callback;

		OptInfo (Keyword<Char> const & keyword, std::vector<_private::TypeKind> && typeKinds, Callback const & callback)
			: keyword(keyword)
			, typeKinds(std::move(typeKinds))
			, callback(callback)
		{}

		OptInfo (OptInfo && other)
			: keyword(std::move(other.keyword))
			, typeKinds(std::move(other.typeKinds))
			, callback(std::move(other.callback))
		{}

	public:
		Keyword<Char> keyword;
		std::vector<_private::TypeKind> typeKinds;
		Callback callback;
	};


	template <typename Func>
	struct FuncTraits : public FuncTraits<decltype(&Func::operator())> {};

	template <typename X, typename R>
	struct FuncTraits<R(X::*)() const> {
		enum { arity = 0 };
		struct Return { typedef R type; };
	};

	template <typename X, typename R, typename A>
	struct FuncTraits<R(X::*)(A) const> : FuncTraits<R(X::*)() const> {
		enum { arity = 1 };
		struct Arg0 { typedef A type; };
	};

	template <typename X, typename R, typename A, typename B>
	struct FuncTraits<R(X::*)(A, B) const> : FuncTraits<R(X::*)(A) const> {
		enum { arity = 2 };
		struct Arg1 { typedef B type; };
	};

	template <typename X, typename R, typename A, typename B, typename C>
	struct FuncTraits<R(X::*)(A, B, C) const> : FuncTraits<R(X::*)(A, B) const> {
		enum { arity = 3 };
		struct Arg2 { typedef C type; };
	};

	template <typename X, typename R, typename A, typename B, typename C, typename D>
	struct FuncTraits<R(X::*)(A, B, C, D) const> : FuncTraits<R(X::*)(A, B, C) const> {
		enum { arity = 4 };
		struct Arg3 { typedef D type; };
	};

	template <typename X, typename R, typename A, typename B, typename C, typename D, typename E>
	struct FuncTraits<R(X::*)(A, B, C, D, E) const> : FuncTraits<R(X::*)(A, B, C, D) const> {
		enum { arity = 5 };
		struct Arg4 { typedef E type; };
	};


	namespace _private
	{
		class TypeKind;


		template <typename Char>
		struct OpaqueParser {
			typedef UniqueOpaque (*Type)(ParseState<Char> &);
		};

		template <typename Char>
		struct DynamicParserMap {
			typedef std::vector<std::pair<TypeKind, typename OpaqueParser<Char>::Type>> Type;
		};


		template <typename T>
		struct ReturnType {
			static bool const allowed = false;
		};

		template <>
		struct ReturnType<void> {
			static bool const allowed = true;
		};

		template <>
		struct ReturnType<ParseResult> {
			static bool const allowed = true;
		};


		template <typename Char>
		class OptionsImpl {
		public:
			typedef std::basic_string<Char> String;
			typedef lambda_options::Keyword<Char> Keyword;


			OptionsImpl (OptionsConfig const & config)
				: config(config)
			{}


			String HelpDescription (FormattingConfig<Char> const & config) const;


			void SetGroupPriority (String const & group, Priority priority)
			{
				Priority * p = _private::Lookup(groupPriorities, group);
				if (p == nullptr) {
					groupPriorities.emplace_back(group, priority);
				}
				else {
					*p = priority;
				}
			}


			template <typename Func, size_t N>
			struct Adder {};


			template <typename Func>
			struct Adder<Func, 0> {
				static void Add (OptionsImpl & opts, Keyword const & keyword, Func const & f)
				{
					typedef typename FuncTraits<Func>::Return::type R;
					static_assert(ReturnType<R>::allowed, "Illegal return type.");
					opts.AddImpl(Tag<R>(), keyword, f);
				}
			};


			template <typename Func>
			struct Adder<Func, 1> {
				static void Add (OptionsImpl & opts, Keyword const & keyword, Func const & f)
				{
					typedef typename FuncTraits<Func>::Arg0::type A;
					typedef typename FuncTraits<Func>::Return::type R;
					static_assert(ReturnType<R>::allowed, "Illegal return type.");
					opts.AddImpl<A>(Tag<R>(), keyword, f);
				}
			};


			template <typename Func>
			struct Adder<Func, 2> {
				static void Add (OptionsImpl & opts, Keyword const & keyword, Func const & f)
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
				static void Add (OptionsImpl & opts, Keyword const & keyword, Func const & f)
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
				static void Add (OptionsImpl & opts, Keyword const & keyword, Func const & f)
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
				static void Add (OptionsImpl & opts, Keyword const & keyword, Func const & f)
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

			template <typename Func>
			void AddOption (Keyword const & keyword, Func const & f)
			{
				Keyword kw = MassageKeyword(keyword);
				Adder<Func, FuncTraits<Func>::arity>::Add(*this, kw, f);
			}


			Keyword MassageKeyword (Keyword const & proto)
			{
				Keyword kw(proto);
				std::vector<String> & names = kw.names;
				for (String & name : names) {
					switch (config.keywordStyle) {
						case KeywordStyle::Exact: {
							continue;
						} break;
						case KeywordStyle::Gnu: {
							if (name.size() == 1) {
								name.insert(0, 1, '-');
							}
							else if (name.size() == 2 && name[0] == '-') {
								continue;
							}
							else {
								while (!_private::IsPrefixOf("--", name)) {
									name.insert(0, 1, '-');
								}
							}
						} break;
						case KeywordStyle::Windows: {
							if (name.size() >= 1 && name[0] == '/') {
								continue;
							}
							name.insert(0, 1, '/');
						} break;
						default: {
							ASSERT(__LINE__, false);
						}
					}
				}

				std::vector<String> & exactNames = kw.exactNames;
				names.insert(names.end(), exactNames.begin(), exactNames.end());
				exactNames.clear();

				std::sort(names.begin(), names.end());
				auto it = std::unique(names.begin(), names.end());
				names.erase(it, names.end());

				return kw;
			}


			bool Intersecting (Keyword const & kw1, Keyword const & kw2, size_t & i, size_t & j) const
			{
				ASSERT(__LINE__, kw1.exactNames.empty());
				ASSERT(__LINE__, kw2.exactNames.empty());

				for (i = 0; i < kw1.names.size(); ++i) {
					String const & name = kw1.names[i];
					for (j = 0; j < kw2.names.size(); ++j) {
						String const & otherName = kw2.names[j];
						if (MatchesName(config.matchFlags, name, otherName)) {
							return true;
						}
					}
				}
				return false;
			}


			void NewInfo (Keyword const & keyword, std::vector<TypeKind> & typeKinds, typename OptInfo<Char>::Callback const & func, size_t arity)
			{
				if (infosByArity.size() <= arity) {
					infosByArity.resize(arity + 1);
				}
				auto & infos = infosByArity[arity];
				for (auto & info : infos) {
					size_t i;
					size_t j;
					if (Intersecting(keyword, info.keyword, i, j) && info.typeKinds == typeKinds) {
						throw OptionConflictException<Char>(keyword.names[i], info.keyword.names[j], arity);
					}
				}
				infos.emplace_back(keyword, std::move(typeKinds), func);
			}

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
					throw EmptyOptionException();
				}
				auto wrapper = [=] (OpaqueValues &) {
					return func();
				};
				std::vector<TypeKind> typeKinds;
				NewInfo(keyword, typeKinds, wrapper, 0);
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
				auto wrapper = [=] (OpaqueValues & vals) {
					A2 && a = ReifyOpaque<A2>(vals[0]);
					return func(std::forward<A>(a));
				};
				std::vector<TypeKind> typeKinds;
				PushTypeKind<A2>(typeKinds);
				NewInfo(keyword, typeKinds, wrapper, 1);
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
				auto wrapper = [=] (OpaqueValues & vals) {
					A2 && a = ReifyOpaque<A2>(vals[0]);
					B2 && b = ReifyOpaque<B2>(vals[1]);
					return func(std::forward<A>(a), std::forward<B>(b));
				};
				std::vector<TypeKind> typeKinds;
				PushTypeKind<A2>(typeKinds);
				PushTypeKind<B2>(typeKinds);
				NewInfo(keyword, typeKinds, wrapper, 2);
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
				auto wrapper = [=] (OpaqueValues & vals) {
					A2 && a = ReifyOpaque<A2>(vals[0]);
					B2 && b = ReifyOpaque<B2>(vals[1]);
					C2 && c = ReifyOpaque<C2>(vals[2]);
					return func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c));
				};
				std::vector<TypeKind> typeKinds;
				PushTypeKind<A2>(typeKinds);
				PushTypeKind<B2>(typeKinds);
				PushTypeKind<C2>(typeKinds);
				NewInfo(keyword, typeKinds, wrapper, 3);
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
				auto wrapper = [=] (OpaqueValues & vals) {
					A2 && a = ReifyOpaque<A2>(vals[0]);
					B2 && b = ReifyOpaque<B2>(vals[1]);
					C2 && c = ReifyOpaque<C2>(vals[2]);
					D2 && d = ReifyOpaque<D2>(vals[3]);
					return func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d));
				};
				std::vector<TypeKind> typeKinds;
				PushTypeKind<A2>(typeKinds);
				PushTypeKind<B2>(typeKinds);
				PushTypeKind<C2>(typeKinds);
				PushTypeKind<D2>(typeKinds);
				NewInfo(keyword, typeKinds, wrapper, 4);
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
				auto wrapper = [=] (OpaqueValues & vals) {
					A2 && a = ReifyOpaque<A2>(vals[0]);
					B2 && b = ReifyOpaque<B2>(vals[1]);
					C2 && c = ReifyOpaque<C2>(vals[2]);
					D2 && d = ReifyOpaque<D2>(vals[3]);
					E2 && e = ReifyOpaque<E2>(vals[4]);
					return func(std::forward<A>(a), std::forward<B>(b), std::forward<C>(c), std::forward<D>(d), std::forward<E>(e));
				};
				std::vector<TypeKind> typeKinds;
				PushTypeKind<A2>(typeKinds);
				PushTypeKind<B2>(typeKinds);
				PushTypeKind<C2>(typeKinds);
				PushTypeKind<D2>(typeKinds);
				PushTypeKind<E2>(typeKinds);
				NewInfo(keyword, typeKinds, wrapper, 5);
			}


			template <typename T>
			void AddDynamicParser ()
			{
				TypeKind typeKind = TypeKind::Get<T>();
				if (_private::Lookup(dynamicParserMap, typeKind) == nullptr) {
					auto parser = OpaqueParse<Char, T>;
					dynamicParserMap.emplace_back(std::move(typeKind), parser);
				}
			}


			typename OpaqueParser<Char>::Type LookupDynamicParser (TypeKind const & k) const
			{
				auto const * pParser = _private::Lookup(dynamicParserMap, k);
				ASSERT(__LINE__, pParser != nullptr);
				return *pParser;
			}


			template <typename T>
			void PushTypeKind (std::vector<TypeKind> & kinds)
			{
				kinds.push_back(TypeKind::Get<T>());
				AddDynamicParser<T>();
			}


		public:
			typename DynamicParserMap<Char>::Type dynamicParserMap;
			std::vector<std::pair<String, Priority>> groupPriorities;
			OptionsConfig config;
			std::vector<std::vector<OptInfo<Char>>> infosByArity;
		};


		template <typename Char>
		class ParseContextImpl {
			friend class ArgsIter<Char>;

			typedef lambda_options::_private::OptionsImpl<Char> OptionsImpl;
			typedef std::basic_string<Char> String;
			typedef typename String::const_iterator StringIter;

		private:
			ParseContextImpl (ParseContextImpl const &); // disable
			void operator= (ParseContextImpl const &);   // disable

		public:
			ParseContextImpl (std::shared_ptr<OptionsImpl const> opts, std::vector<String> && args)
				: opts(opts)
				, args(std::move(args))
				, begin(this->args.begin(), this->args.end(), this)
				, end(this->args.end(), this->args.end(), this)
				, iter(begin)
				, parseState(iter, end)
				, highestArgIndex(0)
			{}

			std::vector<String> const & Args () const
			{
				return args;
			}

			void Run ()
			{
				iter = begin;
				while (TryParse()) {
					continue;
				}
				if (iter == end) {
					return;
				}
				size_t currArgIndex = static_cast<size_t>(iter.iter - begin.iter);
				throw ParseFailedException(currArgIndex, highestArgIndex);
			}

		private:
			UniqueOpaque OpaqueParse (TypeKind const & typeKind)
			{
				auto parser = opts->LookupDynamicParser(typeKind);
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

			bool MatchKeyword (Keyword<Char> const & keyword)
			{
				if (keyword.names.empty()) {
					return true;
				}
				for (String const & name : keyword.names) {
					if (MatchesName(opts->config.matchFlags, *iter, name)) {
						++iter;
						return true;
					}
				}
				return false;
			}

			ParseResult TryParse (bool useKeyword, std::vector<OptInfo<Char>> const & infos)
			{
				if (infos.empty()) {
					return ParseResult::Reject;
				}

				auto const startIter = iter;

				for (auto const & info : infos) {
					size_t const arity = info.typeKinds.size();
					if (info.keyword.names.empty() == useKeyword) {
						continue;
					}
					iter = startIter;
					if (MatchKeyword(info.keyword)) {
						auto const & typeKinds = info.typeKinds;
						ASSERT(__LINE__, typeKinds.size() == arity);
						OpaqueValues parsedArgs = ParseArgs(typeKinds);
						if (parsedArgs.size() == arity) {
							ParseResult res = info.callback(parsedArgs);
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

			ParseResult TryParse (std::vector<std::vector<OptInfo<Char>>> const & infosByArity)
			{
				ParseResult res = ParseResult::Reject;
				bool const useKeywordState[] = { true, false };
				for (bool useKeyword : useKeywordState) {
					auto const end = infosByArity.rend();
					for (auto it = infosByArity.rbegin(); it != end; ++it) {
						auto & infos = *it;
						res = TryParse(useKeyword, infos);
						if (res != ParseResult::Reject) {
							return res;
						}
					}
				}
				return res;
			}

			bool TryParse ()
			{
				if (iter == end) {
					return false;
				}
				ParseResult res = TryParse(opts->infosByArity);
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
			std::shared_ptr<OptionsImpl const> opts;
			std::vector<String> args;
			ArgsIter<Char> const begin;
			ArgsIter<Char> const end;
			ArgsIter<Char> iter;
			ParseState<Char> parseState;
			size_t highestArgIndex;
		};
	}


	template <typename Char = char>
	class Options {
	private:
		typedef lambda_options::_private::OptionsImpl<Char> OptionsImpl;
		friend class ArgsIter<Char>;
		typedef std::basic_string<Char> String;

	public:
		typedef Char CharType;
		typedef String StringType;


		Options ()
			: impl(new OptionsImpl(OptionsConfig()))
		{}

		Options (OptionsConfig const & config)
			: impl(new OptionsImpl(config))
		{}


		template <typename Func>
		void AddOption (String const & keyword, Func const & func)
		{
			Keyword<Char> kw(keyword);
			impl->AddOption<Func>(kw, func);
		}

		template <typename Func>
		void AddOption (Keyword<Char> const & keyword, Func const & func)
		{
			impl->AddOption<Func>(keyword, func);
		}

		String HelpDescription () const
		{
			return impl->HelpDescription(FormattingConfig<Char>());
		}

		String HelpDescription (FormattingConfig<Char> const & config) const
		{
			return impl->HelpDescription(config);
		}

		void SetGroupPriority (String const & group, int priority)
		{
			impl->SetGroupPriority(group, priority);
		}

		template <typename StringIter>
		ParseContext<Char> CreateParseContext (StringIter begin, StringIter end) const;


	private:
		std::shared_ptr<OptionsImpl> impl;
	};


	namespace _private
	{
		template <typename Char>
		class Formatter {
			enum Phase { EmitName, EmitDesc };
			typedef std::basic_string<Char> String;

		public:
			Formatter (FormattingConfig<Char> const & config)
				: config(config)
			{
				this->config.maxWidth = std::max<size_t>(config.maxWidth, 30);
			}

			void FormatKeyword (Keyword<Char> const & keyword)
			{
				if (AllowGroup(keyword.group)) {
					width = 0;
					ChangeIndentation(0);
					NewLine();
					FormatKeywordNames(keyword);
					FormatKeywordArgs(keyword);
					FormatKeywordHelp(keyword);
					FlushWord();
				}
			}

			String ToString () const
			{
				return String(emittedChars.begin(), emittedChars.end());
			}

		private:
			bool AllowGroup (String const & group) const
			{
				return config.groupFilter.empty() 
					|| _private::Contains(config.groupFilter.begin(), config.groupFilter.end(), group);
			}

			void FormatKeywordNames (Keyword<Char> const & keyword)
			{
				std::vector<String> names = keyword.names;
				std::sort(names.begin(), names.end(), [] (String const & n1, String const & n2) {
					if (n1.size() < n2.size()) {
						return true;
					}
					if (n1.size() > n2.size()) {
						return false;
					}
					return n1 < n2;
				});

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

			void FormatKeywordArgs (Keyword<Char> const & keyword)
			{
				if (!keyword.args.empty()) {
					FlushWord();
					ChangeIndentation(width + 1);
					Emit(keyword.args);
				}
			}

			void FormatKeywordHelp (Keyword<Char> const & keyword)
			{
				ChangeIndentation(29);
				Emit(keyword.desc);
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
				FlushWord();
				indentation = newAmount;
				Indent();
			}

			void Indent (bool flushWord = true)
			{
				if (flushWord) {
					FlushWord();
				}
				if (width > indentation) {
					NewLine();
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

			void EmitSpace ()
			{
				if (FlushWord()) {
					if (width < config.maxWidth) {
						emittedChars.push_back(' ');
						++width;
					}
					else {
						NewLine();
					}
				}
			}

			void Emit (Char c)
			{
				switch (c) {
					case ' ': {
						EmitSpace();
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
			FormattingConfig<Char> config;
			std::vector<Char> emittedChars;
			std::vector<Char> word;
			Phase phase;
			size_t width;
			size_t indentation;
		};
	}


	template <typename Char>
	size_t ArgsIter<Char>::Index () const
	{
		using namespace _private;
		auto const & parseContext = *static_cast<ParseContextImpl<Char> const *>(opaqueParseContext);
		return std::distance(parseContext.begin.iter, iter);
	}


	template <typename Char>
	ArgsIter<Char> & ArgsIter<Char>::operator++ ()
	{
		using namespace _private;
		if (iter == end) {
			throw IteratorException("Cannot increment past 'end' iterator.");
		}
		++iter;
		auto & parseContext = *static_cast<ParseContextImpl<Char> *>(opaqueParseContext);
		parseContext.highestArgIndex = std::max(parseContext.highestArgIndex, Index());
		return *this;
	}


	template <typename Char>
	template <typename StringIter>
	ParseContext<Char> Options<Char>::CreateParseContext (StringIter begin, StringIter end) const
	{
		return ParseContext<Char>(impl, std::vector<String>(begin, end));
	}


	template <typename Char>
	FormattingConfig<Char>::FormattingConfig ()
		: maxWidth(80)
	{}


	template <typename Char>
	Keyword<Char>::Keyword (
		String const & name1,
		String const & name2,
		String const & name3,
		String const & name4)
	{
		String const * pNames[] = { &name1, &name2, &name3, &name4 };
		for (String const * pName : pNames) {
			if (pName == &nil) {
				break;
			}
			names.push_back(*pName);
		}
	}


	template <typename Char>
	ParseContext<Char>::ParseContext (std::shared_ptr<OptionsImpl const> opts, std::vector<String> && args)
		: impl(new ParseContextImpl(opts, std::move(args)))
	{}


	template <typename Char>
	ParseContext<Char>::ParseContext (ParseContext && other)
		: impl(std::move(other.impl))
	{}


	template <typename Char>
	ParseContext<Char> & ParseContext<Char>::operator= (ParseContext && other)
	{
		impl = std::move(other.impl);
	}


	namespace _private
	{
		template <typename Char>
		auto OptionsImpl<Char>::HelpDescription (FormattingConfig<Char> const & config) const -> String
		{
			std::vector<Keyword const *> keywords;
			for (auto const & infos : infosByArity) {
				for (auto const & info : infos) {
					keywords.push_back(&info.keyword);
				}
			}

			auto getPriority = [&] (String const & group) {
				Priority const * pPriority = _private::Lookup(groupPriorities, group);
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
				return p1 < p2;
			});

			Formatter<Char> formatter(config);
			for (Keyword const * keyword : keywords) {
				formatter.FormatKeyword(*keyword);
			}
			return formatter.ToString();
		}
	}


	namespace with_char
	{
		typedef lambda_options::ArgsIter<char> ArgsIter;
		typedef lambda_options::FormattingConfig<char> FormattingConfig;
		typedef lambda_options::Keyword<char> Keyword;
		typedef lambda_options::Options<char> Options;
		typedef lambda_options::ParseContext<char> ParseContext;
		typedef lambda_options::ParseState<char> ParseState;
	}
}






