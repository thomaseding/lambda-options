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
#include <string>
#include <vector>


//////////////////////////////////////////////////////////////////////////


template <typename Char>
class LambdaOpts {
	enum TypeKind { T_Int, T_Uint, T_Float, T_Double, T_Char, T_CString, T_String };

	typedef void const * V;

	typedef Char const * CString;
	typedef std::basic_string<Char> String;

	template <typename FuncSig>
	struct OptInfo {
		String option;
		std::vector<TypeKind> types;
		std::function<FuncSig> callback;
	};

private:
	static TypeKind GetTypeKind (int *) { return T_Int; }
	static TypeKind GetTypeKind (unsigned int *) { return T_Uint; }
	static TypeKind GetTypeKind (float *) { return T_Float; }
	static TypeKind GetTypeKind (double *) { return T_Double; }
	static TypeKind GetTypeKind (Char *) { return T_Char; }
	static TypeKind GetTypeKind (CString *) { return T_CString; }
	static TypeKind GetTypeKind (String *) { return T_String; }

	// This code is suboptimal compared to struct template specialization, but VC11 is buggy.
	template <typename T>
	static TypeKind GetTypeKind ()
	{
		return GetTypeKind(static_cast<T *>(nullptr));
	}

private:
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

private:
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

private:
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

private:
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

public:
	~LambdaOpts ();
	LambdaOpts (int argc, Char const * const argv[]);

	template <typename Func>
	void Add (String option, Func f)
	{
		Adder<Func, FuncTraits<Func>::arity>::Add(*this, option, f);
	}

	bool Parse (int & parseFailureIndex);
	String const & Program () const;
	bool Peek (String & outArg) const ;
	bool Next ();

private:
	size_t RemainingArgs () const;

	void * Parse_int (String const & arg);
	void * Parse_unsigned_int (String const & arg);
	void * Parse_float (String const & arg);
	void * Parse_double (String const & arg);
	void * Parse_Char (String const & arg);
	void * Parse_CString (String const & arg);
	void * Parse_String (String const & arg);
	void * Parse (TypeKind type, String const & arg);

	template <typename GenericOptInfo>
	size_t TryParse (std::vector<GenericOptInfo> const & infos);

	bool TryParse ();

private:
	template <typename T>
	void * Allocate (T value);

	void * Allocate_CString (CString str);

	void FreeParseAllocations ();

private:
	String program;
	std::vector<String> args;
	size_t argPos;

	std::vector<char const *> parseAllocations;

	std::vector<OptInfo<void()>> infos0;
	std::vector<OptInfo<void(V)>> infos1;
	std::vector<OptInfo<void(V,V)>> infos2;
	std::vector<OptInfo<void(V,V,V)>> infos3;
	std::vector<OptInfo<void(V,V,V,V)>> infos4;
	std::vector<OptInfo<void(V,V,V,V,V)>> infos5;
};



















