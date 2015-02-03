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


#include "cmdline/LambdaOpts.h"

#include <assert.h>


//////////////////////////////////////////////////////////////////////////


#define ASSERT(x) \
	assert(x)


#define UNREFERENCED(x) \
	(void)(x)


#define COUNTOF(x) \
	(sizeof(x) / sizeof(x[0]))


//////////////////////////////////////////////////////////////////////////


namespace
{
	template <typename Char>
	size_t StrLen (Char const * str)
	{
		size_t size = 0;
		while (*str++) {
			++size;
		}
		return size;
	}
}


namespace
{
	typedef void const * V;
	typedef std::vector<void const *> OpaqueArgs;		// TODO: Can it be <void const *> ?
}


namespace
{
	void Apply (std::function<void()> const & func, OpaqueArgs const & args)
	{
		UNREFERENCED(args);
		func();
	}


	void Apply (std::function<void(V)> const & func, OpaqueArgs const & args)
	{
		func(args[0]);
	}


	void Apply (std::function<void(V,V)> const & func, OpaqueArgs const & args)
	{
		func(args[0], args[1]);
	}


	void Apply (std::function<void(V,V,V)> const & func, OpaqueArgs const & args)
	{
		func(args[0], args[1], args[2]);
	}


	void Apply (std::function<void(V,V,V,V)> const & func, OpaqueArgs const & args)
	{
		func(args[0], args[1], args[2], args[3]);
	}


	void Apply (std::function<void(V,V,V,V,V)> const & func, OpaqueArgs const & args)
	{
		func(args[0], args[1], args[2], args[3], args[4]);
	}
}


namespace
{
	bool Scan (std::string const & str, char const * format, void * dest)
	{
		char dummy;
		return sscanf(str.c_str(), format, dest, &dummy) == 1;
	}


	bool Scan (std::wstring const & str, char const * format, void * dest)
	{
		wchar_t wformat[8];
		size_t len = strlen(format) + 1;
		ASSERT(len <= COUNTOF(wformat));
		for (size_t i = 0; i < len; ++i) {
			wformat[i] = format[i];
		}
		wchar_t dummy;
		return swscanf(str.c_str(), wformat, dest, &dummy) == 1;
	}
}


//////////////////////////////////////////////////////////////////////////


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
LambdaOpts<Char>::~LambdaOpts ()
{
	FreeParseAllocations();
}


template <typename Char>
size_t LambdaOpts<Char>::RemainingArgs () const
{
	ASSERT(argPos <= args.size());
	return args.size() - argPos;
}


template <typename Char>
void * LambdaOpts<Char>::Parse_int (String const & arg)
{
	int x;
	if (Scan(arg, "%d%c", &x)) {
		return Allocate<int>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::Parse_unsigned_int (String const & arg)
{
	unsigned int x;
	if (Scan(arg, "%u%c", &x)) {
		return Allocate<unsigned int>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::Parse_float (String const & arg)
{
	float x;
	if (Scan(arg, "%f%c", &x)) {
		return Allocate<float>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::Parse_double (String const & arg)
{
	double x;
	if (Scan(arg, "%lf%c", &x)) {
		return Allocate<double>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::Parse_Char (String const & arg)
{
	Char x;
	if (Scan(arg, "%c%c", &x)) {
		return Allocate<Char>(x);
	}
	return nullptr;
}


template <typename Char>
void * LambdaOpts<Char>::Parse_CString (String const & arg)
{
	void * x = Allocate_CString(arg.c_str());
	return x;
}


template <typename Char>
void * LambdaOpts<Char>::Parse_String (String const & arg)
{
	return Parse_CString(arg);
}


template <typename Char>
void * LambdaOpts<Char>::Parse (TypeKind type, String const & arg)
{
	switch (type) {
		case T_Int: return Parse_int(arg);
		case T_Uint: return Parse_unsigned_int(arg);
		case T_Float: return Parse_float(arg);
		case T_Double: return Parse_double(arg);
		case T_Char: return Parse_Char(arg);
		case T_CString: return Parse_CString(arg);
		case T_String: return Parse_String(arg);
	}
	ASSERT(false);
	return nullptr;
}


template <typename Char>
template <typename GenericOptInfo>
size_t LambdaOpts<Char>::TryParse (std::vector<GenericOptInfo> const & infos)
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


template <typename Char>
bool LambdaOpts<Char>::TryParse ()
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


template <typename Char>
template <typename T>
void * LambdaOpts<Char>::Allocate (T value) {
	char * p = new char[sizeof(T)];
	parseAllocations.push_back(p);
	memcpy(p, &value, sizeof(T));
	return p;
}


template <typename Char>
void * LambdaOpts<Char>::Allocate_CString (CString str)
{
	size_t size = sizeof(Char) * (StrLen(str) + 1);
	char * p = new char[size];
	parseAllocations.push_back(p);
	memcpy(p, str, size);
	return p;
}


template <typename Char>
void LambdaOpts<Char>::FreeParseAllocations ()
{
	for (char const * p : parseAllocations) {
		delete [] p;
	}
	parseAllocations.clear();
}


//////////////////////////////////////////////////////////////////////////


template class LambdaOpts<char>;
template class LambdaOpts<wchar_t>;


//////////////////////////////////////////////////////////////////////////

















