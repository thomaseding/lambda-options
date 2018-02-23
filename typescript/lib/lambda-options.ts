import { Nat, Z, S } from "nat-ts";
import { inplace } from "stable";
import QueryString from "query-string";

export { Nat, Z, S }


export namespace HelpConfig {
    export const minMaxWidth = 30;
}

export interface HelpConfig {
    maxWidth: number;
    helpGroupFilter?: string[];
}


function isShortName(
    name: string)
{
    if (name.length <= 1) {
        return true;
    }
    if (name.length != 2) {
        return false;
    }
    const c = name[0];
    return c === '-' || c === '/';
}


class Formatter {

    constructor(
        config: HelpConfig)
    {
        this._config = config;
        this._config.maxWidth = Math.max(config.maxWidth, HelpConfig.minMaxWidth);

        this._emittedChars = [];
        this._word = [];

        this._width = 0;
        this._indentation = 0;
    }

    public formatOption(
        option: Option)
        : void
    {
        if (this._allowHelpGroup(option.helpGroup)) {
            this._width = 0;
            this._changeIndentation(0);
            this._newLine();
            this._formatOptionNames(option);
            this._formatOptionArgsText(option);
            this._formatOptionText(option);
            this._flushWord();
        }
    }

    public toString()
        : string
    {
        return this._emittedChars.join("");
    }

    private _allowHelpGroup(
        helpGroup: string)
        : boolean
    {
        if (this._config.helpGroupFilter === undefined || this._config.helpGroupFilter.length === 0) {
            return true;
        }
        const idx = this._config.helpGroupFilter.indexOf(helpGroup);
        return idx >= 0;
    }

    private _formatOptionNames(
        option: Option)
        : void
    {
        const names = option.names;
        names.sort((n1: string, n2: string) => {
            if (n1.length < n2.length) {
                return -1;
            }
            if (n1.length > n2.length) {
                return 1;
            }
            return 0;
        });

        if (names.length > 0) {
            let idx = 0;
            if (isShortName(names[idx])) {
                this._changeIndentation(1);
                this._emitString(names[idx]);
                ++idx;
            }
            for ( ; idx < names.length; ++idx) {
                if (idx > 0) {
                    this._emitChar(",");
                }
                this._changeIndentation(5);
                this._emitString(names[idx]);
            }
        }
    }

    private _formatOptionArgsText(
        option: Option)
        : void
    {
        const argsText = option.argsText();
        if (argsText.length > 0) {
            this._flushWord();
            this._changeIndentation(this._width + 1);
            this._emitString(argsText);
        }
    }

    private _formatOptionText(
        option: Option)
        : void
    {
        this._flushWord();
        if (option.text.length > 0) {
            this._changeIndentation(this._width + 1);
            this._changeIndentation(HelpConfig.minMaxWidth - 1);
            this._emitString(option.text);
        }
    }

    private _flushWord()
        : boolean
    {
        if (this._word.length === 0) {
            return false;
        }
        if (!(this._width === this._indentation || this._word.length + this._width <= this._config.maxWidth)) {
            this._newLine(false);
        }
        for (const c of this._word) {
            this._emittedChars.push(c);
        }
        this._width += this._word.length;
        this._word.length = 0;
        return true;
    }

    private _changeIndentation(
        newAmount: number)
        : void
    {
        this._flushWord();
        this._indentation = newAmount;
        this._indent();
    }

    private _indent(
        flushWord: boolean = true)
        : void
    {
        if (flushWord) {
            this._flushWord();
        }
        if (this._width > this._indentation) {
            this._newLine();
            return;
        }
        const amount = this._indentation - this._width;
        for (let i = 0; i < amount; ++i) {
            this._emittedChars.push(" ");
        }
        this._width = this._indentation;
    }

    private _newLine(
        flushWord: boolean = true)
        : void
    {
        if (this._emittedChars.length > 0) {
            this._emittedChars.push("\n");
        }
        this._width = 0;
        this._indent(flushWord);
    }

    private _emitSpace()
        : void
    {
        if (this._flushWord()) {
            if (this._width < this._config.maxWidth) {
                this._emittedChars.push(" ");
                ++this._width;
            }
            else {
                this._newLine();
            }
        }
    }

    private _emitChar(
        c: string)
        : void
    {
        if (c === " ") {
            this._emitSpace();
        }
        else {
            console.assert(c.length === 1);
            this._word.push(c);
        }
    }

    private _emitString(
        str: string)
    {
        for (const c of str) {
            this._emitChar(c);
        }
    }


    private readonly _config: HelpConfig;
    private readonly _emittedChars: string[];
    private readonly _word: string[];
    private _width: number;
    private _indentation: number;

}


export interface FixedArray<T> {
    "1": [T];
    "2": [T, T];
    "3": [T, T, T];
    "4": [T, T, T, T];
    "5": [T, T, T, T, T];
    "6": [T, T, T, T, T, T];
    "7": [T, T, T, T, T, T, T];
    "8": [T, T, T, T, T, T, T, T];
    "9": [T, T, T, T, T, T, T, T, T];
   "10": [T, T, T, T, T, T, T, T, T, T];
   "11": [T, T, T, T, T, T, T, T, T, T, T];
   "12": [T, T, T, T, T, T, T, T, T, T, T, T];
   "13": [T, T, T, T, T, T, T, T, T, T, T, T, T];
   "14": [T, T, T, T, T, T, T, T, T, T, T, T, T, T];
   "15": [T, T, T, T, T, T, T, T, T, T, T, T, T, T, T];
   "16": [T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T];
   "17": [T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T];
   "18": [T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T];
   "19": [T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T];
   "20": [T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T, T];
}


export type Callback0 = () => void;

export type Callback1<
    TypeMap,
    A extends keyof TypeMap> = (
        a: TypeMap[A]) => void;

export type Callback2<
    TypeMap,
    A extends keyof TypeMap,
    B extends keyof TypeMap> = (
        a: TypeMap[A],
        b: TypeMap[B]) => void;

export type Callback3<
    TypeMap,
    A extends keyof TypeMap,
    B extends keyof TypeMap,
    C extends keyof TypeMap> = (
        a: TypeMap[A],
        b: TypeMap[B],
        c: TypeMap[C]) => void;

export type Callback4<
    TypeMap,
    A extends keyof TypeMap,
    B extends keyof TypeMap,
    C extends keyof TypeMap,
    D extends keyof TypeMap> = (
        a: TypeMap[A],
        b: TypeMap[B],
        c: TypeMap[C],
        d: TypeMap[D]) => void;

export type Callback5<
    TypeMap,
    A extends keyof TypeMap,
    B extends keyof TypeMap,
    C extends keyof TypeMap,
    D extends keyof TypeMap,
    E extends keyof TypeMap> = (
        a: TypeMap[A],
        b: TypeMap[B],
        c: TypeMap[C],
        d: TypeMap[D],
        e: TypeMap[E]) => void;

export type Callback6<
    TypeMap,
    A extends keyof TypeMap,
    B extends keyof TypeMap,
    C extends keyof TypeMap,
    D extends keyof TypeMap,
    E extends keyof TypeMap,
    F extends keyof TypeMap> = (
        a: TypeMap[A],
        b: TypeMap[B],
        c: TypeMap[C],
        d: TypeMap[D],
        e: TypeMap[E],
        f: TypeMap[F]) => void;

export type Callback7<
    TypeMap,
    A extends keyof TypeMap,
    B extends keyof TypeMap,
    C extends keyof TypeMap,
    D extends keyof TypeMap,
    E extends keyof TypeMap,
    F extends keyof TypeMap,
    G extends keyof TypeMap> = (
        a: TypeMap[A],
        b: TypeMap[B],
        c: TypeMap[C],
        d: TypeMap[D],
        e: TypeMap[E],
        f: TypeMap[F],
        g: TypeMap[G]) => void;

export type Callback8<
    TypeMap,
    A extends keyof TypeMap,
    B extends keyof TypeMap,
    C extends keyof TypeMap,
    D extends keyof TypeMap,
    E extends keyof TypeMap,
    F extends keyof TypeMap,
    G extends keyof TypeMap,
    H extends keyof TypeMap> = (
        a: TypeMap[A],
        b: TypeMap[B],
        c: TypeMap[C],
        d: TypeMap[D],
        e: TypeMap[E],
        f: TypeMap[F],
        g: TypeMap[G],
        h: TypeMap[H]) => void;

export type Callback9<
    TypeMap,
    A extends keyof TypeMap,
    B extends keyof TypeMap,
    C extends keyof TypeMap,
    D extends keyof TypeMap,
    E extends keyof TypeMap,
    F extends keyof TypeMap,
    G extends keyof TypeMap,
    H extends keyof TypeMap,
    I extends keyof TypeMap> = (
        a: TypeMap[A],
        b: TypeMap[B],
        c: TypeMap[C],
        d: TypeMap[D],
        e: TypeMap[E],
        f: TypeMap[F],
        g: TypeMap[G],
        h: TypeMap[H],
        i: TypeMap[I]) => void;

export type Callback10<
    TypeMap,
    A extends keyof TypeMap,
    B extends keyof TypeMap,
    C extends keyof TypeMap,
    D extends keyof TypeMap,
    E extends keyof TypeMap,
    F extends keyof TypeMap,
    G extends keyof TypeMap,
    H extends keyof TypeMap,
    I extends keyof TypeMap,
    J extends keyof TypeMap> = (
        a: TypeMap[A],
        b: TypeMap[B],
        c: TypeMap[C],
        d: TypeMap[D],
        e: TypeMap[E],
        f: TypeMap[F],
        g: TypeMap[G],
        h: TypeMap[H],
        i: TypeMap[I],
        j: TypeMap[J]) => void;


export type Description = string;


export type Keyword = string | string[] | [string[], string];


export type Arg<TypeName extends (string | [string])>
    = [TypeName, string]
    | { typeName: TypeName; argText: string; }
    ;


export interface NoParse {
    readonly __NoParse: void;
}

export const NoParse: NoParse = Object.create(null) as NoParse;


export type Parser<T> = (startIndex: number, args: string[]) => [T | NoParse, number];
export type SimpleParser<T> = (arg: string) => T | NoParse;


export function simpleParse<T>(
    parser: (s: string) => T | NoParse)
    : Parser<T>
{
    return (startIndex: number, args: string[]) => {
        if (args.length === startIndex) {
            return [NoParse, 0];
        }
        const result = parser(args[startIndex]);
        if (result === NoParse) {
            return [NoParse, 0];
        }
        return [result, 1];
    };
}


export function parseString(
    arg: string)
    : string | NoParse
{
    return arg;
}


export function parseBoolean(
    arg: string)
    : boolean | NoParse
{
    if (arg === "true") {
        return true;
    }
    if (arg === "false") {
        return false;
    }
    return NoParse;
}


export function parseNumber(
    arg: string)
    : number | NoParse
{
    const n = Number(arg);
    if (isNaN(n)) {
        return NoParse;
    }
    return n;
}


const isIntegerRegex = /^-?\d+$/;
const isUnsignedRegex = /^\d+$/;


export function parseInteger(
    arg: string)
    : number | NoParse
{
    if (isIntegerRegex.test(arg)) {
        return parseInt(arg, 10);
    }
    return NoParse;
}


export function parseUnsigned(
    arg: string)
    : number | NoParse
{
    if (isUnsignedRegex.test(arg)) {
        return parseInt(arg, 10);
    }
    return NoParse;
}


function createParseInt(
    bitCount: number)
    : SimpleParser<number>
{
    const max = 1 << (bitCount - 1);
    const min = -max;

    return (arg: string) => {
        const n = parseInteger(arg);
        if (n === NoParse || n >= max || n < min) {
            return NoParse;
        }
        return n;
    };
}


function createParseUint(
    bitCount: number)
    : SimpleParser<number>
{
    const max = 1 << bitCount;

    return (arg: string) => {
        const n = parseUnsigned(arg);
        if (n === NoParse || n >= max) {
            return NoParse;
        }
        return n;
    };
}


export const parseInt8 = createParseInt(8);
export const parseInt16 = createParseInt(16);
export const parseInt32 = createParseInt(32);

export const parseUint8 = createParseUint(8);
export const parseUint16 = createParseUint(16);
export const parseUint32 = createParseUint(32);


export function createOptionalParser<T>(
    parser: Parser<T>)
    : Parser<T | undefined>
{
    return (startIndex: number, args: string[]) => {
        const result = parser(startIndex, args) as [any, number];
        if (result[0] === NoParse) {
            result[0] = undefined;
            result[1] = 0;
        }
        return result;
    };
}


export function createFixedArrayParser<
    T,
    N extends keyof FixedArray<T>>(
    sizeLiteral: N,
    parser: Parser<T>)
    : Parser<FixedArray<T>[N]>
{
    const size = Number(sizeLiteral);

    return (startIndex: number, args: string[]) => {
        const parsedItems: T[] = [];

        let currIndex = startIndex;

        while (currIndex < args.length && parsedItems.length < size) {
            const [parsedItem, consumedCount] = parser(currIndex, args);
            currIndex += consumedCount;

            if (parsedItem === NoParse) {
                break;
            }
            parsedItems.push(parsedItem as T);
        }

        const totalConsumedCount = currIndex - startIndex;

        if (parsedItems.length === size) {
            return [parsedItems as any, totalConsumedCount];
        }

        return [NoParse, totalConsumedCount];
    };
}


export function createArrayParser<T>(
    parser: Parser<T>)
    : Parser<T[]>
{
    return (startIndex: number, args: string[]) => {
        const parsedItems: T[] = [];

        let currIndex = startIndex;
        let totalConsumedCount: number;

        while (currIndex < args.length) {
            const [parsedItem, consumedCount] = parser(currIndex, args);
            if (parsedItem === NoParse) {
                break;
            }
            parsedItems.push(parsedItem as T);
            currIndex += consumedCount;
        }

        totalConsumedCount = currIndex - startIndex;
        return [parsedItems, totalConsumedCount];
    };
}


export function createUnionParser<A, B>(
    parserA: Parser<A>,
    parserB: Parser<B>)
    : Parser<A | B>
{
    return (startIndex: number, args: string[]) => {
        const resultA = parserA(startIndex, args);
        if (resultA[0] !== NoParse) {
            return resultA;
        }
        const resultB = parserB(startIndex, args);
        resultB[1] = Math.max(resultA[1], resultB[1]);
        return resultB;
    };
}


function createParsePackedArray<PackedArray>(
    constructor: any,
    simpleParser: SimpleParser<any>)
    : Parser<PackedArray>
{
    const parser = createArrayParser(simpleParse(simpleParser));

    return (startIndex: number, args: string[]) => {
        const result = parser(startIndex, args) as [any, number];
        if (result[0] === NoParse) {
            return result;
        }
        result[0] = new constructor(result[0]);
        return result;
    };
}


export const parseInt8Array = createParsePackedArray<Int8Array>(Int8Array, parseInt8);
export const parseInt16Array = createParsePackedArray<Int16Array>(Int16Array, parseInt16);
export const parseInt32Array = createParsePackedArray<Int32Array>(Int32Array, parseInt32);

export const parseUint8Array = createParsePackedArray<Uint8Array>(Uint8Array, parseUint8);
export const parseUint16Array = createParsePackedArray<Uint16Array>(Uint16Array, parseUint16);
export const parseUint32Array = createParsePackedArray<Uint32Array>(Uint32Array, parseUint32);

//const parseFloat32Array = createParsePackedArray<Float32Array>(Float32Array, parseNumber);
//const parseFloat64Array = createParsePackedArray<Float64Array>(Float64Array, parseNumber);


class ArgInfo {

    constructor(
        typeName: string,
        argText: string)
    {
        this.typeName = typeName;
        this.argText = argText;
    }

    public readonly typeName: string;
    public readonly argText: string;

}


class Option {

    constructor(
        keywords: string[],
        argInfos: ArgInfo[],
        text: string,
        helpGroup: string,
        func: Function)
    {
        this.names = keywords;
        this.argInfos = argInfos;
        this.text = text;
        this.helpGroup = helpGroup;
        this.func = func;
    }

    public argsText()
        : string
    {
        return this.argInfos.map((info: ArgInfo) => {
            return info.argText;
        }).join(" ");
    }

    public readonly names: string[];
    public readonly argInfos: ArgInfo[];
    public readonly text: string;
    public readonly helpGroup: string;
    public readonly func: Function;

}


function isNameList(
    keyword: Keyword)
    : keyword is string[]
{
    return typeof keyword !== "string"
        && (keyword.length === 0
            || typeof keyword[0] === "string")
        ;
}


class OptionsImpl<TypeMap, N extends Nat> {

    private constructor() {}


    public static createRaw()
        : OptionsImpl<{}, Z>
    {
        return new OptionsImpl<{}, Z>();
    }


    public registerParser<TypeName extends string, Type>(
        typeName: TypeName,
        parser: Parser<Type>)
        : OptionsImpl<TypeMap & { [P in TypeName]: Type; }, S<N>>
    {
        if (this._parserMap.has(typeName)) {
            throw new Error();
        }
        this._parserMap.set(typeName, parser);
        return this as OptionsImpl<any, any>;
    }


    public getParser(
        typeName: string)
        : Parser<any> | undefined
    {
        return this._parserMap.get(typeName);
    }


    public addOption(
        keyword: Keyword,
        ...opaqueArgs: (Arg<string> | Description | Function)[])
        : void
    {
        const func = opaqueArgs.pop() as Function;
        const text = opaqueArgs.pop() as Description;

        const argInfos: ArgInfo[] = [];
        for (const opaqueArg of (opaqueArgs as Arg<string>[])) {
            let typeName: string;
            let argText: string;

            if (opaqueArg instanceof Array) {
                typeName = opaqueArg[0];
                argText = opaqueArg[1];
            }
            else {
                typeName = opaqueArg.typeName;
                argText = opaqueArg.argText;
            }

            const argInfo = new ArgInfo(typeName, argText);
            argInfos.push(argInfo);
        }

        const defaultHelpGroup = "";

        if (typeof keyword === "string") {
            keyword = [[keyword], defaultHelpGroup];
        }
        else if (isNameList(keyword)) {
            keyword = [keyword, defaultHelpGroup];
        }

        const names = keyword[0];
        const helpGroup = keyword[1];

        const option = new Option(names, argInfos, text, helpGroup, func);

        const arity = argInfos.length;
        console.assert(func.length <= arity);

        let options = this._optionsByArity.get(arity);
        if (options === undefined) {
            options = [];
            this._optionsByArity.set(arity, options);
            this._maxArity = Math.max(this._maxArity, arity);
        }

        // TODO: Check for conflicting keywords.
        options.push(option);
    }


    public forEachOption(
        func: (option: Option) => boolean)
        : boolean
    {
        for (let arity = 0; arity <= this._maxArity; ++arity) {
            const options = this._optionsByArity.get(arity);
            if (options !== undefined) {
                for (const option of options) {
                    if (func(option)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }


    public getHelpDescription(
        config: HelpConfig)
        : string
    {
        const options: Option[] = [];
        for (let arity = 0; arity <= this._maxArity; ++arity) {
            const opts = this._optionsByArity.get(arity);
            if (opts !== undefined) {
                for (const o of opts) {
                    options.push(o);
                }
            }
        }

        const getPriority = (helpGroup: string) => {
            const priority = this._helpGroupPriorities.get(helpGroup);
            if (priority === undefined) {
                return 0;
            }
            return priority;
        };

        inplace(options, (opt1: Option, opt2: Option) => {
            const g1 = opt1.helpGroup;
            const g2 = opt2.helpGroup;

            const p1 = getPriority(g1);
            const p2 = getPriority(g2);

            if (p1 < p2) {
                return -1;
            }
            if (p1 > p2) {
                return 1;
            }

            if (opt1.names.length === 0) {
                if (opt2.names.length === 0) {
                    return 0;
                }
                return -1;
            }
            if (opt2.names.length === 0) {
                return 1;
            }

            const n1 = opt1.names[0];
            const n2 = opt2.names[0];
            if (n1 < n2) {
                return -1;
            }
            if (n1 > n2) {
                return 1;
            }
            return 0;
        });
    
        const formatter = new Formatter(config);
        for (const option of options) {
            formatter.formatOption(option);
        }
        return formatter.toString();
    }


    public setHelpGroupPriority(
        helpGroup: string,
        priority: number)
        : void
    {
        this._helpGroupPriorities.set(helpGroup, priority);
    }


    protected readonly __Options!: void & N;

    private readonly _parserMap = new Map<string, Parser<any>>();
    private readonly _helpGroupPriorities = new Map<string, number>();
    private readonly _optionsByArity = new Map<number, Option[]>();
    private _maxArity = 0;

}


export class Exception extends Error {

    public static create(
        beginIndex: number,
        endIndex: number,
        args: string[])
        : Exception
    {
        const begin = args[beginIndex];
        let message: string;

        if (endIndex === beginIndex) {
            message = `Unknown option at index ${beginIndex}: \`${begin}'`;
        }
        else if (endIndex === args.length) {
            message = `Bad input for \`${begin}' at index ${endIndex}: End of input.`;
        }
        else {
            const end = args[endIndex];
            message = `Bad input for \`${begin}' at index ${endIndex}: \`${end}'`;
        }

        return new Exception(beginIndex, endIndex, message);
    }

    private constructor(
        beginIndex: number,
        endIndex: number,
        message: string)
    {
        super(message);
        Object.setPrototypeOf(this, Exception.prototype);

        this.beginArgsIndex = beginIndex;
        this.endArgsIndex = endIndex;
    }


    public readonly beginArgsIndex: number;
    public readonly endArgsIndex: number;

}


export class ParseContext {

    public constructor(
        impl: any,
        args: string[])
    {
        this._impl = impl;
        this._args = args.slice();
    }


    public run()
        : void
    {
        const error = this._tryParse();
        if (error === null) {
            return;
        }
        throw error;
    }


    private _tryParse()
        : Exception | null
    {
        const matchOptionKeyword = (option: Option, arg: string) => {
            if (option.names.length === 0) {
                return 0;
            }
            for (const name of option.names) {
                if (name === arg) {
                    return 1;
                }
            }
            return null;
        };

        let argsIndex = 0;
        let highArgsIndex = argsIndex;

        const parsedItemBuffer: any[] = [];
        const lazyCalls: (() => void)[] = [];

        while (argsIndex < this._args.length) {
            const parsedAnOption = this._impl.forEachOption((option: Option) => {
                const keywordConsumedCount = matchOptionKeyword(option, this._args[argsIndex]);
                if (keywordConsumedCount === null) {
                    return false;
                }

                let localArgsIndex = argsIndex + keywordConsumedCount;
                highArgsIndex = Math.max(highArgsIndex, localArgsIndex);

                for (let i = 0; i < option.argInfos.length; ++i) {
                    const argInfo = option.argInfos[i];
                    const parser = this._impl.getParser(argInfo.typeName)!;
                    const [parsedItem, consumedCount] = parser(localArgsIndex, this._args);
                    highArgsIndex = Math.max(highArgsIndex, localArgsIndex + consumedCount);
                    if (parsedItem === NoParse) {
                        return false;
                    }
                    else {
                        localArgsIndex += consumedCount;
                        parsedItemBuffer[i] = parsedItem;
                    }
                }

                const parsedItems = parsedItemBuffer.slice(0, localArgsIndex - argsIndex);
                lazyCalls.push(() => {
                    option.func.apply(null, parsedItems);
                });

                argsIndex = localArgsIndex;
                return true;
            });

            if (!parsedAnOption) {
                return Exception.create(argsIndex, highArgsIndex, this._args);
            }
        }

        console.assert(argsIndex === this._args.length);

        for (const lazyCall of lazyCalls) {
            lazyCall();
        }

        return null;
    }


    protected readonly __ParseContext!: void;
    private readonly _impl: OptionsImpl<any, any>;
    private readonly _args: string[];

}


export class Options<TypeMap, N extends Nat> {

    private constructor(
        impl: OptionsImpl<TypeMap, N>)
    {
        this._impl = impl;
    }


    public static createRaw()
        : Options<{}, Z>
    {
        const impl = OptionsImpl.createRaw();
        return new Options(impl);
    }


    public static createDefault()
    {
        return this.createRaw()
            .registerParsers("boolean", "boolean?", "boolean[]", parseBoolean)
            .registerParsers("string", "string?", "string[]", parseString)
            .registerParsers("number", "number?", "number[]", parseNumber)
            .registerParsers("integer", "integer?", "integer[]", parseInteger)
            .registerParsers("unsigned", "unsigned?", "unsigned[]", parseUnsigned)

            .registerParsers("Int8", "Int8?", "Int8[]", parseInt8)
            .registerParsers("Int16", "Int16?", "Int16[]", parseInt16)
            .registerParsers("Int32", "Int32?", "Int32[]", parseInt32)

            .registerParsers("Uint8", "Uint8?", "Uint8[]", parseUint8)
            .registerParsers("Uint16", "Uint16?", "Uint16[]", parseUint16)
            .registerParsers("Uint32", "Uint32?", "Uint32[]", parseUint32)

            .registerParser("Int8Array", parseInt8Array)
            .registerParser("Int16Array", parseInt16Array)
            .registerParser("Int32Array", parseInt32Array)

            .registerParser("Uint8Array", parseUint8Array)
            .registerParser("Uint16Array", parseUint16Array)
            .registerParser("Uint32Array", parseUint32Array)

            ;
    }


    public registerParsers<
        TypeName extends string,
        OptionalTypeName extends string,
        ArrayTypeName extends string,
        Type>(
        typeName: TypeName,
        optionalTypeName: OptionalTypeName,
        arrayTypeName: ArrayTypeName,
        simpleParser: SimpleParser<Type>)
        : Options<
            TypeMap &
            { [P in TypeName]: Type; } &
            { [P in OptionalTypeName]?: Type; } &
            { [P in ArrayTypeName]: Type[]; },
            S<S<S<N>>>>
    {
        if (optionalTypeName !== typeName + "?") {
            throw new TypeError(`Expected \`optionalTypeName' to be "${typeName}?" instead of "${optionalTypeName}"`);
        }
        if (arrayTypeName !== typeName + "[]") {
            throw new TypeError(`Expected \`optionalTypeName' to be "${typeName}[]" instead of "${optionalTypeName}"`);
        }

        const parser = simpleParse(simpleParser);

        const optionalParser = createOptionalParser(parser);
        const arrayParser = createArrayParser(parser);

        return this
            .registerParser(typeName, parser)
            .registerParser(optionalTypeName, optionalParser)
            .registerParser(arrayTypeName, arrayParser)
            ;
    }


    // XXX:
    // Is it possible to restrict [[TypeName]] to exactly one string type?
    // As it is, the type system does not prevent one from doing
    // [[ options.registerParser(String(Math.random()), simpleParse(id)); ]]
    // The problem with this is that [[ TypeName ==== string ]]
    // Even doing the following is not desirable:
    // [[ options.registerParser(s as ("hello" | "world"), simpleParse(id)); ]]
    // Fortunately this shouldn't be a problem in practice, as people are
    // usually going to type in string literals, as it is natural to do so.
    public registerParser<TypeName extends string, Type>(
        typeName: TypeName,
        parser: Parser<Type>)
        : Options<TypeMap & { [P in TypeName]: Type; }, S<N>>
    {
        if (this._impl === null) {
            throw new Error();
        }
        const oldImpl = this._impl;
        this._impl = null;
        const newImpl = oldImpl.registerParser(typeName, parser);
        return new Options(newImpl);
    }


    public setHelpGroupPriority(
        helpGroup: string,
        priority: number)
        : void
    {
        if (this._impl === null) {
            throw new Error();
        }
        this._impl.setHelpGroupPriority(helpGroup, priority);
    }


    public addOption(
        keyword: Keyword,
        text: Description,
        func: Callback0)
        : void;

    public addOption<
        A extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        text: Description,
        func: Callback1<TypeMap, A>)
        : void;

    public addOption<
        A extends keyof TypeMap,
        B extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        b: Arg<B>,
        text: Description,
        func: Callback2<TypeMap, A, B>)
        : void;

    public addOption<
        A extends keyof TypeMap,
        B extends keyof TypeMap,
        C extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        b: Arg<B>,
        c: Arg<C>,
        text: Description,
        func: Callback3<TypeMap, A, B, C>)
        : void;

    public addOption<
        A extends keyof TypeMap,
        B extends keyof TypeMap,
        C extends keyof TypeMap,
        D extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        b: Arg<B>,
        c: Arg<C>,
        d: Arg<D>,
        text: Description,
        func: Callback4<TypeMap, A, B, C, D>)
        : void;

    public addOption<
        A extends keyof TypeMap,
        B extends keyof TypeMap,
        C extends keyof TypeMap,
        D extends keyof TypeMap,
        E extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        b: Arg<B>,
        c: Arg<C>,
        d: Arg<D>,
        e: Arg<E>,
        text: Description,
        func: Callback5<TypeMap, A, B, C, D, E>)
        : void;

    public addOption<
        A extends keyof TypeMap,
        B extends keyof TypeMap,
        C extends keyof TypeMap,
        D extends keyof TypeMap,
        E extends keyof TypeMap,
        F extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        b: Arg<B>,
        c: Arg<C>,
        d: Arg<D>,
        e: Arg<E>,
        f: Arg<F>,
        text: Description,
        func: Callback6<TypeMap, A, B, C, D, E, F>)
        : void;

    public addOption<
        A extends keyof TypeMap,
        B extends keyof TypeMap,
        C extends keyof TypeMap,
        D extends keyof TypeMap,
        E extends keyof TypeMap,
        F extends keyof TypeMap,
        G extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        b: Arg<B>,
        c: Arg<C>,
        d: Arg<D>,
        e: Arg<E>,
        f: Arg<F>,
        g: Arg<G>,
        text: Description,
        func: Callback7<TypeMap, A, B, C, D, E, F, G>)
        : void;

    public addOption<
        A extends keyof TypeMap,
        B extends keyof TypeMap,
        C extends keyof TypeMap,
        D extends keyof TypeMap,
        E extends keyof TypeMap,
        F extends keyof TypeMap,
        G extends keyof TypeMap,
        H extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        b: Arg<B>,
        c: Arg<C>,
        d: Arg<D>,
        e: Arg<E>,
        f: Arg<F>,
        g: Arg<G>,
        h: Arg<H>,
        text: Description,
        func: Callback8<TypeMap, A, B, C, D, E, F, G, H>)
        : void;

    public addOption<
        A extends keyof TypeMap,
        B extends keyof TypeMap,
        C extends keyof TypeMap,
        D extends keyof TypeMap,
        E extends keyof TypeMap,
        F extends keyof TypeMap,
        G extends keyof TypeMap,
        H extends keyof TypeMap,
        I extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        b: Arg<B>,
        c: Arg<C>,
        d: Arg<D>,
        e: Arg<E>,
        f: Arg<F>,
        g: Arg<G>,
        h: Arg<H>,
        i: Arg<I>,
        text: Description,
        func: Callback9<TypeMap, A, B, C, D, E, F, G, H, I>)
        : void;

    public addOption<
        A extends keyof TypeMap,
        B extends keyof TypeMap,
        C extends keyof TypeMap,
        D extends keyof TypeMap,
        E extends keyof TypeMap,
        F extends keyof TypeMap,
        G extends keyof TypeMap,
        H extends keyof TypeMap,
        I extends keyof TypeMap,
        J extends keyof TypeMap>(
        keyword: Keyword,
        a: Arg<A>,
        b: Arg<B>,
        c: Arg<C>,
        d: Arg<D>,
        e: Arg<E>,
        f: Arg<F>,
        g: Arg<G>,
        h: Arg<H>,
        i: Arg<I>,
        j: Arg<J>,
        text: Description,
        func: Callback10<TypeMap, A, B, C, D, E, F, G, H, I, J>)
        : void;

    public addOption(
        keyword: Keyword,
        ...args: any[])
        : void
    {
        if (this._impl === null) {
            throw new Error();
        }
        if (this._spawnedParseContexts) {
            throw new Error();
        }
        this._impl.addOption(keyword, ...args);
    }


    public static queryStringToArgs(
        queryString: string,
        arrayFormat: "bracket" | "index" | "none" = "none")
        : string[]
    {
        const config = { arrayFormat: arrayFormat };
        const dict = QueryString.parse(queryString, config);
        const keys = Object.keys(dict);
        const args: string[] = [];
        for (const key of keys) {
            const value = dict[key] as string | string[];
            if (typeof value === "string") {
                args.push(key, value);
            }
            else {
                for (const s of value) {
                    args.push(key, s);
                }
            }
        }
        return args;
    }


    public createParseContext(
        args: string[])
        : ParseContext
    {
        if (this._impl === null) {
            throw new Error();
        }
        this._spawnedParseContexts = true;
        return new ParseContext(this._impl, args);
    }


    public getHelpDescription(
        config?: HelpConfig)
        : string
    {
        if (this._impl === null) {
            throw new Error();
        }
        if (config === undefined) {
            config = {
                maxWidth: 80,
                helpGroupFilter: [],
            };
        }
        return this._impl.getHelpDescription(config);
    }


    protected readonly __Options!: void;
    private _impl: OptionsImpl<TypeMap, N> | null;
    private _spawnedParseContexts = false;

}

