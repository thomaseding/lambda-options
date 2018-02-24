### About

* `lambda-options` is a declarative argument parser:
*   * Use callbacks with typed arguments to dictate parsing.
* `lambda-options` is strongly typed:
*   * Misuse of callbacks leads to compiler errors:
* `lambda-options` is extensible:
*   * You can add your own typed parsers.

----

### Why

Typescript was lacking a good CLI argument parser. All the parsers I found were either weakly typed (usually from a vanilla JS project), didn't allow custom parsers, or proved difficult to use.

`lambda-options` was designed to solve all these problems.

----

### How

With `lambda-options`, you register fully-typed parse callbacks for pattern matching keywords and their arguments.

Example:
```typescript
import { Options } from "lambda-options";

const options = Options.createDefault();

// Matches ["--take", "5", "helloworld"]
options.addOption(
    "--take",           // Name of the keyword
    ["unsigned", "N"],  // [Type of 1st keyword argument, Argument help description name]
    ["string", "STR"],  // [Type of 2nd keyword argument, Argument help description name]
    "Takes the first N characters of STR.", // Help description
    (n: number,         // First argument. Strongly typed from "unsigned"
     s: string)         // Second argument. Strongly typed from "string"
     => {
        // Parse match action
        console.log(s.substr(0, n));
    }
);
```

The options structure that the option is registered to understands the types involved and will pattern match accordingly. For example, the above snippet will not match `["--take", "-1", "helloworld"]`.

Also note that if the callback had a different type for `n: number`, you will get a compiler error. Likewise for the `s: string` type.

A comprehensive list of built-in type parsers are listed later in this document.

If the built-in parsers are not sufficient for your needs, you can register your own parsers. These too are typesafe.

----

### Quickstart Example

```typescript
import { Options, Exception } from "lambda-options";

const options = Options.createDefault();

options.addOption(
    ["--help", "-h"],
    "Display this help message.",
    () => {
        console.log(options.getHelpDescription());
    }
);
options.addOption(
    "--take",
    ["unsigned", "NUM"],
    ["string", "STR"],
    "Takes the first NUM characters of STR.",
    (n: number, s: string) => {
        console.log(s.substr(0, n));
    }
);
options.addOption(
    "--sum",
    ["number", "NUM"],
    ["number[]", "NUMS"],
    "Takes the sum of 1 or more numbers.",
    (x: number, xs: number[]) => {
        console.log(xs.reduce((y, z) => y + z, x));
    }
);

declare const process: { argv: string[] };
const args = process.argv.slice(2);

const parseContext = options.createParseContext(args);

try {
    parseContext.run();
}
catch (e) {
    if (e instanceof Exception) {
        console.log(e.message);
        console.log(options.getHelpDescription());
    }
    else {
        console.log(e);
    }
}
```

----

### Registering custom type parsers.

With `Options.prototype.registerParser(typeName, parser)` you can make `lambda-options` aware of your own custom types.

Example A:
```typescript
import { Options, NoParse, parseNumber } from "lambda-options";

type Point = { x: number, y: number };

function pointParser(
    startIndex: number,
    args: string[])
    : [Point | NoParse, number]
{
    const [result1, consumed1] = parseNumber(startIndex, args);
    if (result1 === NoParse) {
        return [NoParse, consumed1];
    }
    startIndex += consumed1;

    const [result2, consumed2] = parseNumber(startIndex, args);
    if (result2 === NoParse) {
        return [NoParse, consumed1 + cosumed2];
    }

    const point = { x: result1, y: result2 };
    return [point, consumed1 + cosumed2];
}

const options = Options.createDefault()
    .registerParser("Point", pointParser);
```

There are serveral helper functions available too.

Example B:
```typescript
import {
    Options,
    NoParse,
    parseNumber,
    createFixedArrayParser,
} from "lambda-options";

type Point = { x: number, y: number };

const parseNumber2 = createFixedArrayParser("2", parseNumber);

function pointParser(
    startIndex: number,
    args: string[])
    : [Point | NoParse, number]
{
    const [result, consumed] = parseNumber2(startIndex, args);
    if (result === NoParse) {
        return [NoParse, consumed];
    }
    const point = { x: result[0], y: result[1] };
    return [point, consumed];
}

const options = Options.createDefault()
    .registerParser("Point", pointParser);
```

You can use `Options.prototype.registerParsers` to easily get `T?` and `T[]` semantics as well.

Example C:
```typescript
import {
    Options,
    NoParse,
    parseNumber,
    createFixedArrayParser,
} from "lambda-options";

type Point = { x: number, y: number };

const parseNumber2 = createFixedArrayParser("2", parseNumber);

function pointParser(
    startIndex: number,
    args: string[])
    : [Point | NoParse, number]
{
    const [result, consumed] = parseNumber2(startIndex, args);
    if (result === NoParse) {
        return [NoParse, consumed];
    }
    const point = { x: result[0], y: result[1] };
    return [point, consumed];
}

const options = Options.createDefault()
    .registerParsers("Point", "Point?", "Point[]", pointParser);
```

----

### Built-in parse types
* `"boolean"` → `boolean`
* `"boolean?"` → `boolean | undefined`
* `"boolean[]"` → `boolean[]`
* `"string"` → `string`
* `"string?"` → `string | undefined`
* `"string[]"` → `string[]`
* `"number"` → `number`
* `"number?"` → `number | undefined`
* `"number[]"` → `number[]`
* `"integer"` → `number`
* `"integer?"` → `number | undefined`
* `"integer[]"` → `number[]`
* `"unsigned"` → `number`
* `"unsigned?"` → `number | undefined`
* `"unsigned[]"` → `number[]`
* `"Int8"` → `number`
* `"Int8?"` → `number | undefined`
* `"Int8[]"` → `number[]`
* `"Int16"` → `number`
* `"Int16?"` → `number | undefined`
* `"Int16[]"` → `number[]`
* `"Int32"` → `number`
* `"Int32?"` → `number | undefined`
* `"Int32[]"` → `number[]`
* `"Uint8"` → `number`
* `"Uint8?"` → `number | undefined`
* `"Uint8[]"` → `number[]`
* `"Uint16"` → `number`
* `"Uint16?"` → `number | undefined`
* `"Uint16[]"` → `number[]`
* `"Uint32"` → `number`
* `"Uint32?"` → `number | undefined`
* `"Uint32[]"` → `number[]`
* `"Int8Array"` → `Int8Array`
* `"Int16Array"` → `Int16Array`
* `"Int32Array"` → `Int32Array`
* `"Uint8Array"` → `Uint8Array`
* `"Uint16Array"` → `Uint16Array`
* `"Uint32Array"` → `Uint32Array`

