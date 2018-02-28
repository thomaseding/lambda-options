import { Options, Exception } from "./lambda-options";

const options = Options.createDefault();
options.setHelpGroupPriority("help", -1);

options.addOption(
    ["--help", "-h"],
    "Display this help message.",
    () => {
        console.log(options.getHelpDescription({ maxWidth:80, enableDollarReplacement: true }));
    }
);
options.addOption(
    "--take",
    ["unsigned", "NUM"],
    ["string", "STR"],
    "Takes the first $1 characters of $2.",
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
    }
    else {
        console.log(e);
    }
}

