declare const process: { argv: string[] };
import { Options, Exception } from "./lambda-options";

const options = Options.createDefault();

options.addOption(
    [["--help"], "help"],
    "Display this help message.",
    () => {
        console.log(options.getHelpDescription());
    }
);
options.addOption(
    "--add",
    ["number", "X"],
    ["number", "Y"],
    "Adds X and Y.",
    (x: number, y: number) => {
        console.log(x + y);
    }
);
options.addOption(
    "--take",
    ["unsigned", "N"],
    ["string", "STR"],
    "Takes the first N characters of STR.",
    (n: number, s: string) => {
        console.log(s.substr(0, n));
    }
);

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

