import { createRequire } from "node:module";
import { execFileSync } from "node:child_process";
import { existsSync, mkdtempSync, realpathSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const root = resolve(dirname(fileURLToPath(import.meta.url)), "..");
function findPackage() {
  const explicit = process.env.PI_PACKAGE_DIR;
  if (explicit && existsSync(join(explicit, "dist/index.js"))) return explicit;
  const launcher = realpathSync(execFileSync("which", ["pi"], { encoding: "utf8" }).trim());
  const candidates = [
    resolve(dirname(launcher), "../lib/node_modules/@earendil-works/pi-coding-agent"),
    resolve(dirname(launcher), "../libexec/lib/node_modules/@earendil-works/pi-coding-agent"),
    resolve(dirname(launcher), ".."),
  ];
  for (const candidate of candidates) if (existsSync(join(candidate, "dist/index.js"))) return candidate;
  const npmRoot = execFileSync("npm", ["root", "-g"], { encoding: "utf8" }).trim();
  const candidate = join(npmRoot, "@earendil-works/pi-coding-agent");
  if (existsSync(join(candidate, "dist/index.js"))) return candidate;
  throw new Error("Cannot locate installed pi; set PI_PACKAGE_DIR to the package directory.");
}
const pkg = findPackage();
const require = createRequire(join(pkg, "package.json"));
const aliases = {
  "@earendil-works/pi-coding-agent": join(pkg, "dist/index.js"),
  "@mariozechner/pi-coding-agent": join(pkg, "dist/index.js"),
  "@earendil-works/pi-tui": require.resolve("@earendil-works/pi-tui"),
  "@earendil-works/pi-ai": join(pkg, "node_modules/@earendil-works/pi-ai/dist/compat.js"),
  "@mariozechner/pi-ai": join(pkg, "node_modules/@earendil-works/pi-ai/dist/compat.js"),
  "typebox": require.resolve("typebox"),
};

if (!process.argv.includes("--unit") && !process.argv.includes("--bridge")) {
  const temporary = mkdtempSync(join(tmpdir(), "pi-agent-team-check-"));
  try {
    const paths = Object.fromEntries(Object.entries(aliases).map(([name, entry]) => [name, [entry.replace(/\.js$/, ".d.ts").replace(/\.mjs$/, ".d.mts")]]));
    paths.typebox = [join(pkg, "node_modules/typebox/build/index.d.mts")];
    writeFileSync(join(temporary, "tsconfig.json"), JSON.stringify({
      compilerOptions: { target: "ES2023", module: "ESNext", moduleResolution: "Bundler", strict: true,
        noEmit: true, skipLibCheck: true, types: ["node"], typeRoots: [join(pkg, "node_modules/@types")], paths },
      include: [join(root, "**/*.ts")],
    }, null, 2));
    execFileSync("tsc", ["-p", join(temporary, "tsconfig.json")], { stdio: "inherit" });
  } finally { rmSync(temporary, { recursive: true, force: true }); }
}

const { createJiti } = require("jiti");
const jiti = createJiti(import.meta.url, { alias: aliases, interopDefault: false });
const files = process.argv.includes("--unit") ? ["worker.test.ts", "mailbox.test.ts", "guard.test.ts", "keybindings.test.ts", "viewer.test.ts"]
  : process.argv.includes("--bridge") ? ["rpc-bridge.test.ts"]
  : ["worker.test.ts", "mailbox.test.ts", "guard.test.ts", "runtime.test.ts", "rpc-bridge.test.ts", "extension.test.ts", "keybindings.test.ts", "viewer.test.ts", "team-integration.test.ts"];
for (const file of files) if (existsSync(join(root, file))) await jiti.import(join(root, file));
