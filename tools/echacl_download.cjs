// Download the ECHA Annex VI xlsx through a real (headless) browser so the
// Azure WAF JS challenge is solved automatically, then save to <out>.
// Usage: node echacl_download.cjs <out.xlsx>
// Exit 0 + valid file on success; non-zero with a clear message otherwise.
//
// Env overrides (paths specific to this machine; override when needed):
//   FCMSAFETY_CHROME   Chrome/Chromium executable path
//   FCMSAFETY_PW_MODULE  playwright-core module dir (must contain package.json)
const path = require("path");
const fs = require("fs");

function firstExisting(paths) {
  for (const p of paths) {
    try {
      if (p && fs.existsSync(p)) return p;
    } catch (e) {
      /* ignore */
    }
  }
  return "";
}

// Chrome: env override -> per-OS common locations -> the original dev
// machine's bundled path (kept last so that machine keeps working as-is).
const CHROME =
  process.env.FCMSAFETY_CHROME ||
  firstExisting([
    "C:\\Users\\13432\\.agent-browser\\browsers\\chrome-152.0.7977.75\\chrome.exe",
    "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
    "/Applications/Chromium.app/Contents/MacOS/Chromium",
    "/usr/bin/google-chrome",
    "/usr/bin/google-chrome-stable",
    "/usr/bin/chromium-browser",
    "/usr/bin/chromium",
  ]);

// playwright-core: env override -> resolvable from this script's own module
// tree / global install -> the original dev machine's path.
function resolvePwModule() {
  if (process.env.FCMSAFETY_PW_MODULE) return process.env.FCMSAFETY_PW_MODULE;
  try {
    // require.resolve() lands on the module entry file; requiring the entry
    // file directly is equivalent to requiring the module dir, but keep the
    // dir form for compatibility with require(PW_MODULE) below.
    return path.dirname(require.resolve("playwright-core"));
  } catch (e) {
    /* not installed here */
  }
  return firstExisting([
    "C:\\Users\\13432\\.workbuddy\\binaries\\node\\workspace\\node_modules\\playwright-core",
  ]);
}
const PW_MODULE = resolvePwModule();

if (!CHROME) {
  console.error(
    "Chrome/Chromium not found. Install it or set FCMSAFETY_CHROME to the " +
      "browser executable path, then retry."
  );
  process.exit(2);
}
if (!PW_MODULE) {
  console.error(
    "playwright-core not found. Install it (npm install playwright-core) or " +
      "set FCMSAFETY_PW_MODULE to the module directory, then retry."
  );
  process.exit(2);
}
const { chromium } = require(PW_MODULE);
const URL = "https://echa.europa.eu/information-on-chemicals/annex-vi-to-clp";

const out = process.argv[2];
if (!out) {
  console.error("usage: node echacl_download.cjs <out.xlsx>");
  process.exit(2);
}

async function main() {
  const browser = await chromium.launch({ executablePath: CHROME, headless: true });
  try {
    const ctx = await browser.newContext({
      userAgent:
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/152.0.0.0 Safari/537.36",
      locale: "en-US",
      acceptDownloads: true,
    });
    const page = await ctx.newPage();
    const resp = await page.goto(URL, { waitUntil: "domcontentloaded", timeout: 60000 });
    console.log("page status:", resp && resp.status());

    // Azure WAF JS challenge runs in the background and reloads the page.
    const passed = await page
      .waitForFunction(
        () => document.title.includes("harmonised entries") || document.title.includes("Annex VI"),
        null,
        { timeout: 90000 }
      )
      .then(() => true)
      .catch(() => false);
    console.log("WAF challenge passed:", passed);
    if (!passed) throw new Error("Azure WAF challenge did not clear within 90 s");

    // Wait (poll) for the annex_vi_clp download link to appear -- the ECHA
    // page renders it asynchronously.
    const hasLink = await page
      .waitForFunction(
        () =>
          Array.from(document.querySelectorAll("a")).some((a) => /annex_vi_clp/i.test(a.href)),
        null,
        { timeout: 45000 }
      )
      .then(() => true)
      .catch(() => false);
    console.log("download link visible:", hasLink);
    if (!hasLink) throw new Error("no annex_vi_clp download link found on the page");
    await page.waitForTimeout(2000);

    // Locate the newest annex_vi_clp download link (last match wins).
    const href = await page.evaluate(() => {
      const as = Array.from(document.querySelectorAll("a")).filter((a) =>
        /annex_vi_clp/i.test(a.href)
      );
      return as.length ? as[as.length - 1].href : null;
    });
    if (!href) throw new Error("no annex_vi_clp download link found on the page");
    console.log("download url:", href);

    // Click it and capture the browser download (cookie included automatically).
    const dlPromise = page.waitForEvent("download", { timeout: 120000 });
    await page.evaluate((h) => {
      const a = document.createElement("a");
      a.href = h;
      a.download = "";
      document.body.appendChild(a);
      a.click();
      a.remove();
    }, href);
    const download = await dlPromise;
    console.log("downloaded:", download.suggestedFilename());

    const tmp = await download.path();
    if (!tmp) throw new Error("download produced no file");
    fs.copyFileSync(tmp, out);
    const size = fs.statSync(out).size;
    const magic = fs.readFileSync(out).slice(0, 2).toString("latin1");
    if (size < 1000 || magic !== "PK") {
      fs.unlinkSync(out);
      throw new Error("downloaded file is not a valid xlsx (size=" + size + ", magic=" + magic + ")");
    }
    console.log("saved:", out, "(" + size + " bytes)");
  } finally {
    await browser.close();
  }
}

main().catch((e) => {
  console.error("DOWNLOAD FAILED:", e.message);
  process.exit(1);
});
