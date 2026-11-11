// The browser application against a real server, in headless Chromium with
// real pointer and key events:
//   node test/web_app_test.mjs FY_BINARY WEB_DIR [SCREENSHOT.png]
import { spawn, execFileSync } from 'node:child_process';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

const [fy, web, shot] = process.argv.slice(2);
const data = mkdtempSync(join(process.env.FY_TEST_TMP || tmpdir(), 'fy-web-test-'));
const init = execFileSync(fy, ['serve', '--init', '--data', join(data, 'd')]).toString();
const admin = init.trim().split('\n').pop();
const port = 20000 + Math.floor(Math.random() * 20000);
// FY_TEST_BASE=/yard runs everything below a path prefix, as behind a proxy.
const base = process.env.FY_TEST_BASE || '';
const server = spawn(fy, ['serve', '--data', join(data, 'd'), '--listen', `127.0.0.1:${port}`,
  '--web', web, '--workers', '1', '--threads', '4', '--base', base], { stdio: 'ignore' });
const url = `http://127.0.0.1:${port}${base}/`;

const browser = spawn(process.env.CHROME || 'chromium', ['--headless=new',
  '--remote-debugging-port=0', '--no-first-run', '--window-size=1280,800',
  `--user-data-dir=${join(data, 'chrome')}`, 'about:blank'], { stdio: ['ignore', 'ignore', 'pipe'], detached: true });
const wsUrl = await new Promise((resolve, reject) => {
  let err = '';
  browser.stderr.on('data', (d) => {
    err += d;
    const m = err.match(/ws:\/\/[^\s]+/);
    if (m) resolve(m[0]);
  });
  setTimeout(() => reject(new Error('browser did not start')), 15000);
});
const targets = await (await fetch(wsUrl.replace('ws://', 'http://').replace(/\/devtools.*/, '/json'))).json();
const ws = new WebSocket(targets.find((t) => t.type === 'page').webSocketDebuggerUrl);
await new Promise((r) => ws.addEventListener('open', r));
let seq = 0;
const pending = new Map();
const logs = [];
ws.addEventListener('message', (e) => {
  const msg = JSON.parse(e.data);
  if (msg.id && pending.has(msg.id)) { pending.get(msg.id)(msg); pending.delete(msg.id); }
  if (msg.method === 'Runtime.consoleAPICalled') logs.push(msg.params.args.map((a) => a.value).join(' '));
  if (msg.method === 'Runtime.exceptionThrown') logs.push('exception: ' + msg.params.exceptionDetails.text);
});
const send = (method, params = {}) => new Promise((resolve) => {
  const id = ++seq;
  pending.set(id, resolve);
  ws.send(JSON.stringify({ id, method, params }));
});
const evaluate = async (expr) => (await send('Runtime.evaluate', { expression: expr, returnByValue: true })).result.result.value;
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));
async function until(expr, what) {
  for (let i = 0; i < 100; i++) {
    if (await evaluate(expr)) return;
    await sleep(50);
  }
  throw new Error('timed out waiting for ' + what);
}
async function click(selector) {
  const [x, y] = await evaluate(`(() => { const r = document.querySelector(${JSON.stringify(selector)}).getBoundingClientRect();
    return [r.left + r.width / 2, r.top + r.height / 2]; })()`);
  for (const type of ['mouseMoved', 'mousePressed', 'mouseReleased']) {
    await send('Input.dispatchMouseEvent', { type, x, y, button: 'left', clickCount: 1 });
    await sleep(30);
  }
}
async function type(text) {
  for (const ch of text) {
    await send('Input.dispatchKeyEvent', { type: 'keyDown', key: ch, text: ch });
    await send('Input.dispatchKeyEvent', { type: 'keyUp', key: ch });
  }
}
const hasText = (t) => `[...document.querySelectorAll('.fui')].some((e) => e.textContent === ${JSON.stringify(t)})`;

let failed = 0;
const check = (ok, what) => { console.log((ok ? 'PASS ' : 'FAIL ') + what); if (!ok) failed++; };
// The location below the base, as the application sees it.
const path = async () => ((await evaluate('location.pathname + location.search')).slice(base.length) || '/');
const bg = () => evaluate(`getComputedStyle(document.body).backgroundColor`);
const scheme = (value) => send('Emulation.setEmulatedMedia', { features: [{ name: 'prefers-color-scheme', value }] });
const linkTo = (href) => `a[href=${JSON.stringify(base + href)}]`;
const noOverflow = `document.documentElement.scrollWidth <= document.documentElement.clientWidth`;

try {
  await send('Runtime.enable');
  await send('Page.enable');
  await send('Network.enable');
  // Marks the server's elements once the HTML is parsed, before the
  // WebAssembly composes its first frame.
  await send('Page.addScriptToEvaluateOnNewDocument', { source: `document.addEventListener('DOMContentLoaded', () => {
    document.querySelectorAll('[data-key]').forEach((e) => { e.__ssr = 1; });
    window.__ssrCount = document.querySelectorAll('[data-key]').length;
    window.__ssrTexts = [...document.querySelectorAll('.fui')].map((e) => e.textContent);
    window.__ssrBoxes = [...document.querySelectorAll('[data-box]')].map((e) => e.dataset.box); });` });
  const adopted = async () => {
    await until(`window.Module && typeof Module._fortui_web_step === 'function'`, 'WebAssembly running');
    await sleep(500);
    const ok = await evaluate(`(() => { const all = [...document.querySelectorAll('[data-key]')];
      return all.length === window.__ssrCount && all.every((e) => e.__ssr === 1); })()`);
    if (!ok) console.log('  not adopted: ' + await evaluate(`[...document.querySelectorAll('[data-key]')]
      .filter((e) => e.__ssr !== 1).map((e) => e.dataset.key).slice(0, 8).join(' ') + ' | server elements: ' + window.__ssrCount
      + ', now: ' + document.querySelectorAll('[data-key]').length`));
    return ok;
  };
  const load = async (to) => {
    await send('Page.navigate', { url: url + to.replace(/^\//, '') });
    await until(`window.__ssrCount > 0 && document.readyState !== 'loading'`, 'page parsed');
  };
  await scheme('light');
  await load('/');
  check(await evaluate(`!!document.querySelector('[data-id$="/token"]')`), 'the server sends the sign-in form in the page');
  check(await evaluate(`!/serif/.test(getComputedStyle(document.querySelector('.fui-k1')).fontFamily.replace('sans-serif', ''))`),
    'the page sets its font');
  check(await adopted(), 'the WebAssembly adopts the page from the server');

  // Pasted, as people sign in.
  await click('[data-id$="/token"]');
  // The field is native: the browser inserts the text as on paste.
  await send('Input.insertText', { text: admin });
  await until(`document.querySelector('[data-id$="/token"]').value === ${JSON.stringify(admin)}`, 'pasted token');
  check(true, 'a pasted token stays in the field');
  await click('[data-id$="/signin"]');
  await until(hasText('admin'), 'signed in');
  check(true, 'signing in with a token shows the account');
  const cookies = (await send('Network.getCookies', { urls: [url] })).result.cookies;
  const session = cookies.find((c) => c.name === 'fy_session');
  check(session && session.httpOnly && session.sameSite === 'Strict', 'the session is an HttpOnly, SameSite=Strict cookie');
  check(await evaluate(`!document.cookie.includes('fy_session')`), 'scripts on the page cannot read it');

  // The drawer leads to every page; each page has its own address and title.
  await click('[data-id$="/menu"]');
  await until(`!!document.querySelector('[data-box="drawer"]')`, 'drawer');
  await click(`[data-box="drawer"] ${linkTo('/server')}`);
  await until(hasText('Version'), 'server page');
  check(await path() === '/server' && await evaluate(`document.title`) === 'Server · fortyard' &&
    !await evaluate(`!!document.querySelector('[data-box="drawer"]')`), 'a drawer link goes to its page and closes the drawer');
  check(await evaluate(hasText((await (await fetch(url + 'api/v1/version')).json()).commit)),
    'the server page shows the server commit');

  await click('[data-id$="/menu"]');
  await click(`[data-box="drawer"] ${linkTo('/tokens')}`);
  await until(`!!document.querySelector('[data-id$="/newname"]')`, 'tokens page');
  await until(hasText('this browser'), 'session list');
  check(await evaluate(hasText('bootstrap')), 'the token list shows the bootstrap token');
  await click('[data-id$="/newname"]');
  await type('laptop');
  await click('[data-id$="/create"]');
  await until(`[...document.querySelectorAll('.fui-k1')].some((e) => /^fy_[0-9a-f]{64}$/.test(e.textContent))`, 'new token');
  await until(hasText('laptop'), 'token list with the new token');
  check(true, 'a created token is shown once and listed');
  await click('[data-box="card-tokens"] [data-id*="/revoke#"]');
  await until(`!(${hasText('laptop')})`, 'revocation');
  check(await evaluate(hasText('bootstrap')), 'revoking a token removes it from the list');

  // A mirrored repository: registered through the API, its mirror cloned
  // from a local repository (the registered URL is never reached).
  const up = join(data, 'up');
  const g = ['-c', 'user.name=T', '-c', 'user.email=t@example.org', '-C', up];
  execFileSync('git', ['init', '-q', '-b', 'main', up]);
  writeFileSync(join(up, 'hello.f90'), 'program hello\n  print *, "hi"\nend program\n');
  writeFileSync(join(up, 'README.md'), '# Demo\n\nA *small* demo.\n');
  execFileSync('git', [...g, 'add', 'hello.f90', 'README.md']);
  execFileSync('git', [...g, 'commit', '-q', '-m', 'Add hello']);
  const added = await fetch(url + 'api/v1/repos', { method: 'POST',
    headers: { Cookie: `fy_session=${session.value}`, 'Content-Type': 'application/json' },
    body: JSON.stringify({ name: 'demo', upstream: 'https://example.invalid/demo.git' }) });
  check(added.status === 201, 'an administrator registers a repository');
  execFileSync('git', ['clone', '-q', '--mirror', up, join(data, 'd', 'repos', 'demo.git')]);
  await click('[data-id$="/home"]');
  await until(`!!document.querySelector(${JSON.stringify(linkTo('/demo'))})`, 'repository listed');
  check(await path() === '/', 'the logo leads to the repositories');
  await click(linkTo('/demo'));
  await until(`!!document.querySelector(${JSON.stringify(linkTo('/demo/-/blob/hello.f90'))})`, 'tree');
  await until(hasText('Demo'), 'README');
  check(await path() === '/demo', 'a repository is at its path');
  check(await evaluate(`getComputedStyle([...document.querySelectorAll('.fui')].find((e) => e.textContent === 'Demo')).fontSize`) === '32px',
    'README headings have their own sizes');
  await click(linkTo('/demo/-/blob/hello.f90'));
  await until(`[...document.querySelectorAll('[data-style="5"]')].some((e) => e.textContent.endsWith('  print *, "hi"') && e.querySelector('span[data-t="5"]')?.textContent === 'print')`, 'file');
  check(await path() === '/demo/-/blob/hello.f90', 'a file of the repository is shown as code at its own address');
  await click('[data-id$="repotabs/history"]');
  await until(`!!document.querySelector('[data-box^="row-c"] [data-id$="/id"]')`, 'history');
  check(await evaluate(hasText('Add hello')) && await path() === '/demo/-/commits', 'the history lists the commit');
  await click('[data-box^="row-c"] [data-id$="/id"]');
  await until(`[...document.querySelectorAll('[data-tone="11"]')].some((e) => e.textContent.endsWith('+ program hello'))`, 'diff');
  check((await path()).startsWith('/demo/-/commit/'), 'the commit shows its diff with added lines marked');
  const commitPath = await path();

  // Back and Forward move between the pages, from what is already loaded.
  let requests = 0;
  const count = (e) => { const m = JSON.parse(e.data); if (m.method === 'Network.requestWillBeSent') requests++; };
  ws.addEventListener('message', count);
  await evaluate('history.back()');
  await until(`location.pathname.slice(${base.length}) === '/demo/-/commits' && ${hasText('Add hello')}`, 'back to the history');
  await evaluate('history.back()');
  await until(`location.pathname.slice(${base.length}) === '/demo/-/blob/hello.f90' && [...document.querySelectorAll('[data-style="5"]')].some((e) => e.textContent.endsWith('end program'))`, 'back to the file');
  check(requests === 0, 'Back shows the history and the file again without asking the server');
  await evaluate('history.forward()');
  await until(`location.pathname.slice(${base.length}) === '/demo/-/commits'`, 'forward');
  ws.removeEventListener('message', count);
  check(true, 'Forward goes to the next page again');

  // Every page can be bookmarked: a reload of a deep address renders it.
  await load(commitPath);
  check(await evaluate(`window.__ssrTexts.some((t) => t.endsWith('+ program hello'))`) &&
    await evaluate(`document.title`) === 'Add hello · demo@' + commitPath.slice(-40, -33) + ' · fortyard',
    'a reload of a commit address renders that commit straight from the server');
  check(await adopted(), 'and the WebAssembly adopts that page');
  await load('/demo/-/blob/README.md');
  check(await evaluate(`window.__ssrTexts.includes('Demo') && !window.__ssrTexts.some((t) => t.includes('# Demo'))`),
    'a Markdown file is shown rendered');
  await click(linkTo('/demo/-/blob/README.md?plain=1'));
  await until(`[...document.querySelectorAll('[data-style="5"]')].some((e) => e.textContent.endsWith('# Demo'))`, 'code view');
  check(true, 'and as code on request');
  check(await evaluate(`document.querySelector(${JSON.stringify(linkTo('/demo/-/blob/README.md'))}).tagName === 'A'`),
    'links are real links, so a middle click opens a new tab');
  await load('/demo/-/blob/nothing.f90');
  check(await evaluate(hasText('This file does not exist.')), 'a missing file says so');

  // The drawer can be pinned beside the page; the server knows it on reload.
  await send('Emulation.setDeviceMetricsOverride', { width: 1280, height: 800, deviceScaleFactor: 1, mobile: false });
  await load('/demo');
  await adopted();
  await click('[data-id$="/menu"]');
  await click('[data-id$="drawer/pin"]');
  await until(`!!document.querySelector('[data-box="sidebar"]')`, 'pinned sidebar');
  check(await evaluate(`getComputedStyle(document.querySelector('[data-box="sidebar"]')).position`) === 'sticky',
    'a pinned sidebar stands beside the page');
  await load('/demo');
  check(await evaluate(`window.__ssrBoxes.includes('sidebar')`), 'the server renders the pinned sidebar after a reload');
  check(await adopted(), 'and the WebAssembly adopts it');

  // Light and dark follow the system while the page is open, unless chosen.
  await scheme('dark');
  await until(`getComputedStyle(document.body).backgroundColor === 'rgb(13, 17, 23)'`, 'dark');
  await scheme('light');
  await until(`getComputedStyle(document.body).backgroundColor === 'rgb(255, 255, 255)'`, 'light');
  check(true, 'the theme switches with the system at once');
  await click('[data-box="sidebar"] [data-id$="/theme#3"]');
  await until(`document.documentElement.dataset.theme === 'dark'`, 'dark chosen');
  check(await bg() === 'rgb(13, 17, 23)', 'a chosen dark theme holds while the system is light');
  await load('/demo');
  check(await evaluate(`document.documentElement.dataset.theme === 'dark'`), 'the server applies the chosen theme');
  await click('[data-box="sidebar"] [data-id$="/theme#1"]');
  await until(`!document.documentElement.dataset.theme`, 'system theme');

  // Phones: no sideways scrolling; the navigation is an overlay drawer.
  await send('Emulation.setDeviceMetricsOverride', { width: 375, height: 740, deviceScaleFactor: 2, mobile: true });
  let wide = [];
  for (const to of ['/', '/demo', commitPath, '/demo/-/commits', '/tokens', '/server', '/demo/-/blob/hello.f90']) {
    await load(to);
    await adopted();
    if (!await evaluate(noOverflow)) wide.push(to);
  }
  check(wide.length === 0, 'no page scrolls sideways on a phone' + (wide.length ? ': ' + wide.join(' ') : ''));
  check(await evaluate(`getComputedStyle(document.querySelector('[data-box="sidebar"]')).display`) === 'none',
    'a pinned sidebar gives way to the page on a phone');
  await click('[data-id$="/menu"]');
  await until(`!!document.querySelector('[data-box="drawer"]')`, 'phone drawer');
  check(await evaluate(`getComputedStyle(document.querySelector('[data-box="drawer"]')).position`) === 'fixed',
    'the menu opens the drawer over the page');
  check(await evaluate(`[...document.querySelectorAll('[data-box="drawer"] a, [data-box="drawer"] button')]
    .filter((e) => getComputedStyle(e).display !== 'none').every((e) => e.getBoundingClientRect().height >= 40)`),
    'its targets are at least 40 pixels tall');
  await send('Input.dispatchKeyEvent', { type: 'keyDown', key: 'Escape', code: 'Escape', windowsVirtualKeyCode: 27 });
  await send('Input.dispatchKeyEvent', { type: 'keyUp', key: 'Escape', code: 'Escape', windowsVirtualKeyCode: 27 });
  await until(`!document.querySelector('[data-box="drawer"]')`, 'drawer closed');
  check(true, 'Escape closes the drawer');
  await send('Emulation.setDeviceMetricsOverride', { width: 1280, height: 800, deviceScaleFactor: 1, mobile: false });

  // The jobs page is live: a landing queued elsewhere appears without input.
  await load('/jobs');
  await until(hasText('No jobs yet.'), 'jobs page');
  check(await evaluate('document.title') === 'Jobs · fortyard', 'the jobs page has its address and title');
  const auth = { Cookie: `fy_session=${session.value}`, 'Content-Type': 'application/json' };
  await fetch(url + 'api/v1/repos', { method: 'POST', headers: auth, body: JSON.stringify({ name: 'live' }) });
  await fetch(url + 'api/v1/repos/live/land', { method: 'POST', headers: auth, body: JSON.stringify({ branch: 'nothing' }) });
  await until(hasText('live · nothing'), 'the queued landing appears');
  check(true, 'a job queued elsewhere appears on the open jobs page');
  await until(hasText('failed'), 'the landing ends');
  check(true, 'its end appears too');

  // Changes and issues: a pushed branch is a change with its own pages.
  await fetch(url + 'api/v1/repos', { method: 'POST', headers: auth, body: JSON.stringify({ name: 'work' }) });
  const wt = join(data, 'work');
  const wg = (...args) => execFileSync('git', ['-C', wt, '-c', 'user.name=T', '-c', 'user.email=t@example.org', ...args],
    { env: { ...process.env, GIT_CONFIG_GLOBAL: '/dev/null', GIT_CONFIG_NOSYSTEM: '1' } });
  execFileSync('git', ['init', '-q', '-b', 'feature', wt]);
  writeFileSync(join(wt, 'notes.txt'), 'first line\nsecond line\n');
  wg('add', '-A');
  wg('commit', '-q', '-m', 'Explain the feature', '-m', 'Why it matters for the study.');
  for (let i = 0; i < 100 && !(await fetch(url + 'api/v1/repos/work/changes', { headers: auth })).ok; i++) await sleep(100);
  wg('push', '-q', `http://x:${admin}@127.0.0.1:${port}${base}/work.git`, 'HEAD:feature');
  await load('/work/-/changes');
  await until(hasText('Explain the feature'), 'the change listed');
  check(await evaluate('document.title') === 'Changes · work · fortyard', 'a pushed branch is listed as a change');
  await click(linkTo('/work/-/changes/1'));
  await until(hasText('Why it matters for the study.'), 'the change page');
  check(await path() === '/work/-/changes/1', 'a change has its own page with its description');
  await click('[data-id$="/comment"]');
  check(await evaluate(`document.querySelector('[data-id$="/comment"]').tagName`) === 'TEXTAREA',
    'the comment field takes several lines');
  await type('Looks right.');
  await click('[data-id$="/send"]');
  await until(hasText('Looks right.'), 'the comment shown');
  check(true, 'a comment is added from the page');
  await click(linkTo('/work/-/changes/1/files'));
  await until(`[...document.querySelectorAll('.fui')].some((e) => e.textContent.endsWith('+ second line'))`, 'the diff');
  check(await path() === '/work/-/changes/1/files', 'the files of a change show its diff');
  await load('/work/-/issues/new');
  await until(`!!document.querySelector('[data-id$="/newtitle"]')`, 'the new issue form');
  await click('[data-id$="/newtitle"]');
  await type('Plots are blank');
  await click('[data-id$="/create"]');
  await until(hasText('Plots are blank'), 'the new issue page');
  check(await path() === '/work/-/issues/2', 'a new issue gets the next number and its own page');

  if (shot) {
    const png = await send('Page.captureScreenshot', { format: 'png' });
    writeFileSync(shot, Buffer.from(png.result.data, 'base64'));
  }
  await click('[data-box="sidebar"] [data-id$="/signout"]');
  await until(`!!document.querySelector('[data-id$="/token"]')`, 'signed out');
  const after = await fetch(url + 'api/v1/whoami', { headers: { Cookie: `fy_session=${session.value}` } });
  check(after.status === 401, 'signing out ends the session on the server');

  // A session that writes without admin: no token form, and only its own token.
  const made = await (await fetch(url + 'api/v1/tokens', {
    method: 'POST',
    headers: { Authorization: `Bearer ${admin}`, 'Content-Type': 'application/json' },
    body: JSON.stringify({ name: 'agent-1', scopes: 'write' }),
  })).json();
  await click('[data-id$="/token"]');
  await send('Input.insertText', { text: made.token });
  await click('[data-id$="/signin"]');
  await until(hasText('admin'), 'signed in with a write token');
  await load('/tokens');
  await until(hasText('This session creates no tokens. That needs the admin scope.'), 'the note');
  check(!(await evaluate(`!!document.querySelector('[data-id$="/newname"]')`)),
    'a session with write and without admin is offered no token form');
  await until(hasText('agent-1'), 'its token');
  check(!(await evaluate(hasText('bootstrap'))), 'such a session lists only its own token');
} catch (e) {
  check(false, e.message);
} finally {
  if (logs.length) console.log('console:\n  ' + logs.join('\n  '));
  ws.close();
  const exited = new Promise((r) => browser.once('exit', r));
  try { process.kill(-browser.pid, 'SIGKILL'); } catch {}
  server.kill();
  await exited;
  rmSync(data, { recursive: true, force: true, maxRetries: 20, retryDelay: 100 });
}
process.exit(failed ? 1 : 0);
