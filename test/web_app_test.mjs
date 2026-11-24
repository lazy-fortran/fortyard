// The browser application against a real server, in headless Chromium with
// real pointer and key events:
//   node test/web_app_test.mjs FY_BINARY WEB_DIR [SCREENSHOT.png]
import { spawn, execFileSync } from 'node:child_process';
import { mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs';
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
// The repository row: its links in order, and the one marked as the page shown.
const rowLinks = `[...document.querySelectorAll('[data-box="reporow"] a')].map((e) => e.textContent).join(' | ')`;
const rowCurrent = `[...document.querySelectorAll('[data-box="reporow"] [data-look="current"]')].map((e) => e.textContent).join(' | ')`;
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
  // The repository row leads from the code to the changes and issues and back.
  await until(`${rowLinks} === 'demo | Code | Changes | Issues 0'`, 'the repository row');
  check(await evaluate(rowCurrent) === 'Code', 'the code page has the repository row with Code marked and the open issues counted');
  await click('[data-id$="reporow/changes"]');
  await until(hasText('No changes in demo. Pushing a branch opens one: fy push.'), 'the changes from the code');
  check(await path() === '/demo/-/changes' && await evaluate(rowCurrent) === 'Changes',
    'the row leads from the code to the changes and marks them');
  await click('[data-id$="reporow/issues"]');
  await until(hasText('No open issues.'), 'the issues from the changes');
  check(await path() === '/demo/-/issues' && await evaluate(rowCurrent) === 'Issues 0' &&
    await evaluate('document.title') === 'Issues · demo · fortyard', 'and from the changes to the issues');
  await click('[data-id$="reporow/code"]');
  await until(`!!document.querySelector(${JSON.stringify(linkTo('/demo/-/blob/hello.f90'))})`, 'back at the code');
  check(await path() === '/demo', 'and back to the code');
  await click(linkTo('/demo/-/blob/hello.f90'));
  await until(`[...document.querySelectorAll('[data-style="5"]')].some((e) => e.textContent.endsWith('  print *, "hi"') && e.querySelector('span[data-t="5"]')?.textContent === 'print')`, 'file');
  check(await path() === '/demo/-/blob/hello.f90', 'a file of the repository is shown as code at its own address');
  check(await evaluate(rowCurrent) === 'Code', 'a file is a page of the code');
  await click('[data-id$="repotabs/history"]');
  await until(`!!document.querySelector('[data-box^="row-c"] [data-id$="/id"]')`, 'history');
  check(await evaluate(hasText('Add hello')) && await path() === '/demo/-/commits', 'the history lists the commit');
  check(await evaluate(rowCurrent) === 'Code', 'and is a page of the code');
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
  for (const to of ['/', '/demo', commitPath, '/demo/-/commits', '/tokens', '/server', '/demo/-/blob/hello.f90',
    '/demo/-/changes', '/demo/-/issues', '/demo/-/issues/new']) {
    await load(to);
    await adopted();
    if (!await evaluate(noOverflow)) wide.push(to);
  }
  check(wide.length === 0, 'no page scrolls sideways on a phone' + (wide.length ? ': ' + wide.join(' ') : ''));
  check(await evaluate(`[...document.querySelectorAll('[data-box="reporow"] a')].every((e) => e.getBoundingClientRect().height >= 44)`),
    'the links of the repository row are 44 pixels tall there');
  // Repository names have up to 64 characters: the row stays one line and cuts the name.
  const long = 'computational-plasma-physics-neoclassical-transport-solver-next';
  const medium = 'neo-2-transport-solver-benchmarks';
  const cookie = { Cookie: `fy_session=${session.value}`, 'Content-Type': 'application/json' };
  for (const name of [long, medium]) {
    await fetch(url + 'api/v1/repos', { method: 'POST', headers: cookie, body: JSON.stringify({ name }) });
    for (let i = 0; i < 100 && !(await fetch(url + `api/v1/repos/${name}/issues`, { headers: cookie })).ok; i++) await sleep(100);
  }
  wide = [];
  let lines = [];
  for (const to of [`/${long}/-/issues`, `/${long}/-/changes`, `/${long}/-/issues/new`, `/${medium}/-/issues`]) {
    await load(to);
    await adopted();
    if (!await evaluate(noOverflow)) wide.push(to);
    if (await evaluate(`new Set([...document.querySelectorAll('[data-box="reporow"] a')].map((e) => Math.round(e.getBoundingClientRect().top))).size`) !== 1 ||
      !await evaluate(`[...document.querySelectorAll('[data-box="reporow"] a')].every((e) => e.getBoundingClientRect().right <= document.documentElement.clientWidth)`)) lines.push(to);
  }
  check(wide.length === 0, 'nor does a page of a repository with a long name' + (wide.length ? ': ' + wide.join(' ') : ''));
  check(lines.length === 0, 'and its repository row stays one line within the page' + (lines.length ? ': ' + lines.join(' ') : ''));
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
  await until(hasText('No jobs match.'), 'jobs page');
  check(await evaluate('document.title') === 'Jobs · fortyard', 'the jobs page has its address and title');
  const auth = { Cookie: `fy_session=${session.value}`, 'Content-Type': 'application/json' };
  await fetch(url + 'api/v1/repos', { method: 'POST', headers: auth, body: JSON.stringify({ name: 'live' }) });
  await fetch(url + 'api/v1/repos/live/land', { method: 'POST', headers: auth, body: JSON.stringify({ branch: 'nothing' }) });
  await until(hasText('live · nothing'), 'the queued landing appears');
  check(true, 'a job queued elsewhere appears on the open jobs page');
  await until(hasText('failed'), 'the landing ends');
  check(true, 'its end appears too');

  // A job has its own page with its stored log; every line has an address.
  await click(linkTo('/jobs/1'));
  await until(`!!document.getElementById('L1')`, 'the job page');
  check(await path() === '/jobs/1' && await evaluate('document.title') === 'Job 1 · fortyard' &&
    await evaluate(hasText('live · nothing')), 'a row of the jobs page leads to the page of its job');
  const job1 = await (await fetch(url + 'api/v1/jobs/1', { headers: auth })).json();
  const lines1 = job1.log.replace(/\n$/, '').split('\n');
  check(await evaluate(`document.querySelectorAll('[data-box="joblog"] > .fui').length`) === lines1.length &&
    await evaluate(`document.getElementById('L${lines1.length}').textContent`) === String(lines1.length).padStart(5) + '  ' + lines1.at(-1),
    'the page shows the whole stored log, one numbered line each');
  check(await evaluate(hasText('No check is recorded for this job.')), 'a job without a recorded check says so');
  await load('/jobs/999');
  check(await evaluate(hasText('There is no job 999.')) &&
    (await fetch(url + 'jobs/999', { headers: auth })).status === 404, 'a job that does not exist says so, with status 404');
  await load('/jobs/1#L1');
  check(await evaluate(`document.getElementById('L1').__ssr === 1 && document.querySelector(':target').id === 'L1'`),
    'the server sends the log with its line addresses, so #L1 works before any script');
  check(await adopted(), 'and the WebAssembly adopts the job page');

  // The list is filtered through the address.
  await load('/jobs?state=landed');
  check(await evaluate(hasText('No jobs match.')) && !await evaluate(`!!document.querySelector('[data-box="row-j1"]')`),
    'a state in the address filters the jobs');
  await click(linkTo('/jobs?state=failed'));
  await until(`!!document.querySelector('[data-box="row-j1"]')`, 'the failed job');
  check(await path() === '/jobs?state=failed', 'a filter is a link to its address');
  await click(linkTo('/jobs?kind=verify&state=failed'));
  await until(hasText('No jobs match.'), 'no verification');
  check(await path() === '/jobs?kind=verify&state=failed', 'kind and state filter together');

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
  await load('/work/-/issues');
  check(await evaluate(`window.__ssrTexts.includes('No open issues.') && window.__ssrTexts.includes('Issues 0')`),
    'the server sends the empty issue list with the repository row');
  check(await adopted(), 'and the WebAssembly adopts it');
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
  // Issues, reached from the change through the repository row: the list,
  // a new issue, a comment, and closing it.
  check(await evaluate(rowLinks) === 'work | Code | Changes | Issues 0' && await evaluate(rowCurrent) === 'Changes',
    'a change page has the repository row with Changes marked');
  await click('[data-id$="reporow/issues"]');
  await until(hasText('No open issues.'), 'the issue list');
  check(await path() === '/work/-/issues', 'the row leads from a change to the issues');
  await click(linkTo('/work/-/issues/new'));
  await until(`!!document.querySelector('[data-id$="/newtitle"]')`, 'the new issue form');
  check(await path() === '/work/-/issues/new' && await evaluate(rowCurrent) === 'Issues 0',
    'the issue list leads to the form for a new issue');
  await click('[data-id$="/newtitle"]');
  await type('Plots are blank');
  await click('[data-id$="/create"]');
  await until(hasText('Plots are blank'), 'the new issue page');
  check(await path() === '/work/-/issues/2', 'a new issue gets the next number and its own page');
  await until(`${rowCurrent} === 'Issues 1'`, 'the count after opening');
  check(true, 'the repository row counts the new issue at once');
  await click('[data-id$="/comment"]');
  await type('Seen on the phone too.');
  await click('[data-id$="/send"]');
  await until(hasText('Seen on the phone too.'), 'the comment on the issue');
  check(await evaluate(`document.querySelector('[data-id$="/comment"]').value`) === '', 'a comment is added to the issue');
  await click('[data-id$="reporow/issues"]');
  await until(hasText('Plots are blank'), 'the issue listed');
  await until(`[...document.querySelectorAll('[data-box="row-i2"] .fui')].some((e) => e.textContent.startsWith('admin · 1 comment · '))`,
    'the comment counted');
  check(await path() === '/work/-/issues', 'the list shows the open issue with its comment');
  await load('/work/-/issues');
  check(await evaluate(`window.__ssrTexts.includes('Plots are blank') && window.__ssrTexts.includes('Issues 1') &&
    !window.__ssrTexts.includes('No open issues.')`), 'a reload gets the list with the issue from the server');
  check(await adopted(), 'and the WebAssembly adopts it');
  await click(linkTo('/work/-/issues/2'));
  await until(`!!document.querySelector('[data-id$="/closeitem"]')`, 'the issue page');
  check(await evaluate(hasText('Seen on the phone too.')), 'the list leads to the issue with its comments');
  await click('[data-id$="/closeitem"]');
  await until(`!!document.querySelector('[data-id$="/reopen"]') && ${rowCurrent} === 'Issues 0'`, 'the issue closed');
  check(await evaluate(`document.querySelector('[data-box="itemmeta"] [data-id$="badge"]').textContent`) === 'closed',
    'closing an issue shows it closed and takes it from the count');
  // The answer for the list is held back: until it comes the list shown before stays.
  await evaluate(`window.__fetch = window.fetch; window.__held = [];
    window.fetch = (p, i) => String(p).endsWith('/work/issues') ? new Promise((done) => window.__held.push(() => done(window.__fetch(p, i)))) : window.__fetch(p, i); 0`);
  await click('[data-id$="reporow/issues"]');
  await until(`window.__held.length === 1`, 'the list asked for');
  check(await path() === '/work/-/issues' && await evaluate(hasText('Plots are blank')) && !await evaluate(hasText('Loading...')),
    'after closing an issue the list keeps its rows until the new answer');
  await evaluate('window.fetch = window.__fetch; window.__held.forEach((go) => go()); 0');
  await until(hasText('No open issues.'), 'the list without the closed issue');
  check(true, 'the list of open issues no longer has it');
  await click(linkTo('/work/-/issues?all'));
  await until(hasText('Plots are blank'), 'all issues');
  check(await path() === '/work/-/issues?all', 'the list of all issues has it');
  // The server composed this list before, when the issue was open.
  await load('/work/-/issues');
  check(await evaluate(`window.__ssrTexts.includes('No open issues.') && window.__ssrTexts.includes('Issues 0') &&
    !window.__ssrTexts.includes('Plots are blank')`), 'the server sends the same list again as it is now');
  check(await adopted(), 'and the WebAssembly adopts it');

  // Issues are mostly opened elsewhere (fy, agents): the row asks for its
  // count on entering a repository and with the issue list, which it must
  // agree with, and no other page of the repository asks for it.
  const elsewhere = await fetch(url + 'api/v1/repos/work/issues', { method: 'POST', headers: auth,
    body: JSON.stringify({ title: 'From elsewhere', body: '' }) });
  check(elsewhere.status === 201, 'an issue is opened through the API');
  await evaluate(`window.__asked = []; const f = window.fetch; window.fetch = (p, i) => { window.__asked.push(String(p)); return f(p, i); }; 0`);
  const countsAsked = `window.__asked.filter((p) => p.endsWith('/api/v1/repos')).length`;
  await click('[data-id$="reporow/changes"]');
  await until(`${rowCurrent} === 'Changes' && ${hasText('Explain the feature')}`, 'the changes');
  await click('[data-id$="reporow/code"]');
  await until(`${rowCurrent} === 'Code'`, 'the code');
  check(await evaluate(countsAsked) === 0, 'the pages of the code and the changes in the same repository ask for no count');
  await click('[data-id$="reporow/issues"]');
  await until(`${hasText('From elsewhere')} && ${rowCurrent} === 'Issues 1'`, 'the issue opened elsewhere listed');
  check(await evaluate(countsAsked) === 1, 'the issue list asks for the count once and shows the issue opened elsewhere under it');
  await fetch(url + 'api/v1/repos/work/issues', { method: 'POST', headers: auth, body: JSON.stringify({ title: 'Another', body: '' }) });
  await click(linkTo('/work/-/issues/3'));
  await until(`!!document.querySelector('[data-id$="/closeitem"]')`, 'the issue page');
  await evaluate('history.back()');
  await until(`${hasText('Another')} && ${rowCurrent} === 'Issues 2'`, 'the list after Back');
  check(true, 'Back to the list shows what was opened since, under the same count');

  // A phone gets the same elements: the pages of an item are adopted there too.
  await send('Emulation.setDeviceMetricsOverride', { width: 375, height: 740, deviceScaleFactor: 2, mobile: true });
  wide = [];
  for (const to of ['/work/-/issues/3', '/work/-/changes/1']) {
    await load(to);
    if (!await adopted() || !await evaluate(noOverflow)) wide.push(to);
  }
  check(wide.length === 0, 'the pages of an issue and of a change are adopted on a phone and fit it' + (wide.length ? ': ' + wide.join(' ') : ''));
  check(await evaluate(`getComputedStyle(document.querySelector('[data-box="itemtitle"] [data-id$="/number"]')).display`) === 'none',
    'where the number beside the title is hidden');
  await send('Emulation.setDeviceMetricsOverride', { width: 1280, height: 800, deviceScaleFactor: 1, mobile: false });

  // What is not there: a missing item keeps the row of its repository, a
  // repository that does not exist has none.
  await load('/work/-/issues/99');
  check(await evaluate(`window.__ssrTexts.includes('There is no such page.')`) && await evaluate(rowCurrent) === 'Issues 2',
    'a missing issue says so under the row of its repository');
  for (const to of ['/ghost/-/issues', '/ghost/-/changes/3', '/ghost']) {
    await load(to);
    await adopted();
    check(await evaluate(`!window.__ssrBoxes.includes('reporow') && !document.querySelector('[data-box="reporow"]')`),
      `${to}, of a repository that does not exist, has no repository row`);
  }

  // Signing out forgets what the session saw: the next one asks again.
  await load('/work/-/issues/3');
  await until(`!!document.querySelector('[data-id$="/closeitem"]')`, 'the open issue');
  await click('[data-box="sidebar"] [data-id$="/signout"]');
  await until(`!!document.querySelector('[data-id$="/token"]')`, 'signed out');
  const closed = await fetch(url + 'api/v1/repos/work/issues/3', { method: 'POST',
    headers: { Authorization: `Bearer ${admin}`, 'Content-Type': 'application/json' }, body: JSON.stringify({ state: 'closed' }) });
  check(closed.ok, 'the issue is closed through the API meanwhile');
  await click('[data-id$="/token"]');
  await send('Input.insertText', { text: admin });
  await click('[data-id$="/signin"]');
  await until(hasText('admin'), 'signed in again');
  await evaluate(`history.pushState(null, '', ${JSON.stringify(base + '/work/-/issues/3')}); dispatchEvent(new PopStateEvent('popstate'))`);
  await until(`!!document.querySelector('[data-id$="/reopen"]') && ${rowCurrent} === 'Issues 1'`, 'the issue as it is now');
  check(true, 'after signing out and in again the issue is shown as it is now, not as the last session saw it');

  // The changes page: every state with its words, newest activity first, and
  // filters that are links in the address.
  // The token, not the session: the section before signs out and in again.
  const api = (to, method = 'GET', body) => fetch(url + 'api/v1/' + to, { method,
    headers: { Authorization: `Bearer ${admin}`, 'Content-Type': 'application/json' },
    body: body && JSON.stringify(body) });
  const listed = async (query) => (await (await api('repos/work/changes?' + query)).json()).changes;
  // Issues and changes share numbers, and earlier sections open issues here.
  const num = async (title) => (await listed('state=all')).find((c) => c.title === title).number;
  const remote = `http://x:${admin}@127.0.0.1:${port}${base}/work.git`;
  const branch = (name, file, subject) => {
    writeFileSync(join(wt, file), name + '\n');
    wg('add', '-A');
    wg('commit', '-q', '-m', subject);
    wg('push', '-q', remote, 'HEAD:' + name);
  };
  await api('repos/work/changes/1/land', 'POST');
  for (let i = 0; i < 100 && !(await listed('state=landed')).length; i++) await sleep(100);
  const first = (await listed('state=landed'))[0];
  wg('fetch', '-q', remote, 'main');
  wg('checkout', '-q', '-b', 'second', 'FETCH_HEAD');
  branch('second', 'more.txt', 'Second idea');
  wg('checkout', '-q', '-b', 'dropped', 'FETCH_HEAD');
  branch('dropped', 'less.txt', 'Dropped idea');
  for (let i = 0; i < 100 && !(await listed('state=all')).find((c) => c.title === 'Dropped idea'); i++) await sleep(100);
  const second = await num('Second idea'), dropped = await num('Dropped idea');
  await api(`repos/work/changes/${dropped}`, 'POST', { state: 'closed' });
  // Unrelated history with the same file cannot land.
  wg('checkout', '-q', '--orphan', 'clash');
  branch('clash', 'notes.txt', 'Clashing notes');
  for (let i = 0; i < 100 && !(await listed('state=all')).find((c) => c.title === 'Clashing notes'); i++) await sleep(100);
  const clash = await num('Clashing notes');
  await api(`repos/work/changes/${clash}/land`, 'POST');
  for (let i = 0; i < 100 && !(await listed('state=all&attention=needs-author')).length; i++) await sleep(100);
  const rowsShown = `[...document.querySelectorAll('[data-box^="row-i"]')].map((e) => e.dataset.box.slice(5)).join(' ')`;
  const word = (n, id) => evaluate(`(() => { const e = document.querySelector('[data-box="row-i${n}"] [data-id$="/${id}"]');
    return e ? e.textContent + '|' + e.dataset.tone : ''; })()`);
  await load('/work/-/changes');
  check(await evaluate(`window.__ssrTexts.includes(${JSON.stringify('landed ' + first.commit.slice(0, 7))})`) &&
    await evaluate(rowsShown) === `${clash} ${dropped} ${second} 1`,
    'the server renders the changes of every state, the landed one with its commit, newest activity first');
  check(await adopted(), 'and the WebAssembly adopts that list');
  check(await word(second, 'state') === 'draft|0' && await word(dropped, 'state') === 'closed|3' &&
    await word(clash, 'state') === 'needs author · entry 1 of 1|17',
    'a state is a word: plain, muted when closed, amber when the author is needed');
  check(await word(second, 'review') === 'unreviewed|17' && await word(second, 'author') === 'bootstrap|3',
    'a row has its review state and the session (token) that pushed it');
  // The rows stay until the filtered ones are here: no "Loading..." in between.
  await evaluate(`window.__card = document.querySelector('[data-box="card-changes"]'); window.__loading = false;
    new MutationObserver(() => { if (document.getElementById('app').textContent.includes('Loading...')) window.__loading = true; })
      .observe(document.getElementById('app'), { childList: true, subtree: true, characterData: true })`);
  await click(linkTo('/work/-/changes?state=landed'));
  await until(`${rowsShown} === '1'`, 'the landed changes');
  check(await evaluate(`!window.__loading && window.__card === document.querySelector('[data-box="card-changes"]')`),
    'a filter keeps the list on the page and never empties it on the way');
  check(await path() === '/work/-/changes?state=landed' &&
    await evaluate(`document.querySelector(${JSON.stringify(linkTo('/work/-/changes?state=landed'))}).dataset.tone`) === '15',
    'a filter is a link: the address names it and the list follows');
  await click(linkTo('/work/-/changes?state=landed&review=flagged'));
  await until(hasText('No landed flagged changes in work.'), 'the empty list');
  check(true, 'an empty list says so in a sentence that names the filters');
  await click(linkTo('/work/-/changes?state=landed'));
  await until(`${rowsShown} === '1'`, 'the filter taken back');
  check(true, 'a filter that is on leads back to the list without it');
  await load('/work/-/changes?attention=needs-author');
  check(await evaluate(`window.__ssrTexts.includes('Clashing notes') && !window.__ssrTexts.includes('Second idea')`),
    'a filtered address is rendered by the server');
  await load('/work/-/changes');
  await adopted();
  const key = async (k, extra = {}) => {
    await send('Input.dispatchKeyEvent', { type: 'keyDown', key: k, ...extra });
    await send('Input.dispatchKeyEvent', { type: 'keyUp', key: k, ...extra });
  };
  await key('/', { text: '/' });
  await until(`(document.activeElement.dataset.id || '').endsWith('/filter')`, 'the filter field focused');
  check(true, 'the key / moves to the filter field');
  await evaluate(`document.activeElement.value = ''`);
  await type('SECOND');
  await key('Enter', { code: 'Enter', windowsVirtualKeyCode: 13 });
  await until(`${rowsShown} === '${second}'`, 'the changes matching the text');
  check(await path() === '/work/-/changes?q=SECOND', 'text typed into the field filters by title, in the address');
  await load('/work/-/changes?q=dropped&state=closed');
  check(await evaluate(`${rowsShown} === '${dropped}' && document.querySelector('[data-id$="/filter"]').value === 'dropped'`),
    'a reload keeps the text and the list');
  await load('/work/-/changes?before=junk');
  check(await evaluate(`window.__ssrTexts.includes('This address is no place in the list of changes.')`) &&
    await evaluate(`!!document.querySelector(${JSON.stringify(linkTo('/work/-/changes?state=landed'))}) &&
      document.querySelector('[data-id$="/newest"]').getAttribute('href').endsWith('/work/-/changes')`),
    'an address with a key that is none keeps the filters and leads to the newest changes');
  await send('Emulation.setDeviceMetricsOverride', { width: 375, height: 740, deviceScaleFactor: 2, mobile: true });
  await load('/work/-/changes');
  check(await adopted() && await evaluate(noOverflow) && await evaluate(rowsShown) === `${clash} ${dropped} ${second} 1`,
    'the changes page is the same page on a phone and fits it');
  check(await evaluate(`[...document.querySelectorAll('[data-box^="row-i"]')].every((row) => {
      const at = (id) => row.querySelector('[data-id$="/' + id + '"]');
      const box = (id) => at(id).getBoundingClientRect();
      const whole = (e) => e.scrollWidth <= e.clientWidth && e.getBoundingClientRect().right <= innerWidth;
      const below = (a, b) => box(a).top >= box(b).bottom - 1;
      const level = (a, b) => box(a).top < box(b).bottom && box(b).top < box(a).bottom;
      return whole(at('state')) && whole(at('review')) && below('state', 'item') && below('review', 'item') &&
        level('state', 'review') && level('item', 'author') && level('item', 'age') && level('item', 'number'); })`),
    'on a phone every row has the same two lines and no state or review is cut');
  check(await evaluate(`[...document.querySelectorAll('[data-box^="row-i"]')].every((row) => {
      const s = row.querySelector('[data-id$="/state"]').getBoundingClientRect();
      const hit = document.elementFromPoint(s.left + s.width / 2, s.top + s.height / 2);
      return row.getBoundingClientRect().height >= 44 &&
        hit && hit.closest('a') === row.querySelector('a[data-id$="/item"]'); })`),
    'on a phone the whole row, at least 44 px high, opens its change');
  check(await evaluate(`[...document.querySelectorAll('[data-box="filters"] a, [data-box="filters"] input')]
      .every((e) => e.getBoundingClientRect().height >= 44)`),
    'on a phone every filter and the filter field are targets of 44 px');
  await load('/work/-/changes?before=junk');
  check(await evaluate(`document.querySelector('[data-id$="/newest"]').getBoundingClientRect().height >= 44`),
    'on a phone the link to the newest changes is a target of 44 px');
  await send('Emulation.setDeviceMetricsOverride', { width: 1280, height: 800, deviceScaleFactor: 1, mobile: false });

  // A review of an older version is stale: the newest version waits for one.
  await api(`repos/work/changes/${second}/reviewed`, 'POST');
  wg('checkout', '-q', 'second');
  branch('second', 'again.txt', 'Second idea, again');
  for (let i = 0; i < 100 && !(await listed('state=all')).find((c) => c.number === second && c.review_stale); i++) await sleep(100);
  await load('/work/-/changes?review=unreviewed');
  check(await word(second, 'review') === 'approved an older version|17',
    'a stale review is shown as one, in amber, among the unreviewed changes');

  // More changes than one answer holds: the older ones follow by key.
  await api('repos', 'POST', { name: 'many' });
  for (let i = 0; i < 100 && !(await api('repos/many/changes')).ok; i++) await sleep(100);
  const mt = join(data, 'many');
  execFileSync('sh', ['-c', `git init -q -b base "$0" && cd "$0" && for i in $(seq 101); do
    git -c user.name=T -c user.email=t@example.org commit -q --allow-empty -m "Step $i" && git branch b$i || exit 1; done &&
    git push -q "$1" 'refs/heads/b*:refs/heads/b*'`, mt, remote.replace('/work.git', '/many.git')],
    { env: { ...process.env, GIT_CONFIG_GLOBAL: '/dev/null', GIT_CONFIG_NOSYSTEM: '1' } });
  await load('/many/-/changes');
  const shown = `document.querySelectorAll('[data-box^="row-i"]').length`;
  check(await evaluate(shown) === 100 && await evaluate(`!!document.querySelector('[data-id$="/older"]')`),
    'a long list shows its newest hundred changes and a link to the older ones');
  await evaluate(`document.querySelector('[data-id$="/older"]').scrollIntoView()`);
  await click('[data-id$="/older"]');
  await until(`${shown} === 1`, 'the older changes');
  check((await path()).startsWith('/many/-/changes?before=') &&
    await evaluate(`!!document.querySelector('[data-id$="/newest"]')`),
    'the older changes have their own address, by key, and lead back to the newest');
  await load('/work/-/changes');
  await adopted();

  // Two entries of one change: the first fails its check, the second lands.
  // Its own change on main, and the token: earlier sections landed change 1
  // and signed out and in again.
  const tokenAuth = { Authorization: `Bearer ${admin}`, 'Content-Type': 'application/json' };
  const jobState = async (id) => (await (await fetch(url + 'api/v1/jobs/' + id, { headers: tokenAuth })).json()).state;
  wg('fetch', '-q', `http://x:${admin}@127.0.0.1:${port}${base}/work.git`, 'main');
  wg('checkout', '-q', '-B', 'jobs-demo', 'FETCH_HEAD');
  writeFileSync(join(wt, 'jobs.txt'), 'jobs\n');
  wg('add', '-A');
  wg('commit', '-q', '-m', 'Show the entries of a change');
  const pushCheck = (script) => {
    mkdirSync(join(wt, '.fortyard'), { recursive: true });
    writeFileSync(join(wt, '.fortyard', 'check'), script, { mode: 0o755 });
    wg('add', '-A');
    wg('commit', '-q', '--amend', '--no-edit');
    wg('push', '-q', '-f', `http://x:${admin}@127.0.0.1:${port}${base}/work.git`, 'HEAD:jobs-demo');
  };
  const landChange = async () => (await (await fetch(url + 'api/v1/repos/work/land',
    { method: 'POST', headers: tokenAuth, body: JSON.stringify({ branch: 'jobs-demo' }) })).json()).landing;
  pushCheck('#!/bin/sh\necho checking\necho "FAIL: not yet"\nexit 1\n');
  const firstJob = await landChange();
  const jobChange = (await (await fetch(url + 'api/v1/repos/work/changes?state=all', { headers: tokenAuth })).json())
    .changes.find((c) => c.title === 'Show the entries of a change').number;
  for (let i = 0; i < 200 && await jobState(firstJob) !== 'failed'; i++) await sleep(100);
  await load('/jobs/' + firstJob);
  const failedJob = await (await fetch(url + 'api/v1/jobs/' + firstJob, { headers: tokenAuth })).json();
  check(await evaluate(hasText('job ' + firstJob)) && await evaluate(hasText('failed')) &&
    await evaluate(`document.querySelector(${JSON.stringify(linkTo('/work/-/changes/' + jobChange))}).textContent`) === `work #${jobChange} Show the entries of a change`,
    'the page of a job names its change and links to it');
  check(failedJob.error_line > 0 &&
    await evaluate(`document.getElementById('L${failedJob.error_line}').textContent.endsWith('FAIL: not yet')`),
    'the first line with Error or FAIL is named');
  await click(linkTo(`/jobs/${firstJob}#L${failedJob.error_line}`));
  await until(`location.hash === '#L${failedJob.error_line}'`, 'the line address');
  check(await path() === '/jobs/' + firstJob, 'and linked at the top: the link leads to the address of that line');
  check(await evaluate(`(document.querySelector(':target') || {}).id`) === 'L' + failedJob.error_line,
    'and the line it leads to is marked as the target');
  // The page is live: the entry that replaces this one appears without input.
  pushCheck('#!/bin/sh\necho checking\n');
  const secondJob = await landChange();
  await until(`!!document.querySelector(${JSON.stringify(linkTo('/jobs/' + secondJob))})`, 'the next entry');
  await until(`document.querySelector(${JSON.stringify(linkTo('/jobs/' + secondJob))}).textContent === 'job ${secondJob}, landed'`, 'its landing');
  check(await evaluate(hasText('entry 1 of 2')) && await evaluate(hasText('next entry of this change:')),
    'an open job page learns of the next entry of its change and of its outcome');
  await click(linkTo('/jobs/' + secondJob));
  await until(hasText('entry 2 of 2'), 'the second entry');
  check(await path() === '/jobs/' + secondJob && await evaluate(hasText('landed')) &&
    !await evaluate(hasText('next entry of this change:')), 'the entry that landed is entry 2 of 2 of the same change');
  await load('/jobs?repo=work');
  const toChange = (id) => `!!document.querySelector('[data-box="row-j${id}"] ' + ${JSON.stringify(linkTo('/work/-/changes/' + jobChange))})`;
  check(await evaluate(toChange(firstJob)) && await evaluate(toChange(secondJob)) &&
    await evaluate(`[...document.querySelectorAll('[data-box="row-j${firstJob}"] .fui')].some((e) => e.textContent.includes('entry 1 of 2'))`) &&
    await evaluate(`[...document.querySelectorAll('[data-box="row-j${secondJob}"] .fui')].some((e) => e.textContent.includes('entry 2 of 2'))`),
    'the rows of the jobs page link to the change and show the two entries of it');
  // A change with a title of ordinary length whose check prints CR LF line
  // ends and fails; its check is recorded with a runner (no runner runs here).
  const longTitle = 'Jobs name their change, and a job has a page with its log and its checks';
  wg('fetch', '-q', `http://x:${admin}@127.0.0.1:${port}${base}/work.git`, 'main');
  wg('checkout', '-q', '-b', 'long', 'FETCH_HEAD');
  writeFileSync(join(wt, '.fortyard', 'check'), '#!/bin/sh\nprintf "one\\r\\ntwo Error\\r\\n"\nexit 1\n', { mode: 0o755 });
  wg('add', '-A');
  wg('commit', '-q', '-m', longTitle);
  wg('push', '-q', `http://x:${admin}@127.0.0.1:${port}${base}/work.git`, 'HEAD:long');
  const changes = await (await fetch(url + 'api/v1/repos/work/changes', { headers: tokenAuth })).json();
  const longChange = changes.changes.find((c) => c.title === longTitle).number;
  const third = (await (await fetch(url + `api/v1/repos/work/changes/${longChange}/land`,
    { method: 'POST', headers: tokenAuth, body: '{}' })).json()).landing;
  for (let i = 0; i < 200 && await jobState(third) !== 'failed'; i++) await sleep(100);
  execFileSync('sqlite3', [join(data, 'd', 'fortyard.db'), `INSERT INTO checks (repo, ref, state, runner, created_at,
    claimed_at, finished_at) VALUES ('work', 'refs/fortyard/checks/${third}', 'failed', 'faepkub4',
    '2026-11-11T09:41:12Z', '2026-11-11T09:41:16Z', '2026-11-11T09:42:36Z');`]);
  await load('/jobs/' + third);
  const logLines = `JSON.stringify([...document.querySelectorAll('[data-box="joblog"] > .fui')].map((e) => e.textContent))`;
  const ssrLines = await evaluate(logLines);
  check(await adopted() && ssrLines === await evaluate(logLines) && JSON.parse(ssrLines).includes('    1  one'),
    'a log line that ends in CR LF is the same from the server and in the application, without the CR');
  // A check that writes a line, waits for a gate, writes another and the start
  // of a UTF-8 character, waits for a second gate, writes the rest of it and
  // 2997 more lines, writes a line every 0.2 s until a third gate and then a
  // last line: its lines appear on the open job page while it runs.
  const liveTitle = 'Show the log of a running check';
  wg('fetch', '-q', `http://x:${admin}@127.0.0.1:${port}${base}/work.git`, 'main');
  wg('checkout', '-q', '-b', 'live', 'FETCH_HEAD');
  const gate = (n) => join(data, 'gate' + n);
  writeFileSync(join(wt, '.fortyard', 'check'), `#!/bin/sh\necho "first line"\nwhile [ ! -f ${gate(1)} ]; do sleep 0.05; done\n` +
    `echo "second line"\nprintf 'third line \\342\\234'\nwhile [ ! -f ${gate(2)} ]; do sleep 0.05; done\nprintf '\\223\\n'\n` +
    `seq 4 3000 | sed 's/^/line /'\nwhile [ ! -f ${gate(3)} ]; do echo tick; sleep 0.2; done\necho "last line"\n`, { mode: 0o755 });
  wg('add', '-A');
  wg('commit', '-q', '-m', liveTitle);
  wg('push', '-q', `http://x:${admin}@127.0.0.1:${port}${base}/work.git`, 'HEAD:live');
  const liveChanges = await (await fetch(url + 'api/v1/repos/work/changes', { headers: tokenAuth })).json();
  const liveChange = liveChanges.changes.find((c) => c.title === liveTitle).number;
  const fourth = (await (await fetch(url + `api/v1/repos/work/changes/${liveChange}/land`,
    { method: 'POST', headers: tokenAuth, body: '{}' })).json()).landing;
  const logRows = `[...document.querySelectorAll('[data-box="joblog"] > .fui')]`;
  const logText = `${logRows}.map((e) => e.textContent).join('\\n')`;
  const lineText = (n) => `(document.getElementById('L${n}') || {}).textContent || ''`;
  const waitFor = async (expr) => {
    for (let i = 0; i < 400; i++) {
      if (await evaluate(expr)) return true;
      await sleep(50);
    }
    return false;
  };
  const fetchesOf = (part) => `performance.getEntriesByType('resource').filter((e) => e.name.includes(${JSON.stringify(part)})).length`;
  for (let i = 0; i < 400; i++) {
    const log = await (await fetch(url + `api/v1/jobs/${fourth}/log`, { headers: tokenAuth })).json();
    if (log.live && log.text.includes('first line')) break;
    await sleep(50);
  }
  // This server runs the check itself. Add a runner record for its jobs pages.
  const clockDb = (sql) => execFileSync('sqlite3', [join(data, 'd', 'fortyard.db'), sql]);
  const clockRef = `refs/fortyard/checks/${fourth}`;
  clockDb(`INSERT INTO checks (repo, ref, state, created_at, queued_at)
    VALUES ('work', '${clockRef}', 'queued', strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-6 hours'),
    strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-7 seconds'));`);
  await load('/jobs/' + fourth);
  check(await evaluate(hasText('waiting')) && await adopted() && await evaluate(hasText('waiting')),
    'an active job waiting on a runner shows as waiting');
  clockDb(`UPDATE checks SET state = 'running', runner = 'runner-test',
    claimed_at = strftime('%Y-%m-%dT%H:%M:%SZ', 'now'),
    queued_at = strftime('%Y-%m-%dT%H:%M:%SZ', 'now', '-7 seconds')
    WHERE ref = '${clockRef}';`);
  const hasAttemptClock = `[...document.querySelectorAll('.fui')].some((e) =>
    /running for \\d+ s · queued for 7 s/.test(e.textContent))`;
  await load('/jobs/' + fourth);
  check(await waitFor(`${logText}.endsWith('    1  first line')`) && await jobState(fourth) === 'running' &&
    await evaluate(hasText('· live')) && await evaluate(`!!document.getElementById('L1')`),
    'the page of a running job shows the lines its check wrote so far, each with its address');
  check(await evaluate(hasAttemptClock), 'the job page shows the current run and its own queue wait');
  await load('/jobs?repo=work');
  check(await waitFor(`[...document.querySelectorAll('[data-box="row-j${fourth}"] .fui')].some((e) => e.textContent === 'first line')`),
    'the jobs list shows the last line of a running job');
  check(await evaluate(hasAttemptClock), 'the jobs list shows the current run and its own queue wait');
  clockDb(`DELETE FROM checks WHERE ref = '${clockRef}';`);
  await load('/jobs/' + fourth);
  await until(`window.Module && typeof Module._fortui_web_step === 'function'`, 'WebAssembly running');
  await evaluate(`performance.setResourceTimingBufferSize(100000)`);
  writeFileSync(gate(1), '');
  check(await waitFor(`${logText}.endsWith('    3  third line')`) && await jobState(fourth) === 'running',
    'lines the check writes later appear on the open page while the check still runs: ' +
    JSON.stringify((await evaluate(logText)).slice(-80)));
  // The log ends inside a character: the page asks for more only when the
  // log grows again, not in a loop.
  const asked = await evaluate(fetchesOf(`/jobs/${fourth}/log`));
  const askedAt = Date.now();
  await waitFor(`performance.now() - ${await evaluate('performance.now()')} > 1500`);
  const askedMore = await evaluate(fetchesOf(`/jobs/${fourth}/log`)) - asked;
  check(askedMore <= 1 + Math.floor((Date.now() - askedAt) / 2000),
    `the page does not ask in a loop while the log ends inside a character (${askedMore} requests)`);
  writeFileSync(gate(2), '');
  check(await waitFor(`${lineText(3000)}.endsWith('line 3000') && ${lineText(3)}.endsWith('third line ✓')`),
    'a long log keeps each line at its address, the first and the newest lines');
  const rows = await evaluate(`${logRows}.length`);
  check(rows <= 510 && await evaluate(`${logText}.includes('are left out here')`) && await evaluate(`!document.getElementById('L1500')`),
    `the page keeps the first and the last lines of a long log, not all of them (${rows} rows)`);
  // Each line the check writes reaches the jobs list at most once in 5 s.
  await load('/jobs?repo=work');
  await until(`window.Module && typeof Module._fortui_web_step === 'function'`, 'WebAssembly running');
  await evaluate(`performance.setResourceTimingBufferSize(100000)`);
  await waitFor(`[...document.querySelectorAll('[data-box="row-j${fourth}"] .fui')].some((e) => e.textContent === 'tick')`);
  await waitFor(`${fetchesOf('/api/v1/jobs?')} >= 1`);
  const jobsAsked = await evaluate(fetchesOf('/api/v1/jobs?'));
  const listedAt = Date.now();
  await sleep(8000);
  const listedMore = await evaluate(fetchesOf('/api/v1/jobs?')) - jobsAsked;
  check(listedMore <= 1 + Math.floor((Date.now() - listedAt) / 5000),
    `a check that writes all the time makes the jobs list ask at most every 5 s (${listedMore} requests in 8 s)`);
  await load('/jobs/' + fourth);
  writeFileSync(gate(3), '');
  check(await waitFor(`${logText}.includes('last line') && !${hasText('· live')}`) &&
    await jobState(fourth) === 'landed' && await evaluate(`${lineText(3000)}.endsWith('line 3000')`) &&
    await evaluate(`${lineText(2)}.endsWith('second line')`),
    'when the job ends the page shows its stored log, its lines at the addresses they had');
  // A filter the server refuses matches no job, from the server and in the application.
  await load('/jobs?kind=bogus');
  check(await evaluate(hasText('No jobs match.')) && await adopted() && await evaluate(hasText('No jobs match.')) &&
    !await evaluate(hasText('Loading...')), 'an unknown kind in the address matches no job');
  await send('Emulation.setDeviceMetricsOverride', { width: 375, height: 740, deviceScaleFactor: 2, mobile: true });
  wide = [];
  for (const to of ['/jobs', '/jobs/' + firstJob]) {
    await load(to);
    await adopted();
    if (!await evaluate(noOverflow)) wide.push(to);
  }
  check(wide.length === 0, 'the jobs pages do not scroll sideways on a phone' + (wide.length ? ': ' + wide.join(' ') : ''));
  await load('/jobs/' + third);
  await adopted();
  check(await evaluate(noOverflow) && await evaluate(`(() => { const e = [...document.querySelectorAll('.fui')]
    .find((e) => e.textContent === 'runner faepkub4'); return !!e && e.getBoundingClientRect().right <= innerWidth; })()`),
    'on a phone a long change title wraps and the runner of a check stays in view');
  await send('Emulation.setDeviceMetricsOverride', { width: 1280, height: 800, deviceScaleFactor: 1, mobile: false });
  await load('/jobs?repo=work');
  await adopted();

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
  const landButton = `!!document.querySelector('[data-id$="/land"]')`;
  await load('/work/-/changes/1');
  await until(hasText('Why it matters for the study.'), 'the change page for a write session');
  check(!(await evaluate(landButton)), 'a session without the land scope is offered no Land button');

  // The land scope lands without admin, so its session gets the button.
  const lands = await (await fetch(url + 'api/v1/tokens', {
    method: 'POST',
    headers: { Authorization: `Bearer ${admin}`, 'Content-Type': 'application/json' },
    body: JSON.stringify({ name: 'agent-2', scopes: 'land' }),
  })).json();
  await click('[data-box="sidebar"] [data-id$="/signout"]');
  await until(`!!document.querySelector('[data-id$="/token"]')`, 'signed out of the write session');
  await click('[data-id$="/token"]');
  await send('Input.insertText', { text: lands.token });
  await click('[data-id$="/signin"]');
  await until(hasText('admin'), 'signed in with a land token');
  // Change 1 has landed by now; an open one is offered for landing.
  await load(`/work/-/changes/${second}`);
  await until(landButton, 'the Land button for a land session');
  check(true, 'a session with the land scope and without admin is offered the Land button');

  // Alerts lead the server page, for any signed-in scope: a failed
  // verification of main of its own repository is an alert there until main
  // passes again. The admin token for the API: earlier sections sign out and in.
  {
    const alertAuth = { Authorization: `Bearer ${admin}`, 'Content-Type': 'application/json' };
    await fetch(url + 'api/v1/repos', { method: 'POST', headers: alertAuth, body: JSON.stringify({ name: 'alerting' }) });
    const verified = (state) => execFileSync('sqlite3', [join(data, 'd', 'fortyard.db'),
      `INSERT INTO landings (repo, branch, account, state, kind, created_at, finished_at) VALUES ('alerting', 'main',
      'fortyard', '${state}', 'verify', strftime('%Y-%m-%dT%H:%M:%SZ', 'now'), strftime('%Y-%m-%dT%H:%M:%SZ', 'now'));`]);
    const alertRow = `[...document.querySelectorAll('[data-box="card-alerts"] .fui')].some((e) => e.textContent === 'alerting')`;
    verified('failed');
    const fired = await (await fetch(url + 'api/v1/alerts', { headers: alertAuth })).json();
    check(fired.some((a) => a.rule === 'main-broken' && a.repo === 'alerting' && a.detail.endsWith('failed')),
      'GET /api/v1/alerts lists the failed verification of main as main-broken');
    await load('/server');
    check(await evaluate(`window.__ssrTexts.includes('main-broken') && window.__ssrBoxes.indexOf('card-alerts') >= 0 &&
      window.__ssrBoxes.indexOf('card-alerts') < window.__ssrBoxes.indexOf('card-server')`),
      'the server sends the alert in the page, above the version and commit');
    check(await adopted(), 'and the WebAssembly adopts the server page with its alerts');
    await until(`${hasText('main-broken')} && ${alertRow}`, 'the alert on the server page');
    check(await evaluate(hasText('Delivery')), 'the server page names what the instance serves against main');
    verified('passed');
    await load('/server');
    await until(`!!document.querySelector('[data-box="card-alerts"]') && !(${alertRow})`, 'the alert gone');
    check(true, 'the alert is gone once main passes again');
    check((await fetch(url + 'readyz')).status === 200 && (await fetch(url + 'healthz')).status === 200,
      '/readyz and /healthz answer 200 on a migrated store');
  }
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
