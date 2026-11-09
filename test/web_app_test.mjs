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
const server = spawn(fy, ['serve', '--data', join(data, 'd'), '--listen', `127.0.0.1:${port}`,
  '--web', web, '--workers', '1', '--threads', '4'], { stdio: 'ignore' });
const url = `http://127.0.0.1:${port}/`;

const browser = spawn(process.env.CHROME || 'chromium', ['--headless=new',
  '--remote-debugging-port=0', '--no-first-run', '--window-size=1000,700',
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

try {
  await send('Runtime.enable');
  await send('Page.enable');
  await send('Network.enable');
  await send('Page.navigate', { url });
  await until(`!!document.querySelector('[data-id="token"]')`, 'sign-in form');
  check(true, 'the application loads and asks to sign in');

  await click('[data-id="token"]');
  await type(admin);
  await click('[data-id="signin"]');
  await until(hasText('admin'), 'signed in');
  check(true, 'signing in with a token shows the account');
  const cookies = (await send('Network.getCookies', { urls: [url] })).result.cookies;
  const session = cookies.find((c) => c.name === 'fy_session');
  check(session && session.httpOnly && session.sameSite === 'Strict', 'the session is an HttpOnly, SameSite=Strict cookie');
  check(await evaluate(`document.cookie === ''`), 'scripts on the page cannot read it');
  check(await evaluate(hasText('Commit ' + (await (await fetch(url + 'api/v1/version')).json()).commit)),
    'the overview shows the server commit');

  await click('[data-id$="#2"]');
  await until(`!!document.querySelector('[data-id="newname"]')`, 'tokens tab');
  await until(hasText('this session'), 'token list');
  check(await evaluate(hasText('bootstrap')), 'the token list shows the bootstrap token');
  await click('[data-id="newname"]');
  await type('laptop');
  await click('[data-id="create"]');
  await until(`[...document.querySelectorAll('.fui-k1')].some((e) => /^fy_[0-9a-f]{64}$/.test(e.textContent))`, 'new token');
  check(await evaluate(hasText('laptop')), 'a created token is shown once and listed');
  const revokes = await evaluate(`document.querySelectorAll('[data-id*="/revoke#"]').length`);
  await click('[data-id*="/revoke#"]');
  await until(`document.querySelectorAll('[data-id*="/revoke#"]').length === ${revokes - 1}`, 'revocation');
  check(await evaluate(hasText('revoked')), 'revoking a token marks it revoked');

  if (shot) {
    const png = await send('Page.captureScreenshot', { format: 'png' });
    writeFileSync(shot, Buffer.from(png.result.data, 'base64'));
  }
  await click('[data-id="signout"]');
  await until(`!!document.querySelector('[data-id="token"]')`, 'signed out');
  const after = await fetch(url + 'api/v1/whoami', { headers: { Cookie: `fy_session=${session.value}` } });
  check(after.status === 401, 'signing out ends the session on the server');
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
