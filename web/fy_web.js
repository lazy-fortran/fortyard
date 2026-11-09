// HTTP requests and the location for the fortyard browser application.
// Fortran starts a request and polls for its result by id; a finished request
// wakes the fortui frame loop, so an idle page reacts without user input.
// Requests go to the page's own origin with its session cookie. Responses the
// server already sent with the page (<script id="fy-pre">) answer the first
// request for their path at once, so the page continues without asking again.
// The application's paths never carry the path prefix it is served under (for
// example /yard, from <meta name="fy-base">); it is added and removed here.
addToLibrary({
  $FY__deps: ['$FUI', '$UTF8ToString'],
  $FY: { next: 1, requests: new Map(), pre: null, popped: null, title: '' },
  $FY_base__deps: [],
  $FY_base: function () {
    var m = document.querySelector('meta[name="fy-base"]');
    return m ? m.content : '';
  },
  $FY_here__deps: ['$FY_base'],
  $FY_here: function () {
    var b = FY_base(), p = location.pathname;
    if (b && p.indexOf(b) === 0) p = p.slice(b.length) || '/';
    return p + location.search;
  },

  fy_fetch_start__deps: ['$FY', '$FY_base'],
  fy_fetch_start: function (mPtr, mLen, pPtr, pLen, bPtr, bLen) {
    var method = UTF8ToString(mPtr, mLen);
    var path = UTF8ToString(pPtr, pLen);
    var body = bLen > 0 ? UTF8ToString(bPtr, bLen) : undefined;
    var id = FY.next++;
    var req = { done: false, status: 0, body: new Uint8Array(0) };
    FY.requests.set(id, req);
    if (FY.pre === null) {
      FY.pre = new Map();
      var el = document.getElementById('fy-pre');
      if (el) JSON.parse(el.textContent).forEach(function (r) { FY.pre.set(r.p, r); });
    }
    var pre = method === 'GET' ? FY.pre.get(path) : undefined;
    if (pre) {
      FY.pre.delete(path);
      req.done = true;
      req.status = pre.s;
      req.body = new TextEncoder().encode(pre.b);
      return id;
    }
    var init = { method: method, credentials: 'same-origin', headers: {} };
    if (body !== undefined) {
      init.body = body;
      init.headers['Content-Type'] = 'application/json';
    }
    fetch(path.charAt(0) === '/' ? FY_base() + path : path, init).then(function (r) {
      req.status = r.status;
      return r.arrayBuffer();
    }).then(function (buf) {
      req.body = new Uint8Array(buf);
    }).catch(function () {
      req.status = 0;
    }).finally(function () {
      req.done = true;
      FUI.wakeUp();
    });
    return id;
  },

  // -1 while pending, else the HTTP status (0 when the server was not reached).
  fy_fetch_status__deps: ['$FY'],
  fy_fetch_status: function (id) {
    var req = FY.requests.get(id);
    if (!req) return 0;
    return req.done ? req.status : -1;
  },

  fy_fetch_length__deps: ['$FY'],
  fy_fetch_length: function (id) {
    var req = FY.requests.get(id);
    return req ? req.body.length : 0;
  },

  // Copies the body and forgets the request.
  fy_fetch_take__deps: ['$FY'],
  fy_fetch_take: function (id, ptr, cap) {
    var req = FY.requests.get(id);
    if (!req) return 0;
    var n = Math.min(cap, req.body.length);
    HEAPU8.set(req.body.subarray(0, n), ptr);
    FY.requests.delete(id);
    return n;
  },

  // Server-sent event streams (web/fy_stream.f90): each event of the topic
  // counts and wakes the frame loop.
  fy_stream_open__deps: ['$FY', '$FY_base', '$FUI'],
  fy_stream_open: function (pPtr, pLen, tPtr, tLen) {
    var id = FY.next++, s = { count: 0, es: new EventSource(FY_base() + UTF8ToString(pPtr, pLen)) };
    s.es.addEventListener(UTF8ToString(tPtr, tLen), function () { s.count++; FUI.wakeUp(); });
    FY.streams = FY.streams || new Map();
    FY.streams.set(id, s);
    return id;
  },
  fy_stream_count__deps: ['$FY'],
  fy_stream_count: function (id) {
    var s = FY.streams && FY.streams.get(id);
    return s ? s.count : 0;
  },
  fy_stream_close__deps: ['$FY'],
  fy_stream_close: function (id) {
    var s = FY.streams && FY.streams.get(id);
    if (s) { s.es.close(); FY.streams.delete(id); }
  },

  // Location, title, preferences and clipboard (web/fy_nav.f90).
  fy_nav_call__deps: ['$FY', '$FY_base', '$FY_here'],
  fy_nav_call: function (what, pPtr, pLen, qPtr, qLen) {
    var s = UTF8ToString(pPtr, pLen);
    switch (what) {
      case 1:
        if (s !== FY_here()) history.pushState(null, '', FY_base() + s);
        window.scrollTo(0, 0);
        break;
      case 2:
        if (s !== FY.title) { FY.title = s; document.title = s; }
        break;
      case 3:
        location.assign(s.charAt(0) === '/' ? FY_base() + s : s);
        break;
      case 4: {
        var v = UTF8ToString(qPtr, qLen);
        document.cookie = s + '=' + encodeURIComponent(v) + '; Path=' + (FY_base() || '/') + '; Max-Age=31536000; SameSite=Lax';
        if (s === 'fy_theme') {
          if (v === 'light' || v === 'dark') document.documentElement.dataset.theme = v;
          else delete document.documentElement.dataset.theme;
        }
        break;
      }
      case 5:
        if (s.charAt(0) === '/') s = location.origin + FY_base() + s;
        // Plain HTTP (no secure context) has no Clipboard API: copy a selection.
        if (navigator.clipboard) { navigator.clipboard.writeText(s).catch(function () {}); break; }
        var ta = document.createElement('textarea');
        ta.value = s; ta.setAttribute('readonly', '');
        ta.style.position = 'fixed'; ta.style.opacity = '0';
        document.body.appendChild(ta); ta.select();
        try { document.execCommand('copy'); } catch (e) {}
        ta.remove();
        break;
    }
  },

  // The page's path and query (1); the location Back or Forward went to, once
  // (2); a cookie's value (3); the path prefix the application is served under (4).
  // Copied into buf; the result is its length.
  fy_nav_get__deps: ['$FY', '$FUI', '$FY_here', '$FY_base'],
  fy_nav_get: function (what, nPtr, nLen, buf, cap) {
    if (FY.popped === null) {
      FY.popped = '';
      window.addEventListener('popstate', function () {
        FY.popped = FY_here();
        FUI.wakeUp();
      });
    }
    var s = '';
    if (what === 1) s = FY_here();
    if (what === 4) s = FY_base();
    if (what === 2) { s = FY.popped; FY.popped = ''; }
    if (what === 3) {
      var name = UTF8ToString(nPtr, nLen) + '=';
      document.cookie.split('; ').forEach(function (c) {
        if (c.indexOf(name) === 0) s = decodeURIComponent(c.slice(name.length));
      });
    }
    var bytes = new TextEncoder().encode(s);
    var n = Math.min(bytes.length, cap);
    HEAPU8.set(bytes.subarray(0, n), buf);
    return n;
  },
});
