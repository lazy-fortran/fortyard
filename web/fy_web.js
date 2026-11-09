// HTTP requests for the fortyard browser application. Fortran starts a
// request and polls for its result by id; a finished request wakes the
// fortui frame loop, so an idle page reacts without user input.
// Requests go to the page's own origin with its session cookie.
addToLibrary({
  $FY__deps: ['$FUI', '$UTF8ToString'],
  $FY: { next: 1, requests: new Map() },

  fy_fetch_start__deps: ['$FY'],
  fy_fetch_start: function (mPtr, mLen, pPtr, pLen, bPtr, bLen) {
    var method = UTF8ToString(mPtr, mLen);
    var path = UTF8ToString(pPtr, pLen);
    var body = bLen > 0 ? UTF8ToString(bPtr, bLen) : undefined;
    var id = FY.next++;
    var req = { done: false, status: 0, body: new Uint8Array(0) };
    FY.requests.set(id, req);
    var init = { method: method, credentials: 'same-origin', headers: {} };
    if (body !== undefined) {
      init.body = body;
      init.headers['Content-Type'] = 'application/json';
    }
    fetch(path, init).then(function (r) {
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
});
