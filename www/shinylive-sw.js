// The site used to run on Shinylive, which installed a service worker at this
// path. Browsers that still have it fetch this file when they check for an
// update; it removes itself so the static site loads directly.
self.addEventListener("install", function () { self.skipWaiting(); });
self.addEventListener("activate", function (event) {
  event.waitUntil(self.registration.unregister().then(function () {
    return self.clients.matchAll({ type: "window" });
  }).then(function (clients) {
    clients.forEach(function (c) { c.navigate(c.url); });
  }));
});
