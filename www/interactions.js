// Motion and game feedback for the dashboard.
//
// Principles in play:
//   squash & stretch  - buttons, rail icons and the HUD on impact
//   staging           - a page enters first, then its sections in order
//   follow-through    - spring curves overshoot and settle (indicator, pages, flips)
//   arcs              - XP coins fly along a curved path to the HUD
//   secondary action  - icon wiggle, tilt glare and the drifting contact shadow
//   timing            - one set of durations and curves (see styles.css :root)
//   solid drawing     - 3D tilt with perspective, depth and a moving shadow
// Everything decorative is skipped when the person prefers reduced motion.
(function () {
  "use strict";

  var reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var finePointer = window.matchMedia("(hover: hover) and (pointer: fine)").matches;
  var SPRING = "cubic-bezier(0.34, 1.56, 0.64, 1)";
  var EASE_OUT = "cubic-bezier(0.22, 1, 0.36, 1)";
  var MAX_TILT = 7;
  var navOrigin = null;      // the element a page should spring out from

  function $(sel, root) { return (root || document).querySelector(sel); }
  function $all(sel, root) { return Array.prototype.slice.call((root || document).querySelectorAll(sel)); }
  function center(el) {
    var r = el.getBoundingClientRect();
    return { x: r.left + r.width / 2, y: r.top + r.height / 2, r: r };
  }

  // ---------- Squash & stretch on press ----------
  function squash(el) {
    if (reduceMotion || !el) return;
    el.classList.remove("is-squash");
    void el.offsetWidth;                       // restart the animation
    el.classList.add("is-squash");
  }
  document.addEventListener("animationend", function (e) {
    if (e.animationName === "squash") e.target.classList.remove("is-squash");
  });
  document.addEventListener("pointerdown", function (e) {
    var t = e.target.closest(".btn-pill, .qa-toggle, .qa-item, .rail-item .rail-icon, .rail-item");
    if (!t) return;
    squash(t.classList.contains("rail-item") ? t.querySelector(".rail-icon") : t);
  });

  // ---------- Ripple ----------
  function ripple(host, e) {
    if (reduceMotion) return;
    var r = host.getBoundingClientRect();
    var size = Math.max(r.width, r.height);
    var dot = document.createElement("span");
    dot.className = "ripple";
    dot.style.width = dot.style.height = size + "px";
    dot.style.left = ((e.clientX || r.left + r.width / 2) - r.left - size / 2) + "px";
    dot.style.top = ((e.clientY || r.top + r.height / 2) - r.top - size / 2) + "px";
    host.appendChild(dot);
    dot.addEventListener("animationend", function () { dot.remove(); });
  }

  // ---------- Rail: active indicator with follow-through ----------
  function placeIndicator(item) {
    var ind = $(".rail-indicator");
    if (!ind || !item) return;
    var horizontal = window.matchMedia("(max-width: 760px)").matches;
    ind.style.transform = horizontal
      ? "translateX(" + item.offsetLeft + "px)"
      : "translateY(" + item.offsetTop + "px)";
  }
  function setActive(target) {
    $all(".rail-item").forEach(function (b) {
      var on = b.getAttribute("data-target") === target;
      b.classList.toggle("is-active", on);
      if (on) { b.setAttribute("aria-current", "page"); placeIndicator(b); }
      else b.removeAttribute("aria-current");
    });
  }
  $all(".rail-item").forEach(function (b, i) { b.style.setProperty("--i", i); });
  window.addEventListener("resize", function () { placeIndicator($(".rail-item.is-active")); });
  document.addEventListener("DOMContentLoaded", function () { placeIndicator($(".rail-item.is-active")); });
  window.addEventListener("load", function () { placeIndicator($(".rail-item.is-active")); });

  function go(target, originEl) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    navOrigin = originEl || null;
    setActive(target);
    closeQuickActions();
    Shiny.setInputValue("rail_go", target, { priority: "event" });
  }

  // ---------- Page transition: spring out of the icon, then stage ----------
  function springIn(pane) {
    if (reduceMotion || !pane.animate) return;
    var from = navOrigin ? center(navOrigin) : null;
    var to = center(pane);
    var dx = from ? from.x - to.x : 0;
    var dy = from ? Math.min(from.y, window.innerHeight) - Math.max(to.r.top + 120, to.y) : 40;
    pane.animate([
      { transform: "translate(" + dx + "px," + dy + "px) scale(0.06)", opacity: 0, borderRadius: "48px" },
      { transform: "translate(0,0) scale(1.025)", opacity: 1, offset: 0.62 },
      { transform: "scale(0.992)", offset: 0.82 },
      { transform: "none", opacity: 1, borderRadius: "0px" }
    ], { duration: 660, easing: EASE_OUT });

    // Staging: heading, then the main panels, then supporting detail.
    ["stage-1", "stage-2", "stage-3"].forEach(function (cls, i) {
      $all("." + cls, pane).forEach(function (el, j) {
        el.animate([
          { transform: "translateY(22px) scale(0.98)", opacity: 0 },
          { transform: "translateY(-3px) scale(1.005)", opacity: 1, offset: 0.7 },
          { transform: "none", opacity: 1 }
        ], { duration: 560, delay: 160 + i * 110 + j * 45, easing: EASE_OUT, fill: "backwards" });
      });
    });
    navOrigin = null;
  }

  if (window.jQuery) {
    jQuery(document).on("shown.bs.tab", "#pages a", function (e) {
      var pane = document.querySelector(e.target.getAttribute("href"));
      var value = e.target.getAttribute("data-value");
      setActive(value);
      if (pane) springIn(pane);
      window.dispatchEvent(new Event("resize"));   // let plots resize
    });
  }

  // ---------- Quick actions ----------
  function closeQuickActions() {
    var qa = $(".quick-actions");
    if (!qa) return;
    qa.classList.remove("is-open");
    var t = $(".qa-toggle");
    if (t) t.setAttribute("aria-expanded", "false");
  }
  function toggleQuickActions() {
    var qa = $(".quick-actions");
    var open = !qa.classList.contains("is-open");
    qa.classList.toggle("is-open", open);
    $(".qa-toggle").setAttribute("aria-expanded", open ? "true" : "false");
    if (open) { var first = $(".qa-item"); if (first) setTimeout(function () { first.focus(); }, 120); }
  }
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") closeQuickActions();
  });

  // ---------- Click routing ----------
  document.addEventListener("click", function (e) {
    var card = e.target.closest(".flip-card");
    if (card) {
      toggleFlip(card);
      var face = card.querySelector(card.classList.contains("is-flipped") ? ".flip-back" : ".flip-front");
      if (face) ripple(face, e);
      return;
    }
    var toggle = e.target.closest(".qa-toggle");
    if (toggle) { toggleQuickActions(); return; }

    var action = e.target.closest("[data-action='play']");
    if (action) {
      go("play", action.closest(".qa-item") ? $(".rail-item[data-target='play']") : action);
      Shiny.setInputValue("qa_play", Date.now(), { priority: "event" });
      return;
    }
    var nav = e.target.closest("[data-target]");
    if (nav) {
      var fromRail = nav.classList.contains("rail-item");
      if (fromRail) ripple(nav, e);
      var origin = fromRail ? nav.querySelector(".rail-icon")
                            : $(".rail-item[data-target='" + nav.getAttribute("data-target") + "'] .rail-icon");
      go(nav.getAttribute("data-target"), origin);
      return;
    }
    var pill = e.target.closest(".btn-pill");
    if (pill) ripple(pill, e);
    if (!e.target.closest(".quick-actions")) closeQuickActions();
  });

  // ---------- Flip cards ----------
  function toggleFlip(card) {
    var flipped = card.classList.toggle("is-flipped");
    card.setAttribute("aria-pressed", flipped ? "true" : "false");
  }
  document.addEventListener("keydown", function (e) {
    var card = e.target.closest && e.target.closest(".flip-card");
    if (card && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); toggleFlip(card); }
  });

  // ---------- 3D tilt with glare and drifting shadow ----------
  if (!reduceMotion && finePointer) {
    document.addEventListener("pointermove", function (e) {
      var card = e.target.closest && e.target.closest(".flip-card");
      if (!card) return;
      var tilt = card.querySelector(".flip-tilt");
      var r = card.getBoundingClientRect();
      var px = (e.clientX - r.left) / r.width;
      var py = (e.clientY - r.top) / r.height;
      tilt.style.transform = "rotateX(" + ((0.5 - py) * 2 * MAX_TILT).toFixed(2) + "deg) rotateY(" +
        ((px - 0.5) * 2 * MAX_TILT).toFixed(2) + "deg) translateZ(0)";
      card.style.setProperty("--gx", (px * 100).toFixed(1) + "%");
      card.style.setProperty("--gy", (py * 100).toFixed(1) + "%");
      card.style.setProperty("--sx", ((0.5 - px) * 16).toFixed(1) + "px");
    });
    document.addEventListener("pointerout", function (e) {
      var card = e.target.closest && e.target.closest(".flip-card");
      if (card && !card.contains(e.relatedTarget)) {
        card.querySelector(".flip-tilt").style.transform = "";
        card.style.setProperty("--sx", "0px");
      }
    });
  }

  // ---------- Count-up numbers ----------
  function countTo(el, to, ms) {
    if (!el) return;
    var from = parseFloat((el.textContent || "0").replace(/,/g, "")) || 0;
    if (reduceMotion || from === to) { el.textContent = Math.round(to).toLocaleString(); return; }
    var start = performance.now();
    function frame(now) {
      var t = Math.min(1, (now - start) / ms);
      var eased = 1 - Math.pow(1 - t, 3);
      el.textContent = Math.round(from + (to - from) * eased).toLocaleString();
      if (t < 1) requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);
  }

  // ---------- Guess readout follows the slider ----------
  // Shiny and ion.rangeSlider fire jQuery events, so listen through jQuery.
  function syncGuess(bump) {
    var input = document.getElementById("guess");
    var el = $("#guess-num");
    if (!input || !el) return;
    el.textContent = Math.round(parseFloat(input.value) || 0).toLocaleString();
    if (bump && !reduceMotion) { el.classList.remove("is-bump"); void el.offsetWidth; el.classList.add("is-bump"); }
  }

  function revealResult() {
    var card = $(".result-card");
    if (!card || !$(".race", card)) { if (card) card.classList.remove("is-revealed"); return; }
    card.classList.remove("is-revealed");
    void card.offsetWidth;
    requestAnimationFrame(function () { card.classList.add("is-revealed"); });
    var num = $(".actual-num", card);
    if (num) { num.textContent = "0"; countTo(num, parseFloat(num.getAttribute("data-count")), 900); }
  }

  if (window.jQuery) {
    jQuery(document).on("change input", "#guess", function () { syncGuess(true); });
    jQuery(document).on("shiny:value", function (e) {
      if (e.name === "game_result") setTimeout(revealResult, 0);
      if (e.name === "game_round") setTimeout(function () { syncGuess(false); }, 60);
    });
    jQuery(document).on("shiny:updateinput", function (e) {
      if (e.target && e.target.id === "guess") setTimeout(function () { syncGuess(false); }, 0);
    });
  }

  // ---------- XP coin along an arc ----------
  function flyCoin(fromEl, toEl, delay, done) {
    if (reduceMotion || !fromEl || !toEl) { if (done) done(); return; }
    var a = center(fromEl), b = center(toEl);
    var lift = Math.max(120, Math.abs(a.x - b.x) * 0.35);
    var cx = (a.x + b.x) / 2, cy = Math.min(a.y, b.y) - lift;   // control point above both
    var frames = [];
    for (var i = 0; i <= 16; i++) {
      var t = i / 16, u = 1 - t;
      var x = u * u * a.x + 2 * u * t * cx + t * t * b.x;
      var y = u * u * a.y + 2 * u * t * cy + t * t * b.y;
      // Stretch along the path mid-flight, squash on arrival.
      var s = i === 16 ? "scale(1.35,0.7)" : "scale(" + (1 + 0.25 * Math.sin(Math.PI * t)) + "," + (1 - 0.15 * Math.sin(Math.PI * t)) + ")";
      frames.push({ transform: "translate(" + (x - 13) + "px," + (y - 13) + "px) " + s, offset: t });
    }
    var coin = document.createElement("span");
    coin.className = "xp-coin";
    coin.style.transform = frames[0].transform;
    document.body.appendChild(coin);
    var anim = coin.animate(frames, { duration: 820, delay: delay || 0, easing: "cubic-bezier(0.45, 0, 0.25, 1)", fill: "both" });
    anim.onfinish = function () { coin.remove(); if (done) done(); };
  }

  function waitFor(sel, ms, cb) {
    var start = performance.now();
    (function poll() {
      var el = $(sel);
      if (el) return cb(el);
      if (performance.now() - start > ms) return cb(null);
      setTimeout(poll, 40);
    })();
  }

  function toast(html) {
    var stack = $(".toast-stack");
    if (!stack) { stack = document.createElement("div"); stack.className = "toast-stack"; document.body.appendChild(stack); }
    var t = document.createElement("div");
    t.className = "toast-glass glass";
    t.setAttribute("role", "status");
    t.innerHTML = html;
    stack.appendChild(t);
    setTimeout(function () {
      t.classList.add("is-leaving");
      t.addEventListener("animationend", function () { t.remove(); });
    }, 3200);
  }

  function updateHud(m) {
    countTo($("#hud-xp"), m.xp, 700);
    $("#hud-level").textContent = m.level;
    $("#hud-streak").textContent = m.streak;
    $("#hud-badges").textContent = m.badges;
    var ring = $(".ring-fill");
    if (ring) ring.style.strokeDashoffset = (113.1 * (1 - m.progress)).toFixed(1);
    (m.held || []).forEach(function (id) {
      var tile = $("#badge-" + id);
      if (tile) tile.classList.add("is-earned");
    });
    var hud = $(".hud");
    if (hud && !reduceMotion) { hud.classList.remove("is-hit"); void hud.offsetWidth; hud.classList.add("is-hit"); }
  }

  if (window.Shiny) {
    Shiny.addCustomMessageHandler("hud", function (m) {
      var target = $(".hud-level");
      var apply = function () {
        updateHud(m);
        (m.new_badges || []).forEach(function (b, i) {
          var tile = $("#badge-" + b.id);
          if (tile) { tile.classList.add("is-earned", "just-earned"); setTimeout(function () { tile.classList.remove("just-earned"); }, 1000); }
          setTimeout(function () {
            toast('<i class="fas fa-medal" aria-hidden="true"></i> Badge unlocked: ' + b.name);
          }, i * 350);
        });
      };
      if (m.gained > 0 && m.source === "result") {
        // Wait for the result card to render, then send coins from the XP chip.
        waitFor("#xp-chip", 1500, function (chip) {
          if (!chip) { apply(); return; }
          var n = Math.min(5, Math.max(1, Math.round(m.gained / 10)));
          var landed = 0;
          for (var i = 0; i < n; i++) {
            flyCoin(chip, target, 650 + i * 90, function () { if (++landed === n) apply(); });
          }
        });
      } else if (m.gained > 0 && m.source === "rail") {
        flyCoin($(".rail-item.is-active .rail-icon"), target, 200, apply);
      } else {
        apply();
      }
    });
  }
})();
