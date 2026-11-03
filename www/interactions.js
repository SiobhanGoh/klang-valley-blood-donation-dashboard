// Motion for the dashboard.
//
//   staging         - a page enters first, then its panels in order
//   follow-through  - spring curves settle the rail indicator and segment thumbs
//   secondary action- card backs fill their bars and waves after a flip
//   timing          - one set of durations and curves (see styles.css :root)
// Everything decorative is skipped when the person prefers reduced motion.
(function () {
  "use strict";

  var reduceMotion = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  var EASE_OUT = "cubic-bezier(0.22, 1, 0.36, 1)";
  var navOrigin = null;      // the element a page should grow out of

  function $(sel, root) { return (root || document).querySelector(sel); }
  function $all(sel, root) { return Array.prototype.slice.call((root || document).querySelectorAll(sel)); }
  function center(el) {
    var r = el.getBoundingClientRect();
    return { x: r.left + r.width / 2, y: r.top + r.height / 2, r: r };
  }

  // ---------- Rail: active indicator ----------
  function placeIndicator(item) {
    var ind = $(".rail-indicator");
    if (!ind || !item) return;
    var horizontal = window.matchMedia("(max-width: 760px)").matches;
    ind.style.transform = horizontal
      ? "translateX(" + item.offsetLeft + "px)"
      : "translateY(" + item.offsetTop + "px)";
    ind.style.width = horizontal ? item.offsetWidth + "px" : "";
  }
  function setActive(target) {
    $all(".rail-item").forEach(function (b) {
      var on = b.getAttribute("data-target") === target;
      b.classList.toggle("is-active", on);
      if (on) { b.setAttribute("aria-current", "page"); placeIndicator(b); }
      else b.removeAttribute("aria-current");
    });
  }
  function refreshLayout() {
    placeIndicator($(".rail-item.is-active"));
    $all(".seg").forEach(placeThumb);
  }
  window.addEventListener("resize", refreshLayout);
  window.addEventListener("load", refreshLayout);

  function go(target, originEl) {
    if (!window.Shiny || !Shiny.setInputValue) return;
    navOrigin = originEl || null;
    setActive(target);
    closeQuickActions();
    window.scrollTo({ top: 0, behavior: reduceMotion ? "auto" : "smooth" });
    Shiny.setInputValue("rail_go", target, { priority: "event" });
  }

  // ---------- Page transition: grow out of the icon, then stage ----------
  function springIn(pane) {
    if (reduceMotion || !pane.animate) return;
    var from = navOrigin ? center(navOrigin) : null;
    var to = center(pane);
    var dx = from ? (from.x - to.x) * 0.25 : 0;
    var dy = from ? Math.max(-80, Math.min(80, (from.y - to.r.top) * 0.15)) : 24;
    pane.animate([
      { transform: "translate(" + dx + "px," + dy + "px) scale(0.965)", opacity: 0 },
      { transform: "none", opacity: 1 }
    ], { duration: 520, easing: EASE_OUT });

    // Staging: heading, then the main panels, then supporting detail.
    ["stage-1", "stage-2", "stage-3"].forEach(function (cls, i) {
      $all("." + cls, pane).forEach(function (el, j) {
        el.animate([
          { transform: "translateY(16px)", opacity: 0 },
          { transform: "none", opacity: 1 }
        ], { duration: 520, delay: 90 + i * 90 + j * 40, easing: EASE_OUT, fill: "backwards" });
      });
    });
    navOrigin = null;
  }

  if (window.jQuery) {
    jQuery(document).on("shown.bs.tab", "#pages a", function (e) {
      var pane = document.querySelector(e.target.getAttribute("href"));
      setActive(e.target.getAttribute("data-value"));
      if (pane) { springIn(pane); setTimeout(function () { countUpIn(pane); }, 160); }
      // Charts drawn in a hidden page need their real size.
      setTimeout(function () { window.dispatchEvent(new Event("resize")); }, 30);
    });
  }

  // ---------- Segmented controls: a thumb that slides to the choice ----------
  function placeThumb(seg) {
    var group = $(".shiny-options-group", seg);
    if (!group) return;
    var thumb = $(".seg-thumb", group);
    if (!thumb) {
      thumb = document.createElement("span");
      thumb.className = "seg-thumb";
      thumb.setAttribute("aria-hidden", "true");
      group.insertBefore(thumb, group.firstChild);
    }
    var checked = $("input:checked", group);
    var label = checked && checked.closest("label");
    if (!label || !label.offsetWidth) { thumb.style.opacity = "0"; return; }
    thumb.style.opacity = "1";
    thumb.style.width = label.offsetWidth + "px";
    thumb.style.height = label.offsetHeight + "px";
    thumb.style.transform = "translate(" + label.offsetLeft + "px," + label.offsetTop + "px)";
  }
  document.addEventListener("change", function (e) {
    var seg = e.target.closest && e.target.closest(".seg");
    if (seg) placeThumb(seg);
    // Switching report type reveals other controls; place their thumbs once visible.
    if (e.target.name === "report_type") setTimeout(function () { $all(".seg").forEach(placeThumb); }, 80);
  });
  if (window.jQuery) {
    jQuery(document).on("shiny:connected shiny:bound", function () {
      setTimeout(refreshLayout, 50);
    });
  }
  document.addEventListener("DOMContentLoaded", function () { setTimeout(refreshLayout, 50); });

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
    if (card) { toggleFlip(card); return; }

    // Blood-type picker feeds a Shiny input.
    var pick = e.target.closest(".type-pick");
    if (pick) {
      $all(".type-pick", pick.parentElement).forEach(function (b) {
        var on = b === pick;
        b.classList.toggle("is-on", on);
        b.setAttribute("aria-checked", on ? "true" : "false");
      });
      if (window.Shiny) Shiny.setInputValue("bt_pick", pick.getAttribute("data-type"), { priority: "event" });
      return;
    }
    var toggle = e.target.closest(".qa-toggle");
    if (toggle) { toggleQuickActions(); return; }

    var nav = e.target.closest("[data-target]");
    if (nav) {
      var fromRail = nav.classList.contains("rail-item");
      var origin = fromRail ? nav.querySelector(".rail-icon")
                            : $(".rail-item[data-target='" + nav.getAttribute("data-target") + "'] .rail-icon");
      go(nav.getAttribute("data-target"), origin);
      return;
    }
    if (!e.target.closest(".quick-actions")) closeQuickActions();
  });

  // ---------- Flip cards ----------
  // Only the inner layer rotates; the card keeps its box, so nothing shifts.
  function toggleFlip(card) {
    var flipped = card.classList.toggle("is-flipped");
    card.setAttribute("aria-pressed", flipped ? "true" : "false");
    var back = $(".flip-back", card), front = $(".flip-front", card);
    if (back) back.setAttribute("aria-hidden", flipped ? "false" : "true");
    if (front) front.setAttribute("aria-hidden", flipped ? "true" : "false");
  }
  document.addEventListener("keydown", function (e) {
    var card = e.target.closest && e.target.closest(".flip-card");
    if (card && (e.key === "Enter" || e.key === " ")) { e.preventDefault(); toggleFlip(card); }
  });



  // ---------- Glass tooltips for every chart ----------
  // Plotly still tracks the hover; its own label is hidden (styles.css) and a
  // frosted tooltip with roomy spacing shows the same values instead.
  var MONTHS = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
  var MONTHS_LONG = ["January", "February", "March", "April", "May", "June", "July", "August",
                     "September", "October", "November", "December"];
  var DAYS = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  function fmtDate(v, fmt) {
    var m = /^(\d{4})-(\d{2})-(\d{2})/.exec(String(v));
    if (!m) return String(v);
    var d = new Date(Date.UTC(+m[1], +m[2] - 1, +m[3]));
    fmt = fmt || "%a %d %b %Y";
    return fmt.replace("%a", DAYS[d.getUTCDay()]).replace("%d", String(d.getUTCDate()))
              .replace("%B", MONTHS_LONG[d.getUTCMonth()]).replace("%b", MONTHS[d.getUTCMonth()])
              .replace("%Y", m[1]);
  }
  function fmtNum(y) {
    if (typeof y !== "number") return String(y);
    var dp = Math.abs(y) >= 100 ? 0 : 1;
    return y.toLocaleString("en-GB", { minimumFractionDigits: dp, maximumFractionDigits: dp });
  }
  function traceColour(t) {
    return (t.marker && typeof t.marker.color === "string" && t.mode !== "lines+markers" ? t.marker.color : null) ||
           (t.line && t.line.color) || (t.marker && t.marker.color) || "#A3213F";
  }
  var tipEl = null;
  function ensureTip() {
    if (tipEl) return tipEl;
    tipEl = document.createElement("div");
    tipEl.className = "glass-tip";
    tipEl.setAttribute("role", "tooltip");
    document.body.appendChild(tipEl);
    return tipEl;
  }
  function showTip(gd, ev) {
    var pts = (ev.points || []).filter(function (p) { return p.data && p.data.visible !== "legendonly"; });
    if (!pts.length) return;
    var unified = gd.layout && /unified/.test(gd.layout.hovermode || "");
    var xfmt = gd.layout && gd.layout.xaxis && gd.layout.xaxis.hoverformat;
    var html = "";
    if (unified) {
      var x = pts[0].x;
      html += '<div class="gt-head">' + (/^\d{4}-\d{2}-\d{2}/.test(String(x)) ? fmtDate(x, xfmt) : x) + "</div>";
      pts.slice().sort(function (a, b) { return (b.y || 0) - (a.y || 0); }).forEach(function (p) {
        html += '<div class="gt-row"><span class="gt-sw" style="background:' + traceColour(p.fullData || p.data) + '"></span>' +
                '<span class="gt-name">' + p.data.name + '</span><span class="gt-val">' + fmtNum(p.y) + "</span></div>";
      });
    } else {
      pts.forEach(function (p) {
        var body = p.hovertext || p.text;
        if (Array.isArray(body)) body = body[p.pointNumber];
        html += '<div class="gt-block"><span class="gt-sw" style="background:' + traceColour(p.fullData || p.data) + '"></span>' +
                "<div>" + (body || (p.data.name + "<br>" + fmtNum(p.y))) + "</div></div>";
      });
    }
    var tip = ensureTip();
    tip.innerHTML = html;
    var e = ev.event || {};
    var x0 = e.clientX || 0, y0 = e.clientY || 0;
    tip.classList.add("is-on");
    var w = tip.offsetWidth, h = tip.offsetHeight;
    var left = x0 + 18, top = y0 - h / 2;
    if (left + w > window.innerWidth - 12) left = x0 - w - 18;
    top = Math.max(12, Math.min(window.innerHeight - h - 12, top));
    tip.style.transform = "translate(" + Math.round(left) + "px," + Math.round(top) + "px)";
  }
  function hideTip() { if (tipEl) tipEl.classList.remove("is-on"); }
  // A redraw can drop Plotly's listeners, so re-attach on every new render.
  function wireTips(gd) {
    if (!gd.on) return;
    if (gd.__tipHover) {
      gd.removeListener("plotly_hover", gd.__tipHover);
      gd.removeListener("plotly_unhover", hideTip);
    }
    gd.__tipHover = function (ev) { showTip(gd, ev); };
    gd.on("plotly_hover", gd.__tipHover);
    gd.on("plotly_unhover", hideTip);
    if (!gd.__tipLeave) { gd.__tipLeave = true; gd.addEventListener("mouseleave", hideTip); }
  }
  window.addEventListener("scroll", hideTip, { passive: true });

  // ---------- Chart entrances ----------
  // Bars rise from the baseline; lines draw from left to right. Each chart
  // animates when new data arrives and it is on screen, never on hover.
  function animateChart(gd) {
    if (reduceMotion || !gd.animate) return;
    var bars = $all(".barlayer .point path", gd);
    bars.forEach(function (bar, i) {
      bar.style.transformBox = "fill-box";
      bar.style.transformOrigin = "50% 100%";
      bar.animate([{ transform: "scaleY(0)" }, { transform: "scaleY(1)" }],
                  { duration: 720, delay: Math.min(i * 14, 420), easing: EASE_OUT, fill: "backwards" });
    });
    $all(".scatterlayer", gd).forEach(function (layer) {
      if (!layer.querySelector(".js-line, .point")) return;
      layer.animate([{ clipPath: "inset(-20px 100% -20px -20px)" },
                     { clipPath: "inset(-20px -20px -20px -20px)" }],
                    { duration: 1300, easing: "cubic-bezier(0.45, 0, 0.2, 1)" });
    });
  }
  // On phones the 2 x 2 group grids have no room for method names under the
  // bars; the legend and the hover labels name them instead.
  function compactForPhone(gd) {
    if (window.innerWidth > 600 || !window.Plotly || !gd.layout || !gd.layout.xaxis4) return;
    var upd = { "legend.y": -0.08 };
    ["xaxis", "xaxis2", "xaxis3", "xaxis4"].forEach(function (a) { upd[a + ".showticklabels"] = false; });
    window.Plotly.relayout(gd, upd);
  }
  var pendingCharts = new Set();
  var chartObserver = "IntersectionObserver" in window ? new IntersectionObserver(function (entries) {
    entries.forEach(function (en) {
      if (en.isIntersecting && pendingCharts.has(en.target)) {
        pendingCharts.delete(en.target);
        chartObserver.unobserve(en.target);
        animateChart(en.target);
      }
    });
  }, { threshold: 0.25 }) : null;
  // ---------- Bars with depth: a soft top-lit gradient, and focus on hover ----------
  function rgbOf(str) {
    var m = /rgba?\(([^)]+)\)/.exec(str || "");
    if (!m) return null;
    var p = m[1].split(",").map(function (v) { return parseFloat(v); });
    return { r: p[0], g: p[1], b: p[2], a: p.length > 3 ? p[3] : 1 };
  }
  function mix(c, t, w) {
    return "rgb(" + Math.round(c.r + (t - c.r) * w) + "," + Math.round(c.g + (t - c.g) * w) + "," +
           Math.round(c.b + (t - c.b) * w) + ")";
  }
  function polishBars(gd) {
    var svg = gd.querySelector(".main-svg");
    if (!svg) return;
    // Stacked bars stay flat: a gradient on every segment reads as stripes.
    if (gd._fullLayout && gd._fullLayout.barmode === "stack") return;
    var defs = svg.querySelector("defs") || svg.insertBefore(document.createElementNS("http://www.w3.org/2000/svg", "defs"), svg.firstChild);
    var made = {};
    $all(".barlayer .point path", gd).forEach(function (path) {
      var inline = path.style.fill;
      if (!inline || inline.indexOf("url(") === 0) return;
      var c = rgbOf(inline);
      if (!c) return;
      // Ids are per chart: a gradient defined inside a hidden page would not paint here.
      var key = "kvg-" + (gd.id || "c") + "-" + [c.r, c.g, c.b].join("-") + (chartDark(gd) ? "-d" : "");
      if (!made[key] && !defs.querySelector("#" + key)) {
        var g = document.createElementNS("http://www.w3.org/2000/svg", "linearGradient");
        g.setAttribute("id", key); g.setAttribute("x1", "0"); g.setAttribute("y1", "0");
        g.setAttribute("x2", "0"); g.setAttribute("y2", "1");
        var stops = [[0, mix(c, 255, 0.26)], [0.6, "rgb(" + c.r + "," + c.g + "," + c.b + ")"],
                     [1, chartDark(gd) ? mix(c, 0, 0.12) : "rgb(" + c.r + "," + c.g + "," + c.b + ")"]];
        stops.forEach(function (s) {
          var st = document.createElementNS("http://www.w3.org/2000/svg", "stop");
          st.setAttribute("offset", s[0]); st.setAttribute("stop-color", s[1]);
          if (c.a < 1) st.setAttribute("stop-opacity", c.a);
          g.appendChild(st);
        });
        defs.appendChild(g);
      }
      made[key] = true;
      path.style.fill = "url(#" + key + ")";
    });
  }
  function wireBars(gd) {
    if (gd.__barsWired || !gd.on) return;
    gd.__barsWired = true;
    gd.on("plotly_afterplot", function () { polishBars(gd); });
    gd.on("plotly_hover", function (ev) {
      if (!gd.querySelector(".barlayer .point")) return;
      var pt = ev.points && ev.points[0];
      if (!pt || pt.data.type !== "bar") return;
      $all(".barlayer .point.is-hot", gd).forEach(function (n) { n.classList.remove("is-hot"); });
      $all(".barlayer .trace", gd).forEach(function (tr) {
        var d = tr.__data__ && tr.__data__[0];
        if (!d || !d.trace || d.trace.index !== pt.curveNumber) return;
        $all(".point", tr).forEach(function (node) {
          if (node.__data__ && node.__data__.i === pt.pointNumber) node.classList.add("is-hot");
        });
      });
      gd.classList.add("is-focus");
    });
    gd.on("plotly_unhover", function () {
      gd.classList.remove("is-focus");
      $all(".barlayer .point.is-hot", gd).forEach(function (n) { n.classList.remove("is-hot"); });
    });
  }

  function queueChart(gd, tries) {
    if (!gd.querySelector(".main-svg")) {
      if ((tries || 0) < 60) setTimeout(function () { queueChart(gd, (tries || 0) + 1); }, 40);
      return;
    }
    compactForPhone(gd);
    wireTips(gd);
    wireBars(gd);
    polishBars(gd);
    gd.__origColours = null;       // a new render brings fresh colours
    if (chartDark(gd)) themeChart(gd);
    if (!chartObserver) { animateChart(gd); return; }
    pendingCharts.add(gd);
    chartObserver.unobserve(gd);
    chartObserver.observe(gd);
  }
  if (window.jQuery) {
    jQuery(document).on("shiny:value", function (e) {
      var el = e.target;
      if (el && el.classList && el.classList.contains("kvbd-chart")) {
        setTimeout(function () { queueChart(el, 0); }, 30);
      }
    });
  }


  // ---------- Scroll reveal and reading progress ----------
  if (!reduceMotion && "IntersectionObserver" in window) {
    document.documentElement.classList.add("reveal-ready");
    var revealer = new IntersectionObserver(function (entries) {
      entries.forEach(function (en) {
        if (en.isIntersecting) { en.target.classList.add("is-in"); revealer.unobserve(en.target); }
      });
    }, { threshold: 0.12, rootMargin: "0px 0px -6% 0px" });
    var REVEAL = ".tab-pane .panel, .tab-pane .narration, .tab-pane .block, .tab-pane .kpi-row, " +
                 ".tab-pane .story-strip, .tab-pane .findings-panel, .tab-pane .two-up, .tab-pane .chapter-nav";
    var armReveal = function (root) {
      if (!root) return;
      $all(REVEAL, root).forEach(function (el) {
        if (el.classList.contains("reveal")) return;
        if (el.parentElement.closest(".two-up, .panel, .overview-grid")) return;
        el.classList.add("reveal");
        // Anything already on screen shows at once.
        if (el.getBoundingClientRect().top < window.innerHeight * 0.92) el.classList.add("is-in");
        else revealer.observe(el);
      });
    };
    document.addEventListener("DOMContentLoaded", function () { armReveal($(".tab-pane.active")); });
    if (window.jQuery) {
      jQuery(document).on("shown.bs.tab", "#pages a", function (e) {
        var pane = document.querySelector(e.target.getAttribute("href"));
        if (pane) setTimeout(function () { armReveal(pane); }, 20);
      });
    }
  }
  var bar = document.createElement("div");
  bar.className = "read-progress";
  bar.setAttribute("aria-hidden", "true");
  document.addEventListener("DOMContentLoaded", function () { document.body.appendChild(bar); });
  window.addEventListener("scroll", function () {
    var max = document.documentElement.scrollHeight - window.innerHeight;
    bar.style.transform = "scaleX(" + (max > 0 ? Math.min(1, window.scrollY / max) : 0) + ")";
  }, { passive: true });


  // ---------- The aurora leans gently towards the pointer ----------
  if (!reduceMotion && window.matchMedia("(hover: hover)").matches) {
    document.addEventListener("pointermove", function (e) {
      var head = e.target.closest && e.target.closest(".chapter-head, .intro");
      $all(".tab-pane.active .aurora").forEach(function (orb) {
        if (!head) { orb.style.setProperty("--mx", "0px"); orb.style.setProperty("--my", "0px"); return; }
        var r = orb.getBoundingClientRect();
        var dx = (e.clientX - (r.left + r.width / 2)) / window.innerWidth;
        var dy = (e.clientY - (r.top + r.height / 2)) / window.innerHeight;
        orb.style.setProperty("--mx", (dx * 80).toFixed(1) + "px");
        orb.style.setProperty("--my", (dy * 60).toFixed(1) + "px");
      });
    }, { passive: true });
  }


  // ---------- Reports: save as PDF and download CSV ----------
  // The preview is copied into a print-only root, so the PDF holds just the report.
  document.addEventListener("click", function (e) {
    if (!e.target.closest(".js-print")) return;
    var paper = document.querySelector("#report_preview .paper");
    if (!paper) return;
    var root = document.getElementById("print-root");
    if (!root) { root = document.createElement("div"); root.id = "print-root"; document.body.appendChild(root); }
    root.innerHTML = "";
    root.appendChild(paper.cloneNode(true));
    document.documentElement.classList.add("is-printing");
    var done = function () {
      document.documentElement.classList.remove("is-printing");
      root.innerHTML = "";
      window.removeEventListener("afterprint", done);
    };
    window.addEventListener("afterprint", done);
    setTimeout(function () { window.print(); }, 60);
  });
  if (window.Shiny) {
    Shiny.addCustomMessageHandler("download_csv", function (m) {
      var blob = new Blob(["﻿" + m.text], { type: "text/csv;charset=utf-8" });
      var url = URL.createObjectURL(blob);
      var a = document.createElement("a");
      a.href = url; a.download = m.name; a.rel = "noopener";
      document.body.appendChild(a); a.click();
      setTimeout(function () { URL.revokeObjectURL(url); a.remove(); }, 1500);
    });
  }


  // ---------- Donation journey lights up one step at a time ----------
  function runJourney(svg) {
    if (svg.__journey) return;
    svg.__journey = true;
    var steps = $all(".jr-step", svg), bar = $(".jr-progress", svg), n = steps.length, k = 0;
    if (reduceMotion) { steps.forEach(function (s) { s.classList.add("is-lit"); }); if (bar) bar.style.strokeDashoffset = 0; return; }
    var FLOW = 900, PAUSE = 450;   // FLOW matches the CSS transition on .jr-progress
    function light(i) { steps[i].classList.add("is-lit"); }
    function reset() {
      steps.forEach(function (s) { s.classList.remove("is-lit"); });
      bar.style.transition = "none";
      bar.style.strokeDashoffset = 100;
      void bar.getBoundingClientRect();
      bar.style.transition = "";
    }
    // Step 1 lights; then the line flows to the next step, and only when it
    // arrives does that step light up.
    function tick() {
      if (!document.body.contains(svg)) return;
      if (k === 0) { reset(); light(0); k = 1; setTimeout(tick, PAUSE + 300); return; }
      if (k === n) { k = 0; setTimeout(tick, 2200); return; }
      bar.style.strokeDashoffset = 100 - 100 * (k / Math.max(1, n - 1));
      var i = k;
      setTimeout(function () { light(i); }, FLOW);
      k += 1;
      setTimeout(tick, FLOW + PAUSE);
    }
    tick();
  }
  if ("IntersectionObserver" in window) {
    var jObs = new IntersectionObserver(function (es) {
      es.forEach(function (e) { if (e.isIntersecting) { runJourney(e.target); jObs.unobserve(e.target); } });
    }, { threshold: 0.4 });
    document.addEventListener("DOMContentLoaded", function () { $all(".journey").forEach(function (s) { jObs.observe(s); }); });
  }

  // ---------- Sources & notes drawer ----------
  var lastFocus = null;
  function openDrawer() {
    var d = $("#sources-drawer"); if (!d) return;
    lastFocus = document.activeElement;
    d.classList.add("is-open"); d.setAttribute("aria-hidden", "false");
    document.documentElement.classList.add("drawer-open");
    var c = $(".drawer-close", d); if (c) setTimeout(function () { c.focus(); }, 60);
    closeQuickActions();
  }
  function closeDrawer() {
    var d = $("#sources-drawer"); if (!d || !d.classList.contains("is-open")) return;
    d.classList.remove("is-open"); d.setAttribute("aria-hidden", "true");
    document.documentElement.classList.remove("drawer-open");
    if (lastFocus) lastFocus.focus();
  }
  document.addEventListener("click", function (e) {
    if (e.target.closest("[data-open='sources']")) { e.preventDefault(); e.stopPropagation(); openDrawer(); return; }
    if (e.target.closest("[data-close='sources']")) { closeDrawer(); }
  }, true);
  document.addEventListener("keydown", function (e) { if (e.key === "Escape") closeDrawer(); });


  // ---------- Plain-language terms: hover, focus or tap for a meaning ----------
  var termOpen = null;
  function showTerm(el) {
    var tip = ensureTip();
    tip.innerHTML = '<div class="gt-head">' + el.getAttribute("data-term") + '</div><div class="gt-def">' +
                    el.getAttribute("data-def") + "</div>";
    tip.classList.add("is-on", "is-term");
    var r = el.getBoundingClientRect(), w = tip.offsetWidth, h = tip.offsetHeight;
    var left = Math.max(12, Math.min(window.innerWidth - w - 12, r.left + r.width / 2 - w / 2));
    var top = r.bottom + 10;
    if (top + h > window.innerHeight - 12) top = r.top - h - 10;
    tip.style.transform = "translate(" + Math.round(left) + "px," + Math.round(top) + "px)";
    termOpen = el;
  }
  function hideTerm() {
    if (tipEl && termOpen) { tipEl.classList.remove("is-on", "is-term"); termOpen = null; }
  }
  document.addEventListener("mouseover", function (e) {
    var t = e.target.closest && e.target.closest(".term");
    if (t) showTerm(t);
  });
  document.addEventListener("mouseout", function (e) {
    var t = e.target.closest && e.target.closest(".term");
    if (t && !t.contains(e.relatedTarget)) hideTerm();
  });
  document.addEventListener("focusin", function (e) { if (e.target.classList && e.target.classList.contains("term")) showTerm(e.target); });
  document.addEventListener("focusout", function (e) { if (e.target.classList && e.target.classList.contains("term")) hideTerm(); });
  document.addEventListener("click", function (e) {
    var t = e.target.closest && e.target.closest(".term");
    if (t) { if (termOpen === t) hideTerm(); else showTerm(t); return; }
    if (termOpen) hideTerm();
  });
  window.addEventListener("scroll", hideTerm, { passive: true });


  // ---------- Light and dark themes ----------
  // Only colours change; layout and content stay the same. Charts are recoloured
  // in place so nothing redraws or moves.
  var CHART_DARK = {
    "#4A1426": "#F7ECEE", "#8A6470": "#B89AA2", "#F3E1E5": "rgba(255,255,255,0.07)",
    "#F2C2CC": "#6B3A48", "#E3909F": "#9C4A60", "#C45A72": "#CE5F7B", "#7D2440": "#F48AA2",
    "#F2B3C0": "#7A3F50", "#E07D93": "#A84F66", "#B8445F": "#D06580", "#6E1D37": "#F4A3B6"
  };
  function isDark() { return document.documentElement.getAttribute("data-theme") === "dark"; }
  // A chart on a dark "ink" panel uses the dark palette in either theme.
  function chartDark(gd) { return isDark() || !!(gd && gd.closest && gd.closest(".panel-ink")); }
  function mapColour(c, dark) {
    if (typeof c !== "string") return c;
    if (!dark) return c;
    return CHART_DARK[c.toUpperCase()] || CHART_DARK[c] || c;
  }
  function themeChart(gd) {
    if (!window.Plotly || !gd.data || !gd.layout) return;
    var dark = chartDark(gd);
    if (!gd.__origColours) {
      gd.__origColours = gd.data.map(function (t) {
        return { m: t.marker && t.marker.color, ml: t.marker && t.marker.line && t.marker.line.color,
                 l: t.line && t.line.color, f: t.fillcolor };
      });
      gd.__origLayout = {
        font: gd.layout.font && gd.layout.font.color,
        ann: (gd.layout.annotations || []).map(function (a) { return a.font && a.font.color; })
      };
    }
    var lay = { "font.color": mapColour("#4A1426", dark), "legend.font.color": mapColour("#4A1426", dark) };
    Object.keys(gd.layout).forEach(function (k) {
      if (/^[xy]axis\d*$/.test(k)) {
        lay[k + ".tickfont.color"] = mapColour("#8A6470", dark);
        lay[k + ".title.font.color"] = mapColour("#8A6470", dark);
        lay[k + ".gridcolor"] = mapColour("#F3E1E5", dark);
      }
    });
    (gd.__origLayout.ann || []).forEach(function (c, i) { lay["annotations[" + i + "].font.color"] = mapColour(c || "#4A1426", dark); });
    try {
      gd.__origColours.forEach(function (o, i) {
        var u = {};
        if (o.m !== undefined) u["marker.color"] = [mapColour(o.m, dark)];
        if (o.ml !== undefined) u["marker.line.color"] = [mapColour(o.ml, dark)];
        if (o.l !== undefined) u["line.color"] = [mapColour(o.l, dark)];
        if (o.f !== undefined) u.fillcolor = [mapColour(o.f, dark)];
        if (Object.keys(u).length) window.Plotly.restyle(gd, u, [i]);
      });
      window.Plotly.relayout(gd, lay);
    } catch (e) { /* chart not ready */ }
  }
  function themeAllCharts() { $all(".js-plotly-plot").forEach(function (gd) { if (gd.__origColours || chartDark(gd)) themeChart(gd); }); }
  function setTheme(t) {
    var root = document.documentElement;
    root.classList.add("theme-switching");
    root.setAttribute("data-theme", t);
    try { localStorage.setItem("kvbd-theme", t); } catch (e) { /* private mode */ }
    $all(".theme-toggle").forEach(function (b) { b.setAttribute("aria-pressed", t === "dark" ? "true" : "false"); });
    themeAllCharts();
    setTimeout(function () { root.classList.remove("theme-switching"); }, 520);
  }
  document.addEventListener("click", function (e) {
    if (!e.target.closest(".theme-toggle")) return;
    setTheme(isDark() ? "light" : "dark");
  });
  document.addEventListener("DOMContentLoaded", function () {
    $all(".theme-toggle").forEach(function (b) { b.setAttribute("aria-pressed", isDark() ? "true" : "false"); });
  });


  // ---------- Chart output binding ----------
  // R sends a plotly.js figure as JSON; draw or update it in place.
  if (window.Shiny && Shiny.OutputBinding) {
    var chartBinding = new Shiny.OutputBinding();
    chartBinding.find = function (scope) { return jQuery(scope).find(".kvbd-chart"); };
    chartBinding.renderValue = function (el, value) {
      if (!window.Plotly) return;
      if (value === null || value === undefined) { window.Plotly.purge(el); return; }
      var fig = typeof value === "string" ? JSON.parse(value) : value;
      window.Plotly.react(el, fig.data, fig.layout, fig.config);
    };
    chartBinding.renderError = function (el, err) {
      if (window.Plotly) window.Plotly.purge(el);
      Shiny.OutputBinding.prototype.renderError.call(this, el, err);
    };
    Shiny.outputBindings.register(chartBinding, "kvbd.chart");
  }


  // ---------- Sortable, paged tables ----------
  function initSortTable(wrap) {
    if (wrap.__st) return;
    wrap.__st = true;
    var table = $("table", wrap), body = $("tbody", table), foot = $(".st-foot", wrap);
    var rows = $all("tr", body), per = parseInt(wrap.getAttribute("data-page") || "8", 10), page = 0;
    function draw() {
      var pages = Math.max(1, Math.ceil(rows.length / per));
      page = Math.min(page, pages - 1);
      rows.forEach(function (r, i) { r.style.display = (i >= page * per && i < (page + 1) * per) ? "" : "none"; });
      if (pages <= 1) { foot.innerHTML = ""; return; }
      var html = '<span class="st-info">' + (page * per + 1) + "–" + Math.min(rows.length, (page + 1) * per) +
                 " of " + rows.length + '</span><span class="st-pages">';
      html += '<button type="button" data-p="prev"' + (page === 0 ? " disabled" : "") + ' aria-label="Previous page">‹</button>';
      for (var p = 0; p < pages; p++) html += '<button type="button" data-p="' + p + '"' + (p === page ? ' class="is-on" aria-current="page"' : "") + ">" + (p + 1) + "</button>";
      html += '<button type="button" data-p="next"' + (page === pages - 1 ? " disabled" : "") + ' aria-label="Next page">›</button></span>';
      foot.innerHTML = html;
    }
    foot.addEventListener("click", function (e) {
      var b = e.target.closest("button"); if (!b || b.disabled) return;
      var v = b.getAttribute("data-p");
      page = v === "prev" ? page - 1 : v === "next" ? page + 1 : parseInt(v, 10);
      draw();
    });
    $all("th", table).forEach(function (th, col) {
      function sort() {
        var dir = th.getAttribute("aria-sort") === "ascending" ? "descending" : "ascending";
        $all("th", table).forEach(function (h) { h.setAttribute("aria-sort", "none"); });
        th.setAttribute("aria-sort", dir);
        var num = th.classList.contains("num"), sign = dir === "ascending" ? 1 : -1;
        rows.sort(function (a, b) {
          var x = a.children[col], y = b.children[col];
          var p = num ? parseFloat(x.getAttribute("data-v")) : x.textContent.trim();
          var q = num ? parseFloat(y.getAttribute("data-v")) : y.textContent.trim();
          return (num ? (p - q) : String(p).localeCompare(String(q))) * sign;
        });
        rows.forEach(function (r) { body.appendChild(r); });
        page = 0; draw();
      }
      th.addEventListener("click", sort);
      th.addEventListener("keydown", function (e) { if (e.key === "Enter" || e.key === " ") { e.preventDefault(); sort(); } });
    });
    draw();
  }
  if (window.jQuery) {
    jQuery(document).on("shiny:value", function (e) {
      setTimeout(function () { $all(".st-wrap", e.target).forEach(initSortTable); }, 0);
    });
  }

  // ---------- Count-up numbers ----------
  function countTo(el, to, ms, decimals, from, done) {
    if (!el) return;
    decimals = decimals || 0;
    var pre = el.getAttribute("data-prefix") || "", suf = el.getAttribute("data-suffix") || "";
    var show = function (v) {
      el.textContent = pre + v.toLocaleString("en-GB", { minimumFractionDigits: decimals,
                                                       maximumFractionDigits: decimals }) + suf;
    };
    if (reduceMotion || from === to) { show(to); if (done) done(); return; }
    var start = performance.now();
    function frame(now) {
      var t = Math.min(1, (now - start) / ms);
      var eased = 1 - Math.pow(1 - t, 3);
      show(from + (to - from) * eased);
      if (t < 1) requestAnimationFrame(frame); else if (done) done();
    }
    requestAnimationFrame(frame);
  }

  // ---------- Number reveals ----------
  // A lead-in line ("Let's see how many people gave blood...") fades in word by
  // word; then its number counts up and lands with a small pop. Big numbers
  // also send out a few tiny drops.
  function splitLeadIn(el) {
    if (el.__split) return el.__words || 0;
    el.__split = true;
    var words = el.textContent.trim().split(/\s+/);
    el.textContent = "";
    words.forEach(function (w, i) {
      var s = document.createElement("span");
      s.className = "li-word";
      s.style.setProperty("--w", i);
      s.textContent = w;
      el.appendChild(s);
      el.appendChild(document.createTextNode(" "));
    });
    el.__words = words.length;
    return words.length;
  }
  function playLeadIn(el) {
    var n = splitLeadIn(el);
    el.classList.remove("is-playing"); void el.offsetWidth; el.classList.add("is-playing");
    return reduceMotion ? 0 : Math.min(1400, n * 70 + 250);
  }
  function burst(el) {
    if (reduceMotion) return;
    var host = el.closest(".ov-big, .kpi-value, .big-stat-value, .burst-host") || el;
    for (var i = 0; i < 7; i++) {
      var d = document.createElement("span");
      d.className = "num-drop";
      var a = (Math.PI * 2 * i) / 7 + Math.random() * 0.5;
      d.style.setProperty("--dx", Math.round(Math.cos(a) * (40 + Math.random() * 30)) + "px");
      d.style.setProperty("--dy", Math.round(Math.sin(a) * (30 + Math.random() * 24) - 10) + "px");
      d.style.left = (20 + Math.random() * 60) + "%";
      host.appendChild(d);
      d.addEventListener("animationend", function () { this.remove(); });
    }
  }
  function revealNumber(el, wait) {
    var to = parseFloat(el.getAttribute("data-count"));
    if (isNaN(to)) return;
    var dec = parseInt(el.getAttribute("data-decimals") || "0", 10);
    el.classList.remove("is-landed");
    setTimeout(function () {
      countTo(el, to, 1000, dec, 0, function () {
        el.classList.remove("is-landed"); void el.offsetWidth; el.classList.add("is-landed");
        if (el.hasAttribute("data-burst")) burst(el);
      });
    }, wait || 0);
  }
  // Count each number when it scrolls into view (or at once if already there).
  var numObserver = "IntersectionObserver" in window ? new IntersectionObserver(function (es) {
    es.forEach(function (e) {
      if (!e.isIntersecting) return;
      numObserver.unobserve(e.target);
      startReveal(e.target);
    });
  }, { threshold: 0.4 }) : null;
  function startReveal(el) {
    var group = el.closest("[data-reveal-group]") || el.parentElement;
    var lead = group && group.querySelector(".lead-in");
    var wait = lead && !lead.__played ? (lead.__played = true, playLeadIn(lead)) : 0;
    if (wait && !reduceMotion) {
      // Hold the number back (shown as a soft placeholder) until the lead-in has played.
      el.classList.add("is-waiting");
      setTimeout(function () { el.classList.remove("is-waiting"); }, wait);
    }
    revealNumber(el, wait + (parseInt(el.getAttribute("data-delay") || "0", 10)));
  }
  function countUpIn(root) {
    $all(".lead-in", root).forEach(function (l) { l.__played = false; });
    $all(".countup", root).forEach(function (el) {
      if (!numObserver) { startReveal(el); return; }
      numObserver.unobserve(el);
      numObserver.observe(el);
    });
  }
  window.addEventListener("load", function () {
    var first = document.querySelector(".tab-pane.active");
    if (first) setTimeout(function () { countUpIn(first); }, 200);
  });
  // Numbers that arrive later from the server (region cards, reports) count up too.
  if (window.jQuery) {
    jQuery(document).on("shiny:value", function (e) {
      if (!e.target || !e.target.querySelector) return;
      setTimeout(function () { if (e.target.querySelector(".countup")) countUpIn(e.target); }, 30);
    });
  }
})();

// ---------- Tabs and folds: shorter pages, detail on demand ----------
(function () {
  function placeTabThumb(bar) {
    var on = bar.querySelector(".kv-tab.is-on");
    var thumb = bar.querySelector(".kv-tab-thumb");
    if (!thumb) {
      thumb = document.createElement("span");
      thumb.className = "kv-tab-thumb"; thumb.setAttribute("aria-hidden", "true");
      bar.insertBefore(thumb, bar.firstChild);
    }
    if (!on || !on.offsetWidth) { thumb.style.opacity = "0"; return; }
    thumb.style.opacity = "1";
    thumb.style.width = on.offsetWidth + "px";
    thumb.style.height = on.offsetHeight + "px";
    thumb.style.transform = "translate(" + on.offsetLeft + "px," + on.offsetTop + "px)";
  }
  function placeAll() { document.querySelectorAll(".kv-tabs").forEach(placeTabThumb); }
  function wake(root) {
    // Let Shiny render outputs that were hidden, and size charts drawn while hidden.
    if (window.jQuery) jQuery(root).trigger("shown");
    setTimeout(function () {
      window.dispatchEvent(new Event("resize"));
      root.querySelectorAll(".kvbd-chart").forEach(function (c) {
        if (c._fullLayout && window.Plotly) { try { Plotly.Plots.resize(c); } catch (e) {} }
      });
    }, 40);
  }
  function select(btn) {
    var set = btn.closest(".kv-tabset");
    var k = btn.getAttribute("data-tab");
    set.querySelectorAll(":scope > .kv-tabs > .kv-tab").forEach(function (b) {
      var on = b === btn;
      b.classList.toggle("is-on", on);
      b.setAttribute("aria-selected", on ? "true" : "false");
    });
    set.querySelectorAll(":scope > .kv-pane").forEach(function (p) {
      var on = p.getAttribute("data-pane") === k;
      p.hidden = !on;
      p.classList.toggle("is-on", on);
      if (on) {
        p.classList.remove("pane-in"); void p.offsetWidth; p.classList.add("pane-in");
        wake(p);
      }
    });
    placeTabThumb(set.querySelector(":scope > .kv-tabs"));
  }
  document.addEventListener("click", function (e) {
    var btn = e.target.closest && e.target.closest(".kv-tab");
    if (btn) select(btn);
  });
  document.addEventListener("keydown", function (e) {
    var btn = e.target.closest && e.target.closest(".kv-tab");
    if (!btn || (e.key !== "ArrowRight" && e.key !== "ArrowLeft")) return;
    var tabs = Array.prototype.slice.call(btn.parentElement.querySelectorAll(".kv-tab"));
    var i = tabs.indexOf(btn) + (e.key === "ArrowRight" ? 1 : -1);
    var next = tabs[(i + tabs.length) % tabs.length];
    next.focus(); select(next); e.preventDefault();
  });
  document.addEventListener("toggle", function (e) {
    if (e.target.classList && e.target.classList.contains("fold") && e.target.open) wake(e.target);
  }, true);
  window.addEventListener("resize", placeAll);
  window.addEventListener("load", function () { setTimeout(placeAll, 60); });
  if (window.jQuery) {
    jQuery(document).on("shiny:connected", function () { setTimeout(placeAll, 80); });
    // Switching pages shows tab bars that were hidden; place their thumbs once visible.
    jQuery(document).on("shiny:inputchanged", function (e) {
      if (e.name === "pages") setTimeout(placeAll, 120);
    });
  }
  document.addEventListener("click", function (e) {
    if (e.target.closest && e.target.closest(".rail-item, [data-target]")) setTimeout(placeAll, 450);
  });
})();
