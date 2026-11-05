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
    ind.style.transform = "translateY(" + item.offsetTop + "px)";
    ind.style.height = item.offsetHeight ? item.offsetHeight + "px" : "";
  }
  function setActive(target) {
    var shown = null;
    $all(".rail-item[data-target]").forEach(function (b) {
      var on = b.getAttribute("data-target") === target;
      b.classList.toggle("is-active", on);
      if (on) b.setAttribute("aria-current", "page"); else b.removeAttribute("aria-current");
      if (on) shown = b;
    });
    if (shown) {
      placeIndicator(shown);
      // The phone's edge tab shows where you are.
      var tabIcon = $(".rail-tab .rt-icon"), ic = shown.querySelector(".rail-icon");
      if (tabIcon && ic) tabIcon.innerHTML = ic.innerHTML;
    }
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
    var chip = e.target.closest && e.target.closest(".chips-keyed .checkbox-inline");
    if (chip && !reduceMotion) { chip.classList.remove("is-pop"); void chip.offsetWidth; chip.classList.add("is-pop");
      setTimeout(function () { chip.classList.remove("is-pop"); }, 600); }
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

  // ---------- Phone menu: tucked into an edge tab, slides out on tap ----------
  var phoneQ = window.matchMedia("(max-width: 760px)");
  function setDock(open) {
    var tab = $(".rail-tab");
    document.documentElement.classList.toggle("rail-open", !!open);
    if (tab) { tab.setAttribute("aria-expanded", open ? "true" : "false"); tab.setAttribute("aria-label", open ? "Close the menu" : "Open the menu"); }
    if (open) {
      var act = $(".rail-item.is-active");
      setTimeout(function () { placeIndicator(act); if (act) act.focus({ preventScroll: true }); }, 60);
    }
  }
  function closeQuickActions() { setDock(false); }
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape" && document.documentElement.classList.contains("rail-open")) {
      setDock(false); var t = $(".rail-tab"); if (t) t.focus();
    }
  });
  // The tab can be dragged up or down the edge; where it was left is remembered.
  (function () {
    var tab = $(".rail-tab"); if (!tab) return;
    var KEY = "kvbd-tab-y", startY = 0, startTop = 0, dragging = false, moved = false;
    function clampTop(y) { var h = window.innerHeight; return Math.max(72, Math.min(h - 72, y)); }
    function setTop(y) { tab.style.top = clampTop(y) + "px"; }
    try { var saved = parseFloat(localStorage.getItem(KEY)); if (saved > 0 && saved < 1) setTop(saved * window.innerHeight); } catch (e) {}
    tab.addEventListener("pointerdown", function (e) {
      dragging = true; moved = false; startY = e.clientY;
      startTop = tab.getBoundingClientRect().top + tab.offsetHeight / 2;
      try { tab.setPointerCapture(e.pointerId); } catch (err) {}
    });
    tab.addEventListener("pointermove", function (e) {
      if (!dragging) return;
      var dy = e.clientY - startY;
      if (!moved && Math.abs(dy) < 8) return;
      moved = true; tab.classList.add("is-dragging"); setTop(startTop + dy);
    });
    function end() {
      if (!dragging) return; dragging = false; tab.classList.remove("is-dragging");
      if (moved) {
        var y = tab.getBoundingClientRect().top + tab.offsetHeight / 2;
        try { localStorage.setItem(KEY, String(y / window.innerHeight)); } catch (e) {}
      }
    }
    tab.addEventListener("pointerup", end); tab.addEventListener("pointercancel", end);
    tab.addEventListener("click", function (e) {
      e.stopPropagation();
      if (moved) { moved = false; return; }
      setDock(!document.documentElement.classList.contains("rail-open"));
    });
    window.addEventListener("resize", function () { if (tab.style.top) setTop(parseFloat(tab.style.top)); });
  })();
  document.addEventListener("click", function (e) {
    if (e.target.closest && e.target.closest(".rail-scrim")) setDock(false);
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

    var nav = e.target.closest("[data-target]");
    if (nav) {
      var fromRail = nav.classList.contains("rail-item");
      var origin = fromRail ? nav.querySelector(".rail-icon")
                            : $(".rail-item[data-target='" + nav.getAttribute("data-target") + "'] .rail-icon");
      go(nav.getAttribute("data-target"), origin);
      return;
    }
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
  function tipHtml(gd, pts) {
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
    return html;
  }
  function isLineChart(gd) { return !!(gd.layout && /unified/.test(gd.layout.hovermode || "")); }
  function showTip(gd, ev) {
    if (gd.__pin) return;                       // a pinned reading stays put; no hover tip on top of it
    var pts = (ev.points || []).filter(function (p) { return p.data && p.data.visible !== "legendonly"; });
    if (!pts.length) return;
    var html = tipHtml(gd, pts);
    if (isLineChart(gd)) html += '<div class="gt-hint">Click to pin this day</div>';
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
  // ---------- Line charts: click to pin a reading in place ----------
  // A click on a day draws a line there and keeps its numbers on screen; a click
  // anywhere without a value (outside the plot, or elsewhere on the page) clears it.
  function drawPin(gd) {
    $all(".pin-line, .pin-card", gd).forEach(function (n) { n.remove(); });
    var pin = gd.__pin, fl = gd._fullLayout;
    if (!pin || !fl || !fl.xaxis) return;
    var xa = fl.xaxis, ya = fl.yaxis;
    var xc = xa.d2c(pin.x);
    if (xc === undefined || isNaN(xc)) return;
    var px = xa._offset + xa.c2p(xc);
    if (px < xa._offset - 1 || px > xa._offset + xa._length + 1) return;
    var line = document.createElement("div"); line.className = "pin-line";
    line.style.left = px + "px"; line.style.top = ya._offset + "px"; line.style.height = ya._length + "px";
    var card = document.createElement("div"); card.className = "glass-tip pin-card is-on";
    card.innerHTML = pin.html + '<div class="gt-hint">Pinned. Click empty space to clear.</div>';
    gd.appendChild(line); gd.appendChild(card);
    var w = card.offsetWidth;
    var left = px + 14;
    if (left + w > xa._offset + xa._length) left = px - w - 14;
    card.style.left = Math.max(4, left) + "px"; card.style.top = (ya._offset + 6) + "px";
  }
  function clearPin(gd) { gd.__pin = null; $all(".pin-line, .pin-card", gd).forEach(function (n) { n.remove(); }); }
  function wirePin(gd) {
    if (gd.__pinWired || !gd.on) return;
    gd.__pinWired = true;
    gd.on("plotly_click", function (ev) {
      if (!isLineChart(gd)) return;
      var pts = (ev.points || []).filter(function (p) { return p.data && p.data.visible !== "legendonly"; });
      if (!pts.length) return;
      gd.__pinAt = Date.now();
      gd.__pin = { x: pts[0].x, html: tipHtml(gd, pts) };
      hideTip(); drawPin(gd);
    });
    gd.on("plotly_afterplot", function () { if (gd.__pin) drawPin(gd); });
  }
  document.addEventListener("click", function () {
    setTimeout(function () {
      $all(".kvbd-chart").forEach(function (gd) {
        if (gd.__pin && Date.now() - (gd.__pinAt || 0) > 250) clearPin(gd);
      });
    }, 0);
  });
  document.addEventListener("keydown", function (e) {
    if (e.key === "Escape") $all(".kvbd-chart").forEach(function (gd) { if (gd.__pin) clearPin(gd); });
  });

  // A redraw can drop Plotly's listeners, so re-attach on every new render.
  function wireTips(gd) {
    if (!gd.on) return;
    wirePin(gd);
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
  // ---------- Call-outs: a pill in the headroom above the data, a thin line to the point ----------
  function placeCallouts(gd) {
    $all(".co", gd).forEach(function (n) { n.remove(); });
    var fl = gd._fullLayout, meta = gd.layout && gd.layout.meta;
    var list = meta && meta.callouts;
    if (!fl || !list || !list.length) return;
    list.forEach(function (c, i) {
      var xa = fl[(c.xref || "x").replace(/^x/, "xaxis")], ya = fl[(c.yref || "y").replace(/^y/, "yaxis")];
      if (!xa || !ya) return;
      var xc = xa.d2c(c.x);
      if (xc === undefined || xc === null || isNaN(xc)) return;
      var px = xa._offset + xa.c2p(xc + (c.dx || 0));
      var py = ya._offset + ya.c2p(c.y);
      var top = ya._offset + 4;
      var box = document.createElement("div");
      box.className = "co" + (c.align === "end" ? " co-end" : "");
      box.style.setProperty("--i", i);
      var pill = document.createElement("span"); pill.className = "co-pill"; pill.textContent = c.text;
      var line = document.createElement("span"); line.className = "co-line";
      var dot = document.createElement("span"); dot.className = "co-dot";
      box.appendChild(line); box.appendChild(dot); box.appendChild(pill);
      gd.appendChild(box);
      var pw = pill.offsetWidth, ph = pill.offsetHeight;
      var left = c.align === "end" ? px - pw + 10 : px - pw / 2;
      left = Math.max(xa._offset, Math.min(left, xa._offset + xa._length - pw));
      pill.style.left = left + "px"; pill.style.top = top + "px";
      var lineTop = top + ph, lineH = Math.max(0, py - lineTop - 5);
      line.style.left = px + "px"; line.style.top = lineTop + "px"; line.style.height = lineH + "px";
      dot.style.left = px + "px"; dot.style.top = (py - 5) + "px";
    });
  }
  function wireCallouts(gd) {
    if (gd.__coWired || !gd.on) return;
    gd.__coWired = true;
    gd.on("plotly_afterplot", function () { placeCallouts(gd); });
  }

  function animateChart(gd) {
    gd.classList.add("co-go");
    if (reduceMotion || !gd.animate) return;
    var bars = $all(".barlayer .point path", gd);
    bars.forEach(function (bar, i) {
      bar.style.transformBox = "fill-box";
      bar.style.transformOrigin = "50% 100%";
      bar.animate([{ transform: "scaleY(0)" }, { transform: "scaleY(1)" }],
                  { duration: 720, delay: Math.min(i * 14, 420), easing: EASE_OUT, fill: "backwards" });
    });
    // Call-outs draw in after the bars: the pointer grows, then the label fades up.
    $all(".annotation", gd).forEach(function (an, i) {
      an.animate([{ opacity: 0 }, { opacity: 1 }],
                 { duration: 520, delay: 820 + i * 70, easing: EASE_OUT, fill: "backwards" });
      $all(".annotation-arrow-g path", an).forEach(function (pth) {
        var len = pth.getTotalLength ? pth.getTotalLength() : 0;
        if (!len) return;
        pth.style.strokeDasharray = len;
        pth.animate([{ strokeDashoffset: len }, { strokeDashoffset: 0 }],
                    { duration: 520, delay: 760 + i * 70, easing: EASE_OUT, fill: "backwards" });
      });
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
    if (window.innerWidth > 600 || !window.Plotly || !gd.layout) return;
    // Two-line axis labels need the legend a little lower on a phone.
    if (!gd.layout.xaxis4) {
      var twoLine = (gd.data || []).some(function (t) { return (t.x || []).some(function (v) { return String(v).indexOf("<br>") > -1; }); });
      if (twoLine) window.Plotly.relayout(gd, { "legend.y": -0.3 });
      return;
    }
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
    gd.classList.remove("co-go");
    if (gd.__pin) clearPin(gd);
    compactForPhone(gd);
    wireCallouts(gd);
    placeCallouts(gd);
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


  // ---------- Legend chips: hand each chip its chart colour ----------
  function colourChips() {
    $all(".chips-keyed .chip-key").forEach(function (k) {
      var lab = k.closest(".checkbox-inline"); if (!lab) return;
      lab.style.setProperty("--k", k.style.getPropertyValue("--k"));
      lab.style.setProperty("--kd", k.style.getPropertyValue("--kd"));
    });
  }
  document.addEventListener("DOMContentLoaded", colourChips);
  if (document.readyState !== "loading") colourChips();

  // ---------- Donation centres: open now?, from Malaysia time ----------
  function myt() {
    try {
      var parts = new Intl.DateTimeFormat("en-GB", { timeZone: "Asia/Kuala_Lumpur", weekday: "short", hour: "2-digit", minute: "2-digit", hour12: false })
        .formatToParts(new Date());
      var get = function (t) { var p = parts.filter(function (x) { return x.type === t; })[0]; return p ? p.value : ""; };
      return { day: get("weekday"), h: (+get("hour") % 24) + (+get("minute")) / 60 };
    } catch (e) { return null; }
  }
  function fmtH(x) {
    var hh = Math.floor(x), mm = Math.round((x - hh) * 60);
    return (hh % 12 === 0 ? 12 : hh % 12) + (mm ? "." + (mm < 10 ? "0" : "") + mm : "") + (hh < 12 ? " am" : " pm");
  }
  function markCentres(root) {
    var now = myt(); if (!now) return;
    var weekend = now.day === "Sat" || now.day === "Sun";
    var tomorrowWeekend = now.day === "Fri" || now.day === "Sat";
    $all(".centre[data-wk]", root || document).forEach(function (c) {
      var h = c.getAttribute(weekend ? "data-we" : "data-wk").split(",").map(Number);
      var open = now.h >= h[0] && now.h < h[1];
      var st = c.querySelector(".centre-status");
      if (st) {
        st.className = "centre-status " + (open ? "is-open" : "is-closed");
        st.textContent = open ? (h[1] - now.h <= 1 ? "Closing soon · " + fmtH(h[1]) : "Open now · until " + fmtH(h[1]))
                              : (now.h < h[0] ? "Closed · opens " + fmtH(h[0])
                                 : "Closed · opens " + fmtH(+c.getAttribute(tomorrowWeekend ? "data-we" : "data-wk").split(",")[0]) + " tomorrow");
      }
      var rows = c.querySelectorAll(".ht-row");
      rows.forEach(function (r, i) { r.classList.toggle("is-today", (i === 1) === weekend); });
      var mark = c.querySelector(".ht-now");
      if (mark) {
        var pos = (now.h - 6) / 16;
        mark.hidden = pos < 0 || pos > 1;
        mark.style.setProperty("--xf", Math.max(0, Math.min(1, pos)).toFixed(4));
      }
    });
  }
  if (window.jQuery) jQuery(document).on("shiny:value", function (e) { if (e.target && e.target.id === "region_result") markCentres(e.target); });
  document.addEventListener("DOMContentLoaded", function () { markCentres(); });
  setInterval(function () { markCentres(); }, 60000);

  // ---------- Live data badge: how long ago the last recorded day was ----------
  function fillAgo() {
    var now = new Date(); now.setHours(0, 0, 0, 0);
    $all(".sp-ago[data-day]").forEach(function (el) {
      var p = el.getAttribute("data-day").split("-");
      var d = new Date(+p[0], +p[1] - 1, +p[2]);
      var days = Math.round((now - d) / 86400000);
      if (isNaN(days) || days < 0) return;
      el.textContent = days === 0 ? "today" : days === 1 ? "yesterday" : days < 60 ? days + " days ago" : "";
      el.classList.toggle("is-fresh", days <= 2);
    });
  }
  document.addEventListener("DOMContentLoaded", fillAgo);
  if (document.readyState !== "loading") fillAgo();

  // ---------- Phones: open any chart full screen, then drag across it to zoom in ----------
  // The chart itself moves into the full-screen layer (and back on close), so a
  // pinned reading, call-outs and hover all keep working, in portrait or landscape.
  var zoomQ = window.matchMedia("(max-width: 760px), (pointer: coarse) and (max-height: 500px)");
  var zoomState = null;
  function chartTitle(gd) {
    var panel = gd.closest(".panel");
    var t = panel && panel.querySelector(".panel-title");
    return t ? t.textContent.trim() : "Chart";
  }
  function xAxes(gd) {
    var fl = gd._fullLayout || {};
    return Object.keys(fl).filter(function (k) { return /^xaxis\d*$/.test(k); });
  }
  function setZoomable(gd, on) {
    if (!window.Plotly || !gd.layout) return;
    var upd = { dragmode: on ? "zoom" : false };
    // A short full-screen chart (a phone on its side) needs the legend clear of the dates.
    if (on) {
      gd.__legendY = gd.layout.legend ? gd.layout.legend.y : undefined;
      if (gd.layout.legend && gd.offsetHeight < 380) upd["legend.y"] = -0.3;
    } else if (gd.__legendY !== undefined) upd["legend.y"] = gd.__legendY;
    xAxes(gd).forEach(function (a) {
      upd[a + ".fixedrange"] = !on; upd[a + ".autorange"] = true;
      if (gd.layout.xaxis4) upd[a + ".showticklabels"] = on ? true : !(window.innerWidth <= 600);
    });
    window.Plotly.relayout(gd, upd);
  }
  function openZoom(gd) {
    if (zoomState || !window.Plotly) return;
    var dark = !!gd.closest(".panel-ink");
    var layer = document.createElement("div");
    layer.className = "chart-zoom-layer" + (dark ? " panel panel-ink" : "");
    layer.setAttribute("role", "dialog"); layer.setAttribute("aria-modal", "true");
    layer.setAttribute("aria-label", chartTitle(gd) + ", full screen");
    layer.innerHTML =
      '<div class="cz-head"><div class="cz-title"></div>' +
      '<button type="button" class="cz-btn cz-reset" hidden><i class="fas fa-arrows-left-right-to-line" aria-hidden="true"></i><span>Reset</span></button>' +
      '<button type="button" class="cz-btn cz-close" aria-label="Close full screen"><i class="fas fa-xmark" aria-hidden="true"></i></button></div>' +
      '<div class="cz-body"></div>' +
      '<div class="cz-hint"><span><i class="fas fa-hand-pointer" aria-hidden="true"></i> Drag across to zoom in</span>' +
      '<span><i class="fas fa-thumbtack" aria-hidden="true"></i> Tap to pin a reading</span>' +
      '<span class="cz-rotate"><i class="fas fa-mobile-screen-button" aria-hidden="true"></i> Turn sideways for more room</span></div>';
    layer.querySelector(".cz-title").textContent = chartTitle(gd);
    var holder = document.createElement("div");
    holder.className = "chart-zoom-holder";
    holder.style.height = gd.offsetHeight + "px";
    gd.parentNode.insertBefore(holder, gd);
    layer.querySelector(".cz-body").appendChild(gd);
    document.body.appendChild(layer);
    document.documentElement.classList.add("zoom-open");
    zoomState = { gd: gd, holder: holder, layer: layer, height: gd.style.height, focus: document.activeElement };
    gd.style.height = "100%";
    try { history.pushState({ kvbdZoom: 1 }, ""); zoomState.pushed = true; } catch (e) {}
    requestAnimationFrame(function () {
      layer.classList.add("is-in");
      window.Plotly.Plots.resize(gd);
      setZoomable(gd, true);
      layer.querySelector(".cz-close").focus({ preventScroll: true });
    });
    gd.__zoomRelayout = function (ev) {
      var zoomed = Object.keys(ev || {}).some(function (k) { return /^xaxis\d*\.range/.test(k); });
      var reset = layer.querySelector(".cz-reset");
      if (zoomed) reset.hidden = false;
      else if (Object.keys(ev || {}).some(function (k) { return /autorange/.test(k); })) reset.hidden = true;
    };
    gd.on("plotly_relayout", gd.__zoomRelayout);
  }
  function closeZoom(fromHistory) {
    if (!zoomState) return;
    var z = zoomState; zoomState = null;
    if (z.gd.removeListener && z.gd.__zoomRelayout) z.gd.removeListener("plotly_relayout", z.gd.__zoomRelayout);
    z.gd.style.height = z.height;
    z.holder.parentNode.insertBefore(z.gd, z.holder);
    z.holder.remove(); z.layer.remove();
    document.documentElement.classList.remove("zoom-open");
    setZoomable(z.gd, false);
    window.Plotly.Plots.resize(z.gd);
    if (z.pushed && !fromHistory) { try { history.back(); } catch (e) {} }
    var prev = z.gd.previousElementSibling, btn = prev && prev.querySelector(".chart-zoom");
    if (btn) btn.focus({ preventScroll: true });
  }
  window.addEventListener("popstate", function () { if (zoomState) closeZoom(true); });
  document.addEventListener("keydown", function (e) { if (e.key === "Escape" && zoomState) closeZoom(); });
  document.addEventListener("click", function (e) {
    if (!e.target.closest) return;
    var open = e.target.closest(".chart-zoom");
    if (open) {
      var gd = open.parentNode.nextElementSibling;
      if (gd && gd.classList.contains("kvbd-chart")) openZoom(gd);
      return;
    }
    if (e.target.closest(".cz-close")) { closeZoom(); return; }
    if (e.target.closest(".cz-reset") && zoomState) {
      var upd = {}; xAxes(zoomState.gd).forEach(function (a) { upd[a + ".autorange"] = true; });
      window.Plotly.relayout(zoomState.gd, upd);
      e.target.closest(".cz-reset").hidden = true;
    }
  });
  function addZoomButtons() {
    $all(".kvbd-chart").forEach(function (gd) {
      if (gd.__zoomBtn || !gd.parentNode) return;
      gd.__zoomBtn = true;
      var bar = document.createElement("div"); bar.className = "chart-tools";
      bar.innerHTML = '<button type="button" class="chart-zoom" aria-label="Open this chart full screen">' +
        '<i class="fas fa-expand" aria-hidden="true"></i><span>Full screen</span></button>';
      gd.parentNode.insertBefore(bar, gd);
    });
  }
  document.addEventListener("DOMContentLoaded", addZoomButtons);
  if (document.readyState !== "loading") addZoomButtons();

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

// ---------- Today's story: a floating arrow while there is more to read ----------
(function () {
  function update(side) {
    var more = side.scrollHeight - side.clientHeight;
    side.classList.toggle("no-scroll", more <= 4);
    side.classList.toggle("is-end", side.scrollTop >= more - 4);
  }
  function wire() {
    var side = document.querySelector(".ov-side");
    if (!side || side.__moreWired) return;
    side.__moreWired = true;
    side.addEventListener("scroll", function () { update(side); }, { passive: true });
    window.addEventListener("resize", function () { update(side); });
    var btn = side.querySelector(".ov-more-btn");
    if (btn) btn.addEventListener("click", function () {
      side.scrollBy({ top: side.clientHeight * 0.7, behavior: "smooth" });
    });
    update(side);
    setTimeout(function () { update(side); }, 800);
  }
  document.addEventListener("DOMContentLoaded", wire);
  window.addEventListener("load", wire);
  document.addEventListener("click", function (e) {
    if (e.target.closest && e.target.closest("[data-target='overview']")) {
      setTimeout(function () { var s = document.querySelector(".ov-side"); if (s) update(s); }, 400);
    }
  });
})();


// ---------- First-visit help: a welcome card over a softly blurred page ----------
(function () {
  function seen(k) { try { return localStorage.getItem(k) === "1"; } catch (e) { return false; } }
  function mark(k) { try { localStorage.setItem(k, "1"); } catch (e) { /* private mode */ } }
  var layer = document.getElementById("welcome");
  function closeWelcome() {
    if (!layer || layer.hidden || layer.classList.contains("is-out")) return;
    layer.classList.remove("is-in"); layer.classList.add("is-out"); mark("kvbd-welcomed");
    setTimeout(function () { layer.hidden = true; layer.classList.remove("is-out"); }, 650);
  }
  function showWelcome(force) {
    if (!layer || (!force && seen("kvbd-welcomed"))) return;
    layer.hidden = false; layer.classList.remove("is-out");
    requestAnimationFrame(function () {
      layer.classList.add("is-in");
      var b = layer.querySelector(".welcome-go"); if (b) setTimeout(function () { b.focus({ preventScroll: true }); }, 500);
    });
  }
  document.addEventListener("click", function (e) {
    if (e.target.closest && e.target.closest("[data-welcome='close']")) closeWelcome();
    if (e.target.closest && e.target.closest("[data-welcome='open']")) showWelcome(true);
  });
  document.addEventListener("keydown", function (e) { if (e.key === "Escape") closeWelcome(); });
  window.addEventListener("load", function () { setTimeout(function () { showWelcome(); }, 3600); });

  // "How to read this": the label slides out for a few seconds the first time a chart
  // is on screen, tucks back into its icon, and the icon gives a small bounce.
  if ("IntersectionObserver" in window) {
    var io = new IntersectionObserver(function (es) {
      es.forEach(function (e) {
        if (!e.isIntersecting) return;
        io.unobserve(e.target);
        var hr = e.target;
        setTimeout(function () {
          hr.classList.add("hr-peek");
          setTimeout(function () {
            hr.classList.remove("hr-peek"); hr.classList.add("hr-bounce");
            setTimeout(function () { hr.classList.remove("hr-bounce"); }, 1500);
          }, 3800);
        }, 900);
      });
    }, { threshold: 0.9 });
    var watch = function () { $all(".how-read:not([data-hr])").forEach(function (h) { h.setAttribute("data-hr", "1"); io.observe(h); }); };
    window.addEventListener("load", watch);
    document.addEventListener("click", function () { setTimeout(watch, 600); });
  }
  function $all(sel, root) { return Array.prototype.slice.call((root || document).querySelectorAll(sel)); }

  // A "What this shows" sentence glows briefly when it rewrites itself.
  if (window.jQuery) {
    jQuery(document).on("shiny:value", function (e) {
      var el = e.target;
      if (!el || !el.classList || !el.classList.contains("ws-text")) return;
      var box = el.closest(".what-shows");
      if (!box) return;
      box.classList.remove("is-new"); void box.offsetWidth; box.classList.add("is-new");
    });
  }
})();

// ---------- "What this shows": the glow tracks the cursor ----------
document.addEventListener("pointermove", function (e) {
  if (e.pointerType === "touch") return;
  var box = e.target.closest && e.target.closest(".what-shows");
  if (!box) return;
  var r = box.getBoundingClientRect();
  box.style.setProperty("--mx", (e.clientX - r.left) + "px");
  box.style.setProperty("--my", (e.clientY - r.top) + "px");
}, { passive: true });
