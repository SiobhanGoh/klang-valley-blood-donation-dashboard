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



  // ---------- Storyboard numbering ----------
  // Every scene title gets "chapter.scene", so all pages share one sequence.
  function numberScenes() {
    $all(".tab-pane").forEach(function (pane) {
      var num = $(".chapter-num", pane);
      if (!num) return;
      var ch = num.textContent.trim(), k = 0;
      $all(".panel-title", pane).forEach(function (t) {
        var host = t.closest(".panel, .findings-panel");
        if (host && host.parentElement.closest(".panel")) return;   // nested tile
        if (t.querySelector(".scene-num")) return;
        k += 1;
        var tag = document.createElement("span");
        tag.className = "scene-num";
        tag.textContent = ch + "." + k;
        t.insertBefore(tag, t.firstChild);
      });
    });
  }
  document.addEventListener("DOMContentLoaded", numberScenes);

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
  function queueChart(gd, tries) {
    if (!gd.querySelector(".main-svg")) {
      if ((tries || 0) < 60) setTimeout(function () { queueChart(gd, (tries || 0) + 1); }, 40);
      return;
    }
    compactForPhone(gd);
    if (!chartObserver) { animateChart(gd); return; }
    pendingCharts.add(gd);
    chartObserver.unobserve(gd);
    chartObserver.observe(gd);
  }
  if (window.jQuery) {
    jQuery(document).on("shiny:value", function (e) {
      var el = e.target;
      if (el && el.classList && el.classList.contains("plotly")) {
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

  // ---------- Count-up numbers ----------
  function countTo(el, to, ms, decimals, from) {
    if (!el) return;
    decimals = decimals || 0;
    var show = function (v) {
      el.textContent = v.toLocaleString("en-GB", { minimumFractionDigits: decimals,
                                                  maximumFractionDigits: decimals });
    };
    if (reduceMotion || from === to) { show(to); return; }
    var start = performance.now();
    function frame(now) {
      var t = Math.min(1, (now - start) / ms);
      var eased = 1 - Math.pow(1 - t, 3);
      show(from + (to - from) * eased);
      if (t < 1) requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);
  }

  // Headline numbers count up from zero each time their page opens.
  function countUpIn(root) {
    $all(".countup", root).forEach(function (el) {
      var to = parseFloat(el.getAttribute("data-count"));
      if (isNaN(to)) return;
      var dec = parseInt(el.getAttribute("data-decimals") || "0", 10);
      countTo(el, to, 1000, dec, 0);
    });
  }
  window.addEventListener("load", function () {
    var first = document.querySelector(".tab-pane.active");
    if (first) setTimeout(function () { countUpIn(first); }, 200);
  });
})();
