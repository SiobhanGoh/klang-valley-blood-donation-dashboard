// Runs the dashboard as a static site.
//
// R builds everything once a day (scripts/build_static.R): the page, every
// view at its default setting, and every view with a fixed set of choices.
// This file swaps those views in when a control changes, and draws the few
// views that take free dates or year ranges from data/daily.json. It also
// stands in for the small part of Shiny that interactions.js talks to, so the
// rest of the front end is unchanged.
(function () {
  "use strict";

  var METHODS = ["Seven-day baseline", "Weekday average", "Linear regression", "Recent weekday average"];
  var GROUPS = ["A", "AB", "B", "O"];
  var WEEKDAYS = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"];
  var CHARTS = ["overview_plot", "live_plot", "cmp_group_plot", "cmp_daily_plot", "low_plot", "weekly_plot",
                "weekday_plot", "annual_plot", "mix_plot", "monthly_plot"];
  var DAY = 86400000;

  // ---------- The part of Shiny that interactions.js uses ----------
  var handlers = {};
  var S = window.Shiny = {
    __static: true,
    setInputValue: function (name, value) { onInput(name, value); },
    addCustomMessageHandler: function (type, fn) { handlers[type] = fn; },
    OutputBinding: function () {},
    InputBinding: function () {},
    outputBindings: { register: function () {} },
    inputBindings: { register: function () {} },
    bindAll: function () {}, unbindAll: function () {}, initializeInputs: function () {}
  };
  S.OutputBinding.prototype.renderError = function () {};

  // ---------- Data files ----------
  var cache = {};
  function load(name) {
    if (!cache[name]) {
      cache[name] = fetch("data/" + name + ".json", { cache: "no-cache" }).then(function (r) {
        if (!r.ok) throw new Error(name + ": " + r.status);
        return r.json();
      });
    }
    return cache[name];
  }

  // ---------- Putting results on the page ----------
  function fire(el) { if (window.jQuery) window.jQuery(el).trigger("shiny:value"); }
  function setHTML(id, html) {
    var el = document.getElementById(id);
    if (!el || html === undefined || html === null) return;
    el.innerHTML = html;
    fire(el);
  }
  function visible(el) { return !!(el.offsetWidth || el.offsetHeight || el.getClientRects().length); }
  function drawChart(el, fig) {
    el.__pending = null;
    window.Plotly.react(el, fig.data, fig.layout, fig.config);
    fire(el);
  }
  // Charts in hidden pages wait until they are shown, so they draw at their real size.
  function setChart(id, fig) {
    var el = document.getElementById(id);
    if (!el || !window.Plotly) return;
    if (!fig) { window.Plotly.purge(el); return; }
    if (!visible(el)) { el.__pending = fig; return; }
    drawChart(el, fig);
  }
  function drawPending() {
    CHARTS.forEach(function (id) {
      var el = document.getElementById(id);
      if (el && el.__pending && visible(el)) drawChart(el, el.__pending);
    });
  }
  function apply(views) {
    if (!views) return;
    Object.keys(views).forEach(function (k) {
      if (CHARTS.indexOf(k) > -1) setChart(k, views[k]); else setHTML(k, views[k]);
    });
  }

  // ---------- Reading the controls ----------
  function radio(name) { var i = document.querySelector('input[name="' + name + '"]:checked'); return i ? i.value : null; }
  function checks(name) {
    return Array.prototype.map.call(document.querySelectorAll('input[name="' + name + '"]:checked'), function (i) { return i.value; });
  }
  function val(id) { var el = document.getElementById(id); return el ? el.value : ""; }
  function methodKey(ms) {
    var set = METHODS.filter(function (m) { return ms.indexOf(m) > -1; });
    return (set.length ? set : METHODS).join(",");
  }
  function chosenMethods(name) { var m = checks(name); return m.length ? m : METHODS.slice(); }

  // ---------- Dates and numbers, written as the R code writes them ----------
  var MON = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
  var MONTH = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  var WD3 = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  function toTime(s) { var p = s.split("-"); return Date.UTC(+p[0], +p[1] - 1, +p[2]); }
  function iso(t) { return new Date(t).toISOString().slice(0, 10); }
  function d(t) { return new Date(t); }
  function fmtDate(t) { var x = d(t); return x.getUTCDate() + " " + MON[x.getUTCMonth()] + " " + x.getUTCFullYear(); }
  function fmtDay(t) { var x = d(t); return WD3[x.getUTCDay()] + " " + x.getUTCDate() + " " + MON[x.getUTCMonth()]; }
  function num(x, digits) {
    return Number(x).toLocaleString("en-GB", { minimumFractionDigits: digits, maximumFractionDigits: digits });
  }
  function pct(x) { return num(Math.round(x), 0) + "%"; }
  function lowerFirst(s) { return s.charAt(0).toLowerCase() + s.slice(1); }
  function esc(s) { return String(s).replace(/[&<>"]/g, function (c) { return { "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]; }); }
  function clone(o) { return JSON.parse(JSON.stringify(o)); }
  function sortedRange(a, b, fallback) {
    var x = a ? toTime(a) : NaN, y = b ? toTime(b) : NaN;
    if (isNaN(x) || isNaN(y)) return fallback;
    return x <= y ? [x, y] : [y, x];
  }

  // ---------- The daily data ----------
  var daily = null;          // daily.json
  var init = null;           // init.json: every view at its default setting (templates for the drawn charts)
  function countAt(g, t) { var i = Math.round((t - daily._start) / DAY); var v = daily.counts[g][i]; return v === undefined ? null : v; }
  function predAt(m, g, t) { var i = Math.round((t - daily._pstart) / DAY); var v = daily.preds[m][g][i]; return v === undefined ? null : v; }
  function prepDaily(x) {
    x._start = toTime(x.start); x._end = toTime(x.end); x._pstart = toTime(x.pred_start);
    x._pend = x._pstart + (x.preds[METHODS[0]][GROUPS[0]].length - 1) * DAY;
    x._dataEnd = toTime(x.data_end);
    return x;
  }
  function withDaily(fn) {
    return Promise.all([load("daily"), load("init")]).then(function (r) {
      if (!daily) daily = prepDaily(r[0]);
      init = r[1];
      fn();
    });
  }

  // ---------- Compare: recorded and forecast, day by day ----------
  function drawDaily() {
    var g = radio("cmp_group") || "O";
    var ms = chosenMethods("cmp_methods");
    var range = sortedRange(val("cmp_dates_from"), val("cmp_dates_to"), [daily._pend - 30 * DAY, daily._pend]);
    var from = Math.max(range[0], daily._pstart), to = Math.min(range[1], daily._pend);
    var tpl = init.cmp_daily_plot;
    var days = [];
    for (var t = from; t <= to; t += DAY) {
      if (ms.some(function (m) { return predAt(m, g, t) !== null; })) days.push(t);
    }
    var el = document.getElementById("cmp_daily_plot");
    if (!days.length || !tpl) {
      if (el && window.Plotly) window.Plotly.purge(el);
      if (el) el.innerHTML = '<p class="chart-empty">No evaluation days in the selected range.</p>';
      setHTML("daily_explain", "No evaluation days in the selected range.");
      return;
    }
    var ce = el && el.querySelector(".chart-empty"); if (ce) ce.remove();
    var fig = clone(tpl);
    var byName = {}; tpl.data.forEach(function (tr) { byName[tr.name] = tr; });
    fig.data = [];
    METHODS.filter(function (m) { return ms.indexOf(m) > -1; }).forEach(function (m) {
      var tr = clone(byName[m]); var xs = [], ys = [];
      days.forEach(function (t) { var p = predAt(m, g, t); if (p !== null) { xs.push(iso(t)); ys.push(p); } });
      tr.x = xs; tr.y = ys; fig.data.push(tr);
    });
    var rec = clone(byName.Recorded); rec.x = []; rec.y = [];
    days.forEach(function (t) { var c = countAt(g, t); if (c !== null) { rec.x.push(iso(t)); rec.y.push(c); } });
    fig.data.push(rec);
    fig.layout.yaxis.title.text = "Group " + g + " donations per day";
    setChart("cmp_daily_plot", fig);
    setHTML("daily_explain", explainDaily(g, ms, days));
  }
  function explainDaily(g, ms, days) {
    var best = null, bestMae = Infinity;
    METHODS.filter(function (m) { return ms.indexOf(m) > -1; }).forEach(function (m) {
      var s = 0, n = 0;
      days.forEach(function (t) { var p = predAt(m, g, t), c = countAt(g, t); if (p !== null && c !== null) { s += Math.abs(c - p); n++; } });
      if (n && s / n < bestMae) { bestMae = s / n; best = m; }
    });
    if (!best) return "No evaluation days in the selected range.";
    var worst = null, we = -1;
    days.forEach(function (t) { var p = predAt(best, g, t), c = countAt(g, t); if (p !== null && c !== null && Math.abs(c - p) > we) { we = Math.abs(c - p); worst = t; } });
    return "Over these " + days.length + " days the " + lowerFirst(best) + " tracked group " + g +
      " most closely, off by " + num(bestMae, 1) + " donations a day on average. Its biggest miss was " +
      fmtDay(worst) + ", when it expected " + num(Math.round(predAt(best, g, worst)), 0) + " and " +
      num(countAt(g, worst), 0) + " were recorded.";
  }

  // ---------- Patterns: month by month, by weekday, by year ----------
  function patternFilter() {
    var gs = checks("pattern_groups"); if (!gs.length) gs = GROUPS.slice();
    var a = parseInt(val("pattern_years_from"), 10), b = parseInt(val("pattern_years_to"), 10);
    if (isNaN(a)) a = 2006; if (isNaN(b)) b = d(daily._end).getUTCFullYear();
    if (a > b) { var c = a; a = b; b = c; }
    return { groups: gs, from: a, to: b };
  }
  function eachDay(f, fn) {
    var t0 = Math.max(daily._start, Date.UTC(f.from, 0, 1)), t1 = Math.min(daily._end, Date.UTC(f.to, 11, 31));
    for (var t = t0; t <= t1; t += DAY) fn(t);
  }
  function drawPatterns() {
    var f = patternFilter();
    var inGroups = function (g) { return f.groups.indexOf(g) > -1; };
    // Month by month, stacked. A month still in progress is left out.
    var months = {}, order = [];
    var endD = d(daily._dataEnd);
    var lastOfMonth = Date.UTC(endD.getUTCFullYear(), endD.getUTCMonth() + 1, 0);
    var cutoff = daily._dataEnd < lastOfMonth ? Date.UTC(endD.getUTCFullYear(), endD.getUTCMonth(), 1) : Infinity;
    var wsum = {}, wn = {}, ysum = {}, yn = {};
    f.groups.forEach(function (g) { wsum[g] = [0, 0, 0, 0, 0, 0, 0]; wn[g] = [0, 0, 0, 0, 0, 0, 0]; ysum[g] = {}; yn[g] = {}; });
    eachDay(f, function (t) {
      var x = d(t), mk = Date.UTC(x.getUTCFullYear(), x.getUTCMonth(), 1);
      var wd = (x.getUTCDay() + 6) % 7, yr = x.getUTCFullYear();
      f.groups.forEach(function (g) {
        var c = countAt(g, t); if (c === null) return;
        if (mk < cutoff) {
          if (!months[mk]) { months[mk] = {}; order.push(mk); }
          months[mk][g] = (months[mk][g] || 0) + c;
        }
        wsum[g][wd] += c; wn[g][wd]++;
        ysum[g][yr] = (ysum[g][yr] || 0) + c; yn[g][yr] = (yn[g][yr] || 0) + 1;
      });
    });
    // Monthly totals
    var wt = clone(init.weekly_plot), wByName = {};
    init.weekly_plot.data.forEach(function (tr) { wByName[tr.name] = tr; });
    wt.data = ["O", "B", "A", "AB"].filter(inGroups).map(function (g) {
      var tr = clone(wByName["Group " + g]);
      tr.x = order.map(function (mk) { return iso(mk); });
      tr.y = order.map(function (mk) { return months[mk][g] || 0; });
      return tr;
    });
    setChart("weekly_plot", wt);
    // By weekday
    var groupsShown = GROUPS.filter(inGroups);
    var means = {}; groupsShown.forEach(function (g) { means[g] = wsum[g].map(function (s, i) { return wn[g][i] ? s / wn[g][i] : 0; }); });
    var kt = clone(init.weekday_plot), kByName = {};
    init.weekday_plot.data.forEach(function (tr) { kByName[tr.name] = tr; });
    var ymax = 0;
    kt.data = groupsShown.map(function (g) {
      var tr = clone(kByName["Group " + g]);
      tr.x = WEEKDAYS.map(function (w) { return w.slice(0, 3); });
      tr.y = means[g];
      tr.hovertext = WEEKDAYS.map(function (w, i) { return "<b>Group " + g + ", " + w + "</b><br>Mean <b>" + num(Math.round(means[g][i]), 0) + "</b> donations a day"; });
      means[g].forEach(function (v) { ymax = Math.max(ymax, v); });
      return tr;
    });
    var tot = WEEKDAYS.map(function (w, i) { return groupsShown.reduce(function (s, g) { return s + means[g][i]; }, 0); });
    var hi = tot.indexOf(Math.max.apply(null, tot)), lo = tot.indexOf(Math.min.apply(null, tot));
    var topG = groupsShown.reduce(function (a, g) { return means[g][hi] > means[a][hi] ? g : a; }, groupsShown[0]);
    var k = groupsShown.indexOf(topG) + 1, n = groupsShown.length;
    kt.layout.yaxis.range = [0, ymax * 1.35];
    kt.layout.meta = { callouts: [{ x: WEEKDAYS[hi].slice(0, 3), y: means[topG][hi], text: "Busiest day", xref: "x", yref: "y",
                                    dx: -(1 - 0.3) / 2 + (k - 0.5) * (1 - 0.3) / n, align: "center" }] };
    setChart("weekday_plot", kt);
    var r = tot[hi] / tot[lo];
    var how = r >= 2 ? "more than twice as busy as " + WEEKDAYS[lo] + " (" + num(r, 1) + " times)"
            : r >= 1.3 ? "noticeably busier than " + WEEKDAYS[lo] + " (" + num(r, 1) + " times)"
            : "only a little busier than " + WEEKDAYS[lo];
    setHTML("weekday_explain", WEEKDAYS[hi] + " is the busiest day, " + how + ".");
    // By year
    var at = clone(init.annual_plot), aByName = {};
    init.annual_plot.data.forEach(function (tr) { aByName[tr.name] = tr; });
    var years = []; for (var y = f.from; y <= f.to; y++) years.push(y);
    var ytot = {};
    at.data = groupsShown.map(function (g) {
      var tr = clone(aByName["Group " + g]); var xs = [], ys = [];
      years.forEach(function (yy) {
        if (!yn[g][yy]) return;
        var m = ysum[g][yy] / yn[g][yy]; xs.push(yy); ys.push(m); ytot[yy] = (ytot[yy] || 0) + m;
      });
      tr.x = xs; tr.y = ys; return tr;
    });
    setChart("annual_plot", at);
    var ys2 = Object.keys(ytot).map(Number).sort(function (a, b) { return a - b; });
    var text;
    if (ys2.length < 2) text = "Pick at least two years to see a trend.";
    else {
      var first = ytot[ys2[0]], last = ytot[ys2[ys2.length - 1]], ch = 100 * (last - first) / first;
      var peak = ys2.reduce(function (a, yy) { return ytot[yy] > ytot[a] ? yy : a; }, ys2[0]);
      var y0 = ys2[0], y1 = ys2[ys2.length - 1];
      text = (ch >= 10 ? "Donations a day grew by " + pct(ch) + " from " + y0 + " to " + y1 + "."
            : ch <= -10 ? "Donations a day fell by " + pct(-ch) + " from " + y0 + " to " + y1 + "."
            : "Donations a day stayed fairly steady from " + y0 + " to " + y1 + " (" + (ch >= 0 ? "+" : "−") + pct(Math.abs(ch)) + ").") +
             " The busiest year was " + peak + ".";
    }
    setHTML("annual_explain", text);
  }

  // ---------- Export: the data appendix and its CSV ----------
  var COLS = { date: "Date", blood_group: "Group", recorded: "Recorded", seven_day_baseline: "7-day baseline",
               weekday_average: "Weekday average", linear_regression: "Regression", recent_weekday_average: "Recent average" };
  function snake(m) { return m.toLowerCase().replace(/[^a-z]+/g, "_"); }
  function dataExtract() {
    var grp = radio("rep_group") || "all";
    var groups = grp === "all" ? GROUPS : [grp];
    var range = sortedRange(val("rep_dates_from"), val("rep_dates_to"), [daily._dataEnd - 30 * DAY, daily._dataEnd]);
    var withF = checks("rep_extra").indexOf("forecasts") > -1;
    var cols = ["date", "blood_group", "recorded"].concat(withF ? METHODS.map(snake) : []);
    var rows = [];
    for (var t = Math.max(range[0], daily._start); t <= Math.min(range[1], daily._end); t += DAY) {
      groups.forEach(function (g) {
        var c = countAt(g, t); if (c === null) return;
        var row = { date: iso(t), blood_group: g, recorded: c };
        if (withF) METHODS.forEach(function (m) { var p = predAt(m, g, t); row[snake(m)] = p === null ? null : Math.round(p * 100) / 100; });
        rows.push(row);
      });
    }
    return { group: grp, from: range[0], to: range[1], forecasts: withF, cols: cols, rows: rows };
  }
  function reportData() {
    var x = dataExtract(), max = 62, shown = x.rows.slice(0, max);
    var who = x.group === "all" ? "all four ABO groups" : "group " + x.group;
    var h = '<div class="paper paper-data"><div class="rp-mast"><div class="rp-brand"><span class="rp-logo">' +
      '<i class="fas fa-droplet" role="presentation" aria-label="droplet icon"></i></span>' +
      '<span>Klang Valley Blood Donation Forecasts</span></div><div class="rp-kicker">Data appendix</div>' +
      '<h1 class="rp-title">Recorded blood donations, Klang Valley</h1><p class="rp-deck">Daily recorded donations for ' +
      who + ", " + fmtDate(x.from) + " to " + fmtDate(x.to) + ", from the official open data. Figures are counts, not estimates.</p></div>" +
      '<table class="rp-meta"><tr><th>Source</th><td>Government of Malaysia, Daily Blood Donations by Blood Group &amp; State, data.gov.my</td></tr>' +
      "<tr><th>Licence</th><td>Creative Commons Attribution 4.0</td></tr>" +
      "<tr><th>Region</th><td>Selangor and W.P. Kuala Lumpur combined (KL includes Putrajaya)</td></tr>" +
      "<tr><th>Data to</th><td>" + fmtDate(daily._dataEnd) + "</td></tr>" +
      "<tr><th>Rows</th><td>" + num(x.rows.length, 0) + " in this extract" +
      (x.rows.length > max ? "; the first " + max + " are shown, the CSV has all" : "") + "</td></tr>" +
      (x.forecasts ? "<tr><th>Forecasts</th><td>Shown for test periods only; blank before them.</td></tr>" : "") + "</table>" +
      '<table class="rp-table"><thead><tr>' + x.cols.map(function (c) { return "<th>" + COLS[c] + "</th>"; }).join("") + "</tr></thead><tbody>" +
      shown.map(function (r) {
        return "<tr>" + x.cols.map(function (c) {
          var v = r[c];
          return "<td>" + (v === null ? "" : c === "recorded" ? num(v, 0) : typeof v === "number" ? num(v, 1) : esc(v)) + "</td>";
        }).join("") + "</tr>";
      }).join("") + "</tbody></table>" +
      '<h2 class="rp-h2">Definitions</h2><dl class="rp-defs">' +
      "<dt>Recorded</dt><dd>Donations recorded that day in the Blood Bank Information System for the group.</dd>" +
      "<dt>7-day baseline</dt><dd>The count recorded seven days earlier.</dd>" +
      "<dt>Weekday average</dt><dd>The group's mean for that weekday over all years before the evaluation year.</dd>" +
      "<dt>Regression</dt><dd>lm(donations ~ weekday + time_index), fitted per group on all earlier years.</dd>" +
      "<dt>Recent average</dt><dd>The group's mean for that weekday over the three complete years before.</dd></dl>" +
      '<div class="rp-foot"><p>' + esc(daily.data_credit) + " Made " + daily.made +
      " from https://siobhangoh.github.io/klang-valley-blood-donation-dashboard/</p></div></div>";
    return h;
  }
  function csvText() {
    var x = dataExtract();
    var q = function (s) { return '"' + String(s).replace(/"/g, '""') + '"'; };
    var lines = ["# " + daily.data_credit, "# Recorded donations are Selangor and W.P. Kuala Lumpur combined.",
                 x.cols.map(q).join(",")];
    x.rows.forEach(function (r) {
      lines.push(x.cols.map(function (c) {
        var v = r[c];
        return v === null ? "" : (c === "date" || c === "blood_group") ? q(v) : String(v);
      }).join(","));
    });
    return { name: "klang-valley-donations_" + iso(x.from) + "_" + iso(x.to) + ".csv", text: lines.join("\n") };
  }

  // ---------- Export: the donor card's last donation ----------
  function lastDonation() {
    var dd = parseInt(val("card_day"), 10), mm = parseInt(val("card_month"), 10), yy = parseInt(val("card_year"), 10);
    if (isNaN(dd) || isNaN(mm) || isNaN(yy)) return null;
    var t = Date.UTC(yy, mm - 1, dd), x = d(t);
    if (x.getUTCDate() !== dd || x.getUTCMonth() !== mm - 1) return NaN;
    var now = new Date(), today = Date.UTC(now.getFullYear(), now.getMonth(), now.getDate());
    return t > today ? NaN : t;
  }
  function updateCardDate() {
    var v = lastDonation();
    var label = v === null ? "Not set" : isNaN(v) ? "Check the date" : WD3[d(v).getUTCDay()] + " " + fmtDate(v);
    setHTML("card_last_label", '<span class="date-pick-value">' + label + "</span>");
    setHTML("card_last_check", v !== null && isNaN(v)
      ? '<span class="date-pick-warn"><i class="fas fa-circle-exclamation" role="presentation" aria-label="circle-exclamation icon"></i>That date doesn\'t exist or is in the future.</span>'
      : "");
    return v;
  }
  function cardWithDate(html, last) {
    if (last === null || isNaN(last)) return html;
    var x = d(last), next = Date.UTC(x.getUTCFullYear(), x.getUTCMonth() + 3, x.getUTCDate()), n = d(next);
    var block = '<div class="rp-next"><div class="rp-next-lab">Earliest next donation</div><div class="rp-next-date">' +
      n.getUTCDate() + " " + MONTH[n.getUTCMonth()] + " " + n.getUTCFullYear() + '</div><div class="rp-next-note">' +
      "At least three months after your last donation on " + fmtDate(last) + ". Staff confirm eligibility on the day.</div></div>";
    return html.replace('<h2 class="rp-h2">Before I donate</h2>', block + '<h2 class="rp-h2">Before I donate</h2>');
  }

  // ---------- Export: which report ----------
  function conditions() {
    var input = { report_type: radio("report_type") || "summary" };
    document.querySelectorAll("[data-display-if]").forEach(function (el) {
      var ok = false;
      try { ok = !!new Function("input", "return (" + el.getAttribute("data-display-if") + ");")(input); } catch (e) { ok = false; }
      el.style.display = ok ? "" : "none";
      el.classList.toggle("shiny-conditional--shown", ok);
    });
  }
  function updateReport() {
    var kind = radio("report_type") || "summary";
    load("reports").then(function (rep) {
      setHTML("report_blurb", rep.blurb[kind]);
      if (kind === "summary") setHTML("report_preview", rep.summary[radio("rep_period")]);
      else if (kind === "card") {
        var last = updateCardDate();
        setHTML("report_preview", cardWithDate(rep.card[(radio("card_type") || "O+") + "|" + (radio("card_region") || "Central")], last));
      } else withDaily(function () { setHTML("report_preview", reportData()); });
    });
  }

  // ---------- Pages ----------
  function nav(target) {
    var a = document.querySelector('#pages a[data-value="' + target + '"]');
    var pane = document.querySelector('.tab-content > .tab-pane[data-value="' + target + '"]');
    if (!a || !pane) return;
    var content = pane.parentElement;
    Array.prototype.forEach.call(content.children, function (p) {
      var on = p === pane;
      p.classList.toggle("active", on); p.classList.toggle("show", on);
    });
    document.querySelectorAll("#pages a").forEach(function (x) {
      var on = x === a; x.classList.toggle("active", on); x.setAttribute("aria-selected", on ? "true" : "false");
    });
    if (window.jQuery) {
      window.jQuery(a).trigger(window.jQuery.Event("shown.bs.tab", { relatedTarget: null }));
      window.jQuery(document).trigger({ type: "shiny:inputchanged", name: "pages", value: target });
    }
    setTimeout(drawPending, 40);
  }

  // ---------- What each control changes ----------
  function onInput(name, value) {
    switch (name) {
      case "rail_go": nav(value); break;
      case "bt_pick": load("blood101").then(function (b) { setHTML("compat_result", b.compat[value]); }); break;
      default: route(name);
    }
  }
  function route(name) {
    if (name === "live_group") load("live").then(function (x) { apply(x[radio("live_group")]); });
    else if (name === "cmp_year" || name === "cmp_methods") {
      load("compare").then(function (x) { apply(x[radio("cmp_year") + "|" + methodKey(checks("cmp_methods"))]); });
      if (name === "cmp_methods") withDaily(drawDaily);
    }
    else if (name === "cmp_group" || name === "cmp_dates_from" || name === "cmp_dates_to") withDaily(drawDaily);
    else if (name === "low_year") load("low").then(function (x) { apply(x[radio("low_year")]); });
    else if (name === "month_year" || name === "month_methods")
      load("monthly").then(function (x) { apply(x[radio("month_year") + "|" + methodKey(checks("month_methods"))]); });
    else if (name === "pattern_groups" || name === "pattern_years_from" || name === "pattern_years_to") withDaily(drawPatterns);
    else if (name === "region_pick") load("blood101").then(function (b) { setHTML("region_result", b.region[radio("region_pick")]); });
    else if (name === "report_type") { conditions(); updateReport(); }
    else if (/^(rep_|card_)/.test(name)) updateReport();
  }
  document.addEventListener("change", function (e) {
    var t = e.target; if (!t) return;
    route(t.name || t.id);
  });
  document.addEventListener("click", function (e) {
    var c = e.target.closest && e.target.closest("#card_clear");
    if (c) {
      e.preventDefault();
      ["card_day", "card_month", "card_year"].forEach(function (id) { var el = document.getElementById(id); if (el) el.value = ""; });
      updateReport();
    }
    var g = e.target.closest && e.target.closest("#csv_go");
    if (g) withDaily(function () { if (handlers.download_csv) handlers.download_csv(csvText()); });
    if (e.target.closest && e.target.closest(".kv-tab, .fold > summary, .how-read > summary")) setTimeout(drawPending, 60);
  });

  // The old Shinylive site installed a service worker; remove it so it can never serve stale files.
  if ("serviceWorker" in navigator) {
    navigator.serviceWorker.getRegistrations().then(function (rs) { rs.forEach(function (r) { r.unregister(); }); }).catch(function () {});
  }

  // ---------- Start ----------
  document.addEventListener("DOMContentLoaded", function () {
    var $ = window.jQuery;
    conditions();
    if ($) {
      $(document).on("shown.bs.tab shown", function () { setTimeout(drawPending, 40); });
      $(document).trigger("shiny:connected");
      // Tables and text baked into the page still need their behaviour (sorting, count-ups).
      document.querySelectorAll(".shiny-html-output").forEach(fire);
    }
    window.addEventListener("resize", function () { setTimeout(drawPending, 60); });
    load("init").then(function (x) {
      init = x;
      CHARTS.forEach(function (id) { if (x[id]) setChart(id, x[id]); });
      if ($) $(document).trigger("shiny:idle");
      // Warm the files the next clicks are most likely to need.
      setTimeout(function () { load("daily"); load("compare"); }, 1500);
    }).catch(function (e) { console.error("Could not load the dashboard data", e); });
  });
})();
