# Flow and mind-map diagrams in the same visual language as the compatibility
# explorer: drop-shaped nodes, curved links with flowing dashes, a beating hub.
# Each diagram is inline SVG with an accessible label, plus the plain list as a
# fallback that phones show instead (the SVG text would be too small there).
# Sourced by app.R after its helpers.

wrap_lines <- function(text, width = 26, max_lines = 3) {
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  lines <- character(); cur <- ""
  for (w in words) {
    cand <- if (nzchar(cur)) paste(cur, w) else w
    if (nchar(cand) > width && nzchar(cur)) { lines <- c(lines, cur); cur <- w } else cur <- cand
  }
  lines <- c(lines, cur)
  if (length(lines) > max_lines) {
    lines <- lines[seq_len(max_lines)]
    lines[max_lines] <- paste0(sub("[,;.]?$", "", lines[max_lines]), "…")
  }
  lines
}

DROP_D <- "M0 -15 C0 -15 -11 -2 -11 5 a11 11 0 0 0 22 0 C11 -2 0 -15 0 -15 Z"

svg_label <- function(x, y, title, sub = NULL, anchor = "start", width = 30) {
  sub_lines <- if (is.null(sub) || is.na(sub)) character() else wrap_lines(sub, width)
  tags$text(class = "mm-label", x = x, y = y, `text-anchor` = anchor,
            tags$tspan(class = "mm-title", x = x, dy = "0", title),
            lapply(sub_lines, function(l) tags$tspan(class = "mm-sub", x = x, dy = "1.35em", l)))
}

flow_link <- function(x1, y1, x2, y2, i, cls = "out") {
  mx <- (x1 + x2) / 2
  tags$path(class = paste("cf-link", cls), style = sprintf("--d:%dms", 90 * i),
            d = sprintf("M%.1f %.1f C%.1f %.1f %.1f %.1f %.1f %.1f", x1, y1, mx, y1, mx, y2, x2, y2))
}

hub <- function(x, y, lines, r = 64, cls = "") {
  n <- length(lines)
  tags$g(class = paste("mm-hub", cls), transform = sprintf("translate(%.1f %.1f)", x, y),
         tags$circle(class = "cf-halo", r = r + 10),
         tags$circle(class = "mm-hub-core", r = r),
         tags$text(`text-anchor` = "middle", y = sprintf("%.1f", -(n - 1) * 11 + 6),
                   lapply(seq_along(lines), function(i)
                     tags$tspan(x = 0, dy = if (i == 1) "0" else "1.25em", lines[i]))))
}

viz <- function(svg, fallback) div(class = "viz", svg, div(class = "viz-fallback", fallback))

# A hub with branches on both sides. branches: list of list(title, sub, tip).
mindmap <- function(centre, branches, label, key = NULL) {
  n <- length(branches)
  left <- seq(1, n, by = 2); right <- seq(2, n, by = 2)
  rows <- max(length(left), length(right))
  W <- 960; H <- max(340, 112 * rows + 40)
  cx <- W / 2; cy <- H / 2
  ys <- function(k) if (k == 1) cy else seq(70, H - 70, length.out = k)
  yl <- ys(length(left)); yr <- ys(length(right))
  lx <- 250; rx <- W - 250
  node <- function(b, x, y, side, i) {
    on <- is.null(key) || i %in% key
    tags$g(class = paste("mm-node", side, if (on) "is-on" else "is-off"),
           style = sprintf("--d:%dms", 90 * i),
           if (!is.null(b$tip)) tags$title(b$tip),
           if (is.null(b$art)) tags$path(transform = sprintf("translate(%.1f %.1f)", x, y), d = DROP_D)
           else tags$g(class = "art-badge", transform = sprintf("translate(%.1f %.1f)", x, y),
                       tags$circle(class = "art-bg", r = 28), art_icon(b$art)),
           svg_label(if (side == "left") x - (if (is.null(b$art)) 22 else 40) else x + (if (is.null(b$art)) 22 else 40),
                     y - 2, b$title, b$sub, anchor = if (side == "left") "end" else "start"))
  }
  links <- c(
    lapply(seq_along(left), function(k) flow_link(cx - 70, cy, lx + 14, yl[k], k, "in")),
    lapply(seq_along(right), function(k) flow_link(cx + 70, cy, rx - 14, yr[k], k + length(left), "out")))
  tags$svg(class = "mm-svg", viewBox = sprintf("0 0 %d %d", W, H), role = "img", `aria-label` = label,
           links,
           lapply(seq_along(left), function(k) node(branches[[left[k]]], lx, yl[k], "left", left[k])),
           lapply(seq_along(right), function(k) node(branches[[right[k]]], rx, yr[k], "right", right[k])),
           hub(cx, cy, centre))
}

n_sub <- function(sub) if (is.null(sub) || is.na(sub)) 0L else length(wrap_lines(sub, 24))

# Steps along a flowing path, labels alternating above and below.
journey <- function(steps, label) {
  n <- length(steps)
  W <- 1000; H <- 330; pad <- 80
  xs <- seq(pad, W - pad, length.out = n)
  ys <- H / 2 + c(-28, 28)[(seq_len(n) - 1) %% 2 + 1]
  path <- paste0("M", xs[1], " ", ys[1],
                 paste(vapply(2:n, function(i) {
                   mx <- (xs[i - 1] + xs[i]) / 2
                   sprintf(" C%.1f %.1f %.1f %.1f %.1f %.1f", mx, ys[i - 1], mx, ys[i], xs[i], ys[i])
                 }, ""), collapse = ""))
  tags$svg(class = "mm-svg journey", viewBox = sprintf("0 0 %d %d", W, H), role = "img", `aria-label` = label,
           tags$path(class = "jr-track", d = path),
           tags$path(class = "jr-progress", d = path, pathLength = "100"),
           lapply(seq_len(n), function(i) {
             above <- i %% 2 == 1
             tags$g(class = "jr-step", style = sprintf("--d:%dms", 120 * i),
                    tags$circle(class = "jr-ring", cx = xs[i], cy = ys[i], r = 20),
                    tags$circle(class = "jr-dot", cx = xs[i], cy = ys[i], r = 20),
                    tags$text(class = "jr-num", x = xs[i], y = ys[i] + 6, `text-anchor` = "middle", i),
                    svg_label(xs[i], if (above) ys[i] - 42 - 18 * n_sub(steps[[i]]$sub) else ys[i] + 50,
                              steps[[i]]$title,
                              steps[[i]]$sub, anchor = "middle", width = 24))
           }))
}

# Past data feeds four methods, which feed the forecast. The method with the
# lowest error in the latest period is highlighted, with each method's MAE.
method_flow <- function(scores, period_label) {
  W <- 1000; H <- 420
  lx <- 120; mx <- W / 2; rx <- W - 120
  ys <- seq(70, H - 70, length.out = 4)
  best <- scores$method[which.min(scores$MAE)]
  info <- list(
    "Seven-day baseline" = "The count from 7 days earlier",
    "Weekday average" = "Weekday mean of all earlier years",
    "Linear regression" = "Weekday + trend, earlier years",
    "Recent weekday average" = "Weekday mean of the last 3 years")
  tags$svg(class = "mm-svg", viewBox = sprintf("0 0 %d %d", W, H), role = "img",
           `aria-label` = paste0("Recorded donations feed four methods. Lowest error in ", period_label, ": ", best, "."),
           lapply(seq_along(METHODS), function(i) flow_link(lx + 70, H / 2, mx - 140, ys[i], i, "in")),
           lapply(seq_along(METHODS), function(i)
             flow_link(mx + 180, ys[i], rx - 70, H / 2, i + 4,
                       if (METHODS[i] == best) "out is-best" else "out is-dim")),
           lapply(seq_along(METHODS), function(i) {
             m <- METHODS[i]
             mae <- scores$MAE[scores$method == m]
             tags$g(class = paste("mf-node", if (m == best) "is-best"),
                    tags$rect(x = mx - 140, y = ys[i] - 32, width = 320, height = 64, rx = 32),
                    tags$text(class = "mf-title", x = mx - 116, y = ys[i] - 4, m),
                    tags$text(class = "mf-sub", x = mx - 116, y = ys[i] + 17, info[[m]]),
                    tags$text(class = "mf-mae", x = mx + 160, y = ys[i] + 8, `text-anchor` = "end",
                              if (length(mae)) fmt(mae, 1) else ""))
           }),
           tags$text(class = "mf-caption", x = mx + 160, y = 24, `text-anchor` = "end",
                     paste("Average error,", period_label)),
           hub(lx, H / 2, c("Past", "donations"), r = 58, cls = "is-source"),
           hub(rx, H / 2, c("Each day's", "forecast"), r = 58))
}

# Small looping icons for the aftercare map, each about one tip.
art_icon <- function(name) {
  switch(name,
    cup = tags$g(class = "ai ai-cup",
      tags$path(d = "M-10 -2 h18 v9 a7 7 0 0 1 -7 7 h-4 a7 7 0 0 1 -7 -7 Z"),
      tags$path(d = "M8 1 h3 a3.5 3.5 0 0 1 0 7 h-3"),
      tags$path(class = "ai-steam s1", d = "M-5 -7 c-2 -3 2 -5 0 -8"),
      tags$path(class = "ai-steam s2", d = "M2 -7 c-2 -3 2 -5 0 -8")),
    water = tags$g(class = "ai ai-water",
      tags$defs(tags$clipPath(id = "ai-glass", tags$path(d = "M-9 -12 h18 l-2.5 24 h-13 Z"))),
      tags$g(`clip-path` = "url(#ai-glass)",
             tags$path(class = "ai-fill", d = "M-24 0 q6 -3 12 0 t12 0 t12 0 t12 0 V20 H-24 Z")),
      tags$path(d = "M-9 -12 h18 l-2.5 24 h-13 Z")),
    noalcohol = tags$g(class = "ai ai-noalc",
      tags$path(d = "M-7 -12 h14 c0 9 -4 12 -7 12 c-3 0 -7 -3 -7 -12 Z M0 0 v9 M-5 11 h10"),
      tags$path(class = "ai-slash", d = "M-12 12 L12 -12")),
    sun = tags$g(class = "ai ai-sun",
      tags$circle(r = 6),
      tags$g(class = "ai-rays", lapply(0:7, function(k) {
        a <- k * pi / 4
        tags$path(d = sprintf("M%.1f %.1f L%.1f %.1f", 10 * cos(a), 10 * sin(a), 14 * cos(a), 14 * sin(a)))
      }))),
    rest = tags$g(class = "ai ai-rest",
      tags$text(class = "ai-z z1", x = -8, y = 8, "z"),
      tags$text(class = "ai-z z2", x = -1, y = 2, "z"),
      tags$text(class = "ai-z z3", x = 6, y = -4, "z")),
    dizzy = tags$g(class = "ai ai-dizzy",
      tags$path(d = "M0 0 m-1 0 a1 1 0 1 1 2 0 a3 3 0 1 1 -6 0 a5 5 0 1 1 10 0 a7 7 0 1 1 -14 0 a9 9 0 1 1 18 0")),
    NULL)
}
