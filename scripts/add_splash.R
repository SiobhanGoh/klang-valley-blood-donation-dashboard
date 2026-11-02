# Adds the loading screen and page metadata to the Shinylive export.
# Usage: Rscript scripts/add_splash.R _site
args <- commandArgs(trailingOnly = TRUE)
site <- if (length(args)) args[1] else "_site"
index <- file.path(site, "index.html")
html <- paste(readLines(index, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
splash <- paste(readLines("site/splash.html", warn = FALSE, encoding = "UTF-8"), collapse = "\n")

head_extra <- paste(
  '<meta name="description" content="Daily blood donation collections in the Klang Valley, Malaysia: live data, four forecasting methods compared, and general blood donation information.">',
  '<meta name="theme-color" content="#FBF5F3">',
  '<link rel="icon" type="image/svg+xml" href="data:image/svg+xml,%3Csvg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 64 64%22%3E%3Cdefs%3E%3ClinearGradient id=%22g%22 x1=%220%22 y1=%220%22 x2=%220%22 y2=%221%22%3E%3Cstop offset=%220%22 stop-color=%22%23C2344F%22/%3E%3Cstop offset=%221%22 stop-color=%22%234A1426%22/%3E%3C/linearGradient%3E%3C/defs%3E%3Cpath d=%22M32 4C32 4 10 30 10 42a22 22 0 0 0 44 0C54 30 32 4 32 4Z%22 fill=%22url%28%23g%29%22/%3E%3Cpath d=%22M22 42a10 10 0 0 0 7 9.5%22 fill=%22none%22 stroke=%22%23fff%22 stroke-opacity=%22.7%22 stroke-width=%224%22 stroke-linecap=%22round%22/%3E%3C/svg%3E">',
  '<link rel="preconnect" href="https://fonts.googleapis.com">',
  '<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>',
  '<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght,SOFT@9..144,300..700,50&display=swap">',
  sep = "\n")

stopifnot(grepl("</head>", html, fixed = TRUE), grepl("</body>", html, fixed = TRUE))
html <- sub("<title>[^<]*</title>", "<title>Klang Valley Blood Donation Forecasts</title>", html)
if (!grepl("<title>", html, fixed = TRUE)) {
  head_extra <- paste('<title>Klang Valley Blood Donation Forecasts</title>', head_extra, sep = "\n")
}
html <- sub("</head>", paste0(head_extra, "\n</head>"), html, fixed = TRUE)
# The splash goes first in <body> so it paints before anything else loads.
html <- sub("<body([^>]*)>", paste0("<body\\1>\n", gsub("\\\\", "\\\\\\\\", splash)), html)
writeLines(html, index, useBytes = TRUE)
cat("Loading screen added to", index, "\n")
