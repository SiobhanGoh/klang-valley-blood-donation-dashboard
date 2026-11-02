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
