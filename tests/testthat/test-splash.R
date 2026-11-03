# The loading screen lifts only when it finds a class from the first page.
# If a redesign removes that class, the dashboard loads but stays covered.
test_that("the loading screen's ready check matches the app's first page", {
  root <- normalizePath(file.path("..", ".."))
  splash <- paste(readLines(file.path(root, "site", "splash.html"), warn = FALSE), collapse = "\n")
  wanted <- regmatches(splash, gregexpr("\\.(ov-hero|chapter-head|panel)\\b", splash))[[1]]
  expect_true(length(wanted) > 0)
  app <- paste(readLines(file.path(root, "app.R"), warn = FALSE), collapse = "\n")
  expect_true(any(vapply(sub("^\\.", "", unique(wanted)),
                         function(cls) grepl(paste0('"', cls, '[ "]'), app), logical(1))))
})
