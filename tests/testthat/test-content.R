root <- normalizePath(file.path("..", ".."))

source(file.path(root, "R", "content.R"))

test_that("red-cell compatibility follows the standard rule", {
  # O- gives to everyone; AB+ receives from everyone (NHSBT, Héma-Québec).
  expect_setequal(compatibility_for("O-")$gives_to, BLOOD_TYPES)
  expect_setequal(compatibility_for("AB+")$receives_from, BLOOD_TYPES)
  expect_setequal(compatibility_for("O-")$receives_from, "O-")
  expect_setequal(compatibility_for("AB+")$gives_to, "AB+")
  expect_setequal(compatibility_for("A+")$receives_from, c("O-", "O+", "A-", "A+"))
  expect_setequal(compatibility_for("B-")$gives_to, c("B-", "B+", "AB-", "AB+"))
  # Every type can receive its own type.
  for (t in BLOOD_TYPES) expect_true(can_give_red_cells(t, t))
})
