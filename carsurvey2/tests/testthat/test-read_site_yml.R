navbar_args <- list(
  title <- "my website",
  pages <- list(
    list(
      text = "Home",
      href = "index.htnml"
    ),
    list(
      text = "Page 2",
      href = "page2.htnml"
    )
  )
)

test_that("Function returns HTML string from list of arguments", {
  expect_type(build_navbar(navbar_args), "character")
})