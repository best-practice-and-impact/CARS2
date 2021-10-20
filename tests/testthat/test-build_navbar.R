correct_yml <- '
navbar:
  title: "My Website"
  left:
    - text: "Home"
      href: index.html
    - text: "Page 1"
      href: page1.html
'

failing_yml <- '
incorrect-argument:
  title: "My Website"
  left:
    - text: "Home"
      href: index.html
    - text: "Page 1"
      href: page1.html
'

test_that("Function returns list from correctly formatted yaml", {
  expect_type(read_site_yml(text = correct_yml) ,"list")
  expect_identical(names(read_site_yml(text = correct_yml)), c("title", "pages"))
})

test_that("Function returns error for incorrectly formatted yaml", {
  expect_error(read_site_yml(text = failing_yml))
})