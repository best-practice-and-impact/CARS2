library(testthat)

dummy_data <-  carsurvey2::data_rename_cols(carsurvey2::dummy_data) %>%
               carsurvey2::data_derive_rap_scores()

rap_score_components_dummy <- carsurvey2::table_rap_score_components(dummy_data)

test_that("Function checks row names and their frequencies", {
  expect_identical(c(as.character(rownames(rap_score_components_dummy[1,])),as.numeric(rap_score_components_dummy[1,3])), c("cont_integreation_score",332))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[2,])),as.numeric(rap_score_components_dummy[2,3])), c("dep_management_score",332))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[3,])),as.numeric(rap_score_components_dummy[3,3])), c("function_score",364))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[4,])),as.numeric(rap_score_components_dummy[4,3])), c("test_score",364))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[5,])),as.numeric(rap_score_components_dummy[5,3])), c("function_doc_score",364))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[6,])),as.numeric(rap_score_components_dummy[6,3])), c("package_score",364))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[7,])),as.numeric(rap_score_components_dummy[7,3])), c("code_style_score",364))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[8,])),as.numeric(rap_score_components_dummy[8,3])), c("doc_score",154))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[9,])),as.numeric(rap_score_components_dummy[9,3])), c("version_control_score",364))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[10,])),as.numeric(rap_score_components_dummy[10,3])), c("use_open_source_score",364))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[11,])),as.numeric(rap_score_components_dummy[11,3])), c("open_code_score",364))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[11,])),as.numeric(rap_score_components_dummy[11,3])), c("open_code_score",364))
  expect_identical(c(as.character(rownames(rap_score_components_dummy[12,])),as.numeric(rap_score_components_dummy[12,3])), c("peer_review_score",437))
})

test_that("Function checks Components and group", {
  expect_identical(c(as.character(rap_score_components_dummy[1,1]), as.character(rap_score_components_dummy[1,2])), c("Continuous integration","Advanced"))
  expect_identical(c(as.character(rap_score_components_dummy[2,1]), as.character(rap_score_components_dummy[2,2])), c("Dependency management","Advanced"))
  expect_identical(c(as.character(rap_score_components_dummy[3,1]), as.character(rap_score_components_dummy[3,2])), c("Functions","Advanced"))
  expect_identical(c(as.character(rap_score_components_dummy[4,1]), as.character(rap_score_components_dummy[4,2])), c("Unit testing","Advanced"))
  expect_identical(c(as.character(rap_score_components_dummy[5,1]), as.character(rap_score_components_dummy[5,2])), c("Function documentation","Advanced"))
  expect_identical(c(as.character(rap_score_components_dummy[6,1]), as.character(rap_score_components_dummy[6,2])), c("Code packages","Advanced"))
  expect_identical(c(as.character(rap_score_components_dummy[7,1]), as.character(rap_score_components_dummy[7,2])), c("Follow code style guidelines","Advanced"))
  expect_identical(c(as.character(rap_score_components_dummy[8,1]), as.character(rap_score_components_dummy[8,2])), c("Documentation","Basic"))
  expect_identical(c(as.character(rap_score_components_dummy[9,1]), as.character(rap_score_components_dummy[9,2])), c("Version control","Basic"))
  expect_identical(c(as.character(rap_score_components_dummy[10,1]), as.character(rap_score_components_dummy[10,2])), c("Use open source software","Basic"))
  expect_identical(c(as.character(rap_score_components_dummy[11,1]), as.character(rap_score_components_dummy[11,2])), c("Team open source code","Basic"))
  expect_identical(c(as.character(rap_score_components_dummy[12,1]), as.character(rap_score_components_dummy[12,2])), c("Peer review","Basic"))
})

test_that("Function check column names", {
  expect_identical(colnames(rap_score_components_dummy), c("Component","Group","Count"))
})

test_that("Function checks length of rows and columns" ,{
  expect_equal(nrow(rap_score_components_dummy), 12)
  expect_equal(ncol(rap_score_components_dummy), 3)
})