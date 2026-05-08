library(testhat)

text <- "John Does lives in New York in United States of America."
entities <- extract_entity_rb(text)

test_that("extract_entity_rb underline exists", {
  expect_equal(any(grepl(x = entities, "_")))
})

test_that("extract_entity_rb length ok", {
  expect_equal(length(entities), 3)
})

test_that("extract_entity_rb entities correctly", {
  expect_equal(
    entities,
    c("John_Does", "New_York", "United_States_of_America")
  )
})
