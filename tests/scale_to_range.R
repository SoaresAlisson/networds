# Test script for scale_to_range function

# Load required library for more detailed testing (optional)
library(testthat)

# Define the function (assuming it's not already loaded)

# Test 1: Basic functionality with default parameters
test_that("Basic scaling works with default parameters", {
  values <- c(1, 2, 3, 4, 5)
  result <- scale_to_range(values)
  
  expect_equal(min(result), 0.15)
  expect_equal(max(result), 8)
  expect_equal(length(result), length(values))
  expect_equal(result[1], 0.15)  # min maps to new_min
  expect_equal(result[5], 8)      # max maps to new_max
  expect_equal(result[3], (0.15 + 8)/2)  # middle value maps to middle
})

# Test 2: Different input ranges
test_that("Works with different input ranges", {
  # Negative values
  values <- c(-10, 0, 10, 20)
  result <- scale_to_range(values)
  expect_equal(min(result), 0.15)
  expect_equal(max(result), 8)
  
  # Decimal values
  values <- c(0.1, 0.5, 0.9, 1.3)
  result <- scale_to_range(values)
  expect_equal(min(result), 0.15)
  expect_equal(max(result), 8)
  
  # Large numbers
  values <- c(1000, 2000, 3000, 4000)
  result <- scale_to_range(values)
  expect_equal(min(result), 0.15)
  expect_equal(max(result), 8)
})

# Test 3: Custom min/max parameters
test_that("Works with custom new_min and new_max", {
  values <- c(10, 20, 30, 40, 50)
  
  # Scale to 0-1
  result <- scale_to_range(values, new_min = 0, new_max = 1)
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
  expect_equal(result, c(0, 0.25, 0.5, 0.75, 1))
  
  # Scale to -1 to 1
  result <- scale_to_range(values, new_min = -1, new_max = 1)
  expect_equal(min(result), -1)
  expect_equal(max(result), 1)
  expect_equal(result, c(-1, -0.5, 0, 0.5, 1))
  
  # Scale to 5-10
  result <- scale_to_range(values, new_min = 5, new_max = 10)
  expect_equal(min(result), 5)
  expect_equal(max(result), 10)
  expect_equal(result, c(5, 6.25, 7.5, 8.75, 10))
})

# Test 4: Error handling
test_that("Error handling works correctly", {
  values <- c(1, 2, 3)
  
  # Vector with less than 2 values
  expect_error(scale_to_range(c(1)), "Vector must have at least 2 values")
  
  # new_min >= new_max
  expect_error(scale_to_range(values, new_min = 5, new_max = 3), 
               "new_min must be less than new_max")
  expect_error(scale_to_range(values, new_min = 3, new_max = 3), 
               "new_min must be less than new_max")
})

# Test 5: Identical values handling
test_that("Handles identical values correctly", {
  values <- c(5, 5, 5, 5)
  
  expect_warning(result <- scale_to_range(values), 
                 "All values are identical. Returning all values as new_min")
  expect_equal(result, rep(0.15, 4))
  
  # With custom new_min
  expect_warning(result <- scale_to_range(values, new_min = 10, new_max = 20), 
                 "All values are identical. Returning all values as new_min")
  expect_equal(result, rep(10, 4))
})

# Test 6: Preserves proportions
test_that("Preserves proportional relationships", {
  values <- c(2, 4, 8, 16)
  result <- scale_to_range(values, new_min = 0, new_max = 1)
  
  # Check if ratios are preserved
  expect_equal(result[2]/result[1], 2)  # 4/2 = 2 should equal result[2]/result[1]
  expect_equal(result[4]/result[3], 2)  # 16/8 = 2 should equal result[4]/result[3]
})

# Test 7: Edge cases
test_that("Handles edge cases correctly", {
  # Very small range
  values <- c(1.0001, 1.0002)
  result <- scale_to_range(values)
  expect_equal(min(result), 0.15)
  expect_equal(max(result), 8)
  
  # Very large range
  values <- c(1e-10, 1e10)
  result <- scale_to_range(values)
  expect_equal(min(result), 0.15)
  expect_equal(max(result), 8)
  
  # Single unique value with warning (already tested in test 5)
  # But check that it doesn't break with custom new_min
  expect_warning(scale_to_range(c(1,1,1), new_min = 0, new_max = 10))
})

# Test 8: Verify mathematical correctness
test_that("Mathematical transformation is correct", {
  values <- c(2, 5, 8)
  new_min <- 0.15
  new_max <- 8
  
  # Manual calculation
  expected <- c(
    0.15 + ((2-2)/(8-2)) * (8-0.15),  # Should be 0.15
    0.15 + ((5-2)/(8-2)) * (8-0.15),  # Should be (0.15 + 8)/2 = 4.075
    0.15 + ((8-2)/(8-2)) * (8-0.15)   # Should be 8
  )
  
  result <- scale_to_range(values)
  expect_equal(result, expected)
})

# Test 9: Non-numeric input (should error)
test_that("Handles invalid input types", {
  expect_error(scale_to_range(c("a", "b", "c")))
})

# Test 10: Check inverse transformation
test_that("Inverse transformation works", {
  values <- c(1, 3, 5, 7, 9)
  
  # Scale to 0-1
  scaled <- scale_to_range(values, new_min = 0, new_max = 1)
  
  # Inverse manually (should return original values)
  inversed <- min(values) + scaled * (max(values) - min(values))
  expect_equal(inversed, values)
})

# Print success message if all tests pass
cat("All tests passed successfully!\n")
