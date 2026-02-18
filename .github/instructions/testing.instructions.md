---
applyTo: "tests/**/*"
---

# Testing Standards

## Purpose & Scope

Guidelines for writing and organizing tests in this package using `testthat`.

---

## Test File Structure

### Naming

- Test files: `test-<source_file_name>.R`
- Match source file names in `R/` directory

```
R/renv_dep_add.R -> tests/testthat/test-renv_dep_add.R
R/rprofile.R     -> tests/testthat/test-rprofile.R
```

### Organization

- Group related tests with `test_that()` blocks
- Use descriptive test names
- One concept per test

```r
# ✅ Correct
test_that("renvvv_restore function exists", {
  expect_true(is.function(renvvv_restore))
})

test_that("renvvv_update function exists", {
  expect_true(is.function(renvvv_update))
})

# ❌ Incorrect - testing multiple things
test_that("functions work", {
  expect_true(is.function(renvvv_restore))
  expect_true(is.function(renvvv_update))
  expect_equal(result, expected)
})
```

---

## Test Patterns

### Function Existence Tests

- Verify exported functions exist
- Verify internal helper functions exist

```r
test_that("renvvv_dep_add function exists", {
  expect_true(is.function(renvvv_dep_add))
})

test_that(".ensure_cli helper exists", {
  expect_true(is.function(renvvv:::.ensure_cli))
})
```

### Behavior Tests

- Test expected outputs
- Test error conditions
- Use `expect_error()` for expected failures

```r
test_that("function returns expected value", {
  result <- some_function("input")
  expect_equal(result, expected_value)
})

test_that("function handles invalid input", {
  expect_error(some_function(NULL))
})
```

---

## Running Tests

### Commands

```r
# Run all tests
devtools::test()

# Run specific test file
testthat::test_file("tests/testthat/test-renv_dep_add.R")

# Run with verbose output
devtools::test(reporter = "summary")
```

### Before Committing

- All tests must pass
- Add tests for new functionality
- Update tests when changing behavior

---

## Best Practices

✅ **Test one thing per test** - Clear, focused tests

✅ **Use descriptive names** - Explain what's being tested

✅ **Test edge cases** - NULL, empty, invalid inputs

✅ **Clean up after tests** - Use `on.exit()` or `withr::defer()`

❌ **Don't skip tests without reason** - Document why if necessary

❌ **Don't test implementation details** - Focus on behavior
