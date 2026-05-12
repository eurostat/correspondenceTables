
if (requireNamespace("tinytest", quietly = TRUE)) {
  tinytest::test_package("correspondenceTables")
} else {
  message("tinytest not installed: test not performed")
}
