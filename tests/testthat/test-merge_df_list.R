df1 <- data.frame(id = 1:3, value1 = c(10, 20, 30))
df2 <- data.frame(id = 2:4, value2 = c(15, 25, 35))
df3 <- data.frame(id = 3:5, value3 = c(12, 22, 32))
df_list <- list(df1, df2, df3)
merge_df_list(df_list, merge_by = "id")
test_that("merge_df_list merges multiple data frames correctly", {
  merged_df <- merge_df_list(df_list, merge_by = "id")

  expected_df <- data.frame(
    id = 1:5,
    value1 = c(10, 20, 30, NA, NA),
    value2 = c(NA, 15, 25, 35, NA),
    value3 = c(NA, NA, 12, 22, 32)
  )

  expect_equal(merged_df, expected_df)
})
