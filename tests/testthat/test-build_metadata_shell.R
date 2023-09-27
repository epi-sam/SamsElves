test_str_1 <- "my_string"
test_str_2 <- c("my_string", "my_bacon")
test_str_list <- list(first = "my_string", second = "bacon")

test_regex_1 <- "string"
test_regex_2 <- c("string", "bacon")

# extract_command_string -------------------------------------------------------

test_that(
  "extract_command_string returns correct value",
  {
    result <- extract_command_string(
      submit_command_text = test_str_1, regex_to_extract = test_regex_1
    )
    expect_equal(result, "string")
  }
)

test_that(
  "extract_command_string throws proper error",
  {
    expect_error(
      extract_command_string(test_str_2, test_regex_1), 
      regexp = "Must submit text length == 1"
    )
    
    expect_error(
      extract_command_string(test_str_2, test_regex_2), 
      regexp = "Must submit text length == 1"
    )
    
    expect_error(
      extract_command_string(test_str_1, test_regex_2), 
      regexp = "Must submit regex length == 1"
    )
    
    expect_error(
      extract_command_string(
        submit_command_text = test_str_1,
        regex_to_extract = test_regex_1,
        regex_to_ignore = "g"
      ),
      regexp = "No strings were extracted - inspect inputs and regex_to_ignore"
    )
  }
)
