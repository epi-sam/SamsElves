root_SamsElves <- system.file(package = "SamsElves")

test_that(
  ".git folder exists for SamsElves repo",
  {
    expect_true(
      dir.exists(file.path(root_SamsElves, ".git"))
    )
  }
)

# query_git_diff ---------------------------------------------------------------

# query_git_diff(CODE_ROOT = root_SamsElves)

test_that(
  "query_git_diff does not error out", # hard to test - currently returns NULL if it finds nothing
  {
    expect_error(
      object = query_git_diff(root_SamsElves),
      regexp = NA # explicitly expect no error
    )
  }
)

test_that(
  "query_git_diff produces NULL results (user will need to make a commit to get this test to pass)",
  {
    expect_null(
      query_git_diff(root_SamsElves)
    )
  }
)

test_that(
  "assert_git_hash errors correctly",
  {
    expect_error(
      assert_git_hash(launch_hash = "abcde", script_hash = "edcba"),
      regexp = "Launch script git hash does not match downstream script git hash - please inspect and do a clean run."
    )
  }
)
