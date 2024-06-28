library(testthat)
library(fibbR)
library(keyring)

# Skip these tests if not running in an interactive environment
skip_if_not_interactive <- function() {
  if (!interactive()) skip("Live API tests are only run in interactive sessions")
}

# Function to safely check if a key exists in the keyring
key_exists <- function(service) {
  tryCatch({
    key_get(service, username = Sys.info()["user"])
    TRUE
  }, error = function(e) FALSE)
}

skip_if_no_api_key <- function(api) {
  service <- switch(api,
                    openai = "fibbr_openai",
                    claude = "fibbr_claude",
                    stop("Invalid API specified")
  )
  if (!key_exists(service)) skip(paste("No", api, "API key found in keyring"))
}

# Helper function to extract assistant's response from fib result
extract_response <- function(result) {
  assistant_turn <- result[[length(result)]]
  if (is.list(assistant_turn) && "role" %in% names(assistant_turn) && assistant_turn$role == "assistant") {
    return(assistant_turn$content)
  } else {
    stop("Unexpected result structure from fib function")
  }
}

test_that("OpenAI API returns a valid response", {
  skip_if_not_interactive()
  skip_if_no_api_key("openai")
  
  result <- fib("What is the capital of France? Answer in one word.", api = "openai")
  content <- extract_response(result)
  
  expect_type(content, "character")
  expect_true(nchar(content) > 0)
  expect_true(grepl("Paris", content, ignore.case = TRUE))
})

test_that("Claude API returns a valid response", {
  skip_if_not_interactive()
  skip_if_no_api_key("claude")
  
  result <- fib("What is the capital of Germany? Answer in one word.", api = "claude")
  content <- extract_response(result)
  
  expect_type(content, "character")
  expect_true(nchar(content) > 0)
  expect_true(grepl("Berlin", content, ignore.case = TRUE))
})

test_that("API handles special characters correctly", {
  skip_if_not_interactive()
  skip_if_no_api_key("openai")  # You can test this with either API
  
  result <- fib("What does 'résumé' mean?", api = "openai")
  content <- extract_response(result)
  
  expect_type(content, "character")
  expect_true(nchar(content) > 0)
  expect_true(grepl("résumé", content, ignore.case = TRUE))
})

test_that("API handles long prompts", {
  skip_if_not_interactive()
  skip_if_no_api_key("claude")  # You can test this with either API
  
  long_prompt <- paste(rep("Test sentence. ", 100), collapse = "")
  result <- fib(long_prompt, api = "claude")
  content <- extract_response(result)
  
  expect_type(content, "character")
  expect_true(nchar(content) > 0)
})

test_that("API respects max_tokens parameter", {
  skip_if_not_interactive()
  skip_if_no_api_key("openai")  # You can test this with either API
  
  result <- fib("Write a long story about a dog.", api = "openai", max_tokens = 50)
  content <- extract_response(result)
  
  expect_type(content, "character")
  expect_true(nchar(content) > 0)
  expect_true(nchar(content) < 500)  # Approximate, as tokens != characters
})

test_that("API returns correct structure", {
  skip_if_not_interactive()
  skip_if_no_api_key("openai")  # You can test this with either API
  
  result <- fib("Hello", api = "openai")
  
  expect_type(result, "list")
  expect_true(length(result) >= 2)  # At least system and user messages
  expect_equal(result[[length(result)]]$role, "assistant")
  expect_type(result[[length(result)]]$content, "character")
})