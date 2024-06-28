library(testthat)
library(fibbR)
library(mockery)

# Mock functions to simulate API responses
mock_openai_response <- function() {
  list(choices = list(list(message = list(content = "OpenAI response"))))
}

mock_claude_response <- function() {
  list(content = list(text = "Claude response"))
}

# Test suite
test_that("fib function works correctly", {
  # Mock the API request function
  mock_send_api_request <- mock(cycle = TRUE)
  stub(fib, "send_api_request", mock_send_api_request)
  
  # Test OpenAI API
  mock_send_api_request$add(mock_openai_response())
  
  result <- fib("Test prompt", api = "openai")
  expect_equal(mock_send_api_request$calls, 1)
  expect_equal(mock_send_api_request$args[[1]]$body$model, Sys.getenv("FIBBR_OPENAI_MODEL"))
  expect_equal(mock_send_api_request$args[[1]]$body$messages[[2]]$content, "Test prompt")
  expect_equal(result, "OpenAI response")
  
  # Test Claude API
  mock_send_api_request$add(mock_claude_response())
  
  result <- fib("Test prompt", api = "claude")
  expect_equal(mock_send_api_request$calls, 2)
  expect_equal(mock_send_api_request$args[[2]]$body$model, Sys.getenv("FIBBR_CLAUDE_MODEL"))
  expect_equal(mock_send_api_request$args[[2]]$body$messages[[1]]$content, "Test prompt")
  expect_equal(result, "Claude response")
  
  # Test invalid API
  expect_error(fib("Test prompt", api = "invalid"), "Invalid API specified. Use 'openai' or 'claude'.")
})

test_that("create_openai_request_body creates correct structure", {
  body <- create_openai_request_body(
    "gpt-4",
    "System prompt",
    "User prompt",
    NULL,
    FALSE,
    NULL,
    0.7,
    100
  )
  
  expect_equal(body$model, "gpt-4")
  expect_equal(body$messages[[1]]$role, "system")
  expect_equal(body$messages[[1]]$content, "System prompt")
  expect_equal(body$messages[[2]]$role, "user")
  expect_equal(body$messages[[2]]$content, "User prompt")
  expect_equal(body$stream, FALSE)
  expect_equal(body$temperature, 0.7)
  expect_equal(body$max_tokens, 100)
})

test_that("create_claude_request_body creates correct structure", {
  body <- create_claude_request_body(
    "User prompt",
    "System prompt",
    NULL,
    FALSE,
    0.7,
    100
  )
  
  expect_equal(body$model, Sys.getenv("FIBBR_CLAUDE_MODEL"))
  expect_equal(body$messages[[1]]$role, "user")
  expect_equal(body$messages[[1]]$content, "User prompt")
  expect_equal(body$system, "System prompt")
  expect_equal(body$stream, FALSE)
  expect_equal(body$temperature, 0.7)
  expect_equal(body$max_tokens, 100)
})

test_that("get_default_api returns correct default", {
  options(fibbr_default_api = NULL)
  expect_equal(get_default_api(), "claude")
  
  options(fibbr_default_api = "openai")
  expect_equal(get_default_api(), "openai")
  
  options(fibbr_default_api = "invalid")
  expect_warning(result <- get_default_api(), "Invalid default API set. Falling back to 'claude'.")
  expect_equal(result, "claude")
})

test_that("fib_set_default_api sets correct default", {
  fib_set_default_api("openai")
  expect_equal(getOption("fibbr_default_api"), "openai")
  
  fib_set_default_api("claude")
  expect_equal(getOption("fibbr_default_api"), "claude")
  
  expect_error(fib_set_default_api("invalid"), "Invalid API. Please choose 'claude' or 'openai'.")
})