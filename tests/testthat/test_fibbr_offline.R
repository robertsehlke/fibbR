library(testthat)
library(mockery)
library(fibbR)

# Mock functions to simulate API responses
mock_openai_response <- function() {
  list(
    list(role = "system", content = "You are helpful, but concise."),
    list(role = "user", content = "Test prompt"),
    list(role = "assistant", content = "OpenAI response")
  )
}

mock_claude_response <- function() {
  list(
    list(role = "user", content = "Test prompt"),
    list(role = "assistant", content = "Claude response")
  )
}

# Test suite
test_that("fib function works correctly", {
  # Test OpenAI API
  mock_send_api_request <- mock(mock_openai_response())
  stub(fib, "send_api_request", mock_send_api_request)
  
  result <- fib("Test prompt", api = "openai")
  expect_called(mock_send_api_request, 1)
  
  call_args <- mock_args(mock_send_api_request)[[1]]
  # print("OpenAI call args structure:")
  # print(str(call_args))
  
  expect_type(call_args, "list")
  expect_equal(length(call_args), 3)
  expect_equal(call_args[[1]]$model, Sys.getenv("FIBBR_OPENAI_MODEL"))
  expect_equal(call_args[[1]]$messages[[2]]$content, "Test prompt")
  expect_equal(call_args[[2]], "openai")
  expect_equal(call_args[[3]], FALSE)
  
  expect_type(result, "list")
  expect_equal(length(result), 3)  # system, user, and assistant messages
  expect_equal(result[[3]]$role, "assistant")
  expect_equal(result[[3]]$content, "OpenAI response")
  
  # Test Claude API
  mock_send_api_request <- mock(mock_claude_response())
  stub(fib, "send_api_request", mock_send_api_request)
  
  result <- fib("Test prompt", api = "claude")
  expect_called(mock_send_api_request, 1)
  
  call_args <- mock_args(mock_send_api_request)[[1]]
  # print("Claude call args structure:")
  # print(str(call_args))
  
  expect_type(call_args, "list")
  expect_equal(length(call_args), 3)
  expect_equal(call_args[[1]]$model, Sys.getenv("FIBBR_CLAUDE_MODEL"))
  expect_equal(call_args[[1]]$messages[[1]]$content, "Test prompt")
  expect_equal(call_args[[2]], "claude")
  expect_equal(call_args[[3]], FALSE)
  
  expect_type(result, "list")
  expect_equal(length(result), 2)  # user and assistant messages
  expect_equal(result[[2]]$role, "assistant")
  expect_equal(result[[2]]$content, "Claude response")
  
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