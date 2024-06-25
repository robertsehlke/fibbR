# Environment variables for configuration
Sys.setenv(FIBBR_DEFAULT_MODEL = "gpt-4o")
Sys.setenv(FIBBR_CLAUDE_MODEL = "claude-3-5-sonnet-20240620")



#' Set the default API for FibbR
#'
#' @param api A character string specifying the API: "claude" or "openai"
#' @export
fib_set_default_api = function(api) {
  if (!api %in% c("claude", "openai")) {
    stop("Invalid API. Please choose 'claude' or 'openai'.")
  }
  options(fibbr_default_api = api)
}


#' Get the default API for FibbR
#'
#' @return A character string specifying the default API: "claude" or "openai"
get_default_api = function() {
  api = getOption("fibbr_default_api", default = "claude")
  if (!api %in% c("claude", "openai")) {
    warning("Invalid default API set. Falling back to 'claude'.")
    api = "claude"
  }
  api
}


#' Main FibbR function
#'
#' @param prompt A character string representing the user's input prompt.
#' @param model A character string representing the model to be used.
#' @param api A character string specifying the API to use: "openai" or "claude".
#' @param system_start_prompt A character string for the system's starting prompt.
#' @param history A list of previous interactions.
#' @param stream A logical value indicating whether to stream the response.
#' @param logit_bias A list of token biases (OpenAI only).
#' @param temperature A numeric value for response randomness.
#' @param max_tokens Maximum number of tokens to generate.
#'
#' @return An invisible response object.
#' @export
fib = function(prompt, 
               model = Sys.getenv("FIBBR_MODEL"),
               api = "openai",
               system_start_prompt = "You are helpful, but concise.",
               history = NULL,
               stream = FALSE,
               logit_bias = NULL,
               temperature = 0,
               max_tokens = 1000) {
  
  if (api == "openai") {
    model = validate_openai_model(model)
    body = create_openai_request_body(model, system_start_prompt, prompt, history, stream, logit_bias, temperature, max_tokens)
    response = send_api_request(body, api, stream)
  } else if (api == "claude") {
    body = create_claude_request_body(prompt, system_start_prompt, history, stream, temperature, max_tokens)
    response = send_api_request(body, api, stream)
  } else {
    stop("Invalid API specified. Use 'openai' or 'claude'.")
  }
  
  invisible(response)
}

#' Create request body for OpenAI API
create_openai_request_body = function(model, system_start_prompt, prompt, history, stream, logit_bias, temperature, max_tokens) {
  messages = if(is.null(history)) {
    list(
      list(role = "system", content = system_start_prompt),
      list(role = "user", content = prompt)
    )
  } else {
    c(history, list(list(role = "user", content = prompt)))
  }
  
  list(
    model = model,
    messages = messages,
    stream = stream,
    temperature = temperature,
    logit_bias = logit_bias,
    max_tokens = max_tokens
  )
}

#' Create request body for Claude API
create_claude_request_body = function(prompt, system_start_prompt, history, stream, temperature, max_tokens) {
  messages = if(is.null(history)) {
    list(list(role = "user", content = prompt))
  } else {
    c(history, list(list(role = "user", content = prompt)))
  }
  
  list(
    model = Sys.getenv("FIBBR_CLAUDE_MODEL"),
    messages = messages,
    system = system_start_prompt,
    stream = stream,
    temperature = temperature,
    max_tokens = max_tokens
  )
}

#' Send API request
send_api_request = function(body, api, stream) {
  url = if(api == "openai") 
    "https://api.openai.com/v1/chat/completions" else 
      "https://api.anthropic.com/v1/messages"
  headers = get_api_headers(api)
  
  if(stream) {
    POST_stream(url, jsonlite::toJSON(body, auto_unbox = TRUE), headers, api)
  } else {
    response = httr::POST(url, body = body, encode = "json", httr::add_headers(.headers = headers))
    handle_api_response(response, body, api)
  }
}

#' Get API headers
get_api_headers = function(api) {
  common_headers = c("Content-Type" = "application/json")
  if(api == "openai") {
    c(common_headers, "Authorization" = paste("Bearer", get_api_token("openai")))
  } else {
    c(common_headers, 
      "x-api-key" = get_api_token("claude"),
      "anthropic-version" = "2023-06-01")
  }
}

#' Handle API response
handle_api_response = function(response, body, api) {
  parsed = jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
  
  if (!is.null(parsed$error)) {
    stop(paste(api, "API Error:", parsed$error$message))
  }
  
  content = if(api == "openai") parsed$choices$message$content else parsed$content$text[parsed$content$type == "text"]
  
  updated_history = c(body$messages, list(list("role" = "assistant", "content" = content)))
  
  message(content)
  
  updated_history
}

#' Stream API response
POST_stream = function(url, json_body, headers, api) {
  curl_handle = curl::new_handle()
  curl::handle_setheaders(curl_handle, .list = headers)
  curl::handle_setopt(curl_handle, post = TRUE, postfields = json_body)
  
  callback = create_stream_callback(api)
  
  curl::curl_fetch_stream(url, handle = curl_handle, fun = callback)
  
  invisible(TRUE)
}

#' Create stream callback function
create_stream_callback = function(api) {
  document_id = rstudioapi::getActiveDocumentContext()$id
  
  function(data) {
    if (api == "openai") {
      process_openai_stream(data, document_id)
    } else {
      process_claude_stream(data, document_id)
    }
  }
}

#' Process OpenAI stream
process_openai_stream = function(data, document_id) {
  subchunks = strsplit(rawToChar(data), split = '\n\n')[[1]]
  subchunks = subchunks[!subchunks == '[DONE]']
  subchunks = gsub('data: ', '', subchunks)
  
  content = sapply(subchunks, function(chunk) {
    tryCatch({
      jsonlite::fromJSON(chunk)$choices$delta$content
    }, error = function(e) NULL)
  })
  
  content = paste(content[!sapply(content, is.null)], collapse = "")
  rstudioapi::insertText(content, id = document_id)
}

#' Process Claude stream
process_claude_stream = function(data, document_id) {
  events = strsplit(rawToChar(data), split = "\n\n")[[1]]
  
  for (event in events) {
    if (event == "") next
    
    event_parts = strsplit(event, split = "\n")[[1]]
    event_type = sub("event: ", "", event_parts[1])
    event_data = jsonlite::fromJSON(sub("data: ", "", event_parts[2]))
    
    switch(event_type,
           "message_start" = {
             # Initialize message, if needed
           },
           "content_block_start" = {
             # No action needed, content will be handled in content_block_delta
           },
           "content_block_delta" = {
             if (event_data$delta$type == "text_delta") {
               rstudioapi::insertText(event_data$delta$text, id = document_id)
             }
           },
           "content_block_stop" = {
             # No action needed, we've already inserted the text
           },
           "message_delta" = {
             # Update message metadata, if needed
           },
           "message_stop" = {
             # Finalize message, if needed
           },
           "ping" = {
             # Ignore ping events
           },
           {
             warning(paste("Unknown event type:", event_type))
           }
    )
  }
}

#' Validate OpenAI model
validate_openai_model = function(model) {
  if (model == "") {
    model = Sys.getenv("FIBBR_DEFAULT_MODEL")
    message(paste0('Using "', model, '" model. To change, please execute: Sys.setenv(FIBBR_MODEL = "your-preferred-model")'))
  }
  
  engines = get_openai_engines()
  if (!model %in% engines) {
    stop(paste("Invalid model:", model, ". Available models:", paste(engines, collapse = ", ")))
  }
  
  model
}

#' Get OpenAI engines
get_openai_engines = function() {
  url = "https://api.openai.com/v1/engines"
  headers = c(
    'Authorization' = paste('Bearer', get_api_token("openai")),
    'Content-Type' = 'application/json'
  )
  response = httr::GET(url, httr::add_headers(.headers=headers))
  
  jsonlite::fromJSON(httr::content(response, "text"))$data %>%
    dplyr::filter(ready) %>%
    `[[`("id")
}

#' Get API token
get_api_token = function(api) {
  service = if(api == "openai") "fibbr_openai" else "fibbr_claude"
  
  if(!service %in% keyring::key_list()$service) {
    stop(paste(api, "API token not set. Please use fib_set_token() to set it."))
  }
  
  keyring::key_get(service, username = Sys.info()["user"])
}

#' Set API token
#'
#' @param api A character string specifying the API: "openai" or "claude"
#' @export
fib_set_token = function(api) {
  service = if(api == "openai") "fibbr_openai" else "fibbr_claude"
  keyring::key_set(service, username = Sys.info()["user"], prompt = paste(api, "API token: "))
}

#' Generate code using FibbR
#'
#' @param prompt A character string representing the code generation prompt.
#' @param history A list of previous interactions.
#' @param stream A logical value indicating whether to stream the response.
#' @param model A character string specifying the model to be used.
#' @param api A character string specifying the API to use: "openai" or "claude".
#' @param max_tokens Maximum number of tokens to generate.
#'
#' @return An invisible response object.
#' @export
fib_code = function(prompt,
                     history = NULL,
                     stream = TRUE,
                     model = Sys.getenv("FIBBR_MODEL"),
                     api = get_default_api(),
                     max_tokens = 1000) {
  
  system_start_prompt = paste(
    'You are an expert R programmer.',
    'You write code. You dont explain because youre in a hurry.',
    'You realise the situation is serious. People will die if you do not deliver.',
    'If you provide any explanations, you make a comment using the # symbol.',
    'If you encounter R script that is not valid, answer with "# ???", nothing else.',
    collapse = " "
  )
  
  out = fib(prompt = prompt,
             system_start_prompt = system_start_prompt,
             history = history,
             stream = stream,
             model = model,
             api = api,
             logit_bias = if(api == "openai") list("15506" = -100, "63" = -100) else NULL,
             max_tokens = max_tokens)
  
  rstudioapi::insertText(text = "\n")
  invisible(out)
}

#' Generate Roxygen docstring using FibbR
#'
#' @param prompt A character string representing the code for which to generate a docstring.
#' @param history A list of previous interactions.
#' @param stream A logical value indicating whether to stream the response.
#' @param model A character string specifying the model to be used.
#' @param api A character string specifying the API to use: "openai" or "claude".
#' @param max_tokens Maximum number of tokens to generate.
#'
#' @return An invisible response object.
#' @export
fib_roxygen = function(prompt,
                        history = NULL,
                        stream = TRUE,
                        model = Sys.getenv("FIBBR_MODEL"),
                        api = get_default_api(),
                        max_tokens = 1000) {
  
  prompt = paste0('Please write an roxygen docstring for this code (do not repeat the code under any circumstances!)', 
                   prompt, 
                   '\nDocstring:')
  
  system_start_prompt = paste(
    'You are an expert R programmer. You love writing documentation.',
    'You always use roxygen docstrings. They start with #\'',
    'You use line breaks at the right spots to ensure readability.',
    'Most importantly: Do not repeat code. Ever.',
    collapse = " "
  )
  
  fib(prompt = prompt,
      system_start_prompt = system_start_prompt,
      history = history,
      stream = stream,
      model = model,
      api = api,
      max_tokens = max_tokens)
}