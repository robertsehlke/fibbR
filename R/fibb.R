


options(fib_model = "gpt-3.5-turbo")


#' Fib Code Wrapper Function
#'
#' This function is a wrapper for the `fib()` function, which takes a prompt and additional
#' arguments to generate an R script based on the given prompt. It also inserts a new line
#' at the end using the `rstudioapi::insertText()` function.
#'
#' @param prompt A character string representing the prompt for the R script.
#' @param history (Optional) A list of previous interactions to be passed to the `fib()` function.
#' @param stream (Optional) A logical value indicating whether to stream the output. Default is TRUE.
#' @param model (Optional) A character string specifying the model to be used. Default is the first element of the "fib_model" option.
#'
#' @return The result of the `fib()` function call.
#' @export
#'
#' @examples
#' fib_code("Calculate the sum of 1 to 10")
#' fib_code("Create a function to find the factorial of a number")
fib_code = function(prompt,
                    history = NULL,
                    stream = TRUE,
                    model = options("fib_model")[[1]] ) {
  
  prompt = paste0('# R script: ', prompt)
  
  system_start_prompt = 
    list('You are an expert R programmer.',
         'You write code. You dont explain because youre in a hurry.',
         'If you provide any explanations, you make a comment using the # symbol.',
         'If you encounter R script that is not valid, answer with "# ???", nothing else.') %>%
    unlist() %>% paste0(collapse = "")
  
  fib(prompt = prompt,
      system_start_prompt = system_start_prompt,
      history = history,
      stream = stream,
      model = model)
  
  rstudioapi::insertText(text = "\n")
}



#' Generate a Fibonacci Sequence Roxygen Docstring
#'
#' This function generates a roxygen docstring for a given code snippet without repeating the code.
#' It utilizes the `fib()` function to create the docstring based on the provided prompt and other parameters.
#'
#' @param prompt A character string representing the code snippet for which the docstring is to be generated.
#' @param history (Optional) A list of previous interactions to be passed to the `fib()` function.
#' @param stream (Optional) A logical value indicating whether to stream the output. Default is TRUE.
#' @param model (Optional) A character string specifying the model to be used. Default is the value set in options("fib_model")[[1]].
#'
#' @return A character string containing the generated roxygen docstring.
#' @examples
#' \dontrun{
#'   fib_roxygen("example_function = function(x) { return(x + 1) }")
#' }
#' @export

fib_roxygen = function(prompt,
                       history = NULL,
                       stream = TRUE,
                       model = options("fib_model")[[1]] ) {
  
  prompt = paste0('Please write an roxygen docstring for this code (do not repeat the code under any circumstances!)', 
                  prompt, 
                  '\nDocstring:')
  
  system_start_prompt = 
    list('You are an expert R programmer. You love writing documentation.',
         'You always use roxygen docstrings. They start with #\'',
         'You use line breaks at the right spots to ensure readability.',
         'Most importantly: Do not repeat code. Ever.') %>%
    unlist() %>% paste0(collapse = "")
  
  fib(prompt = prompt,
      system_start_prompt = system_start_prompt,
      user_prompt_prefix = user_prompt_prefix,
      history = history,
      stream = stream,
      model = model)
}




#' Fib Function
#'
#' This function sends a request to a specified model with a given prompt and optional parameters.
#' It returns an invisible TRUE value upon successful execution.
#'
#' @param prompt A character string representing the user's input prompt.
#' @param model A character string representing the model to be used.
#' @param system_start_prompt A character string for the system's starting prompt (default: "You want to fib.").
#' @param user_prompt_prefix A character string for the user's prompt prefix.
#' @param history A list of previous interactions (default: NULL).
#' @param stream A logical value indicating whether to stream the response (default: TRUE).
#'
#' @return An invisible TRUE value upon successful execution.
#' @export
#'
#' @examples
#' \dontrun{
#' fib("What is the meaning of life?", "gpt-3.5-turbo")
#' }
fib = function(prompt, 
               model,
               system_start_prompt = "You want to fib.",
               user_prompt_prefix,
               history = NULL,
               stream = TRUE) {
  body = create_request_body(model, system_start_prompt, prompt, history, stream)
  send_request(body)
  return(invisible(TRUE))
}





#### Internal functions ####



create_request_body = function(model, system_start_prompt, prompt, history, stream) {
  token = fib_get_token()
  
  if(is.null(history)){
    input_messages = list(
      list(
        role = "system",
        content = system_start_prompt
      ),
      list(
        role = "user",
        content = prompt
      )
    )
  } else {
    input_messages = c(history,
                       list(
                         role = "user",
                         content = prompt
                       ))
  }
  
  body = list(
    model = model,
    messages = input_messages,
    stream = stream,
    temperature = 0, 
    logit_bias = list("15506" = -100, "63" = -100)
  )
  
  return(body)
}



send_request = function(body) {
  url = "https://api.openai.com/v1/chat/completions"
  headers = c(
    "Content-Type" = "application/json",
    "Authorization" = paste0( "Bearer ", fib_get_token() )
  )
  
  json_body = jsonlite::toJSON(body, auto_unbox = TRUE)
  
  POST_stream(url = url,
              json_body = json_body,
              headers = headers,
              callback_function = process_stream )
}




POST_stream = function(url,
                       json_body,
                       headers,
                       callback_function) {
  
  curl_handle <- curl::new_handle()
  curl::handle_setheaders(curl_handle, .list = headers)
  curl::handle_setopt(curl_handle, post = TRUE, postfields = json_body)
  curl::curl_fetch_stream(url, handle = curl_handle, fun = callback_function)
  
  return(invisible(TRUE))
}



process_current_chunk = function(data) {
  subchunks = 
    rawToChar(data) %>% 
    strsplit(., split = '\n\n') %>% 
    unlist %>% 
    sapply(., function(x) { gsub('data: ', replacement = '', x = x) } ) %>%
    unname()
  
  subchunks = subchunks[!subchunks=='[DONE]']
  
  subchunks_json = subchunks %>%
    lapply(jsonlite::fromJSON)
  
  subchunks_txt_collapsed = 
    subchunks_json %>%
    lapply(., function(chunky) { chunky$choices$delta$content }) %>%
    unlist() %>%
    paste0(collapse = "")
  
  return( list("txt"=subchunks_txt_collapsed,
               "jsons" = subchunks_json) )
}


process_stream = function(data) {
  this = process_current_chunk(data)
  if( grepl('^>', this$txt) ) {
    #stop("I don't have a clue.")
  }
  rstudioapi::insertText(this$txt)
}




#### Credentials ####

fib_get_token = function() {
  if(!"fibbr_openai" %in% keyring::key_list()$service) {
    keyring::key_set("fibbr_openai", username = Sys.info()["user"], prompt = "Open AI API token: ")
  } 
  
  token = keyring::key_get("fibbr_openai", username = Sys.info()["user"])
  return(token)
}

set_token = function() {
  keyring::key_set("fibbr_openai", username = Sys.info()["user"], prompt = "Open AI API token: ")
}




