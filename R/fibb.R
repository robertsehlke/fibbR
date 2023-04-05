
#' Fib Code Wrapper Function
#'
#' This function is a wrapper for the `fib()` function, which takes a prompt and additional
#' arguments to generate an R script based on the given prompt. It also inserts a new line
#' at the end using the `rstudioapi::insertText()` function.
#'
#' @param prompt A character string representing the prompt for the R script.
#' @param history (Optional) A list of previous interactions to be passed to the `fib()` function.
#' @param stream (Optional) A logical value indicating whether to stream the output. Default is TRUE.
#' @param model (Optional) A character string specifying the model to be used. Define via options, e.g. options(fib_model = "gpt-3.5-turbo").
#'
#' @return The result of the `fib()` function call.
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' fib_code("Calculate the sum of 1 to 10")
#' fib_code("Create a function to find the factorial of a number")
fib_code = function(prompt,
                    history = NULL,
                    stream = TRUE,
                    model = Sys.getenv("FIBBR_MODEL") ) {
  
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
      model = model,
      logit_bias = list("15506" = -100, "63" = -100))
  
  rstudioapi::insertText(text = "\n")
}



#' Generate an Roxygen Docstring
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
                       model = Sys.getenv("FIBBR_MODEL") ) {
  
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
               model = Sys.getenv("FIBBR_MODEL"),
               system_start_prompt = "You are helpful, but consise.",
               history = NULL,
               stream = FALSE,
               logit_bias = NULL,
               temperature = 0) {
  
  if(model == "") {
    set_fibr_default_model()
    model = Sys.getenv("FIBBR_MODEL")
  }
  
  body = create_request_body(model, system_start_prompt, prompt, history, stream, logit_bias, temperature)
  response = send_request(body)
  return(response)
}





#### Internal functions ####



create_request_body = function(model, 
                               system_start_prompt, 
                               prompt, 
                               history, 
                               stream,
                               logit_bias,
                               temperature) {
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
                       list(list(
                         role = "user",
                         content = prompt
                       ))
                       )
  }
  
  body = list(
    model = model,
    messages = input_messages,
    stream = stream,
    temperature = temperature, 
    logit_bias = logit_bias
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
  
  if(body$stream) {
    POST_stream(url = url,
                json_body = json_body,
                headers = headers,
                callback_function = process_stream_rstudio )
    return(invisible(NULL))
  } else {
    response = httr::POST(url, body = body, encode = "json", httr::add_headers(.headers = headers))
    parsed = jsonlite::fromJSON( httr::content(response, "text", encoding = "UTF-8"))
    
    updated_history =
      c(body$messages,
        list(list("role" = parsed$choices$message$role,
                  "content" = parsed$choices$message$content)))
    
    message(parsed$choices$message$content)
    
    return( updated_history )
  }
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



#' Process Stream RStudio
#'
#' This function processes the input data stream and inserts the resulting text into the RStudio console.
#'
#' @importFrom magrittr %>%
process_stream_rstudio = function(data) {
  
  subchunks = 
    rawToChar(data) %>% 
    strsplit(., split = '\n\n') %>% 
    unlist %>% 
    sapply(., function(x) { gsub('data: ', replacement = '', x = x) } ) %>%
    unname()
  
  subchunks = subchunks[!subchunks=='[DONE]']
  
  subchunks_json = subchunks %>%
    lapply(jsonlite::fromJSON)
  
  if(!is.null(subchunks_json[[1]]$error)) {
    stop(subchunks_json[[1]]$error$message)
  }
  
  subchunks_txt_collapsed = 
    subchunks_json %>%
    lapply(., function(chunky) { chunky$choices$delta$content }) %>%
    unlist() %>%
    paste0(collapse = "")
  
  rstudioapi::insertText(subchunks_txt_collapsed)
}


set_fibr_default_model = function() {
  default = "gpt-4"
  if( !Sys.getenv("FIBBR_MODEL") %in% c("gpt-3.5-turbo", "gpt-4") ) {
    Sys.setenv(FIBBR_MODEL = default)
    message( paste0('Using "', default, '" model. To change, please execute: Sys.setenv(FIBBR_MODEL = "gpt-3.5-turbo") ') )
  }
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





