#' FibbR Code Addin
#'
#' @export
fib_code_addin = function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("This addin requires the 'rstudioapi' package.")
  }
  
  selection = rstudioapi::getActiveDocumentContext()$selection[[1]]
  selected_text = selection$text
  
  if (selected_text == "") {
    message("Please select some text in the editor.")
    return(NULL)
  }
  
  start_at = selection$range$end + c(0, Inf)
  rstudioapi::setCursorPosition(start_at)
  rstudioapi::insertText(start_at, "\n")
  fib_code(selected_text, api = get_default_api(), stream = TRUE)
  
  return(invisible(NULL))
}

#' FibbR Roxygen Addin
#'
#' @export
fib_roxygen_addin = function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("This addin requires the 'rstudioapi' package.")
  }
  
  selection = rstudioapi::getActiveDocumentContext()$selection[[1]]
  selected_text = selection$text
  
  if (selected_text == "") {
    message("Please select some text in the editor.")
    return(NULL)
  }
  
  start_at = selection$range$start
  rstudioapi::setCursorPosition(start_at)
  rstudioapi::insertText(start_at, "\n")
  rstudioapi::setCursorPosition(start_at)
  fib_roxygen(selected_text, api = get_default_api(), stream = TRUE)
  
  return(invisible(NULL))
}




#' FibbR Code with Context Addin
#'
#' @export
fib_code_with_context_addin = function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("This addin requires the 'rstudioapi' package.")
  }
  
  context = rstudioapi::getActiveDocumentContext()
  file_content = paste(context$contents, collapse = "\n")
  
  selection = rstudioapi::getActiveDocumentContext()$selection[[1]]
  selected_text = selection$text
  
  start_at = selection$range$end + c(0, Inf)
  rstudioapi::setCursorPosition(start_at)
  rstudioapi::insertText(start_at, "\n")
  
  instruction_prompt = paste0("<full file context>\n", file_content, "</full file context>\n",
                              "Instructions:\n",
                              selected_text)
  
  fib_code(instruction_prompt, api = get_default_api(), stream = TRUE)
  
  return(invisible(NULL))
}
