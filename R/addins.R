#' @export
fib_code_addin <- function() {
  
  set_fibr_default_model()
  
  selection = rstudioapi::getActiveDocumentContext()$selection[[1]]
  selected_text = selection$text
  
  # Check if any text is selected
  if (selected_text == "") {
    message("Please select some text in the editor.")
    return(NULL)
  }
  
  start_at = selection$range$end + c(0,Inf)
  rstudioapi::setCursorPosition(start_at)
  rstudioapi::insertText(start_at, "\n")
  fib_code(selected_text)
  
  return(invisible(NULL))
}



#' @export
fib_roxygen_addin <- function() {
  
  set_fibr_default_model()
  
  if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    stop("This addin requires the 'rstudioapi' package.")
  }
  
  
  selection = rstudioapi::getActiveDocumentContext()$selection[[1]]
  selected_text = selection$text
  
  # Check if any text is selected
  if (selected_text == "") {
    message("Please select some text in the editor.")
    return(NULL)
  }
  
  start_at = selection$range$start
  rstudioapi::setCursorPosition(start_at)
  rstudioapi::insertText(start_at, "\n")
  rstudioapi::setCursorPosition(start_at)
  fib_roxygen(selected_text)
  
  return(invisible(NULL))
}


set_fibr_default_model = function() {
  default = "gpt-4"
  if( !Sys.getenv("FIBBR_MODEL") %in% c("gpt-3.5-turbo", "gpt-4") ) {
    Sys.setenv(FIBBR_MODEL = default)
    message( paste0('Using "', default, '" model. To change, please execute: Sys.setenv(FIBBR_MODEL = "gpt-3.5-turbo") ') )
  }
}