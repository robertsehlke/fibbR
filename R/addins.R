#' @export
fib_code_addin <- function() {
  # Ensure rstudioapi is installed
  
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
  
  start_at = selection$range$end + c(0,Inf)
  rstudioapi::setCursorPosition(start_at)
  rstudioapi::insertText(start_at, "\n")
  fib_code(selected_text)
  
  return(invisible(NULL))
}



#' @export
fib_roxygen_addin <- function() {
  # Ensure rstudioapi is installed
  
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