GUI <- function() {
  appDir <- system.file("shiny", "gpApp", package = "gazepath")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `gazepath`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, launch.browser = TRUE)
}