source("app.R", local = TRUE)

if (!is.function(server)) {
  stop("The Shiny server was not created.", call. = FALSE)
}

if (!inherits(ui, "shiny.tag.list")) {
  stop("The Shiny UI was not created.", call. = FALSE)
}

message("  PASS: the Shiny application sources without starting a session")
