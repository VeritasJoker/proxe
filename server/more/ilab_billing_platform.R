server_more_ilab_billing_platform <- function(input, output, session) {
  output$iLab_manual <- renderUI({
    filename <- "iLab_Customer_Manual_for_CPDM.pdf"
    tags$iframe(
      src = filename,
      width = "80%",
      height = "800px"
    )
  })
}
