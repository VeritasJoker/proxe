server_more_line_request_pricing <- function(input, output, server) {
  output$pricing <- DT::renderDataTable({
     pricing <- data.frame(
      "Service Name" = c(
        "Per vial, liquid tumor",
        "Per vial, solid tumor",
        "Handling rate (per shipment)",
        "Consulting (hourly)","Shipping (domestic)","
        Shipping (international)"
        ),
      "DFCI Rate" = c("$700","$778","$94","$125","varies","n/a"),
      "Academic Rate" = c("$900","$1000","$126","$169","by shipment zone","by shipment address"),
      "Corporate Rate" = "<a href=\"mailto:proxe.feedback@gmail.com?Subject=PRoXe%20corporate%20rates\" target=\"_top\">contact us</a>",
    row.names=NULL
    )
    # change colnames to remove automatic periods instead of spaces.
    colnames(pricing) <- c("Service Name", "DFCI Rate", "Academic Rate", "Corporate Rate")
    pricing
  },
  escape = FALSE,
  rownames = FALSE,
  selection = "none",
  options = list(
    dom = "t",
    ordering = FALSE
  )
  )
}
