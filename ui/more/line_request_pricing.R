ui_more_line_requests_pricing <- function() {
  tabPanel(
    "Line Request/Pricing",
    h1("Line Request / Pricing"),
    column(
      width = 8,
      h2("Request process:"),
      img(src = "line_request_user2.png", align = "left", width = "100%"),
      br(), br(), br(), br(), br(), br(), br(),
      h2("Pricing:"),
      dataTableOutput("pricing"),
      br(),
      p("Note: Prices were updated on 8/29/19. All orders placed before this date will be honored at the original price."),
      p(strong("Shipping:"), "For domestic shipments, shipping fees are calculated by shipment zone (zone 2-16, based on FedEx Standard Overnight).
            For international shipments, shipping fees are calculated using FedEx Rate Tools prior to submitting a billing estimate (based on FedEx International First rates.)"),
      p(
        strong("Vials:"), "We can offer one vial per line. If more are required, please",
        a("contact us", href = "mailto:proxe.feedback@gmail.com?Subject=PRoXe%20extra%20vial%20request"),
        "to discuss."
      ),
      p(strong("Consulting:"), "We are happy to discuss scientific details to help you order the appropriate line.  
            For simple inquires, this is free, but for projects that will take more than 5 minutes, 
            after agreement with you we will charge the consulting rate above."),
      br(),
      p(
        tags$b("Please read the following before requesting lines. "),
        "General information relevant to PDX acquisition and use is available on PRoXe.  Each line is annotated with available demographic, pathologic and genomic information, as well as details like the time from injection to engraftment.  Protocols for a variety of PDX approaches, including expansion and subrenal capsule implantation are available in the ", actionLink("Methods_link", "Methods"), " tab.  We provide consulting to assist Investigators, but this is intended to focus on the selection and use of ", tags$i("individual"), " PDXs. Note that billing is processed through the DFCI iLab Solutions platform, and requests are fulfilled by the DFCI Center for Patient Derived Models."
      ),
      p(strong("Note:"), "Inventory last updated in PRoXe", inv_upDate, "."),
      br(),
      a(
        HTML(
          "<button type='button' class='btn btn-primary' style='border-radius: 4px;
                 padding-left: 20px; padding-right: 20px; font-size: x-large;'>
                Click here to request lines
              </button>"
        ),
        href = "https://docs.google.com/forms/d/1RiQU4ABOWssH6vzy24jhdn6qhIjDcSprr6jiC1pLpQQ/viewform", target = "_blank"
      ),
      br(), br(), br()
    )
  )
}
