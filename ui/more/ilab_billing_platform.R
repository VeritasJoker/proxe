ui_more_ilab_billing_platform <- function() {
  tabPanel(
    "iLab Billing Platform",
    h1(a("iLab Solutions",
      href = "https://dfci.ilab.agilent.com/service_center/show_external/7633?name=center-for-patient-derived-models",
      target = "_blank"
    ), "Billing Platform"),
    h2("Manual"),
    uiOutput("iLab_manual")
  )
}
