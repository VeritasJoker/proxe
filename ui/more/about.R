ui_more_about <- function() {
  tabPanel(
    "About",
    h1("About PRoXe"),
    column(
      width = 8,
      includeMarkdown("ui/more/about.md")
    )
  )
}
