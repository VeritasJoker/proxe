## this script is for pre-compiling data and running/uploading app

# set order of read-in scripts
read_in_scripts <- c(
  "clean_liquid.R", "clean_virusseq.R", "clean_gene_fusion_predictions.R",
  "seq.R", "rna_seq.R", "oncoprint2.R",
  "clean_solid.R", "jax.R", "finalize.R"
)

# set working directory based on user -- hardcoded for Mark and Scott
host <- system("hostname", intern = TRUE)
who <- system("whoami", intern = TRUE)
essential_dir <- c("data", "global.R", "server.R", "ui.R", "www", read_in_scripts)
data_outside_app_dir <- file.path("~", "Dropbox", "PRoXe", "data_outside_app") # based on default Dropbox location
if (basename(getwd()) == "PRoXe_app" & all(essential_dir %in% dir())) {
  print("Seem to be in PRoXe_app directory. Continuing...")
  app_dir <- getwd()
} else if (who == "scott") { # not a typo
  app_dir <- file.path("~", "git", "PRoXe_app")
  print(paste("User is probably Scott K. Setting working directory to", app_dir))
  setwd(app_dir)
  # data_outside_app_dir = (default)
} else if (grepl("ArmaVirumque", host)) {
  app_dir <- file.path("C:", "Users", "Mark", "git", "PRoXe", "PRoXe_app")
  # TODO: clone the git repo here and add in gitignore files.
  print(paste("User is Mark. Setting working directory to", app_dir))
  setwd(app_dir)
  data_outside_app_dir <- file.path("C:", "Users", "Mark", "Dropbox (Partners HealthCare)", "PRoXe", "data_outside_app")
} else if (grepl("proxe.dfci.harvard.edu", host)) {
  app_dir <- file.path("~", "git", "PRoXe_app")
  # TODO: clone the git repo here and add in gitignore files.
  print(paste("On proxe.dfci.harvard.edu VM, Setting dir to", app_dir))
  setwd(app_dir)
  data_outside_app_dir <- file.path("~", "Dropbox", "PRoXe", "data_outside_app")
} else if (who == "sharla") {
  app_dir <- file.path("~", "tcb", "proxe")
  print(paste("User is Sharla Gelfand. Setting working directory to", app_dir))
  setwd(app_dir)
  data_outside_app_dir <- file.path("~", "Dropbox (Partners HealthCare)", "PRoXe", "data_outside_app")
} else {
  stop("User not recognized; manually navigate to PRoXe_app directory via setwd()")
}

# read_in_scripts <- read_in_scripts[1:5]

# run data read-in scripts
for (script in read_in_scripts) {
  cat(paste("--- Running:", script, "---\n"))
  source(script)
}

# check on largest-memory items
z <- sapply(ls(), function(x)
  object.size(get(x)))
as.matrix(rev(sort(z))[1:10])

save.image(file = "pre-compiled.RData", compress = FALSE)

# print size of PRoXe_app folder
cat("Size of app:\n")
system("du -h -d 0 ../PRoXe_app")

# -- comment or uncomment sections below as desired -- #

## 1 - deploy
if (F) {
  library(rsconnect)
  # alpha app
  rsconnect::deployApp(appDir = getwd(), appName = "PRoXe_alpha", account = "proxe")
  # main app
  rsconnect::deployApp(appDir = getwd(), appName = "PRoXe", account = "proxe")
}

# capture.output(rsconnect::showLogs(appName="PRoXe_alpha",account="proxe",entries=5000),file="~/logs5000.txt")

# interesting way to visualize dependencies: enable, then press cmd+fn+F3 while app is running.
# options(shiny.reactlog = TRUE)

## 2 - run normally
rm(list = ls()) # this cleans environment to mimic shinyapps.io
runApp(launch.browser = TRUE)
#
## 3 - run for debugging
# runApp(display.mode = "showcase") # for debugging
