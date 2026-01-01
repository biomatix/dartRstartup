# Markdown editting


install.packages(c("devtools", "pkgload", "remotes"))  # if needed
devtools::load_all(".")

learnr::run_tutorial("getting-started", package = "dartRstartup")

rmarkdown::run("inst/tutorials/getting-started/getting-started.Rmd")


file.edit("inst/tutorials/getting-started/getting-started.Rmd")
