library(r4ss)

# read the model output and print diagnostic messages
replist <- SS_output(
  dir = mydir,
  verbose = TRUE,
  printstats = TRUE
)

# plots the results
path <- "~/Desktop/Local/hake-CEATTLE/data/assessment/hake-2022-model-files"

r4ss::run(dir = path, exe = "ss_osx")

replist <- SS_output(dir = path,
                     verbose = TRUE,
                     printstats = TRUE)
SS_plots(replist)
