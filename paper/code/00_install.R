# Install the package version used by the experiments into paper/.lib/current.
# Run from the repository root: Rscript paper/code/00_install.R
dir.create("paper/.lib/current", recursive = TRUE, showWarnings = FALSE)
install.packages(".", lib = "paper/.lib/current", repos = NULL, type = "source")
