required_packages <- c(
  "shiny",
  "ggplot2",
  "readxl",
  "dplyr",
  "tidyr",
  "plotly",
  "RColorBrewer"
)

# Install missing packages only
installed <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed)) {
    install.packages(pkg)
  }
}

lapply(required_packages, library, character.only = TRUE)