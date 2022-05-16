#devtools::install_github("EllaKaye/BradleyTerryScalable")
#devtools::install_github("saschagobel/legislatoR")

# install packages from CRAN
p_needed <- c("WikidataR",
              "WikipediR",
              "MASS",
              "plyr",
              "dplyr",
              "purrr",
              "tidyr",
              "ISLR",
              "janitor",
              "rvest",
              "stringr",
              "networkD3",
              "igraph",
              "magrittr",
              "stargazer",
              "xtable",
              "doMC",
              "psych",
              "GPArotation",
              "corrplot",
              "reshape2",
              "ggplot2",
              "readr",
              "formattable",
              "htmltools",
              "webshot",
              "ggrepel",
              "haven",
              "fuzzyjoin",
              "pageviews",
              "gtools",
              "jsonlite",
              "lubridate",
              "rjags",
              "coda",
              "runjags",
              "R2jags",
              "ggridges",
              "Matrix.utils",
              "BradleyTerryScalable",
              "scales",
              "ggthemes",
              "broom",
              "legislatoR",
              "coefplot2",
              "rtimes",
              "pscl"
)

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]

if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}

lapply(p_needed, require, character.only = TRUE)



