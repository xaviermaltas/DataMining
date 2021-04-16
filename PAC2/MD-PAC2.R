# https://cran.r-project.org/web/packages/ggplot2/index.html
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
# https://cran.r-project.org/web/packages/dplyr/index.html
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')


clientes <- read.csv("clientes.csv", stringsAsFactors = FALSE)
flores <- read.csv('flores.csv', stringsAsFactors = FALSE)
lastfm <- read.csv('lastfm.csv', stringsAsFactors = FALSE)

