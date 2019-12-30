source("r_solutions/intcode.R")
library(purrr)
library(dplyr)
library(ggplot2)


program <- readLines("r_solutions/day13.txt")
program <- as.numeric(unlist(strsplit(program, ",")))

pgm_res <- intcode(input = program, m_inputs = NULL)
m <- matrix(pgm_res$output, ncol=3,  byrow=TRUE)
table(m[,3])

#--------------
# Part 2
#--------------