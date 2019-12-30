source("r_solutions/intcode.R")
library(purrr)
library(dplyr)
library(ggplot2)


program <- readLines("r_solutions/day_11input.txt")
program <- as.numeric(unlist(strsplit(program, ",")))

update_pos <- function(pos, instr) {
  if(instr[1] == 0) pos[[4]] <- "."
  if(instr[1] == 1) pos[[4]] <- "#"
  painted <<- c(painted, list(pos))
  
  if(instr[2] == 1) {
    if    (pos$dir == "U") pos[1:3] <-  list(x = pos[[1]]+1, y = pos[[2]],   dir = "R")
    else if(pos$dir == "R") pos[1:3] <- list(x = pos[[1]],   y = pos[[2]]-1, dir = "D")
    else if(pos$dir == "D") pos[1:3] <- list(x = pos[[1]]-1, y = pos[[2]],   dir = "L")
    else if(pos$dir == "L") pos[1:3] <- list(x = pos[[1]],   y = pos[[2]]+1, dir = "U")
  }
  if(instr[2] == 0) {
    if(pos$dir == "U") pos[1:3] <-      list(x = pos[[1]]-1, y = pos[[2]],   dir = "L")
    else if(pos$dir == "R") pos[1:3] <- list(x = pos[[1]],   y = pos[[2]]+1, dir = "U")
    else if(pos$dir == "D") pos[1:3] <- list(x = pos[[1]]+1, y = pos[[2]],   dir = "R")
    else if(pos$dir == "L") pos[1:3] <- list(x = pos[[1]],   y = pos[[2]]-1, dir = "D")
  }
  return(pos)
}


#-------------
# Run part 1
#--------------
#initialize
pos <- list(x=0, y=0, dir="U", col=NA)
painted <- NULL
pgm_res <- intcode(input = program, m_inputs = 0)

for(i in 1:100000) {
  if (i %% 100 ==0) print(paste("XXXX",i, "XXXX"))
  pos <- update_pos(pos, pgm_res$output)
  input <- get_colour(pos)
  pgm_res <- intcode(input = pgm_res$pgm,
                     pointer = pgm_res$pointer,
                     relative_base = pgm_res$relative_base,
                     m_inputs = input)
}


df <- tibble(x = map_dbl(painted, pluck("x")),
             y = map_dbl(painted, pluck("y")),
             dir = map_chr(painted, pluck("dir")),
             col = map_chr(painted, pluck("col"))
)

distinct(df, x, y) %>% nrow()


#---------------
# Run part 2
#--------------

pos <- list(x=0, y=0, dir="U", col=NA)
painted <- NULL
pgm_res <- intcode(input = program, m_inputs = 1)

for(i in 1:100000) {
  if (i %% 100 ==0) print(paste("XXXX",i, "XXXX"))
  pos <- update_pos(pos, pgm_res$output)
  input <- get_colour(pos)
  pgm_res <- intcode(input = pgm_res$pgm,
                     pointer = pgm_res$pointer,
                     relative_base = pgm_res$relative_base,
                     m_inputs = input)
}


df <- tibble(x = map_dbl(painted, pluck("x")),
             y = map_dbl(painted, pluck("y")),
             dir = map_chr(painted, pluck("dir")),
             col = map_chr(painted, pluck("col"))
)

ggplot(df, aes(x,y, colour=col)) +
  geom_point() +
  theme_minimal() +
  scale_colour_manual(values= c("white", "black"))



#ABEKZGFG

