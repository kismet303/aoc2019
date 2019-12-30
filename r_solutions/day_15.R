source("r_solutions/intcode.R")
library(purrr)
library(dplyr)
library(ggplot2)

program <- readLines("r_solutions/day15input.txt")
program <- as.numeric(unlist(strsplit(program, ",")))

# initialize
pos = c(0,0)
plan <- tibble(x=0, y=0, tile = "-", pos=paste0(x, "-", y), min_route= 0 )
pointer = 1
pgm = program
last_move <- 0
no_move_counter <- 0
min_route <- 0

try_move <- function(direction){
  
  for(j in seq(100000)){
    if (j %% 100 == 0) print(j)
  
    if(last_move == 0) direction <- sample(c(1,2,3,4), size=1)
    if(last_move == 1) direction <- sample(c(1,3,4), size=1)
    if(last_move == 2) direction <- sample(c(2,3,4), size=1)
    if(last_move == 3) direction <- sample(c(1,2,3), size=1)
    if(last_move == 4) direction <- sample(c(1,2,4), size=1)
    if(no_move_counter ==  50){
      if(last_move == 1) direction <- 2
      if(last_move == 2) direction <- 1
      if(last_move == 3) direction <- 4
      if(last_move == 4) direction <- 3
    }
    
    pgm_res <- intcode(input = pgm, 
                       pointer = pointer,
                       m_inputs = direction)
    
    
    if(direction == 1)  try_pos <- c(pos[1],   pos[2]+1)
    if(direction == 2)  try_pos <- c(pos[1],   pos[2]-1)
    if(direction == 3)  try_pos <- c(pos[1]-1, pos[2])
    if(direction == 4)  try_pos <- c(pos[1]+1, pos[2])
    
    
    if(pgm_res$output == 0) {
      plan <<- plan %>% 
        bind_rows(tibble(x=try_pos[1], y=try_pos[2], tile = "X", pos=paste0(pos[1], "-", pos[2])))
      no_move_counter <<- no_move_counter + 1
    } 
    
    if(pgm_res$output == 1) {
      pos <<- try_pos
      
      times_on_square <- filter(plan, x==try_pos[1], y==try_pos[2]) %>% nrow()
      if (times_on_square == 0) {
        min_route <<- min_route + 1
      } else {
        if (times_on_square %% 2 == 1) min_route <<- min_route - 1
        if (times_on_square %% 2 == 0) min_route <<- min_route + 1
      }
      
      plan <<- plan %>% 
        bind_rows(tibble(x=pos[1], y=pos[2], tile = "-", pos=paste0(pos[1], "-", pos[2]), min_route=min_route))
      pgm <- pgm_res$pgm
      pointer <- pgm_res$pointer
      last_move <<- direction
      no_move_counter <<- 0
    } 
    
    if(pgm_res$output == 2) {
      print(paste("holy crap!"))
      print(paste("Oxygen system is at:", try_pos[1],"-", try_pos[2]))
      print(paste("Min route is:", min_route + 1))
      break
    } 
  }
}

try_move("X")
# plot_df <- plan %>% distinct()
# 
# ggplot(plot_df, aes(x=x,y=y, color=tile)) +
#   geom_point() +
#   theme_minimal()+
#   scale_colour_manual(values = c("green", "black"))

#----------------------#
# Part 2
#----------------------#

#Not started


