# <x=-7, y=17, z=-11>
# <x=9, y=12, z=5>
# <x=-9, y=0, z=-4>
# <x=4, y=6, z=0>
#   
# <x=-8, y=-10, z=0>
# <x=5, y=5, z=10>
# <x=2, y=-7, z=3>
# <x=9, y=-8, z=-3>


# <x=-1, y=0, z=2>
# <x=2, y=-10, z=-7>
# <x=4, y=-8, z=8>
# <x=3, y=5, z=-1>

library(purrr)
p0 <- list(
  A = list(pos = c(x=-1, y=0, z=2), 
           vel = c(x=0, y=0, z=0)),
  
  B = list(pos = c(x=2, y=-10, z=-7), 
           vel = c(x=0, y=0, z=0)),
  
  C = list(pos = c(x=4, y=-8, z=8), 
           vel = c(x=0, y=0, z=0)),
  
  D = list(pos = c(x=3, y=5, z=-1), 
           vel = c(x=0, y=0, z=0))
)


p0 <- list(
  A = list(pos = c(x=-8, y=-10, z=0), 
           vel = c(x=0, y=0, z=0)),
  
  B = list(pos = c(x=5, y=5, z=10), 
           vel = c(x=0, y=0, z=0)),
  
  C = list(pos = c(x=2, y=-7, z=3), 
           vel = c(x=0, y=0, z=0)),
  
  D = list(pos = c(x=9, y=-8, z=-3), 
           vel = c(x=0, y=0, z=0))
)


p0 <- list(
  A = list(pos = c(x=-7, y=17, z=-11), 
           vel = c(x=0, y=0, z=0)),
  
  B = list(pos = c(x=9, y=12, z=5), 
           vel = c(x=0, y=0, z=0)),
  
  C = list(pos = c(x=-9, y=0, z=-4), 
           vel = c(x=0, y=0, z=0)),
  
  D = list(pos = c(x=4, y=6, z=0), 
           vel = c(x=0, y=0, z=0))
)

get_g <- function(p0) {
  gM <-  matrix(c(sign(p0$A$pos[1] - p0$B$pos[1]) + 
                    sign(p0$A$pos[1] - p0$C$pos[1]) +
                    sign(p0$A$pos[1] - p0$D$pos[1]),
                  
                  sign(p0$A$pos[2] - p0$B$pos[2]) + 
                    sign(p0$A$pos[2] - p0$C$pos[2]) +
                    sign(p0$A$pos[2] - p0$D$pos[2]),
                  
                  sign(p0$A$pos[3] - p0$B$pos[3]) + 
                    sign(p0$A$pos[3] - p0$C$pos[3]) +
                    sign(p0$A$pos[3] - p0$D$pos[3]),
                  #B
                  
                  sign(p0$B$pos[1] - p0$A$pos[1]) + 
                    sign(p0$B$pos[1] - p0$C$pos[1]) +
                    sign(p0$B$pos[1] - p0$D$pos[1]),
                  
                  sign(p0$B$pos[2] - p0$A$pos[2]) + 
                    sign(p0$B$pos[2] - p0$C$pos[2]) +
                    sign(p0$B$pos[2] - p0$D$pos[2]),
                  
                  sign(p0$B$pos[3] - p0$A$pos[3]) + 
                    sign(p0$B$pos[3] - p0$C$pos[3]) +
                    sign(p0$B$pos[3] - p0$D$pos[3]),
                  #C
                  sign(p0$C$pos[1] - p0$A$pos[1]) + 
                    sign(p0$C$pos[1] - p0$B$pos[1]) +
                    sign(p0$C$pos[1] - p0$D$pos[1]),
                  
                  sign(p0$C$pos[2] - p0$A$pos[2]) + 
                    sign(p0$C$pos[2] - p0$B$pos[2]) +
                    sign(p0$C$pos[2] - p0$D$pos[2]),
                  
                  sign(p0$C$pos[3] - p0$A$pos[3]) + 
                    sign(p0$C$pos[3] - p0$B$pos[3]) +
                    sign(p0$C$pos[3] - p0$D$pos[3]),
                  #D
                  sign(p0$D$pos[1] - p0$A$pos[1]) + 
                    sign(p0$D$pos[1] - p0$B$pos[1]) +
                    sign(p0$D$pos[1] - p0$C$pos[1]),
                  
                  sign(p0$D$pos[2] - p0$A$pos[2]) + 
                    sign(p0$D$pos[2] - p0$B$pos[2]) +
                    sign(p0$D$pos[2] - p0$C$pos[2]),
                  
                  sign(p0$D$pos[3] - p0$A$pos[3]) + 
                    sign(p0$D$pos[3] - p0$B$pos[3]) +
                    sign(p0$D$pos[3] - p0$C$pos[3])
                  
                  
  ), ncol=3, byrow = TRUE)
  -1*gM
}

correct_g <- function(gM, p0) {
  z <- purrr::map(1:4, ~ purrr::pluck(p0,.x,2))
  oM <- matrix(unname(unlist(z)), byrow=TRUE, ncol=3 )
  M <- oM + gM
}

update <- function(p0, get_g) {
  for (i in 1:3){
    p0$A$vel[i] = get_g[1,i]
    p0$A$pos[i] = p0$A$pos[i] + get_g[1,i]
    
    p0$B$vel[i] = get_g[2,i]
    p0$B$pos[i] = p0$B$pos[i] + get_g[2,i]
    
    p0$C$vel[i] = get_g[3,i]
    p0$C$pos[i] = p0$C$pos[i] + get_g[3,i]
    
    p0$D$vel[i] = get_g[4,i]
    p0$D$pos[i] = p0$D$pos[i] + get_g[4,i]
  }
  p0
}

for (i in seq_along(1:1000)) {
  gM <- get_g(p0)  
  gM <- correct_g(gM, p0)
  p0 <- update(p0, gM)
  
  if(i %% 1000 ==0){
  print(glue::glue("-----Iteration: {i}-------"))
  print_info(p0)
  }
  
}



print_info <- function(p0) {
  print(glue::glue("<x={p0[[1]][[1]][[1]]}, y={p0[[1]][[1]][[2]]}, z={p0[[1]][[1]][[3]]}>, vel=<x={p0[[1]][[2]][[1]]}, y={p0[[1]][[2]][[2]]}, z={p0[[1]][[2]][[3]]}>\n
           <x={p0[[2]][[1]][[1]]}, y={p0[[2]][[1]][[2]]}, z={p0[[2]][[1]][[3]]}>, vel=<x={p0[[2]][[2]][[1]]}, y={p0[[2]][[2]][[2]]}, z={p0[[2]][[2]][[3]]}>\n
           <x={p0[[3]][[1]][[1]]}, y={p0[[3]][[1]][[2]]}, z={p0[[3]][[1]][[3]]}>, vel=<x={p0[[3]][[2]][[1]]}, y={p0[[3]][[2]][[2]]}, z={p0[[3]][[2]][[3]]}>\n
           <x={p0[[4]][[1]][[1]]}, y={p0[[4]][[1]][[2]]}, z={p0[[4]][[1]][[3]]}>, vel=<x={p0[[4]][[2]][[1]]}, y={p0[[4]][[2]][[2]]}, z={p0[[4]][[2]][[3]]}>"))
}

map_dbl(1:4, ~ sum(abs(p0[[.x]][[1]])) * sum(abs(p0[[.x]][[2]]))) %>% 
  sum()

#------------------#
# Part 2: Not started
#-------------------#




