path <- function(x,  start = c(0, 0), steps = 0) {
  
  direction <- stringr::str_sub(x, 1,1)
  length <- as.numeric(stringr::str_sub(x, 2))
  
  if(direction == "U") {
    df <- tibble(x = seq(start[1], start[1] + length),
                 y = start[2],
                 steps = seq(steps, steps+length))
    
  } else if(direction == "D") {
    df <- tibble(x = seq(start[1], start[1] - length),
                 y = start[2],
                 steps = seq(steps, steps+length))
    
  } else if(direction == "L") {
    df <- tibble(x = start[1],
                 y = seq(start[2], start[2] - length),
                 steps = seq(steps, steps+length))
    
    
  } else if(direction == "R") {
    df <- tibble(x = start[1],
                 y = seq(start[2], start[2] + length),
                 steps = seq(steps, steps+length)
    )
  }
  df
}


# path("R5")
# path("D10")

route1 <- c("R2","D2","U2")

route1 <- c("R75","D30","R83","U83","L12","D49","R71","U7","L72")
route2 <- c("U62","R66","U55","R34", "D71","R55","D58", "R83")


translate_route <- function(route) {
  for (i in seq_along(route)) {
    print(i)
    if (i == 1) df <- path(route[i])
    else {
      df_append <- path(route[i], 
                        c(tail(df, 1) %>% pull(x),
                          tail(df, 1) %>% pull(y)),
                        steps = tail(df,1) %>% pull(steps)
      )
      
      df <- bind_rows(df, df_append)
      
    }
  }
  df
}

r1 <- translate_route(route1)
r2 <- translate_route(route2)

dplyr::inner_join(r1, r2) %>% 
  slice(2:nrow(.)) %>% 
  dplyr::mutate(d = abs(x) + abs(y)) %>% 
  pull(d) %>% min()

#### actual route
r1 <- translate_route(r1)
r2 <- translate_route(r2)

dplyr::inner_join(r1, r2, by=c("x", "y")) %>% 
  slice(2:nrow(.)) %>% 
  dplyr::mutate(d = abs(x) + abs(y)) %>% 
  pull(d) %>% min()

#=======================
# part 2
#=======================
dplyr::inner_join(r1, r2, by=c("x", "y")) %>% 
  slice(2:nrow(.)) %>% 
  dplyr::mutate(d = steps.x + steps.y) %>% 
  pull(d) %>% min()


