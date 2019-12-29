library(dplyr)
library(purrr)

input <- read.delim("inputs/day_09.txt", sep = "",header = FALSE, stringsAsFactors = FALSE)

df <- NULL
for (i in seq(nrow(input))) { 
  print(i)
  p <- stringr::str_locate_all(input[i,], "#")[[1]][,1]
  df <- bind_rows(df, tibble(y = i-1, x = p-1))
}

no_angles <- function(df, i) {
  pos_x = as.integer(df[i, "x"])
  pos_y = as.integer(df[i, "y"])
  
  df[-i, ] %>% 
    mutate(angle = atan2(y - pos_y, x - pos_x)) %>% 
    pull(angle) %>% 
    n_distinct()
}

df$n <- map_int(seq(nrow(df)), no_angles, df=df)
df %>% filter(n == max(n))

#######
# Part 2
#######
#station is placed at 19,20
# conversion to degrees is a mess
rad2deg <- function(rad) {(rad * 180) / (pi)}

pos_x = 20
pos_y = 19

res <- 
  df %>% 
  dplyr::filter(!(x==20 & y == 19)) %>% 
  mutate(angle = atan2(y - pos_y, x - pos_x)) %>% 
  mutate(distance = sqrt((y - pos_y)**2  +  (x - pos_x)**2)) %>% 
  mutate(degrees = round(rad2deg(angle), digits=3)) %>% 
  mutate(degrees2 = degrees +90) %>% 
  mutate(degrees3 = ifelse(degrees2 <0, 270 + (90+degrees2), degrees2)) %>% 
  group_by(degrees3) %>% 
  arrange(degrees3, distance) %>% 
  mutate(group_n = row_number()) %>% 
  ungroup() %>% 
  arrange(group_n,degrees3, distance) %>% 
  slice(200)

