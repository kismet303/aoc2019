fpath <- "r_solutions/day_14input.txt"

lut <-
  do.call(rbind, lapply(readLines(fpath), function(reaction) {
    
    fml_sides <-
      lapply(strsplit(reaction, " => ")[[1]], function(xhs) {
        lapply(strsplit(xhs, ", "), function(chemqs) {
          do.call(rbind, lapply(strsplit(chemqs, " "), function(chemq) {
            data.frame(chemical = chemq[2], quantity = as.numeric(chemq[1]))
          }))
        })[[1]]
      })
    
    merge(fml_sides[[2]], fml_sides[[1]],
          by = character(0), suffixes = c("_out", "_in"))
    
  }))

mround <- function(x,base){
  base*round((ceiling(x/base)))
}


t_data <- function(chemical){
  lut %>% 
    filter(chemical_out == chemical) %>% 
    tidyr::spread(chemical_in, quantity_in, fill = 0)
  }

input <- map_dfr(unique(lut$chemical_out), t_data) %>% 
  mutate_all(tidyr::replace_na, 0) %>% 
  rename(makes = chemical_out) %>% 
  rename(makes_n = quantity_out)

start_materials <-
  input %>% 
  filter(ORE != 0) %>% 
  pull(makes)

i_materials <- 
  input %>% 
  filter(ORE == 0, makes != "FUEL") %>% 
  pull(makes)

x <- input[input$makes=="FUEL", !names(input) %in% c("makes", "makes_n")]
while(sum(x[i_materials]) != 0) {
  for (i in i_materials) {
    if(x[[i]] > 0 ) {
      print(glue::glue("i={i}"))
      min_make <- filter(input, makes==i) %>% pull(makes_n)
      replace <- input[input$makes == i, !names(input) %in% c("makes", "makes_n")] * x[[i]]/min_make
      print(replace)
      
      for(j in seq(length(replace))) {
        if (replace[j] != 0) {replace[j] <- mround(replace[j], min_make) }
      }

      print(replace)
      x <- x + replace
      x[[i]] <- 0
      print(x)
    }
  }
}

out <- list()
for(j in start_materials) {
  rule <- filter(input, makes == j)
  out[[j]] <- as.integer(rule$ORE) * ceiling(x[[j]] / as.integer(rule$makes_n))
}

sum(unlist(out))