# in progress
# notes explore matrix representation of reactions
library(dplyr)
library(purrr)

fpath <- "inputs/day_14.txt"
fpath <- "inputs/day_14_test.txt"

ll <-  readLines(fpath)

reactions <- 
  tibble(y= map(ll, ~ strsplit(.x, " => ")[[1]])) %>% 
  mutate(chem_in = map_chr(y, ~unlist(pluck(.x)[1]))) %>% 
  mutate(chem_out = map_chr(y, ~unlist(pluck(.x)[2]))) %>% 
  separate_rows(chem_in, sep=",") %>% 
  mutate(chem_in = trimws(chem_in)) %>% 
  separate(chem_in, sep = " ", into=c("quantity_in", "chemical_in")) %>% 
  separate(chem_out, sep = " ", into=c("quantity_out", "chemical_out")) %>% 
  mutate(quantity_out = as.integer(quantity_out),
         quantity_in = as.integer(quantity_in)) %>% 
  select(-y)

# recurse over each chemical  starting with fuel
chemical <- "FUEL"
amount <- 1
reserves <- list()

ore_required <- function(chemical, amount) {
  if (chemical %in% reactions$chemical_out) {
    
    reaction <- filter(reactions, chemical_out == chemical)
    min_produceable <- reaction$quantity_out[1]
    amount_made <- ceiling(amount / min_produceable) * min_produceable
    
    if(amount %% min_produceable != 0) {
      surplus <- amount_made - amount
      reserves[[chemical]] <<- surplus
    }
    
    map2(reaction$chemical_in, 
         reaction$quantity_in, 
         ~ ore_required(.x, (amount_made / min_produceable) * .y)) 
    
    
    # if chemical is required but not producable, it must be ORE
  } else {
    stopifnot(chemical == "ORE")
    amount
  }
}

x <- ore_required("FUEL", 1)
reserves
sum(unlist(x))

# Currently have 165 and not 172 because not using reserves


#- Base R way to generate data for reference:

# reactions <-
#   do.call(rbind, lapply(readLines(fpath), function(reaction) {
#     
#     fml_sides <-
#       lapply(strsplit(reaction, " => ")[[1]], function(xhs) {
#         lapply(strsplit(xhs, ", "), function(chemqs) {
#           do.call(rbind, lapply(strsplit(chemqs, " "), function(chemq) {
#             data.frame(chemical = chemq[2], quantity = as.numeric(chemq[1]))
#           }))
#         })[[1]]
#       })
#     
#     merge(fml_sides[[2]], fml_sides[[1]],
#           by = character(0), suffixes = c("_out", "_in"))
#     
#   }))
