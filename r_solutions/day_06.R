orbits <- c(
  "X)B",
  "B)C",
  "C)D",
  "D)E",
  "E)F",
  "B)G",
  "G)H",
  "D)I",
  "E)J",
  "J)K",
  "K)L")

source("day6-input.R")

list_orbit <- function(orbit) {
  inner = substr(orbit,1,3)
  outer = substr(orbit,5,7)
  out <- c(inner, outer)
}

all_orbits <- purrr::map(orbits, list_orbit)
all_planets <- unique(unlist(all_orbits))

n_o <- 0
count_orbits <- function (planet) {
  for(x in (all_orbits)) {
    if(x[2] == planet) {
      n_o <<- n_o + 1
      count_orbits(x[1])
    }
  }
  return(n_o)
}

purrr::map_dbl(all_planets, count_orbits)

###--------------
# Part 2
##---------------

chain <- NULL
count_orbits <- function (planet) {
  for(x in (all_orbits)) {
    if(x[2] == planet) {
      n_o <<- n_o + 1
      count_orbits(x[1])
      chain <<- c(chain, x[1])
    }
  }
  return(chain)
}

chain <- NULL
you_chain <- count_orbits("YOU")
length(you_chain)
length(unique(you_chain))

chain <- NULL
san_chain <- count_orbits("SAN")
length(san_chain)
length(unique(san_chain))

length(intersect(unique(you_chain), unique(san_chain)))
length(san_chain) - 55 +
  length(you_chain) - 55

