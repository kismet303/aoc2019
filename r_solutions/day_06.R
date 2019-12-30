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

source("r_solutions/day06_input.R")

list_orbit <- function(orbit) {
  o <- unlist(stringr::str_split(string= orbit, pattern= stringr::coll(")") ))
}

all_orbits <- purrr::map(orbits, list_orbit)
all_planets <- unique(unlist(all_orbits))


n_o <- 0
e <- new.env()
count_orbits <- function(planet) {
  for(x in (all_orbits)) {
    if(x[2] == planet) {
      n_o <- n_o + 1
      count_orbits(x[1])
    }
  }
  e[[planet]] = n_o
  return(n_o)
}

c_count_orbits <- function() {
  num_orbits <- 0
  e <- new.env()
  
  f <- function(planet) {
    for(x in (all_orbits)) {
      if(x[2] == planet) {
        num_orbits <<- num_orbits + 1
        count_orbits(x[1])
      }
    }
    e[[planet]] = num_orbits
    return(list(num_orbits,e))
  }
  
}

count_orbits <- c_count_orbits()

res <- purrr::map(all_planets, count_orbits)
res_e <- res[[1]][[2]]
sapply(ls(res_e), function(x) get(x, envir = res_e))

res_e$B
rm(e)
purrr::map_dbl(all_planets, count_orbits)
ls(e)


microbenchmark::microbenchmark(
  purrr::map_dbl(all_planets, count_orbits),
  times = 1
  
)


## LHS is the orbitee, RHS is the orbiter
# https://github.com/mpjdem/adventofcode2019/blob/master/aoc19_day6.R
inp <- read.delim("r_solutions/day06z.txt", sep = ")",
                  header = FALSE,
                  col.names = c("orbitee", "orbiter"),
                  stringsAsFactors = FALSE)
memo_env <- new.env()

count_orbits <- function(orbiter) {
  # print(paste("---orbiter:", orbiter, "-----"))
  if (!is.null(memo_env[[orbiter]])) {
    # print(paste0("getting memoised answer for orbiter:", orbiter))
    memo_env[[orbiter]]
  } else if (orbiter %in% inp$orbiter) {
    memo_env[[orbiter]] <-
      1 + count_orbits(inp[inp$orbiter == orbiter,]$orbitee)
  } else {
    0
  }
  
}
a <- count_orbits("L")
sapply(unique(inp$orbiter), count_orbits)

f <- function() {
  e1 <- new.env()
  e1$x <- 10
}

x <- f()
solution_1 <- sum(sapply(unique(inp$orbiter), count_orbits))#




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

