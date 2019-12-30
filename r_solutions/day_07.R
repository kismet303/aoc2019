process_code <- function(x){
  pad <- stringr::str_pad(as.character(x), 5, pad = "0")
  mode <- c(substr(pad, 3,3), substr(pad,2,2), substr(pad,1,1))
  opcode <- as.numeric(substr(pad,4,5))
  return(list(mode=mode, opcode=opcode))
}

resolve <- function(input, pointer, mode) {
  if(mode == 1) return(input[pointer])
  if(mode == 0) return(input[input[pointer] + 1])
}

d7 <- function(input, m_inputs = 1) {
  output <- numeric()
  pointer <- 1
  m_inputs_counter <- 1
  for (x in 1:1000) {
    z <- input[pointer]
    p <- process_code(input[pointer])
    opcode = p$opcode
    mode = p$mode
    if (opcode %in% c(1,2,7,8)) n_parameters <- 3
    if (opcode %in% c(3,4)) n_parameters <- 1
    print(paste("process code:", z, "opcode:", opcode))
    
    if(opcode == 1) {
      input[input[pointer +3] + 1] <- resolve(input, pointer+1, mode[1]) + resolve(input, pointer+2, mode[2])
    } else if (opcode == 2) {
      input[input[pointer +3] + 1] <- resolve(input, pointer+1, mode[1]) * resolve(input, pointer+2, mode[2])
    } else if (opcode == 3) {
      input_value = m_inputs[m_inputs_counter]
      print(paste0("inputting value: ", input_value))
      input[input[pointer +1] + 1] <- input_value
      m_inputs_counter <- m_inputs_counter + 1
    }  else if (opcode == 4) {
      this_output <- input[input[pointer +1] + 1] 
      output <- c(output, this_output)
      print(paste0("Output: ", this_output ))
      break
    } else if (opcode == 5) {
      
      if(resolve(input, pointer+1, mode[1]) != 0) n_parameters <- resolve(input, pointer+2, mode[2])  - pointer
      if(resolve(input, pointer+1, mode[1]) == 0) n_parameters <- 2
      
    } else if (opcode == 6) {
      
      if(resolve(input, pointer+1, mode[1]) == 0) n_parameters <- resolve(input, pointer+2, mode[2])  - pointer
      if(resolve(input, pointer+1, mode[1]) != 0) n_parameters <- 2
      
    } else if (opcode == 7) {
      
      input[input[pointer +3] + 1] <- as.numeric(resolve(input, pointer+1, mode[1]) < resolve(input, pointer+2, mode[2]))
      
    } else if (opcode == 8) {
      
      input[input[pointer +3] + 1] <- as.numeric(resolve(input, pointer+1, mode[1]) == resolve(input, pointer+2, mode[2]))
      
    } else if (opcode == 99) {
      print("finished at 99")
      break
    } else {stop("you fucked up")}
    
    pointer <- pointer + n_parameters + 1
  }
  return(output)
}



d7_input <- c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
d7_input2 <- c(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
               1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)

d7_input3 <- c(3,8,1001,8,10,8,105,1,0,0,21,42,59,76,85,106,187,268,349,430,99999,3,9,102,3,9,9,1001,9,2,9,1002,9,3,9,1001,9,3,9,4,9,99,3,9,102,3,9,9,101,3,9,9,1002,
               9,2,9,4,9,99,3,9,102,3,9,9,1001,9,4,9,1002,9,5,9,4,9,99,3,9,102,2,9,9,4,9,99,3,9,101,3,9,9,1002,9,2,9,1001,9,4,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,
               3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,
               9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3
               ,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,
               101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,
               101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,
               2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99)

settings_to_try <- gtools::permutations(5, 5, v=c(0:4)) %>% split(1:120)
calc_signal <- function(phase_setting, d7_input) {
  
  for(i in 1:2) {
    print(paste("----- i = ", i, "-----"))
    browser()
    if (i == 1) {
      x1 <- d7(d7_input, m_inputs = c(phase_setting[1],0))
      x2 <- d7(d7_input, m_inputs = c(phase_setting[2], x1))
      x3 <- d7(d7_input, m_inputs = c(phase_setting[3], x2))
      x4 <- d7(d7_input, m_inputs = c(phase_setting[4], x3))
      x5 <- d7(d7_input, m_inputs = c(phase_setting[5], x4))
    }
    
    if (i != 1) {
      # x1 <- d7(d7_input, m_inputs = c(x5))
      # x2 <- d7(d7_input, m_inputs = c(x1))
      # x3 <- d7(d7_input, m_inputs = c(x2))
      # x4 <- d7(d7_input, m_inputs = c(x3))
      # x5 <- d7(d7_input, m_inputs = c(x4))
      
      x1 <- d7(d7_input, m_inputs = c(phase_setting[1], x5))
      x2 <- d7(d7_input, m_inputs = c(phase_setting[2], x1))
      x3 <- d7(d7_input, m_inputs = c(phase_setting[3], x2))
      x4 <- d7(d7_input, m_inputs = c(phase_setting[4], x3))
      x5 <- d7(d7_input, m_inputs = c(phase_setting[5], x4))
    }
  }
  
  
  # tibble(phase_setting = toString(phase_setting),
  #        signal = x5)
}

res <- purrr::map_dfr(settings_to_try, calc_signal, d7_input = d7_input3)

res %>% 
  filter(signal == max(res$signal))

#---------------------#
# Part II
#---------------------#
settings_to_try2 <- gtools::permutations(5, 5, v=c(5:9)) %>% split(1:120)


d7x_input <- c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
               27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)

res <- purrr::map_dfr(settings_to_try2[1], calc_signal, d7_input = d7x_input)

res %>% 
  filter(signal == max(res$signal))

