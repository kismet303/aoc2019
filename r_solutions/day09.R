intcode <- function(input, m_inputs = 1) {
  
  output <- numeric()
  pointer <- 1
  m_inputs_counter <- 1
  relative_base <- 0
  
  process_code <- function(x){
    pad <- stringr::str_pad(as.character(x), 5, pad = "0")
    mode <- c(substr(pad, 3,3), substr(pad,2,2), substr(pad,1,1))
    opcode <- as.numeric(substr(pad,4,5))
    return(list(mode=mode, opcode=opcode))
  }
  
  # mode 0: 
  # mode 1:
  # mode 2: relative mode
  resolve <- function(input, pointer, mode) {
    if(mode == 0) return(input[input[pointer] + 1])
    if(mode == 1) return(input[pointer])
    if(mode == 2) return(input[input[pointer] + 1 + relative_base] )
  }
  
  for (x in 1:1000000) {
    z <- input[pointer]
    p <- process_code(input[pointer])
    opcode = p$opcode
    mode = p$mode
    if (opcode %in% c(1,2,7,8)) n_parameters <- 3
    if (opcode %in% c(3,4,9)) n_parameters <- 1
    print(paste("process code:", z, "opcode:", opcode))
    
    if(opcode == 1) {
      input[input[pointer +3] + 1 + relative_base * as.numeric(mode[3]==2)] <- resolve(input, pointer+1, mode[1]) + resolve(input, pointer+2, mode[2])
    } else if (opcode == 2) {
      input[input[pointer +3] + 1 + relative_base * as.numeric(mode[3]==2)] <- resolve(input, pointer+1, mode[1]) * resolve(input, pointer+2, mode[2])
    } else if (opcode == 3) {
      input_value = m_inputs[m_inputs_counter]
       # browser()
      print(paste0("inputting value: ", input_value, " at position ", resolve(input, pointer+1, mode[1]) ))
      # input[input[pointer +1] + 1] <- input_value
      input[input[pointer +1] + 1 + relative_base * as.numeric(mode[1]==2)] <- input_value
      #input[resolve(input, pointer+1, mode[1])] <- input_value
      # m_inputs_counter <- m_inputs_counter + 1
    }  else if (opcode == 4) {
      # browser()
      this_output <- resolve(input, pointer+1, mode[1]) 
      output <- c(output, this_output)
      print(paste0("Output: ", sprintf("%.0f", this_output )))
      # break
    } else if (opcode == 5) {
      
      if(resolve(input, pointer+1, mode[1]) != 0) n_parameters <- resolve(input, pointer+2, mode[2])  - pointer
      if(resolve(input, pointer+1, mode[1]) == 0) n_parameters <- 2
      
    } else if (opcode == 6) {
      
      # browser()
      if(resolve(input, pointer+1, mode[1]) == 0) n_parameters <- resolve(input, pointer+2, mode[2])  - pointer
      if(resolve(input, pointer+1, mode[1]) != 0) n_parameters <- 2
      
    } else if (opcode == 7) {
      
      input[input[pointer +3] + 1 + relative_base * as.numeric(mode[3]==2)] <- as.numeric(resolve(input, pointer+1, mode[1]) < resolve(input, pointer+2, mode[2]))
      
    } else if (opcode == 8) {
      
      input[input[pointer +3] + 1 + relative_base * as.numeric(mode[3]==2)] <- as.numeric(resolve(input, pointer+1, mode[1]) == resolve(input, pointer+2, mode[2]))
      
    } else if (opcode == 9) {
      
      print(paste("-- Moving relative base by:", resolve(input, pointer+1, mode[1])))
      relative_base <- relative_base + as.numeric(resolve(input, pointer+1, mode[1]))
      
    } else if (opcode == 99) {
      print("finished at 99")
      break
    } else {stop("you fucked up")}
    
    pointer <- pointer + n_parameters + 1
  }
  return(output)
}

test_program1 <- c(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
test_program2 <- c(1102,34915192,34915192,7,4,7,99,0) # should output a 16-digit number.
test_program3 <- c(104,1125899906842624,99) # should output the large number in the middle
intcode(input = test_program1)
#intcode(test_program2)
#intcode(test_program3)

# program <- readLines("r_solutions/day_09input.txt")
# program <- as.numeric(unlist(strsplit(program, ",")))
intcode(input = program, m_inputs = 1)
intcode(input = program, m_inputs = 2)

