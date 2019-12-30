intcode <- function(input, 
                    pointer = 1, 
                    m_inputs = 1, 
                    relative_base = 0,
                    full_log = 0 ) {
  
  output <- numeric()
  m_inputs_counter <- 1

  outputs_n <- numeric()
  outputs_direction <- numeric()
  status <- "RUNNING"
  
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
    if(full_log > 1) print(paste("process code:", z, "opcode:", opcode))
    

# Process opcodes ---------------------------------------------------------
    if(opcode == 1) {
      
      input[input[pointer +3] + 1 + relative_base * as.numeric(mode[3]==2)] <- resolve(input, pointer+1, mode[1]) + resolve(input, pointer+2, mode[2])
    
    } else if (opcode == 2) {
      
      input[input[pointer +3] + 1 + relative_base * as.numeric(mode[3]==2)] <- resolve(input, pointer+1, mode[1]) * resolve(input, pointer+2, mode[2])
    
    } else if (opcode == 3) {
      print(paste0("INPUT requested: " ))
      input_value = m_inputs[m_inputs_counter]
      
      # pause Intcode program until another input is provided
      if(is.na(input_value)) {
        opcode <- 100
        status <- "PAUSED"
        
      } else {
        if (full_log >0) print(paste0("inputting value: ", input_value, " at position ", resolve(input, pointer+1, mode[1]) ))
        input[input[pointer +1] + 1 + relative_base * as.numeric(mode[1]==2)] <- input_value
        m_inputs_counter <- m_inputs_counter + 1
      }

    }  else if (opcode == 4) {
      # browser()
      
      this_output <- resolve(input, pointer+1, mode[1]) 
      output <- c(output, this_output)
      outputs_n <- length(output)
      # if(outputs_n %% 2 == 1)  outputs_direction <- c(outputs_direction, output)
      # if(outputs_n %% 2 == 0)  m_inputs <- c(m_inputs, output)

     if (full_log >0) print(paste0("Output: ", sprintf("%.0f", this_output )))
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
      print("Finished at opcode 99")
      break
      
    } else {stop("you fucked up")}
    
    
    if(opcode == 100) {
      print(paste(" --- INTCODE PAUSED - further input required ----"))
      break
    }
    
    pointer <- pointer + n_parameters + 1
  }
  
  return(list(output = output,
              pgm = input,
              pointer = pointer,
              relative_base = relative_base,
              status = status))
}


