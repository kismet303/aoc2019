input <- c(1,9,10,3,2,3,11,0,99,30,40,50)

input2 <- c(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,1,23,13,27,1,6,27,31,1,9,
            31,35,2,10,35,39,1,39,6,43,1,6,43,47,2,13,47,51,1,51,6,55,2,6,55,59,2,59,6,63,
            2,63,13,67,1,5,67,71,2,9,71,75,1,5,75,79,1,5,79,83,1,83,6,87,1,87,6,91,1,91,5,
            95,2,10,95,99,1,5,99,103,1,10,103,107,1,107,9,111,2,111,10,115,1,115,9,119,1,13,
            119,123,1,123,9,127,1,5,127,131,2,13,131,135,1,9,135,139,1,2,139,143,1,13,143,0,
            99,2,0,14,0)



run_p <- function(input) {
  
  
  current_input <- 1
  
  
  for (x in 1:10) {
    print(input)
    if(input[current_input] == 1) {
      input[input[current_input +3] + 1] <- input[input[current_input+1] + 1] + input[input[current_input+2] + 1]
    } else if (input[current_input] == 2) {
      input[input[current_input +3] + 1] <- input[input[current_input+1] + 1] * input[input[current_input+2] + 1]
    } else if (input[current_input] == 99) {
      print("finished at 99")
      return(input)
    } else {stop("you fucked up")}
    
    current_input <- current_input + 4
  }
  
}

run_p(input)
run_p(input2)


#======================#
# Part 2
#======================#

input2a <- c(1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,10,23,1,23,13,27,1,6,27,31,1,9,
             31,35,2,10,35,39,1,39,6,43,1,6,43,47,2,13,47,51,1,51,6,55,2,6,55,59,2,59,6,63,
             2,63,13,67,1,5,67,71,2,9,71,75,1,5,75,79,1,5,79,83,1,83,6,87,1,87,6,91,1,91,5,
             95,2,10,95,99,1,5,99,103,1,10,103,107,1,107,9,111,2,111,10,115,1,115,9,119,1,13,
             119,123,1,123,9,127,1,5,127,131,2,13,131,135,1,9,135,139,1,2,139,143,1,13,143,0,
             99,2,0,14,0)



run_p2 <- function(input) {
  
  for(i in seq(1:100)) {
    for(j in seq(1:100)) {
      
      pointer <- 1
      input_test <- input
      input_test[2] <- i
      input_test[3] <- j
      print(paste0("Noun: ", i, "-", j ))
      
      for (x in 1:1000) {
        # print(input)
        # print(x)
        opcode = input_test[pointer]
        if (opcode %in% c(1,2)) n_parameters <- 3
        if(opcode == 1) {
          input_test[input_test[pointer +3] + 1] <- input_test[input_test[pointer+1] + 1] + input_test[input_test[pointer+2] + 1]
        } else if (opcode == 2) {
          input_test[input_test[pointer +3] + 1] <- input_test[input_test[pointer+1] + 1] * input_test[input_test[pointer+2] + 1]
        } else if (opcode == 99) {
          print("finished at 99")
          if(input_test[1] == 19690720) {
            print(paste("success: ", i))
            stop()
          }
          else{
            print("fail")
            break
          }
        } else {stop("you fucked up")}
        
        pointer <- pointer + n_parameters + 1
      }
    }
  }
  
}

run_p2(input2a)