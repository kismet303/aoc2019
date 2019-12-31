// For example, suppose you have the following program:

// 1,9,10,3,2,3,11,0,99,30,40,50
// For the purposes of illustration, here is the same program split into multiple lines:

// 1,9,10,3,
// 2,3,11,0,
// 99,
// 30,40,50
// The first four integers, 1,9,10,3, are at positions 0, 1, 2, and 3. Together, they represent the first opcode 
// (1, addition), the positions of the two inputs (9 and 10), and the position of the output (3). 

// To handle this opcode, you first need to get the values at the input positions: position 9 contains 30, 
// and position 10 contains 40. Add these numbers together to get 70. Then, store this value at the output 
// position; here, the output position (3) is at position 3, so it overwrites itself. 
// Afterward, the program looks like this:
// 1,9,10,70,
// 2,3,11,0,
// 99,
// 30,40,50

const program = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50];


intcode = function (program, pointer = 0) {
    
    // while (true) {
    for(let i = 0; i<5; i++){

        var opcode = program[pointer]

        if (opcode === 1) {

            console.log("Entering opcode 1");
            let newval = program[program[pointer + 1]] + program[program[pointer + 2]];
            program[program[pointer + 3]] == newval;
            console.log(program);


        } else if (opcode == 2) {
            console.log("Entering opcode 2");
            let newval = program[program[pointer + 1]] + program[program[pointer + 2]];
            program[program[pointer + 3]] == newval;
            console.log(program);

        } else if (opcode == 99) {
            console.log("Program Complete!");
            console.log(program);
            break
        } else {
            console.log(`Bad opcode: ${opcode}`);
            break;
        };

        var pointer =+ 4;
    }
};

intcode(program);






