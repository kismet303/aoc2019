var fs = require('fs')
// Reading the file

var res = fs.readFileSync("inputs/day_01.txt", 'utf8').split("\n").filter(x => x.length != 0);
const res2 = res.map( (n) => Math.floor(n/3) - 2)
const part1 = res2.reduce((a,b) => a+b,  0)

console.log(part1);

// Part 2
const fuel_req = function(n){
    
    f = Math.floor(n/3) - 2;
    if (f <= 0) {
        return(0);
    } else {
        return(f + fuel_req(f));
    };
};

const part2 = res.map(fuel_req).reduce((a,b) => a + b, 0);

console.log(part2);

