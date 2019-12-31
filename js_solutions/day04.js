// 109165-576723
function count(arr) {
    return arr.reduce(
      (prev, curr) => ((prev[curr] = ++prev[curr] || 1), prev),
      {}
    );
  }
  
  
  const chk_pwd = function(n) {
    var n2 = n.toString().split("");
    var sorted = [...n2].sort();
    var lag = [...n2].slice(1);
  
    let adj = [];
    for (let i = 0; i <= lag.length; i++) {
      adj[i] = lag[i] === n2[i];
    }
  
    if (
      n2.length === sorted.length &&
      n2.every((value, index) => value === sorted[index]) &&
      adj.includes(true)
    ) {
      pwd_opts.push(n);
    }
  };
  
  const chk_pwd2 = function(n) {
    var n2 = n.toString().split("");
    var sorted = [...n2].sort();
    var lag = [...n2].slice(1);
  
    let adj = [];
    for (let i = 0; i <= lag.length; i++) {
      adj[i] = lag[i] === n2[i];
    }
  
    let counts = count(n2);
    let only2 = Object.values(counts).includes(2);
  
    if (
      n2.length === sorted.length &&
      n2.every((value, index) => value === sorted[index]) &&
      adj.includes(true) &&
      only2
    ) {
      pwd_opts.push(n);
    }
  };
  
  var pwd_opts =[]
  for (let i = 109165; i <= 576723; i++) {
    chk_pwd(i);
  }
  console.log(pwd_opts.length);
  
  var pwd_opts =[]
  for (let i = 109165; i <= 576723; i++) {
    chk_pwd2(i);
  }
  console.log(pwd_opts.length);