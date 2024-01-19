module handin.FP_02

// Exercise 2.1
let timediff (hh1,mm1) (hh2,mm2) =
    let minutes1 = (hh1*60) + mm1
    let minutes2 = (hh2*60) + mm2
    abs (minutes1 - minutes2)
    
// Exercise 2.2
let minutes (hh,mm) =
    timediff (hh,mm) (00,00)