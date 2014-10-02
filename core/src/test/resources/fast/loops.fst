//! @tags complexity
//! @citations PodelskiR04

//Generated by c2fsm -cut -nodiv -int 
model main {
var n,x,y;
//parameters n;
states stop,start,lbl_12_1,lbl_10_1;
transition t_22 :={
  from  := start;
  to    := stop;
  guard := (n+1 <= 0);
  action:= x' = n;
};
transition t_34 :={
  from  := start;
  to    := lbl_12_1;
  guard := ( (n <= 1) && (0 <= n) );
  action:= x' = n-1, y' = 1;
};
transition t_35 :={
  from  := start;
  to    := lbl_10_1;
  guard := (2 <= n);
  action:= x' = n, y' = 2;
};
transition t_29 :={
  from  := lbl_12_1;
  to    := stop;
  guard := (x+1 <= 0);
  action:=;
};
transition t_30 :={
  from  := lbl_12_1;
  to    := lbl_12_1;
  guard := ( (x <= 1) && (0 <= x) );
  action:= x' = x-1, y' = 1;
};
transition t_31 :={
  from  := lbl_12_1;
  to    := lbl_10_1;
  guard := (2 <= x);
  action:= y' = 2;
};
transition t_25 :={
  from  := lbl_10_1;
  to    := lbl_10_1;
  guard := (y+1 <= x);
  action:= y' = 2y;
};
transition t_26 :={
  from  := lbl_10_1;
  to    := lbl_12_1;
  guard := (x <= y);
  action:= x' = x-1;
};
}
strategy dumb {
    Region init := { state = start };
}

