//! @tags complexity
//! @citations ChawdharyCGSY08

//Generated by c2fsm -cut -nodiv -int 
model main {
var x,y,z;
//parameters;
states stop,start,lbl_7_1;
transition t_13 :={
  from  := start;
  to    := stop;
  guard := (z <= 0);
  action:=;
};
transition t_14 :={
  from  := start;
  to    := lbl_7_1;
  guard := (1 <= z);
  action:= x' = x+1, y' = y-1, z' = z-1;
};
transition t_11 :={
  from  := lbl_7_1;
  to    := stop;
  guard := (z <= 0);
  action:=;
};
transition t_12 :={
  from  := lbl_7_1;
  to    := lbl_7_1;
  guard := (1 <= z);
  action:= x' = x+1, y' = y-1, z' = z-1;
};
}
strategy dumb {
    Region init := { state = start };
}

