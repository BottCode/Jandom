//! @tags complexity
//! @citations ChawdharyCGSY08

//Generated by c2fsm -cut -nodiv -int 
model main {
var N,i,r,x,y;
//parameters N;
states stop,start,lbl_21,lbl_12_1,lbl_14_1,lbl_17_1,lbl_19_1,lbl_8_1;
transition t_63 :={
  from  := start;
  to    := stop;
  guard := (N <= 0);
  action:= i' = 0, x' = 0, y' = 0;
};
transition t_64 :={
  from  := start;
  to    := lbl_8_1;
  guard := (1 <= N);
  action:= i' = 1, r' = ?, x' = 0, y' = 0;
};
transition t_61 :={
  from  := lbl_21;
  to    := stop;
  guard := (N <= i);
  action:=;
};
transition t_62 :={
  from  := lbl_21;
  to    := lbl_8_1;
  guard := (i+1 <= N);
  action:= i' = i+1, r' = ?;
};
transition t_51 :={
  from  := lbl_12_1;
  to    := stop;
  guard := (N <= i);
  action:=;
};
transition t_52 :={
  from  := lbl_12_1;
  to    := lbl_8_1;
  guard := (i+1 <= N);
  action:= i' = i+1, r' = ?;
};
transition t_53 :={
  from  := lbl_14_1;
  to    := stop;
  guard := (N <= i);
  action:=;
};
transition t_54 :={
  from  := lbl_14_1;
  to    := lbl_8_1;
  guard := (i+1 <= N);
  action:= i' = i+1, r' = ?;
};
transition t_55 :={
  from  := lbl_17_1;
  to    := stop;
  guard := (N <= i);
  action:=;
};
transition t_56 :={
  from  := lbl_17_1;
  to    := lbl_8_1;
  guard := (i+1 <= N);
  action:= i' = i+1, r' = ?;
};
transition t_57 :={
  from  := lbl_19_1;
  to    := stop;
  guard := (N <= i);
  action:=;
};
transition t_58 :={
  from  := lbl_19_1;
  to    := lbl_8_1;
  guard := (i+1 <= N);
  action:= i' = i+1, r' = ?;
};
transition t_45 :={
  from  := lbl_8_1;
  to    := lbl_21;
  guard := ( (r+1 <= 0) || (4 <= r) );
  action:=;
};
transition t_46 :={
  from  := lbl_8_1;
  to    := lbl_12_1;
  guard := (0 = r);
  action:= x' = x+1;
};
transition t_47 :={
  from  := lbl_8_1;
  to    := lbl_14_1;
  guard := (1 = r);
  action:= x' = x-1;
};
transition t_48 :={
  from  := lbl_8_1;
  to    := lbl_17_1;
  guard := (2 = r);
  action:= y' = y+1;
};
transition t_49 :={
  from  := lbl_8_1;
  to    := lbl_19_1;
  guard := (3 = r);
  action:= y' = y-1;
};
}
strategy dumb {
    Region init := { state = start };
}

