//! @tags complexity
//! @citations AliasDFG10

//Generated by c2fsm -cut -nodiv -int 
model main {
var i,j,k,tmp;
//parameters;
states stop,start,lbl_7_1;
transition t_16 :={
  from  := start;
  to    := stop;
  guard := ( (101 <= i) || (k+1 <= j) );
  action:=;
};
transition t_17 :={
  from  := start;
  to    := lbl_7_1;
  guard := ( (i <= 100) && (j <= k) );
  action:= i' = j, j' = i+1, k' = k-1, tmp' = i;
};
transition t_14 :={
  from  := lbl_7_1;
  to    := stop;
  guard := ( (101 <= i) || (k+1 <= j) );
  action:=;
};
transition t_15 :={
  from  := lbl_7_1;
  to    := lbl_7_1;
  guard := ( (i <= 100) && (j <= k) );
  action:= i' = j, j' = i+1, k' = k-1, tmp' = i;
};
}
strategy dumb {
    Region init := { state = start };
}
