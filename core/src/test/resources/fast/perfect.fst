//! @tags complexity
//! @citations AliasDFG10

//Generated by c2fsm -cut -nodiv -int 
model main {
var x,y1,y2,y3;
//parameters x;
states stop,start,lbl_16,lbl_11_1,lbl_8_2;
transition t_33 :={
  from  := start;
  to    := stop;
  guard := (x <= 1);
  action:=;
};
transition t_47 :={
  from  := start;
  to    := lbl_11_1;
  guard := (2 <= x);
  action:= y1' = x-1, y2' = 1, y3' = x;
};
transition t :={
  from  := lbl_16;
  to    := stop;
  guard := true;
  action:=;
};
transition t_41 :={
  from  := lbl_11_1;
  to    := lbl_11_1;
  guard := (y1 <= y2);
  action:= y2' = y2-y1;
};
transition t_42 :={
  from  := lbl_11_1;
  to    := lbl_8_2;
  guard := ( ( (y2+1 <= y1) && (y2+1 <= 0) ) || ( (y2+1 <= y1) && (1 <= y2) )
 );
  action:= y1' = y1-1, y2' = x;
};
transition t_43 :={
  from  := lbl_11_1;
  to    := lbl_8_2;
  guard := ( (1 <= y1) && (0 = y2) );
  action:= y1' = y1-1, y2' = x, y3' = y3-y1;
};
transition t_37 :={
  from  := lbl_8_2;
  to    := lbl_16;
  guard := (y1 <= 0);
  action:=;
};
transition t_44 :={
  from  := lbl_8_2;
  to    := lbl_11_1;
  guard := ( (y1 <= y2) && (1 <= y1) );
  action:= y2' = y2-y1;
};
transition t_45 :={
  from  := lbl_8_2;
  to    := lbl_8_2;
  guard := ( ( (y2+1 <= y1) && (1 <= y2) ) || ( (1 <= y1) && (y2+1 <= 0) ) )
;
  action:= y1' = y1-1, y2' = x;
};
transition t_46 :={
  from  := lbl_8_2;
  to    := lbl_8_2;
  guard := ( (1 <= y1) && (0 = y2) );
  action:= y1' = y1-1, y2' = x, y3' = y3-y1;
};
}
strategy dumb {
    Region init := { state = start };
}
