data schoolDat;
input SCHOOL $ AMOUNT;
datalines;
SMU 34
SMU 37
SMU 37
SMU 38
SMU 41
SMU 42
SMU 43
SMU 44
SMU 44
SMU 45
SMU 45
SMU 45
SMU 46
SMU 48
SMU 49
SMU 53
SMU 53
SMU 54
SMU 54
SMU 55
SMU 56
seattleU 27 
seattleU 33
seattleU 36
seattleU 37
seattleU 38
seattleU 38
seattleU 39
seattleU 42
seattleU 42
seattleU 43
seattleU 43
seattleU 44
seattleU 44
seattleU 44
seattleU 45
seattleU 45
seattleU 45
seattleU 45
seattleU 46
seattleU 46
seattleU 47
seattleU 47
seattleU 48
seattleU 48
seattleU 49
seattleU 49
seattleU 51
seattleU 51
seattleU 52
seattleU 54
;
run;

/* Perform the t-test */

title 'Two Sample T-Test';

 

proc ttest data=schoolDat;

class school; /* defines the grouping variable */

var amount; /* variable whose means will be compared */

run;
