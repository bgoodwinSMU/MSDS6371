FILENAME REFFILE '/folders/myfolders/sasuser.v94/metabolic.csv';
PROC IMPORT DATAFILE=REFFILE
DBMS=CSV
OUT=metabolic;
GETNAMES=YES;
RUN;

*Need to raise mass to 0.75 power;
data metabolic;
set metabolic;
powermass = mass**0.75;
run;



*Create model and view assumptions;
proc glm data = metabolic plots = all;
model metab = powermass/solution;
run;
*Log-transform things;
data metabolic;
set metabolic;
logpowermass=log(powermass);
logmetab=log(metab);
run;
*look at model with log transformed things;
proc glm data = metabolic plots = all;
model logmetab = logpowermass/solution clparm;
run;
