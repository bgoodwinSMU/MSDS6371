FILENAME REFFILE '/folders/myfolders/sasuser.v94/Crab17.csv';
PROC IMPORT DATAFILE=REFFILE
DBMS=CSV
OUT=crabby;
GETNAMES=YES;
RUN;

proc sgplot data=crabby;
styleattrs wallcolor=blanchedalmond;
scatter x=Height y=Force / group = Species
markerattrs = (symbol=CircleFilled size=7)
FILLEDOUTLINEDMARKERS;
run;

DATA logdata;
SET crabby;
LOGVAR=log10(Force);
RUN;


proc sgplot data=logdata;
styleattrs wallcolor=blanchedalmond;
scatter x=Height y=LOGVAR / group = Species
markerattrs = (symbol=CircleFilled size=7)
FILLEDOUTLINEDMARKERS;
run;

proc glm data=crabby plots=all;
class Species(ref="Lophopanopeus bellus");
model Force = Height|Species /solution clparm;
run;
