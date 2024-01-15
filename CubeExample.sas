/*Example*/
%let Nunits=1000;
%let Nstrat = 3;
data Pop;
	do unitid = 1 to &Nunits;
		stratId = mod(unitId,&Nstrat);
		prob=1/4;
		output;
	end;
run;
proc sort data= pop ; by stratId; run;
proc means data = pop noprint; by stratId; output out=stratSizes n=n ;run;

data Alloc (keep= stratId size) ;
	set stratSizes;
	size = floor(n/2);
run;
data coef;
	do unitid = 1 to &Nunits;
		var1=1;
		var2=rand('Bernoulli',0.5);
		var3=rand('Bernoulli',0.5);
		output; 
	end;
run;


%Cube(	Pop=Pop,
		alloc = Alloc,
		cons=coef,
		dataOut=BalSample,
		land=drop,
		seed=1
);

%Cube(Pop=prob,cons=coef,dataOut=BalProb,land=opt,seed=1);


proc sort data = pop; by  unitId;run;
data BalCoef;
	merge pop(keep=unitid prob) coef;
	by unitId;
run;
proc means data= BalCoef noprint; weight prob; var var1 var2 var3; output out=BalSum sum= /autoname;run;
proc transpose data=BalSum out= TBalSum;run;
data def(keep=consId type bound);
	set TBalSum;
	if _N_ >2;
	consId=_NAME_;
	consId=substr(ConsId,1,4);
	type="=";
	Bound=Col1;
run;
proc delete data= BalSum TBalSum;run;

data CheckSample;
	merge balSample(keep=unitid sample) BalCoef;
	by unitId;
	/*weight = 1/prob * sample;*/
run;
proc means data= CheckSample noprint;weight sample;  var var1 var2 var3; output out=SampleCheck sum= /autoname;run;
