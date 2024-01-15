/*Balanced sampling, Cube method*/


%macro Time(from);
/*returns the current time  or if input provided: 
returns the elaspsed time from the input time */
	%local dataTime now time;
	%let datetime = %sysfunc( datetime() );
	%let now=%sysfunc( timepart(&datetime) );

	%if (&from ne ) %then %do;
		%let timefrom = %sysfunc(inputn(&from,time9.));
		%if %sysevalf(&now<&timefrom) %then %do;
			%let time =  %sysevalf(86400-&timefrom,ceil);
			%let time = %sysevalf(&time + %sysevalf(&now,ceil));
		%end;
		%else %do;
			%let time = %sysevalf(&now-&timefrom,ceil);
		%end;
		%let time = %sysfunc(putn(&time,time9.));
	%end;
	%else %do;
		%let time = &now;
		%let time = %sysfunc(putn(&time,time9.));
	%end;
	&time
%mend Time;


%macro Nobs(dataIn);
/*Returns the number of observations in a dataset*/
	%local dataid nobs rc;
	%let dataid=%sysfunc(open(&dataIn));
	%let nobs=%sysfunc(attrn(&dataid,nobs));
	%let rc=%sysfunc(close(&dataid));
	&nobs 
%mend Nobs;

%macro Sum(data,Var);
/* yields the sum of a dataset s variable*/
	%local i dataid varnum nobs sum rc var;
	%let dataid=%sysfunc(open(&data));
	%let varnum=%sysfunc(varnum(&dataId,&var));
	%let nobs = %sysfunc(attrn(&dataId,nobs));
	%let sum=0;
	%do i = 1 %to &nobs;
		%let rc= %sysfunc(fetch(&dataId));
		%let var = %sysfunc(getvarN(&dataId,&varnum));
		%let sum= %sysevalf(&sum+&var);
	%end;
	%let rc = %sysfunc(close(&dataId));
	&sum
%mend Sum;

%macro saveOptions();
	/*save some common options*/
	%local notes mprint symbolgen source options;
	%let notes = %sysfunc(getoption(Notes));
	%let mprint = %sysfunc(getoption(mprint));
	%let symbolgen = %sysfunc(getoption(Symbolgen));
	%let source = %sysfunc(getoption(source));

	%let options = &notes &mprint &symbolgen &source;
	&options;
%mend saveOptions;

%macro ListData(data=,var=);
	/*returns a list that constains the content of the variable var= in the dataset data=*/
	%local id n list val varnum type rc i;
	%let id= %sysfunc(open(&data));
	%let n= %sysfunc(attrn(&id,nobs));
	%let varnum =%sysfunc(varnum(&id,&var));
	%let type=%sysfunc(varType(&id,&varnum));

	%let list=;
	%do i = 1 %to &n;
		%let val=%sysfunc(fetch(&id));
		%if &Type=N %then %do;
			%let val=%sysfunc(getvarn(&id,&varnum));
		%end;
		%else %do; 
			%let val=%sysfunc(getvarC(&id,&varnum));
		%end;
		%let list = &list &val;
	%end;
	%let rc= %sysfunc(close(&id));

	&list
%mend ListData;

%macro ListReplace(List, search= %str( ) , replace= %str(,));
	/*Replace elements of a list*/
	%local replaced;
	%let replaced = %sysfunc(tranwrd(%trim(%cmpres(%Str(&List))), &search, &replace));
	&replaced 
%mend ListReplace;

%macro NVar(dataIn);
/*Returns the number of variables in a dataset*/
	%local dataId nobs rc;
	%let dataid=%sysfunc(open(&dataIn));
	%let nobs=%sysfunc(attrn(&dataid,nvar));
	%let rc=%sysfunc(close(&dataid));
	&nobs 
%mend NVar;

%macro VarNames(Data);
	/*Generates the complete list of column names in a SAS dataset */
	%local VarList CurrentVar DSID NumVars;
	%let DSID = %sysfunc(open(&Data));
	%let NumVars = %sysfunc(attrn(&DSID, nvars));

	/* loop through all variables and get their names */
	%let CurrentVar = 1;
	%do %while(&CurrentVar <= &NumVars);
		%let VarList = &VarList %sysfunc(varname(&DSID, &CurrentVar));
		/* append current variable's name to output list */
		%let CurrentVar = %eval(&CurrentVar + 1);
	%end;

	%let DSID = %sysfunc(close(&DSID));
	&VarList 
%mend VarNames;

%macro varType(data,var);
	/*return a list of types for the variables of a Data set*/
	%local id nvar types rc N i varnum n;
	%let id= %sysfunc(open(&data));

	%if (&var eq) %then %do;
		%let nvar=%sysfunc(attrn(&id,nvar));
		%let types=;
		%do i = 1 %to &nvar;
			%let types= &types %sysfunc(varType(&id,&i));
		%end;
	%end;
	%else %do;
		%let n=  %sysfunc(countw(&var,%str( )));
		%let types=;
		%do i = 1 %to &n;
			%let varnum = %sysfunc(varnum(&id,%scan(&var,&i)));
			%let types= &types %sysfunc(varType(&id,&varnum));
		%end;
	%end;

	%let rc= %sysfunc(close(&id));
	&types
%mend varType;

%macro varExist(Data,var);
	/*Check if a set of variables exists in a data set */
	%local count DSID varexist N varnum;
	%let DSID = %sysfunc(open(&Data));
	%let n=  %sysfunc(countw(&var,%str( )));

	%let count = 1;
	%let varexist=1;
	%do %while(&count <= &N);
		%let varnum = %sysfunc(varnum(&DSID, %scan(&var,&count)));
		%if &varnum eq 0 %then %do;
			%let varexist=0;
		%end;
		%let count = %eval(&count + 1);
	%end;
	
	%let DSID = %sysfunc(close(&DSID));
	&varexist 
%mend varExist;

%macro BuildCons(Pop=, Alloc=, ConsIn=, consOut= , PopOut=);
	%local i Nstrat curStrat stratList;

	%if (&alloc ne ) %then %do;
	
		proc sort data  = &Pop out=&popOut ; by stratId;run;
		proc means data = &popOut noprint; by stratId; output out= __StratSizes n=n;run;

		proc sort data= &alloc; by stratId;run;
		proc sort data= __StratSizes; by stratId;run;

		data __StratSizes;
			merge __StratSizes &alloc;
			by stratId;
		run;

		data &popOut(keep = unitId stratId prob);
			merge &popOut __StratSizes;
			by stratId;
			prob = size/n;
		run;
		proc sort data= &popOut; by unitId;run;
		proc delete data= __stratSizes;run;

		%let StratList  = %listData(data=&Alloc,var=stratId);
		%let nStrat = %sysfunc(countw(&StratList,%str( ))); 
		data &ConsOut (drop= stratId prob);
			set &popOut(keep=unitId stratId prob);
			%do i = 1 %to &Nstrat;
				%let curstrat = %scan(&StratList,&i);
				Strat_&curstrat=0;
				if stratId = &CurStrat then do;
					Strat_&curstrat = 1/prob;
				end;
			%end;
		run;

		proc sort data=&ConsOut;by unitId;run;

		%if (&ConsIn ne ) %then %do;
			proc sort data=&ConsIn;by unitId;run;
			data &ConsOut;
				set &ConsOut;
				set &ConsIn;
			run;
		%end;


	%end;
	%else %do;
		data &ConsOut;
			set &ConsIn;
		run;

		data &popOut;
			set &pop;
		run;
	%end;

	proc sort data = &popOut; by unitId; run;
	proc sort data = &ConsOut; by unitId; run;
%mend BuildCons;

%macro FastFlight(inclProb= , consCoef= , DataOut= ,land=drop);
	%local Nunits Ncons;

	data _Fflight_cons;
		set &consCoef;
		drop unitId;
	run;

	%let Nunits = %nobs(&inclProb);
	%let NCons = %eval(%Nvar(_Fflight_cons));

	ods html;
	/*Flight phase*/
    	proc iml ;

		start flight(Cons) global(prob,roundedProbIndex);

	  		n=nrow(Cons);
			inclprob=J(n,2,0);
			inclprob[,2]=prob;
	        W=J(n,1,1);
	        m=ncol(Cons);
			A=Cons;
	        m=ncol(A);
	        v=J(n,1,0);
	        i=1;
	        last=0;
	        P=prob;
	        k=1;


	        /*Make sure A is full rank*/
	        call gsorth(P, T, lindep, A);
	        if lindep =1 then do;
			/*Matrix A is not full rank*/
			free A;
			A=P[,loc(vecdiag(T)^=0)];
			m=ncol(A);
	        end;
	        free P T;

	        do while (i<= n & ^last) ;

			i=i+1;
			inclprob[,1] =inclprob[,2];
			
			/*déterminer les unités à arrondir*/
			l=(inclprob[,1] =1 |inclprob[,1]=0);
			W=W# ^l;
			Wloc=loc(W);
			nWLoc=sum(W);

		    /*assurer qu il reste des valeurs à arrondir avant de calculer u */
		    u=J(n,1,0);
		    if nWLoc > 0 then do;
			/*le vecteur v est un vecteur unité, un 1 et des 0 ailleur*/
			v=J(n,1,0);
			v[Wloc[k]]=1;
			u=(W#v)-(W#A)*ginv(t(A[Wloc,])*(A[Wloc,]))*t(A[Wloc,])*(v[Wloc]) ;
		    end;
		    else do;
			/*on termine immédiatement et on obtient un vecteur d inclusion partiel pour la phase d atterrissage*/
			last=1;
		    end;
		    fu = fuzz(sum(abs(u))/n);

	            /*lorsque u=0 alors il n est plus possible d arrondir */
	            if (fu=0 & ^last) then do;
	                if ( /*i< n-m &*/ nWLoc>m) then do;
	                    /*le vecteur a été mal choisi, choisissons en un autre à la prochaine itération*/
	                    k=k+1;
	                    i=i-1;
	                end;
	                else do;
				/*on termine immédiatement et on obtient un vecteur d inclusion partiel pour la phase d atterrissage*/
				last=1;
	                end;
	            end;

	            /*lorsque u différent de 0, alors on applique le vecteur u  */
	            if fu>0 then do;
	                k=1;
	                upos=loc(u>0);
	                uneg=loc(u<0);

	                lambda1=min( min((1-inclprob[upos,1])/u[upos]) , min((-inclprob[uneg,1])  /u[uneg]) ) ;
	                lambda2=min( min((inclprob[upos,1])  /u[upos]) , min(-(1-inclprob[uneg,1])/u[uneg]) ) ;

	                /* */
	                q = min(lambda2/(lambda1+lambda2),1);
	                call randgen(lambdaBernVar,'BERN', q);

			inclprob[,2] = fuzz((lambdaBernVar)*(inclprob[,1]+lambda1*u) + (1-lambdaBernVar)*(inclprob[,1]-lambda2*u));
	            end;
	        end;

		/* */
	        prob=inclprob[,2];
		roundedProbIndex= loc(prob =1 |prob=0);
			
		finish flight;

		/*Initialisation*/
		CALL RANDSEED( &seed ,0 ) ; 
		count=%eval(&NCons+1);
		TotalRounded=0;
		roundedProbIndex = 1:%eval(&NCons+1);
		Index = 1:%eval(&NCons+1);
		outIncl = J(&Nunits,1,.);

		/*Extract first lines*/
		use &inclProb;
	        read next %eval(&NCons+1) var {prob} into Prob ;
	        use  _Fflight_cons;
	        read next %eval(&NCons+1) var _all_ into cons ;
		m= ncol(cons);

		%if %upcase(&land) eq OPT %then %do;
		do while(count<&Nunits);
		%end;
		%if %upcase(&land) eq DROP %then %do;
		do while(TotalRounded<&Nunits);
		%end;
			/*file log;
			put count totalrounded ;*/

			run flight(cons);
			nrounded = ncol(roundedProbIndex);
			/*file log;
			put count nrounded;*/
			/*mettre à jour l output*/
			outIncl[index[roundedProbIndex]] = prob[roundedProbIndex];

			/*update input*/
			if count< &Nunits then do;
				do i = 1 to nrounded;
					if count < &Nunits then do;
						TotalRounded=TotalRounded+1;
						count=%eval(&NCons+1) + TotalRounded;
						use &inclProb;
				        read next 1 var {prob} into NewProb ;
				        use  _Fflight_cons;
				        read next 1 var _all_ into NewCons ;
						prob[roundedProbIndex[i],1] = NewProb;
						cons[roundedProbIndex[i],] 	= NewCons;
						index[roundedProbIndex[i]] 	= count;
					end;
				end;
			end;
			%if %upcase(&land) eq DROP %then %do;
				else do;
					TotalRounded=TotalRounded+nrounded;
					m= nrow(prob);
					t= remove(1:m,roundedProbIndex[1:(i-1)]);
					nt = ncol(t);
					if nt >0 then do;
						temp= index;
						free index;
						index= temp[t];
						free temp;	

						temp= cons;
						free cons;
						cons= temp[t,1:nt];
						free  temp;

						temp = prob;
						free prob;
						prob = temp[t];
						free temp;
					end;
					free t;

				end;			

			%end;
		end;

		%if %upcase(&land) eq DROP %then %do;
			nconsDrop = &Ncons - nt;
			call symputx('nconsDrop',nconsDrop);
		%end;

 	/*Output*/
        create &dataOut from outIncl[colname='incl'];
        append from outIncl;
        close &dataOut;
    quit;

	/*proc delete data= _Fflight_cons;run;*/

%mend FastFlight;

%macro Flight(inclProb=,consCoef=,DataOut= ,land=drop );
/*Flight phase*/
    proc iml ;
	CALL RANDSEED( &seed ,0 ) ; 
        use &inclProb;
        read all var {prob} into pi0 ;
        use  &ConsCoef;
        read all var _all_ into Coeff ;

        n=nrow(pi0);
        pi=J(n,n+1,0);
        pi[,1]=pi0;
        W=J(n,1,1);
        m=ncol(Coeff);
        A=Coeff[,2:m];
        /*AllU=J(n,n,0);*/
        m=ncol(A);
        v=J(n,1,0);
        i=1;
        j=1;
        last=0;
        P=pi0;
        k=1;

        /*Make sure A is full rank*/
        call gsorth(P, T, lindep, A);
        if lindep =1 then do;
           /* print "La matrice A n est pas de plein rang!";*/
            A=P;
            m=ncol(A);
        end;
        free P T;
		file log;

        do while (i<= n & ^last) ;
            i=i+1;
			current=time();
			put;
			put current;

            /*Determine which unit to round*/
            l=(pi[,i-1] =1 | pi[,i-1]=0);
            W=W# ^l;
            Wloc=loc(W);
            nWLoc=sum(W);

            /*Make sure there are values to round before calculating u*/
            u=J(n,1,0);
            if nWLoc > 0 then do;
                v=J(n,1,0);
                v[Wloc[k]]=1;

                u=(W#v)-(W#A)*ginv(t(A[Wloc,])*(A[Wloc,]))*t(A[Wloc,])*(v[Wloc]) ;

                /*AllU[,i-1]=u;*/
            end;
            else do;
                last=1;
                i=i-1;
            end;
            fu = fuzz(sum(abs(u))/n);

            /*lorsque u=0 alors il nest plus possible d arrondir */
            if (fu=0 & ^last) then do;

                if (i< n-m & nWLoc>m) then do;
                    /*le vecteur a été mal choisi, choisissons en un autre à la prochaine itération*/
                    /*print i k nWloc m;*/
                    k=k+1;
                    i=i-1;
                end;
                else do;
			i=i-1;
			%if &land=drop %then %do;
				/*Then constaints are eleminated one by one*/
				/*print i j;*/
				if j < m then A=A[,1:(m-j)];
				j=j+1;      
			%end;
			%else %do;
				/*on termine immédiatement et on obtient un vecteur d inclusion partiel pour la phase d atterrissage*/
				last=1;
			%end;
                end;
            end;

            /*lorsque u différent de 0, alors on applique le vecteur u  */
            if fu>0 then do;
                k=1;
                upos=loc(u>0);
                uneg=loc(u<0);

                lambda1=min( min((1-pi[upos,i-1])/u[upos]) , min((-pi[uneg,i-1])  /u[uneg]) ) ;
                lambda2=min( min((pi[upos,i-1])  /u[upos]) , min(-(1-pi[uneg,i-1])/u[uneg]) ) ;

                /**/
                q = min(lambda2/(lambda1+lambda2),1);
                call randgen(lambdaBernVar,'BERN', q);

                pi[ ,i] = fuzz((lambdaBernVar)*(pi[ ,i-1]+lambda1*u) + (1-lambdaBernVar)*(pi[ ,i-1]-lambda2*u));
            end;
			current=time();
			put current;
        end;

        /*Extract non-empty columns*/
        pi=pi[,1:i];
        outPi=pi[,i];
        /*AllU=AllU[,1:i];*/

        /*Output*/
	call symputx('nround',j);
        create &dataOut from outpi[colname='incl'];
        append from outpi;
        close &dataOut;
        /*create Trace from pi;
        append from pi;
        close  Trace;*/
        /*create VecteurU from AllU;
        append from AllU;
        close VecteurU;*/
    quit;
%mend SeqCtrlRandRnd;

%macro OptProbSel(Ens,ProbIncl, coef, Def, ProbSelOut);
/*
Ens : File
    unitId
    (sample1..SampleN): integer 0 or 1 for inclusion in this sample

ProbIncl : File
    unitId  :
    prob    : numeric between 0 and 1

Coef : File
    unitId                  :
    (var1 .. varN)  : numeric between 0 and 1

Def : File
    consId      : consId in (var1 .. varN)
    Type        :
    Bound       : numeric

ProbSelOut : File
    SampleId
    prob

*/

    %local SampList VarList nSamp NbrVar CurSamp CurVar i;

    /*Extract the list of sample names*/
    %let SampList = %VarNames(&Ens);
    %let SampList = %listReplace(&SampList,search=%scan(&SampList,1),replace=%STR( ));
    %let nSamp = %sysfunc(countw(&SampList,%str( ))); 

    /*Number of calibration equations*/
    %let VarList = %VarNames(&Coef);
    %let VarList = %listReplace(&VarList,search=%scan(&VarList,1),replace=%STR( ));
    %let NbrVars = %sysfunc(countw(&VarList,%str( ))); 
    %do i=1 %to &NbrVars;
        %local Var&i;
        %let Var&i = %scan(&VarList,&i);
    %end;

    %let unitIdType = %varType(&Coef,unitId);
    %if &unitIdType= N %then %do;
        %let unitIdType= num;
    %end;
    %else %do;
        %let unitIdType= str;
    %end;


    /*Split samples in different files*/
    data %do i = 1 %to &nSamp;%scan(&SampList,&i)(keep=unitid %scan(&SampList,&i)) %end; ;
        set &Ens;
    run;

    %do i = 1 %to &nSamp;
        %let CurSamp = %SCAN(&Samplist,&i);
        data &CurSamp;
            format SampleID $12.;
            set &CurSamp(rename=(&CurSamp = incl));
            SampleId= "&CurSamp";
        run;
        proc sort data=&CurSamp noduprecs; by sampleId;run;
    %end;

    /*Créer le fichier qui décrit l'inclusion des unités dans chaque échantillon*/
    data __UnitSampleIncl__;
        set &Samplist;
    run;


    /*Split coefficients in different files, one for each equation*/
    data %do i = 1 %to &nbrVars; %scan(&VarList,&i)(keep=unitid %scan(&VarList,&i)) %end; ;
        set &Coef;
    run;

    %do i = 1 %to &NbrVars;
        %let CurVar = %SCAN(&VarList,&i);
        data &CurVar;
            format consId $12.;
            set &CurVar(rename=(&CurVar = coef));
            ConsId= "&CurVar";
        run;
        proc sort data=&CurVar noduprecs; by consId;run;
    %end;

    /*Créer le fichier qui décrit l'inclusion des unités dans chaque échantillon*/
    data __UnitConsCoef__;
        set &Varlist;
    run;


    /*
    Resolve with optmodel.
    Determine selection probabilities such that 1st order inclusion probability are as close as possible from target.
    */
    proc optmodel PRINTLEVEL=0 ;
    /*Declare variables*/
     set <&unitIdType> UnitID ;
     set <str> consID ;
     set <str> SampleID;
     set <&unitIdType,str> UnitSampleID;
     set <&unitIdType,str> UnitConsID;
     num Incl{UnitSampleID};
     num Coef{unitConsId};
     num wgtSum{sampleId,ConsId};
     num pi{UnitID};
     num bound{consID};
     var ps{SampleID} >=0 <=1;

     /*Read data*/
     read data &Coef into UnitID=[UnitID] ;
     read data &plan into sampleID=[sampleID] ;
     read data __UnitConsCoef__ into UnitConsID=[UnitID consID] coef=coef;
     read data __UnitSampleIncl__ into UnitSampleID=[UnitID SampleID] incl=incl;
     read data &Def into consID=[consID] bound=bound;
     read data &ProbIncl into UnitID=[UnitID] pi=prob;

     /*Default values*/
     for {j in SampleID} ps[j]=1/&nSamp;

     /*Constraints*/
     con Probabilite: sum{i in SampleID}  ps[i]  =1;
     con InclProb{i in UnitID}:  sum{<(i),j> in UnitSampleID } incl[i,j] * ps[j] = pi[i];

     /*Minimisation*/
     for {j in SampleId, k in consId}  wgtSum[j,k]= sum{<i,(j)> in UnitSampleID } ((incl[i,j]/pi[i]) * Coef[i,k] );
     min Distance= sum{i in sampleId,j in consId} ((wgtSum[i,j]-Bound[j])**2)*ps[i]  ;
     solve ;

     /*print _con_.name _con_.lb _con_.body _con_.ub ;*/

     /*Output*/
     create data &probSelOut    from [SampleId] prob=ps;
    quit;


    /*   */
    proc delete data= __UnitConsCoef__ __UnitSampleIncl__
        %do i = 1 %to &NbrVars; %SCAN(&VarList,&i) %end;
        %do i = 1 %to &nSamp; %scan(&SampList,&i) %end;
    ;run;
    /*%put %scan(&_OROPTMODEL_,2);*/

%mend OptProbSel;

%macro BuildSampPlan(PartialSample=,plan=,SampleList=);
/*
intput
PartialSample : File
    UnitId  :
    Incl    : numeric 0<=incl<=1

output
	plan
	sampleId prob
	SampleList
		
*/
    %local nobs;
    data __Psamp__;
        set &PartialSample(rename=(incl=prob));
        if prob <1 and prob>0;
    run;
	/*%put __Psamp__ %nobs(__Psamp__);*/

    %let nobs=%Nobs(__Psamp__);
	

    /*make a list of samples*/
    proc clp FINDALLSOLNS out=&plan;
        var (unit1-unit&nobs ) = [0,1];
    quit;
	/*%put &plan %nobs(&plan);*/

    data &plan;
        set &plan;
        sampleId=catt("sample",_N_);
    run;
    proc transpose data=&plan out=T&PLan(drop=_name_);
		id sampleId;
	run;
    data __Ens__;
        set __Psamp__(drop=prob);
        set T&PLan;
    run;
	/*%put  __Ens__ %nobs( __Ens__);*/

    /*adjust totals*/
    data __InterCoefProb__;
        merge __Psamp__(in=ina)  &ConsCoef;
        by unitId;
        if ina;
    run;
    data __InterCoef__;
        set  __InterCoefProb__(drop=prob);
    run;

    %let CoefVar = %Varnames( &ConsCoef);
    %let CoefVar = %ListReplace(&CoefVar,search=%scan(&CoefVar,1),replace=%str( ));
    proc means data= __InterCoefProb__ noprint;
        weight prob ;
        var &CoefVar;
        output out=__InterSum__(drop= _type_ _freq_) sum=&CoefVar;
    run;

    data __InterSum__;
        set __InterSum__;
    run;
    proc transpose data=__InterSum__ out=__NewBounds__(rename=(col1=Bound _name_=ConsId));run;

    %OptProbSel(__Ens__,__Psamp__, __InterCoef__, __NewBounds__, &plan);

    data &SampleList;
        set __Ens__;
    run;


   /* proc delete data=__Ens__ __Psamp__ __InterCoef__ __InterCoefProb__ __InterSum__ __NewBounds__ T&PLan ;run;*/

%mend BuildSampPlan;

%macro SelectSample(Plan=,Samples=, seed=, dataOut=);
/*select a sample from a plan*/
/*
Plan : File
    SampleId    : SampleId in (Sample1.. SampleN)
    prob        : numeric 0<=prob<=1

Samples : File
    unitId              :
    (Sample1 ..SampleN) : inclusion indicator 0 or 1

dataOut : File
    unitId  :
*/

/*
    proc surveyselect noprint seed= &seed data=&Plan method=pps n=1 out=__SelectedSample__;
        size prob ;
    run;
*/
	proc sort data= &plan; by prob; run;

	%let cumprob=  %sum(&plan,prob);
	data &plan;
		set &plan end=eof;
		retain cumprob 0;
		cumprob= cumprob+prob;
		pps= cumprob/&cumprob;
		pps= min(pps,1);
	run;
	proc sort data= &plan; by pps; run;
	data _null_;
		set &plan end=eof;
		retain random;
		if _n_=1 then do;
			call streaminit(&seed);
			random = rand('uniform');
		end;
		if random < pps then do;
			call symputx("SelectedSample",sampleId);
		end;
		else do;
			if eof then do;
				call symputx("SelectedSample",sampleId);
			end;
		end;
	run;
	/*
	data _null_;
		set __SelectedSample__;
		call symputx("SelectedSample",sampleId);
	run;*/
	data &DataOut(rename=(&SelectedSample=Sample));
		set &Samples(keep= unitId &SelectedSample);
	run;
	/*
	proc delete data=__SelectedSample__ ;run;
	*/

%mend SelectSample;

%macro Calibration(inclProb,ConsCoef=,ConsDef=,DataOut=);
/*
<inclProb> : File
    unitId      : numeric of string
    prob        : numeric 0<=prob<=1

ConsCoef : File
    unitId      : numeric of string
    (Var1..VarN):

ConsDef : File
    consId      : list of varaiables containing the coefficients (Var1..VarN)
    type        : string, type in (eq, lt, gt, = , <,>)
    Bound       : numeric

DataOut : File
    untiId      :
    prob        : numeric 0<=prob<=1

*/

    %put;
    %put -----------;
    %put Calibration;
    %put -----------;
    %put;

    %local unitIdType consId Bounds types Varnames Nunits Nvar options start i;

    %let Start = %time();
    %let options = %saveOptions();
    option nonotes nosource;

	/*
    %verif(&inclProb, &ConsCoef,&ConsDef);
	*/

    %let unitIdType = %varType(data=&ConsCoef,var=unitId);
    %if &unitIdType= N %then %do;
        %let unitIdType= num;
    %end;
    %else %do;
        %let unitIdType= str;
    %end;

    %let consid = %listData(data=&ConsDef,var=consId);
    %let Bounds = %listData(data=&ConsDef,var=Bound);
    %let types  = %listData(data=&ConsDef,var=type);
    %let Nunits= %Nobs(&ConsCoef);

    %let VarNames = %GetVarNames(data=&ConsCoef);
    %let VarNames = %ListReplace(&VarNames,search=%scan(&varnames,1),replace= %Str());
    %let Nvar = %sysfunc(countw(&varNames,%str( ))); 

    %put Number of equations 	: &Nvar;
    %put Equation type  	: &types;
    %put Number of units   	: &Nunits;

    ods html close;
    ods output printTable=constraints;
    proc optmodel PRINTLEVEL=0 ;
        /*Declare variables*/
        set <&unitIdType> UnitID ;
        %do i=1 %to &Nvar;
            num Coef&i {UnitID};
        %end;
        %if (&inclProb ne) %then %do;
            num initp{unitId};
        %end;
        var p{UnitID} >=0 <=1;

        /*Read data*/
        %if (&inclProb ne) %then %do;
            read data &inclProb into unitId=[unitId] initp=prob;
        %end;
        read data &ConsCoef into unitId=[unitId] %do i = 1 %to &Nvar;  Coef&i=%scan(&varNames,&i) %end; ;

        /*default values*/
        for {i in unitID} p[i]=0;

        /*Constraints*/
        %do i=1 %to &NVar;
            con cons&i: sum{i in UnitID}  coef&i[i]*p[i]  %scan(&types,&i) %scan(&bounds,&i,%str( )) %str(;)
        %end;

        /*Minimisation*/
        %if (&inclProb ne) %then %do;
            min Distance= sum{i in UnitID} (p[i]-initp[i])**2    ;
        %end;
        %else %do;
            impvar meanprob = sum{i in UnitID} p[i]/&Nunits;
            min Distance= sum{i in UnitID} (p[i]-meanprob)**2    ;
        %end;

        solve with NLP / tech= activeset;
        print _con_.name _con_.body ;

        /*Output*/
        create data &dataOut  from [UnitID]   prob=p;
    quit;
    ods output close;

    data &dataOut;
        set &dataOut;
        prob= round(prob,0.001);
    run;

    option &options;

    %put;
    %put Start at &startTime;
    %put Ended at %Time();
    %put Duration     %time(&Start);
%mend Calibration;

%macro Cube(Pop=,
			cons=,
			alloc=,
			dataOut=,
			land=opt,
			seed=1);
/*
Pop	:   	unitId prob

Pop	:   	unitId stratId
Alloc	:	stratId size

cons	:   	unitId var1...<varN>


consDef	:   	consId type bound
            	such as consId in {var1...varN}
            	such as type in (=,eq) pour le moment, plus tard type in (<,<=,=,>,>=) ou (lt,le,eq,gt,ge)
*/

    %put;
    %put -----------------;
    %put Balanced sampling;
    %put -----------------;
    %put;

    %local startTime options types Nunits VarNames Nvar ConsCoef PopOut nround nconsDrop;

    %let StartTime	= %Time();
    %let options 	= %saveOptions();
    options nonotes nomprint;
    options linesize=120;
    ods html close;
    ods listing;

	%if (not %sysfunc(exist(&Pop))) %then %do;
		%put ERROR: Pop= doit être fourni;
		%goto exit;
	%end;
	%else %do;
		%if %varExist(&Pop, unitId ) eq 0 %then %do;
			%put ERROR: Pop= doit contenir la variable UNITID ;
			%goto exit;
		%end;
	%end;


	%if (&alloc eq ) %then %do;
		%if %varExist(&Pop, prob ) eq 0 %then %do;
			%put ERROR: Pop= doit contenir la variable PROB ;
			%goto exit;
		%end;
	%end;
	%else %do;
		%if (not %sysfunc(exist(&Alloc))) %then %do;
			%put ERROR: Alloc= existe pas;
			%goto exit;
		%end;
		%else %do;
			%if %varExist(&Pop, stratId ) eq 0 %then %do;
				%put ERROR: Pop= doit contenir la variable STRATID ;
				%goto exit;
			%end;
			%if %varExist(&Alloc, stratId size ) eq 0 %then %do;
				%put ERROR: Alloc= doit contenir les variables STRATID et SIZE ;
				%goto exit;
			%end;
		%end;
	%end;

	
	%if ((&alloc eq )   or  ((&alloc ne ) and (not %sysfunc(exist(&Alloc)))))   and   (not %sysfunc(exist(&cons)) or (&cons eq ))%then %do;
		%put ERROR: cons= doit être fourni;
		%goto exit;
	%end;
	%else %do;
		%if %varExist(&cons, unitId) eq 0 %then %do;
			%put ERROR: cons= doit contenir la variable UNITID ;
			%goto exit;
		%end;
	%end;


	/*%if (not %sysfunc(exist(&consDef))) %then %do;
		%put ERROR: consDef= doit être fourni;
		%goto exit;
	%end;
	%else %do;
		%if %varExist(&consDef, consId type bound) eq 0 %then %do;
			%put ERROR: consDef= doit contenir les variables CONSID, TYPE et BOUND ;
			%goto exit;
		%end;
	%end;*/


	%if (%upcase(&land) ne OPT) and (%upcase(&land) ne DROP) %then %do;
		%put ERROR: option d atterissage non valide (OPT ou DROP);
		%goto exit;
	%end;
	%if (&seed <0) %then %do;
		%put ERROR: seed doit être plus grand que 0;
		%goto exit;
	%end;

	%let Nunits		= %Nobs(&Pop);
	%let consCoef 	= __LinCons;
	%let PopOut		= __Pop;

	%BuildCons(Pop=&pop, Alloc=&alloc, ConsIn=&cons, consOut=&ConsCoef, PopOut=&popOut);

    	/*%let types  = %listData(data=&ConsDef,var=type);*/

	/*transform seed*/
	data _null_;
		call streamInit(&seed);
		newseed=ceil( rand('uniform')* (10**9) ) ;
		call symputx('seed',newseed) ;
	run;

    %let VarNames 	= %VarNames(data=&ConsCoef);
    %let VarNames 	= %ListReplace(&VarNames,search=%scan(&varnames,1),replace= %Str());
    %let Nvar	 	= %sysfunc(countw(&varNames,%str( ))); 
	
    %put Nombre d équations : &Nvar;
    %put Nombre d unitées   : &Nunits;

	/*
	%put Type d équations   : &types;
	*/

	/*on mélange les unités pour réduire les problèmes reliés à l imprécision numérique dans la phase de vol*/
	data &PopOut;
		if _N_=1 then do;
			call streaminit(&seed);
		end;
		set &PopOut;
		__order = rand('uniform');
	run;
	data &ConsCoef;
		merge &ConsCoef &popOut(keep =unitId __order);
		by unitId;
	run;
	proc sort data=&PopOut out=&PopOut( keep= unitId prob)	; by __order;run;
	proc sort data=&ConsCoef out= &ConsCoef(drop= __order); by __order;run;

    	/*Flight phase */
	/*%Flight(inclProb=&PopOut,consCoef=&ConsCoef,DataOut=&DataOut,land=&land);*/
	%FastFlight(inclProb=&PopOut,consCoef=&ConsCoef,DataOut=&DataOut,land=&land);

    data &dataOut(rename=(incl=sample)) __FlightOut ;
        set &PopOut(keep=unitid) ;
        set &dataOut;
    run;

	/*remettre les unités en ordre*/
	proc sort data=&PopOut 			; 	by unitId;	run;
	proc sort data=&ConsCoef 		; 	by unitId;	run; 
	proc sort data=&DataOut 		; 	by unitId;	run; 
	proc sort data=__FlightOut  	; 	by unitId;	run; 


	/*compter les valeurs non arrondies*/
	%let nround=0;
	data _null_;
		set __FlightOut(where=(incl > 0 and incl <1 or missing(incl)))  end = eof;
		if eof then call symputx('nround',_N_);
	run;
	data __FlightOut(drop=prob);
		merge __FlightOut &Popout;
		by unitId;
		if missing(incl) then incl=prob;
	run;

    	/*Landing Phase*/
	%if %upcase(&land)=OPT %then %do;
		%if &nround >0 %then %do;
	   		%BuildSampPlan(partialSample=__FlightOut,plan=__Plan,SampleList=__SList);

			%put;
			%put %str(    )Atterissage optimal;
			%put %str(    )Liste de %Nobs(__Plan) échantillons;
			/*%put %str(    )&nround;*/
			
		    %SelectSample(Plan=__Plan,Samples=__SList, seed=&seed, dataOut=&DataOut);
		    data &DataOut(keep= unitId sample);
		        merge &DataOut __FlightOut;
		        by unitId;
		        if missing(sample) then sample=incl;
		    run;
			proc delete data=  __SList __Plan;run;
		%end;
	%end;
	%if %upcase(&land)=DROP %then %do;
		%put;
		%put %str(    )Atterissage par élimination;
		%put %str(    )&NConsDrop contraintes éliminées;
		data &DataOut(keep=unitId sample);
	        set __FlightOut(rename=(incl=sample));
	    run;
	%end;

	/*proc delete data= __FlightOut __linCons __pop ; run;*/

    ods listing close;
	%exit:
    options &options;

    %put;
    %put Start at &startTime;
    %put Ended at %Time();
    /*%put Duration     %Time(&startTime);*/
    %put;
%mend cube;



