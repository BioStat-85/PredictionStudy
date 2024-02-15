

*************;
* Loading Data *;
*************;

%INCLUDE "&ProgramFolder.\ADNI_DataOrigin.sas";



**********************;
* Create Analysis Dataset *;
**********************;

DATA _Data0; SET DataOrigin;
  IF bl_Diagnostic ="CN" THEN bl_Diagnostic ="NC";
  IF bl_&Outcome. = "." THEN DELETE;
  IF bl_Diagnostic = "" THEN DELETE;
  IF AGE  = "." THEN DELETE;
  IF PTGENDER = "" THEN DELETE;
  IF PTEDUCAT = "." THEN DELETE;
  IF APOE4 = "." THEN DELETE;
  IF bl_PIB = "." AND bl_AV45 = "." AND bl_ABETA = "." THEN DELETE;
  IF APOE4 >= 1 THEN APOE = "Posi";
  ELSE APOE = "Nega";
  IF bl_PIB > 1.5 OR bl_AV45 > 1.11 OR . < bl_ABETA < 192 THEN AB = "Posi";
  ELSE AB = "Nega";
  DifOutcome = &Outcome. - bl_&Outcome.;
RUN;

PROC UNIVARIATE DATA = _Data0 NOPRINT;
  BY ID;
  VAR &Outcome.;
  OUTPUT OUT = _N
  N = N_Out;
RUN;

PROC SORT DATA = _Data0; BY ID; RUN;
PROC SORT DATA = _N; BY ID; RUN;

DATA Data; MERGE _Data0 _N;
  BY ID;
  IF N_Out <= %EVAL(&ExNumOut. + 1) THEN DELETE;
RUN;


%MACRO Temp;
%IF &AnalysisSet. = APOEPosi %THEN %DO;
DATA Data; SET _Data(WHERE = (APOE = "Posi")); RUN;
%END;

%IF &AnalysisSet. = ABETA_Posi %THEN %DO;
DATA Data; SET Data(WHERE = (AB = "Posi")); RUN;
%END;

%IF &AnalysisSet. = APOENega %THEN %DO;
DATA Data; SET Data(WHERE = (APOE = "Nega")); RUN;
%END;

%IF &AnalysisSet. = ABETA_Nega %THEN %DO;
DATA Data; SET Data(WHERE = (AB = "Nega")); RUN;
%END;
%MEND;
%Temp;


DATA Data; SET Data;
  IF bl_Diagnostic = "NC" OR bl_Diagnostic = "SMC" THEN DELETE;
  IF bl_Diagnostic = "LMCI" OR bl_Diagnostic = "EMCI" THEN bl_Diagnostic = "MCI";

  IF bl_CDRSB < 0.5 OR bl_CDRSB >8.5 THEN DELETE;
  IF AGE < 50 OR AGE >90 THEN DELETE;
  IF bl_MMSE <22 THEN DELETE;

  IF bl_Diagnostic = "MCI" THEN DELETE;

RUN;



* Limited to long-term follow-up patients *;
DATA _Data; SET Data;
  IF CDRSB = . THEN DELETE;
RUN;

PROC UNIVARIATE DATA = _Data NOPRINT;
  VAR Visit_year;
  BY ID;
  OUTPUT OUT = _Out
  MAX = MaxVisit;
RUN;

DATA _Out; SET _Out;
  IF MaxVisit < %SYSEVALF(&LongFollow.) THEN Check =1;
  ELSE Check = 0;
  KEEP ID Check;
RUN;


DATA Data; MERGE _Data _Out; BY ID;
  IF Check = 1 THEN DELETE;
RUN;

DATA DataAnalysis; SET Data; RUN;

*******************************************************;
* Calculate the starting point for estimation of the prediction curve *;
*******************************************************;

DATA _Temp; SET DataAnalysis(WHERE = (Visit_year = 0 AND bl_Diagnostic = "AD")); RUN;

%MACRO Temp;

%IF &Outcome. = MMSE %THEN %DO;

PROC UNIVARIATE DATA = _Temp NOPRINT;
  VAR &Outcome.;
  OUTPUT OUT = _Out
  Max = Max
  Mean = Mean;
RUN;

DATA _Out; SET _Out;
  IF Max = 30 THEN DO; Max = 29; END;
  CALL SYMPUTX('BaseOutcome', Max, "G");
RUN;

%END;

%IF &Outcome. = CDRSB %THEN %DO;

PROC UNIVARIATE DATA = _Temp NOPRINT;
  VAR &Outcome.;
  OUTPUT OUT = _Out
  Min = Min
  Mean = Mean;
RUN;

DATA _Out; SET _Out;
  IF Min = 0 THEN DO; Min = 0.5; END;
  CALL SYMPUTX('BaseOutcome', Min, "G");
RUN;

%END;

%IF &Outcome. = ADAS13 %THEN %DO;

PROC UNIVARIATE DATA = _Temp NOPRINT;
  VAR &Outcome.;
  OUTPUT OUT = _Out
  Min = Min
  Mean = Mean;
RUN;

DATA _Out; SET _Out;
  IF Min = 0 THEN DO; Min = 1; END;
  CALL SYMPUTX('BaseOutcome', Min, "G");
RUN;

%END;

%MEND;
%Temp;

PROC DATASETS LIB = work NOPRINT; DELETE _:; RUN; QUIT;







****************************;
* Application of Hirakawa method *;
****************************;

* Step I: Clustering of decline rate of cognitive test score in individuals *;

*-----------------------------*;
* Estimation of individual decline rate *;
*-----------------------------*;

ODS OUTPUT SolutionF = _Fixeff;
ODS OUTPUT SolutionR = _Raneff;

PROC MIXED DATA = DataAnalysis METHOD = ml EMPIRICAL;
  CLASS ID bl_Diagnostic(REF='AD'); * # *;
  MODEL DifOutcome =  bl_Diagnostic*Visit_year / NOINT S;
  RANDOM Visit_year / TYPE = UN SUBJECT = ID S;
RUN;

DATA _Fixeff ; SET _Fixeff; 
  Fixed_effect = Estimate;
  KEEP bl_Diagnostic Fixed_effect;
RUN;;

DATA _Disease_id; SET DataAnalysis(WHERE = (Visit_year = 0));
  KEEP bl_Diagnostic ID;
RUN;

PROC SORT DATA = _Disease_id; BY bl_Diagnostic; RUN;
PROC SORT DATA = _Fixeff; BY bl_Diagnostic; RUN;

DATA _Fixeff1; MERGE _Disease_id _Fixeff; 
  BY bl_Diagnostic;
RUN;

DATA _Raneff1; SET _Raneff(WHERE = (Effect = "Visit_year"));
  Random_Effect = Estimate;
  KEEP ID Random_Effect;
RUN;

PROC SORT DATA = _Fixeff1; BY ID; RUN;
PROC SORT DATA = _Raneff1; BY ID; RUN;

DATA _Est; MERGE _Fixeff1 _Raneff1;
  BY ID; 
  Tilde_Beta = Fixed_effect + Random_effect; 
RUN;





*------------------------------------------*;
* Modeling individual inclinations and outcome means *;
*-------------------------------------------*;

DATA _DataAnalysis; SET DataAnalysis;
  IF Visit_year > &Hist. THEN DELETE;
RUN;

ODS OUTPUT SolutionF = _Fixeff_Cate;
ODS OUTPUT SolutionR = _Raneff_Cate;

PROC MIXED DATA = _DataAnalysis METHOD = ml EMPIRICAL;
  CLASS ID bl_Diagnostic(REF='AD'); * # *;
  MODEL DifOutcome =  bl_Diagnostic*Visit_year / NOINT S;
  RANDOM Visit_year / TYPE = UN SUBJECT = ID S;
RUN;

DATA _Fixeff_Cate ; SET _Fixeff_Cate; 
  Fixed_effect = Estimate;
  KEEP bl_Diagnostic Fixed_effect;
RUN;;

DATA _Disease_id_Cate; SET _DataAnalysis(WHERE = (Visit_year = 0));
  KEEP bl_Diagnostic ID;
RUN;

PROC SORT DATA = _Disease_id_Cate; BY bl_Diagnostic; RUN;
PROC SORT DATA = _Fixeff_Cate; BY bl_Diagnostic; RUN;

DATA _Fixeff1_Cate; MERGE _Disease_id_Cate _Fixeff_Cate; 
  BY bl_Diagnostic;
RUN;

DATA _Raneff1_Cate; SET _Raneff_Cate(WHERE = (Effect = "Visit_year"));
  Random_Effect = Estimate;
  KEEP ID Random_Effect;
RUN;

PROC SORT DATA = _Fixeff1_Cate; BY ID; RUN;
PROC SORT DATA = _Raneff1_Cate; BY ID; RUN;

DATA _Est_Cate; MERGE _Fixeff1_Cate _Raneff1_Cate;
  BY ID; 
  Tilde_Beta = Fixed_effect + Random_effect;
RUN;



* ÉJÉeÉSÉäâª *;
*###########################*
-2<=Tilde_Beta<-1 ÅÀ Cate = -2
-1<=Tilde_Beta<0 ÅÀ Cate = -1
0<=Tilde_Beta<1 ÅÀ Cate = 0
1<=Tilde_Beta<2 ÅÀ Cate = 1
*###########################*;

DATA _Est_Cate; SET _Est_Cate;
  IF Tilde_Beta < 0 THEN DO;
    CateG = INT(Tilde_Beta) - 1;
  END;
  ELSE CateG = INT(Tilde_Beta);
RUN;


/*%MACRO Temp;*/
/**/
/*%IF &Outcome. = MMSE %THEN %DO;*/
/*  PROC SORT DATA = _Est_Cate; BY CateG; RUN;*/
/*  DATA _Est_Cate; SET _Est_Cate;*/
/*    IF CateG = -1 THEN CateG = -2;*/
/*    IF CateG = -3 THEN CateG = -4;*/
/*    IF CateG < -4 THEN CateG = -6;*/
/*  RUN;*/
/*%END;*/
/**/
/**/
/*%IF &Outcome. = CDRSB %THEN %DO;*/
/*  PROC SORT DATA =_Est_Cate; BY DESCENDING CateG; RUN;*/
/*  DATA _Est_Cate; SET _Est_Cate;*/
/*    IF CateG > 2 THEN CateG = 2;*/
/*  RUN;*/
/*%END;*/
/**/
/**/
/*%IF &Outcome. = ADAS13 %THEN %DO;*/
/*  PROC SORT DATA =_Est_Cate; BY DESCENDING CateG; RUN;*/
/*  DATA _Est_Cate; SET _Est_Cate;*/
/*    %DO i = 0 %TO 20 %BY 3;*/
/*      IF CateG = &i. THEN CateG = CateG+2;*/
/*    %END;*/
/*    %DO i = 1 %TO 20 %BY 3;*/
/*      IF CateG = &i. THEN CateG = CateG+1;*/
/*    %END;*/
/*    IF CateG > 8 THEN CateG = 8;*/
/*  RUN;*/
/*%END;*/
/**/
/*%MEND;*/
/*%Temp;*/


* In the paper, we estimated the longitudinal trajectory for the group with decline slope >=0 in the AD patients 
because of the small number of patients in the ADNI subpopulation *;
DATA _Est_Cate; SET _Est_Cate;
  IF CateG >= 0 THEN CateG = 0;
RUN;


DATA Est_Cate; SET _Est_Cate;
  KEEP ID CateG;
RUN;

PROC SORT DATA = _Est; BY ID; RUN;
PROC SORT DATA = Est_Cate; BY ID; RUN;

DATA Est; MERGE _Est Est_Cate; RUN;


DATA _CateG; SET Est; KEEP ID CateG; RUN;

PROC SORT DATA = _CateG; BY ID; RUN;
PROC SORT DATA = Data; BY ID; RUN;
PROC SORT DATA = DataAnalysis; BY ID; RUN;

DATA Data; MERGE Data _CateG; BY ID; RUN;
DATA DataAnalysis; MERGE DataAnalysis _CateG; BY ID; RUN;

PROC DATASETS LIB=work NOPRINT; DELETE _:; RUN; QUIT;





* Step II: Modeling relationship between cognitive decline rate and average scores *;

*-----------------------*;
* Estimation of average score *;
*-----------------------*;

PROC UNIVARIATE DATA = DataAnalysis NOPRINT;
  BY ID;
  VAR &Outcome.;
  OUTPUT OUT = Mean_out
  MEAN = Mean;
RUN;

PROC SORT DATA = DataAnalysis; BY ID; RUN;
PROC SORT DATA = Mean_out; BY ID; RUN;
PROC SORT DATA = Est; BY ID; RUN;

DATA DataAnalysis; MERGE DataAnalysis Mean_out Est;
  BY ID;
RUN; 

*----------------------*;
* Estimation of parameter É¡ *;
*----------------------*;

%MACRO Temp;

DATA DataAnalysis; SET DataAnalysis;
  %IF &Outcome. = MMSE %THEN %DO;
    RMean = ((30-Mean)*Mean)/30;
  %END;
  %IF &Outcome. = CDRSB %THEN %DO;
    RMean = ((Mean-18)*Mean)/18;
  %END;
  %IF &Outcome. = ADAS13 %THEN %DO;
    RMean = ((Mean-85)*Mean)/85;
  %END;
RUN;

%MEND;
%Temp;


*-------------------------*;
* Estimation of model parameter *;
*-------------------------*;

DATA _Temp; SET DataAnalysis(WHERE=(Visit_year=0)); RUN;

PROC UNIVARIATE DATA = _Temp NOPRINT;
  VAR CateG;
  OUTPUT OUT = _Cate
  MIN = Min
  MAX = Max;
RUN;

DATA _Cate; SET _Cate;
  CALL SYMPUTX('Min_Cate', Min, 'G');
  CALL SYMPUTX('Max_Cate', Max, 'G');
RUN;

PROC FREQ DATA = Data;
  TABLE Visit_year;
  ODS OUTPUT OneWayFreqs = Visit;
RUN;

DATA _Visit;  SET Visit;
  KEEP F_Visit_year;
RUN;

DATA _Visit; SET _Visit NOBS = NOBS;
  CALL SYMPUTX('NObs_Visit', NOBS, "G");
RUN;


PROC TRANSPOSE DATA = _Visit OUT = _Visit1; VAR F_Visit_year;RUN;

%MACRO Temp;
DATA _Visit2; LENGTH COL $250; SET _Visit1;
  COL = COL1;
  %DO V = 2 %TO &NObs_Visit.;
   COL = catx("," , COL, COL&V.) ;
  %END;
RUN;
%MEND;
%Temp;

DATA _Visit2; SET _Visit2;
  CALL SYMPUTX('Visit', COL, "G");
RUN;


PROC SORT DATA = _Temp; BY CateG; RUN;

ODS TRACE ON;
PROC GENMOD DATA = _Temp;
  BY CateG;
  MODEL Tilde_Beta = RMean / NOINT DIST = NORMAL LINK = IDENTITY;
  BAYES SEED = 1234 OUTPOST = Gamma_est
             COEFFPRIOR = NORMAL NBI = 2000 NMC = 10000 THINNING = 10
			 STATISTICS(ALPHA = &Interval.) = (SUMMARY INTERVAL)
;
  ODS OUTPUT PostSummaries = _PostSum;
  ODS OUTPUT PostIntervals = _PostInt;
RUN;
ODS TRACE OFF;

DATA _PostSum; SET _PostSum;
  IF Parameter ^= "RMean" THEN DELETE;
  Traj = 1;
  RENAME Mean = Estimate;
  KEEP CateG Mean Traj;
RUN;

DATA _PostInt1; SET _PostInt;
  IF Parameter ^= "RMean" THEN DELETE;
  KEEP CateG HPDLower HPDUpper;
RUN;

PROC TRANSPOSE DATA = _PostInt1 OUT = _PostInt2; BY CateG; RUN;

DATA _PostInt2; SET _PostInt2;
  IF _NAME_ = "HPDLower" THEN Traj = 2;
  IF _NAME_ = "HPDUpper" THEN Traj = 3;
  RENAME Col1 = Estimate;
  KEEP CateG Col1 Traj;
RUN;

DATA Gamma_Est; SET _PostSum _PostInt2; 
  IF CateG < 0 THEN DELETE;
RUN;

PROC SORT DATA = Gamma_Est; BY CateG; RUN;





* Step III: Prediction of long-term trajectory of cognitive test scores *;

* Maximum or minimum value for outcome -*
  MMSE: 30points
  CDRSB: 0points
  ADAS13: 0points
*----------------------------------*;
%MACRO Temp;
DATA _MaxOutcome;
  %IF &Outcome. = MMSE %THEN %DO;
    MaxOutcome = 30;
  %END;
  %ELSE %DO;
    MaxOutcome = 0;
  %END;
  CALL SYMPUTX('MaxOutcome', MaxOutcome, 'G');
RUN;
%MEND;
%Temp;

%MACRO Temp;
DATA _CateG;
  %IF &Outcome. = MMSE %THEN %DO;
    MinC = &Min_Cate.;
	MaxC = -2;
	ByC = 2;
  %END;
  %IF &Outcome. = CDRSB %THEN %DO;
    MinC = 0;
	MaxC = &Max_Cate.;
	ByC = 1;  
  %END;
  %IF &Outcome. = ADAS13 %THEN %DO;
    MinC = 2;
	MaxC = &Max_Cate.;
	ByC = 3;  
  %END;
  CALL SYMPUTX('MinC', MinC, 'G');
  CALL SYMPUTX('MaxC', MaxC, 'G');
  CALL SYMPUTX('ByC', ByC, 'G');
RUN;
%MEND;
%Temp;

DATA TrajALL; STOP; RUN;
DATA Data_DisTime; STOP; RUN;

%MACRO Temp;

%DO CateG = &MinC. %TO &MaxC. %BY &ByC.;

%DO Traj = 1 %TO 3;

DATA _Gamma_Est; SET Gamma_Est(WHERE = (CateG = &CateG. AND Traj = &Traj.)); RUN;

PROC IML;
  USE _Gamma_Est;
  READ ALL VAR {Estimate} INTO x[COLNAME = NumerNames];
  CLOSE _Gamma_Est;

  x = t(x[1:2,]);

  START fun(t, outcome) GLOBAL(x);
    %IF &Outcome. = MMSE %THEN %DO; v = x[1]*(((30-outcome)*outcome)/30); %END;
    %IF &Outcome. = CDRSB %THEN %DO; v = x[1]*(((outcome-18)*outcome)/18); %END;
    %IF &Outcome. = ADAS13 %THEN %DO; v = x[1]*(((outcome-85)*outcome)/85); %END;
    RETURN(v);
  FINISH;

  c = &BaseOutcome.;

  DO DisTime=1 TO &maxtau. BY (1/12);
    h = {1E-12 1 1E-5};
    t = do(0, DisTime, 1/12);
    CALL ODE(r1, "FUN", c, t, h) ;
  END;

  Traj = t` || (c` // r1`);

  CREATE Traj from Traj[c={"DisTime" "Pred"}]; 
  APPEND FROM Traj; 
  CLOSE Traj;
QUIT;


DATA Traj; SET Traj;
  %IF &Outcome. = MMSE %THEN %DO;
    IF Pred > &MaxOutcome. THEN Pred = &MaxOutcome.;
  %END;
  %ELSE %DO;
    IF Pred < &MaxOutcome. THEN Pred = &MaxOutcome.; 
  %END;
RUN;


DATA Traj; SET Traj;
  CateG = &CateG.;
  Traj = &Traj.;
RUN;


DATA TrajALL; SET TrajALL Traj; RUN;

%END;

%END;

%MEND;
%Temp;




PROC FORMAT; 
  VALUE Trajf 1 = "Median" 2 = "HPDLower" 3 = "HPDUpper";
RUN;

DATA _Gamma_est; SET Gamma_est;
 KEEP CateG Traj Estimate;
 FORMAT Traj Trajf.;
RUN;

DATA TrajALL; MERGE TrajALL _Gamma_est; BY CateG Traj; RUN;
DATA Data_ADNI; SET Data; RUN;
