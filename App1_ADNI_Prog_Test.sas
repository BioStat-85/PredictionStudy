
* Note 
 The user can execute this program by inputting only "Input Parameters".
*;

* Output Datasets
  Trajectory_for_Test_ADNI: Predicted trajectories of cognitive test score for moderate, intermediate, and rapid cognitive decline groups for Bootstrap test
  PValue_ADNI: P-value of Bootstrap hypothesis test for AUC and time to AD onset
*;


OPTIONS NONOTES NOSOURCE DKRICOND = NOWARN;
ODS LISTING CLOSE;
ODS HTML CLOSE;


*****************
* Input Parameters *
*****************;

* Folder pass where this program is saved *;
%LET ProgramFolder = ;

* Folder pass where ADNI data is saved *;
%LET DataFolderADNI = ;
LIBNAME Data "&DataFolderADNI.";

* ADNI DataSet *;
%LET ADNIData = ;


* Outcome ---------------*
MMSE or CDRSB or ADAS13
*-----------------------*;
%LET Outcome = ;


* Analysis Population----------*
  All population: ALL
  APOEe4-positive: APOEPosi
  ABETA-positive: ABETA_Posi
  APOEe4-negative: APOENega
  ABETA-negative: ABETA_Nega
*--------------------------*;
%LET AnalysisSet = ABETA_Posi;



* AD onset or longitudinal progression *
  Oncet: Oncet
  Progression: Prog
*------------------------------*;
%LET App1 = Oncet;
/*%LET App1 = Prog;*/


* Milestone of AD onset *
Example
  MMSE: 23points
  CDRSB: 4.5points
  ADAS13: 30points
*-------------------*;

%LET TargetOutcome = ;


* Folder to save the results *;
%LET ResultFolder = ;
%LET rc=%SYSFUNC(DCREATE(&ResultFolder., &ProgramFolder.\));
LIBNAME Result "&ProgramFolder.\&ResultFolder.";

* Number of measurement points after baseline ------------------------------*
  Excluding subjects with post-baseline measurement time points below "ExNumOut"
*-------------------------------------------------------------------*;
%LET ExNumOut = 1;


* Period for which the forecast curve is calculated (in years) *;
%LET MaxTau = 50; 



***************************************************
***** The user does not need to modify following codes. *****
***************************************************;



* Paticipants categorization factor -*
  Decline slope: Beta 
  APOE e4: ProgA
*----------------------------*;
%LET WCate = ProgA;

%MACRO Temp;
DATA _ProgFactor;
  %IF &WCate. = ProgA %THEN %DO;
    ProgFactor = "APOE";
    CALL SYMPUTX('ProgFactor', ProgFactor, 'G');
  %END;
RUN;
%MEND;
%Temp;

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

%MACRO Temp;
DATA _Data0; SET _Data0;
  %IF &App1. = Oncet %THEN %DO;
    IF bl_Diagnostic ^="EMCI" AND bl_Diagnostic ^="LMCI" THEN DELETE;
  %END;
  %ELSE %DO;
    IF bl_Diagnostic ^="AD" THEN DELETE;
  %END;
RUN;
%MEND;
%Temp;

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
DATA Data; SET Data(WHERE = (APOE = "Posi")); RUN;
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

DATA DataAnalysis; SET Data; RUN;

%MEND;
%Temp;



PROC DATASETS LIB = work NOPRINT; DELETE _:; RUN; QUIT;



*******************************************************;
* Calculate the starting point for estimation of the prediction curve *;
*******************************************************;


%MACRO Temp;

DATA _Temp; SET DataAnalysis(WHERE = (Visit_year = 0)); 
  %IF &App1. = Oncet %THEN %DO;
    IF bl_Diagnostic ^="EMCI" THEN DELETE;
  %END;
  %ELSE %DO;
    IF bl_Diagnostic ^="AD" THEN DELETE;
  %END;
RUN;


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
  CLASS ID bl_Diagnostic;
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

DATA _Cate; SET DataAnalysis(WHERE = (Visit_year = 0)); 
  KEEP ID APOE;
RUN;

PROC SORT DATA = _Cate; BY ID; RUN;
PROC SORT DATA = _Est; BY ID; RUN;

DATA Est; MERGE _Est _Cate;
  BY ID;
  IF &ProgFactor. = APOE THEN DO;
    IF APOE = "Posi" THEN CateG = 1;
	ELSE CateG = 0;
  END;
RUN;

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
* Estimation of parameter ƒÁ *;
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



DATA TrajBoot; STOP; RUN;
DATA GammaBoot; STOP; RUN;

%MACRO Boot;

%IF &Seed. =1 %THEN %DO;

DATA Temp1; SET _Temp; 
  APOE_Boot = APOE;
RUN; 

%END;


%IF &Seed. >=2 %THEN %DO;

DATA Temp1; STOP; RUN;

%MACRO Temp(APOE);
DATA _Temp_P; SET DataAnalysis(WHERE=(Visit_year=0 AND APOE = "&APOE."));
RUN;

DATA _Temp_P; SET _Temp_P NOBS = Samp;
  CALL SYMPUTX('Samp', Samp, "G");
RUN;

DATA _T; SET DataAnalysis(WHERE=(Visit_year=0)); RUN;

PROC SURVEYSELECT DATA = _T
  METHOD = URS 
  REP = 1 
  SAMPSIZE = &Samp.  
  SEED = &SEED. 
  OUT = _Out
  OUTHITS;
RUN;

DATA _Out; SET _Out; 
  APOE_Boot = "&APOE.";
RUN;

DATA Temp1; SET Temp1 _Out; RUN;

%MEND;
%Temp(APOE = Posi);
%Temp(APOE = Nega);

%END;

DATA _Temp; SET Temp1; 
  IF APOE_Boot = "Posi" THEN CateG = 1;
  IF APOE_Boot = "Nega" THEN CateG = 0;
RUN;


PROC SORT DATA = _Temp; BY CateG; RUN;

ODS OUTPUT SolutionR = Gamma_Est;

PROC MIXED DATA = _Temp METHOD = REML MAXITER = 10000 MAXFUNC = 50000;
  CLASS CateG;
  MODEL Tilde_Beta = / S NOINT;
  RANDOM RMean / S TYPE = TOEP(1) SUBJECT = CateG;
RUN;

DATA Gamma_Est; RETAIN Effect CateG Estimate; SET Gamma_Est; RUN;

PROC DATASETS LIB=work NOPRINT; DELETE _:; RUN; QUIT;


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

DATA TrajALL; STOP; RUN;
DATA Data_DisTime; STOP; RUN;


%MACRO Temp;

%DO CateG = 0 %TO 1 %BY 1;

DATA _Gamma_Est; SET Gamma_Est(WHERE = (CateG = &CateG.)); RUN;

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
RUN;


DATA TrajALL; SET TrajALL Traj; 
  Seed = &Seed.;
RUN;

%END;

%MEND;
%Temp;


PROC DATASETS LIB=work NOPRINT; DELETE _:; RUN; QUIT;

DATA TrajBoot; SET TrajBoot TrajALL; RUN;

DATA Gamma_est; SET Gamma_est;
  Seed = &Seed.;
RUN;

DATA GammaBoot; SET GammaBoot Gamma_est; RUN;

%MEND;

%MACRO SimBoot;

%DO Seed = 1 %TO 1001;
  %Boot;
%END;

%MEND;
%SimBoot;


******;
* AUC *;
*******;

DATA _AUC; SET TrajBoot;
  Pred1 = LAG1(Pred);
  IF DisTime = 0 THEN DELETE;
  AUC = (Pred1 + Pred) * 1/12 /2;
RUN;

PROC UNIVARIATE DATA = _AUC NOPRINT;
  BY Seed CateG;
  VAR AUC;
  OUTPUT OUT = AUC
  SUM = AUC;
RUN;

PROC TRANSPOSE DATA = AUC OUT = Ratio_AUC;
  BY Seed;
RUN; 

DATA Ratio_AUC; SET Ratio_AUC;
  RENAME COL2 = CateG1;
  RENAME COL1 = CateG0;
  Ratio_AUC = COL2 / COL1;
  IF _NAME_ = "CateG" THEN DELETE;
  IF Seed = 1 THEN DO;
    CALL SYMPUTX('AUC', Ratio_AUC, "G");
  END;
RUN;


DATA _Fig; SET Ratio_AUC;
  IF Seed = 1 THEN DELETE;
RUN;


DATA PValue; SET _Fig;
  IF &AUC. < 1 THEN DO;
    IF Ratio_AUC < &AUC. THEN Check = 1;
	ELSE Check = 0;
  END;
  IF &AUC. > 1 THEN DO;
    IF Ratio_AUC > &AUC. THEN Check = 1;
	ELSE Check = 0;
  END;
RUN;


PROC UNIVARIATE DATA = PValue NOPRINT;
  VAR Check;
  OUTPUT OUT = _PValue_AUC
  MEAN = PValue; *One-sided p-value*;
RUN;


DATA PValue_AUC; SET _PValue_AUC; 
  Pvalue_AUC_TwoSided = 2*PValue;
  KEEP Pvalue_AUC_TwoSided;
RUN;



**************;
* AD onset time *;
**************;

%MACRO Temp;
DATA _Temp;
  %IF &Outcome. = MMSE %THEN %DO;
   TargetOutcome = 23;
  %END;
  %IF &Outcome. = ADAS13 %THEN %DO;
    TargetOutcome = 30;
  %END;
  %IF &Outcome. = CDRSB %THEN %DO;
    TargetOutcome = 4.5;
  %END;
  CALL SYMPUTX('TargetOutcome', TargetOutcome, "G");
RUN;

%IF &Outcome. = MMSE %THEN %DO;
DATA _CI; SET TrajBoot;
 Dif = Pred - &TargetOutcome.;
 IF Dif > 0 THEN DELETE;
RUN;

PROC RANK DATA = _CI OUT =_OutTau(WHERE = (RANK in (.,1))) TIES = LOW DESCENDING;
  VAR Dif;
  RANKS RANK;
  BY Seed CateG;
RUN;
%END;


%IF &Outcome. ^= MMSE %THEN %DO;
DATA _CI; SET TrajBoot;
 Dif = Pred - &TargetOutcome.;
 IF Dif < 0 THEN DELETE;
RUN;

PROC RANK DATA = _CI OUT =_OutTau(WHERE = (RANK in (.,1))) TIES = LOW;
  VAR Dif;
  RANKS RANK;
  BY Seed CateG;
RUN;
%END;

%MEND;
%Temp;

PROC TRANSPOSE DATA = _OutTau OUT = Dif_OutTau;
  BY Seed;
RUN; 


DATA Dif_OutTau; SET Dif_OutTau;
  RENAME COL2 = CateG1;
  RENAME COL1 = CateG0;
  Dif_Time = COL2 - COL1;
  IF _NAME_ ^= "DisTime" THEN DELETE;
  IF Seed = 1 THEN DO;
    CALL SYMPUTX('Onset', Dif_Time, "G");
  END;
RUN;


DATA _Fig; SET Dif_OutTau;
  IF Seed = 1 THEN DELETE;
RUN;


DATA PValue; SET _Fig;
  Target = &Onset.;
  IF &Onset. < 0 THEN DO;
    IF Dif_Time < &Onset. THEN Check = 1;
	ELSE Check = 0;

    IF &Onset. < Dif_Time AND -&Onset. > Dif_Time THEN Check1 = 0;
	ELSE Check1 = 1;
  END;
  IF &Onset. > 0 THEN DO;
    IF Dif_Time > &Onset. THEN Check = 1;
	ELSE Check = 0;

	IF &Onset. > Dif_Time AND -&Onset. < Dif_Time THEN Check1 = 0;
	ELSE Check1 = 1;

  END;
RUN;

PROC UNIVARIATE DATA = PValue NOPRINT;
  VAR Check Check1;
  OUTPUT OUT = _PValue_ADOnset
  MEAN = PValue PValue1;
RUN;

DATA PValue_ADOnset; SET _PValue_ADOnset;
  PValue_ADOnset_TwoSided = PValue*2;
  KEEP PValue_ADOnset_TwoSided;
RUN;


DATA PValue_ADNI; MERGE PValue_ADOnset PValue_AUC; RUN;



PROC FORMAT;
  VALUE CateGf 0 = "APOE e4-Negative" 1="APOE e4-Positive";
RUN;


DATA Trajectory_for_Test_ADNI; RETAIN Outcome AnalysisSet Sample Seed CateG DisTime Pred; 
LENGTH Sample $10; SET TrajBoot;
  IF Seed = 1 THEN Sample = "Original";
  ELSE Sample = "Bootstrap";
  Outcome = "&Outcome.";
  AnalysisSet = "&AnalysisSet.";
  RENAME DisTime = DiseaseProgressionTime;
  RENAME Pred = PredictedScore;
  FORMAT CateG CateGf.;
RUN;

PROC SORT DATA = Trajectory_for_Test_ADNI; BY Seed DESCENDING CateG; RUN;





PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Trajectory_for_Test Pvalue;
RUN;
QUIT;

PROC COPY IN = Work OUT = Result MEMTYPE = (data); RUN;

