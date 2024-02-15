
* Note 
 The user can execute this program by inputting only "Input Parameters".
*;

* Output Datasets
  Trajectory_CI_ADNI: Predicted trajectories with 95% confidence intervals of cognitive test score for moderate, intermediate, and rapid cognitive decline groups
  ADOnset_ADNI: Time to milestone in AD onset
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

%IF &Seed. >=2 %THEN %DO;

DATA _Temp; STOP; RUN;

%MACRO Temp(APOE);
DATA _Temp_P; SET DataAnalysis(WHERE=(Visit_year=0 AND APOE = "&APOE."));
RUN;

DATA _Temp_P; SET _Temp_P NOBS = Samp;
  CALL SYMPUTX('Samp', Samp, "G");
RUN;

PROC SURVEYSELECT DATA = _Temp_P 
  METHOD = URS 
  REP = 1 
  SAMPSIZE = &Samp.  
  SEED = &Seed. 
  OUT = _Out
  OUTHITS;
RUN;

DATA _Temp; SET _Temp _Out; RUN;

%MEND;
%Temp(APOE = Posi);
%Temp(APOE = Nega);

%END;


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

%DO Seed = 1 %TO 1001; * Seed=1 is not Bootstrap sample *;
  %Boot;
%END;

%MEND;
%SimBoot;


%MACRO Temp;
DATA _CI; SET TrajBoot(WHERE = (Seed >= 2));
  Dif = Pred - &TargetOutcome.;
  %IF &Outcome. = MMSE %THEN %DO;
    IF Dif > 0 THEN DELETE;
  %END;
  %IF &Outcome. ^= MMSE %THEN %DO;
    IF Dif < 0 THEN DELETE;
  %END;
RUN;
%MEND;
%Temp;



%MACRO Temp;

%IF &Outcome. = MMSE %THEN %DO;

PROC RANK DATA = _CI OUT =_OutTau(WHERE = (RANK in (.,1))) TIES = LOW DESCENDING;
  VAR Dif;
  RANKS RANK;
  BY Seed CateG;
RUN;

%END;

%IF &Outcome. ^= MMSE %THEN %DO;

PROC RANK DATA = _CI OUT =_OutTau(WHERE = (RANK in (.,1))) TIES = LOW;
  VAR Dif;
  RANKS RANK;
  BY Seed CateG;
RUN;

%END;
%MEND;
%Temp;


PROC SORT DATA = _OutTau; BY CateG DisTime Seed; RUN;

DATA _OutTau1; SET _OutTau;
  BY CateG;
  RETAIN Seq;
  IF first.CateG THEN Seq = 0;
  Seq +1;
RUN;

DATA LowerPosi; SET _OutTau1(WHERE = (CateG = 1));
  IF Seq ^= 25 THEN DELETE;
  CALL SYMPUTX('LowerPosi', Seed, "G");
RUN;

DATA UpperPosi; SET _OutTau1(WHERE = (CateG = 1));
  IF Seq ^= 975 THEN DELETE;
  CALL SYMPUTX('UpperPosi', Seed, "G");
RUN;

DATA LowerNega; SET _OutTau1(WHERE = (CateG = 0));
  IF Seq ^= 25 THEN DELETE;
  CALL SYMPUTX('LowerNega', Seed, "G");
RUN;

DATA UpperNega; SET _OutTau1(WHERE = (CateG = 0));
  IF Seq ^= 975 THEN DELETE;
  CALL SYMPUTX('UpperNega', Seed, "G");
RUN;


DATA Lower; SET TrajBoot(WHERE = (CateG = 0 AND Seed = &LowerNega.));
  Lower = Pred;
KEEP DisTime Lower;
RUN;

DATA Upper; SET TrajBoot(WHERE = (CateG = 0 AND Seed = &UpperNega.));
  Upper = Pred;
KEEP DisTime Upper;
RUN;

DATA TrajBoot_Nega; SET TrajBoot(WHERE = (CateG = 0 AND Seed = 1)); RUN;

PROC SORT DATA = TrajBoot_Nega; BY DisTime; RUN;
PROC SORT DATA = Lower; BY DisTime; RUN;
PROC SORT DATA = Upper; BY DisTime; RUN;

DATA TrajBoot_Nega; MERGE TrajBoot_Nega Lower Upper;
  BY DisTime;
RUN;



DATA Lower; SET TrajBoot(WHERE = (CateG = 1 AND Seed = &LowerPosi.));
  Lower = Pred;
KEEP DisTime Lower;
RUN;

DATA Upper; SET TrajBoot(WHERE = (CateG = 1 AND Seed = &UpperPosi.));
  Upper = Pred;
KEEP DisTime Upper;
RUN;

DATA TrajBoot_Posi; SET TrajBoot(WHERE = (CateG = 1 AND Seed = 1)); RUN;

PROC SORT DATA = TrajBoot_Posi; BY DisTime; RUN;
PROC SORT DATA = Lower; BY DisTime; RUN;
PROC SORT DATA = Upper; BY DisTime; RUN;

DATA TrajBoot_Posi; MERGE TrajBoot_Posi Lower Upper;
  BY DisTime;
RUN;

PROC FORMAT;
  VALUE CateGf 0 = "APOE e4-Negative" 1="APOE e4-Positive";
RUN;

DATA Trajectory_CI_ADNI; RETAIN Seed Outcome AnalysisSet CateG DisTime Pred Lower Upper;
  SET TrajBoot_Nega TrajBoot_Posi;
  Outcome = "&Outcome.";
  AnalysisSet = "&AnalysisSet.";
  RENAME DisTime = DiseaseProgressionTime;
  RENAME Pred = PredictedScore;
  RENAME Lower = LowerPredictedScore;
  RENAME Upper = UpperPredictedScore;
  FORMAT CateG CateGf.;
  DROP Seed;
RUN;


%MACRO Temp;
DATA _Temp;
  %IF &Outcome. = MMSE %THEN %DO;
    YMax = 30;
	YLabel = "MMSE";
  %END;
  %IF &Outcome. = CDRSB %THEN %DO;
    YMax = 18;
	YLabel = "CDR-SOB";
  %END;
  %IF &Outcome. = ADAS13 %THEN %DO;
    YMax = 85;
	YLabel = "ADAS-Cog";
  %END;

  CALL SYMPUTX('YMax', YMax, "G");
  CALL SYMPUTX('YLabel', YLabel, "G");

RUN;
%MEND;
%Temp;

ODS HTML CLOSE;
ODS GRAPHICS ON / RESET = ALL IMAGENAME = "Trajectory_APOE_&Outcome._&App1._ADNI" 
IMAGEFMT = PNG ATTRPRIORITY = NONE NOBORDER;
GOPTIONS FTEXT = 'Meiryo' FTITLE = 'Meiryo' HTITLE = 2 HTEXT = 2 VSIZE = 15cm HSIZE = 30cm;
ODS LISTING GPATH = "&ProgramFolder.\&ResultFolder.";

PROC SGPLOT DATA = Trajectory_CI NOAUTOLEGEND NOBORDER;
  STYLEATTRS
    DATACONTRASTCOLORS=(BLUE RED)
    WALLCOLOR = WhiteSmoke
  ;
  BAND X = DiseaseProgressionTime LOWER = LowerPredictedScore UPPER = UpperPredictedScore/ GROUP = CateG TRANSPARENCY=0.5;
  SERIES Y = PredictedScore X = DiseaseProgressionTime / GROUP = CateG LINEATTRS = (THICKNESS=0.05in PATTERN=1);


  XAXIS VALUES = (0 TO 50 BY 5) VALUEATTRS = (SIZE = 16 Family = Meiryo) 
             LABEL = 'Disease progression time (Years)' LABELATTRS = (SIZE = 16 Family = Meiryo)
             OFFSETMIN = 0.04 OFFSETMAX = 0.02
			 DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);
  YAXIS VALUES = (0 TO &YMax.) VALUEATTRS = (SIZE = 16 Family = Meiryo) 
             LABEL = "&YLabel." LABELATTRS = (SIZE = 16 Family = Meiryo)
             OFFSETMIN = 0.05 OFFSETMAX = 0.05
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);
RUN;

ODS LISTING CLOSE;
ODS GRAPHICS / RESET = ALL;
ODS HTML;



***********;
* AD Onset *;
***********;

%MACRO Temp(Level);

%IF &App1. = Oncet %THEN %DO;

DATA _Temp; SET TrajBoot_&Level.;
  Dif_1 = Pred - &TargetOutcome.;
  Dif_2 = Lower - &TargetOutcome.;
  Dif_3 = Upper - &TargetOutcome.;
RUN;

%DO a = 1 %TO 3;

DATA _Temp_&a.; SET _Temp;
  %IF &Outcome. = MMSE %THEN %DO;
  IF Dif_&a. > 0 THEN DELETE;
  %END;
  %IF &Outcome. ^= MMSE %THEN %DO;
   IF Dif_&a. < 0 THEN DELETE;
  %END;
RUN;

DATA _Temp_&a.; SET _Temp_&a.;
  Rank = _N_;
  IF Rank ^= 1 THEN DELETE;
  RENAME DisTime = ADOnset_&a.;
  KEEP DisTime;
RUN;

%END;

DATA ADOnset_&Level.; MERGE _Temp_1 _Temp_2 _Temp_3; 
  Population = "&Level.";
  ADOnset_1 = ROUND(ADOnset_1, .1);
  ADOnset_2 = ROUND(ADOnset_2, .1);
  ADOnset_3 = ROUND(ADOnset_3, .1);

  RENAME ADOnset_1 = ADOnset;
  RENAME ADOnset_2 = ADOnset_LowerCI;
  RENAME ADOnset_3 = ADOnset_UpperCI;
RUN;

%END;

%MEND;
%Temp(Level = Posi);
%Temp(Level = Nega);

%MACRO Temp;

%IF &App1. = Oncet %THEN %DO;

DATA ADOnset_ADNI; RETAIN Population; LENGTH Population $16; SET ADOnset_Posi ADOnset_Nega; 
  IF Population = "Posi" THEN Population = "APOE e4-Positive";
  IF Population = "Nega" THEN Population = "APOE e4-Negative";
RUN;


PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Trajectory_CI_ADNI ADOnset_ADNI;
RUN;
QUIT;

%END;

%IF &App1. ^= Oncet %THEN %DO;

PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Trajectory_CI_ADNI;
RUN;
QUIT;

%END;
%MEND;
%Temp;

PROC COPY IN = Work OUT = Result MEMTYPE = (data); RUN;
