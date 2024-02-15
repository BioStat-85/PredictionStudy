
* Note 
 The user can execute this program by inputting only "Input Parameters".
*;

* Output Datasets
  CR: Standard design, for each simulation study, Bias and RMSE of difference of change of CDR-SOB from baseline to time of the primary efficacy assessment (1.5 years) 
        between the drug and placebo groups, result of statistical hypothesis testing based on MMRM
  SR: Proposed design, for each simulation study, Bias and RMSE of difference of change of CDR-SOB from baseline to time of the primary efficacy assessment (1.5 years) 
        between the drug and placebo groups, result of statistical hypothesis testing based on MMRM
  Performance_RCTdesing: Bias and RMSE of difference of change of CDR-SOB from baseline to time of the primary efficacy 
        assessment (1.5 years) between the drug and placebo groups, power of statistical hypothesis testing based on MMRM
  Number_of_Screening: Number of screened patients to achieve the planned sample size of n = 320
*;



OPTIONS NONOTES NOSOURCE DKRICOND = NOWARN VARLENCHK = NOWARN; 
ODS RESULTS OFF;
ODS SELECT NONE;

*****************
* Input Parameters *
*****************;

* Folder pass where this program is saved *;
%LET ProgramFolder = ;

* Outcome ---------------*
MMSE or CDRSB or ADAS13
*-----------------------*;
%LET Outcome = ;

* Follow-up period for historical data (years) *;
%LET Hist = 1.5;

* Efficacy analysis time points in clinical trials (years) *;
%LET AnalysisTime = 1.5;


* Number of screening patients *;
%LET SampleSize = 1000;

* Target sample size *;
%LET TargetSampleSize = 320;

* Proportion of MCI in the study population (MCI + AD) *;
%LET MCIRate = 0.7; 

* Cutoff *;
%LET CutOffCateG = 1;

* シミュレーションの開始番号と終了番号 *;
%LET SimStart = 1;
%LET SimEnd = 1000;


* Number of years when restricting to data with long follow-up periods in ADNI data *;
%LET LongFollow = 2;

* (1- Coefficient) of Bayes HPD credible interval for parameter Gamma in Decline model *;
%LET Interval = 0.2;

* Folder to save the results *;
%LET ResultFolder = ;
%LET rc=%SYSFUNC(DCREATE(&ResultFolder., &ProgramFolder.\));
LIBNAME Result "&ProgramFolder.\&ResultFolder.";

FILENAME LOGFILE "&ProgramFolder.\test.log";
PROC PRINTTO LOG = LOGFILE NEW; RUN;


* Analysis Population----------*
  All population: ALL
  APOEe4-positive: APOEPosi
  ABETA-positive: ABETA_Posi
  APOEe4-negative: APOENega
  ABETA-negative: ABETA_Nega
*--------------------------*;
%LET AnalysisSet = ABETA_Posi;


* Folder pass where ADNI data is saved *;
%LET DataFolderADNI = ;
LIBNAME Data "&DataFolderADNI.";

* ADNI DataSet *;
%LET ADNIData = 220623_ADNIMERGE.csv;


* Number of measurement points after baseline ------------------------------*
  Excluding subjects with post-baseline measurement time points below "ExNumOut"
*-------------------------------------------------------------------*;
%LET ExNumOut = 1;

* Period for which the forecast curve is calculated (in years) *;
%LET MaxTau = 50; 



***************************************************
***** The user does not need to modify following codes. *****
***************************************************;


***************;
* Planning phase *;
***************;

* Step 1 and 2:  Estimation of longitudinal trajectory *;

%MACRO EstTraj;

%INCLUDE "&ProgramFolder.\App2_Plan_MCI.sas";

DATA DATA_ADNI_MCI; SET Data_ADNI; 
  RENAME DifOutcome = ChangeOutcome;
RUN;
DATA TrajALL_MCI; SET TrajALL; RUN;

PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Data_ADNI_MCI TrajALL_MCI;
RUN;


%INCLUDE "&ProgramFolder.\App2_Plan_AD.sas";

DATA DATA_ADNI_AD; SET Data_ADNI; 
  RENAME DifOutcome = ChangeOutcome;
RUN;
DATA TrajALL_AD; SET TrajALL; RUN;

PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Data_ADNI_MCI TrajALL_MCI Data_ADNI_AD TrajALL_AD;
RUN;
QUIT;

%MEND;
%EstTraj;





DATA CR; STOP; RUN;
DATA SR; STOP; RUN;
DATA SR_No; STOP; RUN;

DATA CRDist; STOP; RUN;
DATA SRDist; STOP; RUN;

DATA Info; STOP; RUN;

%MACRO Sim;

%DO Sim = &SimStart. %TO &SimEnd.;

**************;
* Simulatin Data *;
**************;

PROC IML;
  MEAN_MCI = {0.9 1.1 1.3 1.6 2.0 2.3 2.7};
  MEAN_AD = {2.4 3.0 3.9 4.2 4.9 5.3 6.0};

  COV_MCI = {1.23	1.27	1.27	1.11	1.55	0.85	1.47,
                    1.27	1.77	1.65	1.38	2.09	1.16	1.96,
                    1.27	1.65	2.06	1.60	2.33	1.41	2.42,
                    1.11	1.38	1.60	1.67	2.25	1.31	2.45,
                    1.55	2.09	2.33	2.25	4.09	2.21	3.96,
                    0.85	1.16	1.41	1.31	2.21	1.60	2.67,
                    1.47	1.96	2.42	2.45	3.96	2.67	5.99
                    };

  COV_AD = {2.28	2.18	3.01	1.28	2.66	1.00	0.53,
                   2.18	3.51	3.91	1.50	3.34	1.18	0.69,
                   3.01	3.91	7.35	2.27	4.59	1.72	0.94,
                   1.28	1.50	2.27	1.19	1.93	0.66	0.38,
                   2.66	3.34	4.59	1.93	5.30	1.45	0.77,
                   1.00	1.18	1.72	0.66	1.45	0.67	0.29,
                   0.53	0.69	0.94	0.38	0.77	0.29	0.20
                   };

  CALL RANDSEED(&Sim.);
  RV_MCI = RANDNORMAL(%SYSEVALF(&SampleSize.*&MCIRate.), MEAN_MCI, COV_MCI);
  RV_AD = RANDNORMAL(%SYSEVALF(&SampleSize.*(1-&MCIRate.)), MEAN_AD, COV_AD);

  CREATE _SimData_MCI FROM RV_MCI[COLNAME={'y0' 'y1' 'y2' 'y3' 'y4' 'y5' 'y6'}];
  APPEND FROM RV_MCI;
  CREATE _SimData_AD FROM RV_AD[COLNAME={'y0' 'y1' 'y2' 'y3' 'y4' 'y5' 'y6'}];
  APPEND FROM RV_AD;
QUIT;


%IF &MCIRate. ^= 1 %THEN %DO;
DATA _SimData; SET _SimData_MCI _SimData_AD;
  ID = _N_;
RUN;
%END;

%IF &MCIRate. = 1 %THEN %DO;
DATA _SimData; SET _SimData_MCI;
  ID = _N_;
RUN;
%END;


PROC TRANSPOSE DATA = _SimData OUT = _SimData1; BY ID; RUN;

DATA _SimData1; SET _SimData1;
  IF _NAME_ = 'y0' THEN Visit_year = 0;
  IF _NAME_ = 'y1' THEN Visit_year = 0.5;
  IF _NAME_ = 'y2' THEN Visit_year = 1;
  IF _NAME_ = 'y3' THEN Visit_year = 1.5;
  IF _NAME_ = 'y4' THEN Visit_year = 2;
  IF _NAME_ = 'y5' THEN Visit_year = 2.5;
  IF _NAME_ = 'y6' THEN Visit_year = 3;
  RENAME Col1 = CDRSB;
  DROP _NAME_;
RUN;

DATA _DataSim; RETAIN ID Visit_year CDRSB; SET _SimData1; BY ID; RUN;

DATA DataSim; SET _DataSim;
  IF ID <= %SYSEVALF(&SampleSize.*&MCIRate.) THEN bl_Diagnostic = "MCI";
  ELSE bl_Diagnostic = "AD";

  IF CDRSB<0.25 THEN CDRSB = 0;
  ELSE IF CDRSB >=0.25 AND CDRSB <0.75 THEN CDRSB = 0.5;
  ELSE IF CDRSB >=0.75 AND CDRSB <1.25 THEN CDRSB = 1;
  ELSE IF CDRSB >=1.25 AND CDRSB <1.75 THEN CDRSB = 1.5;
  ELSE IF CDRSB >=1.75 AND CDRSB <2.25 THEN CDRSB = 2;
  ELSE IF CDRSB >=2.25 AND CDRSB <2.75 THEN CDRSB = 2.5;
  ELSE IF CDRSB >=2.75 AND CDRSB <3.25 THEN CDRSB = 3;
  ELSE IF CDRSB >=3.25 AND CDRSB <3.75 THEN CDRSB = 3.5;
  ELSE IF CDRSB >=3.75 AND CDRSB <4.25 THEN CDRSB = 4;
  ELSE IF CDRSB >=4.25 AND CDRSB <4.75 THEN CDRSB = 4.5;
  ELSE IF CDRSB >=4.75 AND CDRSB <5.25 THEN CDRSB = 5;
  ELSE IF CDRSB >=5.25 AND CDRSB <5.75 THEN CDRSB = 5.5;
  ELSE IF CDRSB >=5.75 AND CDRSB <6.25 THEN CDRSB = 6;
  ELSE IF CDRSB >=6.25 AND CDRSB <6.75 THEN CDRSB = 6.5;
  ELSE IF CDRSB >=6.75 AND CDRSB <7.25 THEN CDRSB = 7;
  ELSE IF CDRSB >=7.25 AND CDRSB <7.75 THEN CDRSB = 7.5;
  ELSE IF CDRSB >=7.75 AND CDRSB <8.25 THEN CDRSB = 8;
  ELSE IF CDRSB >=8.25 AND CDRSB <8.75 THEN CDRSB = 8.5;
  ELSE IF CDRSB >=8.75 AND CDRSB <9.25 THEN CDRSB = 9;
  ELSE IF CDRSB >=9.25 AND CDRSB <9.75 THEN CDRSB = 9.5;
  ELSE IF CDRSB >=9.75 AND CDRSB <10.25 THEN CDRSB = 10;
  ELSE IF CDRSB >=10.25 AND CDRSB <10.75 THEN CDRSB = 10.5;
  ELSE IF CDRSB >=10.75 AND CDRSB <11.25 THEN CDRSB = 11;
  ELSE IF CDRSB >=11.25 AND CDRSB <11.75 THEN CDRSB = 11.5;
  ELSE IF CDRSB >=11.75 AND CDRSB <12.25 THEN CDRSB = 12;
  ELSE IF CDRSB >=12.25 AND CDRSB <12.75 THEN CDRSB = 12.5;
  ELSE IF CDRSB >=12.75 AND CDRSB <13.25 THEN CDRSB = 13;
  ELSE IF CDRSB >=13.25 AND CDRSB <13.75 THEN CDRSB = 13.5;
  ELSE IF CDRSB >=13.75 AND CDRSB <14.25 THEN CDRSB = 14;
  ELSE IF CDRSB >=14.25 AND CDRSB <14.75 THEN CDRSB = 14.5;
  ELSE IF CDRSB >=14.75 AND CDRSB <15.25 THEN CDRSB = 15;
  ELSE IF CDRSB >=15.25 AND CDRSB <15.75 THEN CDRSB = 15.5;
  ELSE IF CDRSB >=15.75 AND CDRSB <16.25 THEN CDRSB = 16;
  ELSE IF CDRSB >=16.25 AND CDRSB <16.75 THEN CDRSB = 16.5;
  ELSE IF CDRSB >=16.75 AND CDRSB <17.25 THEN CDRSB = 17;
  ELSE IF CDRSB >=17.25 AND CDRSB <17.75 THEN CDRSB = 17.5;
  ELSE IF CDRSB >=17.75 THEN CDRSB = 18;
RUN;


DATA _Base; SET DataSim;
IF Visit_year ^= 0 THEN DELETE;
bl_CDRSB = CDRSB;
KEEP ID bl_CDRSB;
RUN;

DATA DataSim; MERGE DataSim _Base; BY ID; RUN;

PROC DATASETS LIB = work NOPRINT; DELETE _:; RUN; QUIT;





****************;
* Standard Design *;
****************;

DATA _DataAnalysisALL; SET DataSim;
  IF Visit_year > &Hist. THEN DELETE;
  ChangeOutcome = &Outcome. - bl_&Outcome.;
RUN;

DATA _Data_Enter; SET DataSim;
  IF Visit_year < &Hist. THEN DELETE;
  Visit_year = Visit_year - &Hist.;
  DROP bl_&Outcome.;
RUN;

DATA _Baseline; SET _Data_Enter;
  IF Visit_year ^= 0 THEN DELETE;
  bl_&Outcome. = &Outcome.;
  KEEP ID bl_&Outcome.;
RUN;

DATA _Data_Enter; MERGE _Data_Enter _Baseline; BY ID; 
  ChangeOutcome = &Outcome. - bl_&Outcome.;
RUN;

DATA _Data_Enter; SET _Data_Enter;
  IF Visit_year > &AnalysisTime. THEN DELETE;
  IF ID > (&TargetSampleSize.*&MCIRate.) AND ID <= &SampleSize.*&MCIRate. THEN DELETE;
  IF ID > (&SampleSize.*&MCIRate.+&TargetSampleSize.*(1-&MCIRate.)) THEN DELETE; 
RUN;

PROC FREQ DATA = _Data_Enter;
  TABLE ID*bl_Diagnostic;
  ODS OUTPUT  CrossTabFreqs = _ID;
RUN;

DATA _ID; SET _ID;
  IF Frequency = 0 THEN DELETE;
  IF ID = "." THEN DELETE;
  IF bl_Diagnostic = "" THEN DELETE;
  KEEP ID bl_Diagnostic;
RUN;

PROC SORT DATA = _ID; BY bl_Diagnostic; RUN;

PROC SURVEYSELECT DATA = _ID  
  METHOD = SRS
  REP = 1 
  SAMPRATE = 0.5  
  SEED = &Sim.
  OUT = _Out;
RUN;

DATA _Out; SET _Out;
  Arm = 1; * Drug arm *;
  KEEP ID Arm;
RUN;

PROC SORT DATA = _Data_Enter; BY ID; RUN;
PROC SORT DATA = _Out; BY ID; RUN;

DATA Data_MMRM_CompRandom; MERGE _Data_Enter _Out;
  BY ID;
  IF Arm = . THEN Arm = 0; * Placebo arm *;

  IF Arm = 1 THEN DO;
    IF Visit_year = 0.5 THEN ChangeOutcome = ChangeOutcome - 0.2;
    IF Visit_year = 1 THEN ChangeOutcome = ChangeOutcome - 0.4;
    IF Visit_year = 1.5 THEN ChangeOutcome = ChangeOutcome - 0.45;
 END;
RUN;

PROC MIXED DATA = Data_MMRM_CompRandom EMPIRICAL;
  CLASS Arm Visit_year ID;
  MODEL ChangeOutcome = bl_&Outcome. Arm Visit_year Arm*Visit_year / DDFM = KR;
  LSMEANS Arm*Visit_year;
  ESTIMATE 'Test - Cont at Analysis Time Point'  Arm -1 1
                              Arm*Visit_year 0 0 0 -1 0 0 0 1/ E; 
  REPEATED Visit_year / SUBJECT = ID TYPE = AR(1);
  ODS OUTPUT Estimates = AnalysisResult_CR;
RUN;

DATA AnalysisResult_CR; SET AnalysisResult_CR;
  Sim = &Sim.;
  IF Probt < 0.05 THEN Sig = 1;
  ELSE Sig = 0;
RUN;

DATA CR; SET CR AnalysisResult_CR; RUN;

PROC DATASETS LIB = work NOPRINT; DELETE _:; RUN; QUIT;





*****************;
* Proposed Design *;
*****************;

%MACRO Prepare(Diag);

DATA DataAnalysisALL; SET DataSim(WHERE = (bl_Diagnostic = "&Diag."));
  IF Visit_year > &Hist. THEN DELETE;
  ChangeOutcome = &Outcome. - bl_&Outcome.;
RUN;

DATA Data_Enter; SET DataSim(WHERE = (bl_Diagnostic = "&Diag."));
  IF Visit_year < &Hist. THEN DELETE;
  Visit_year = Visit_year - &Hist.;
  DROP bl_&Outcome.;
RUN;

DATA _Baseline; SET Data_Enter;
  IF Visit_year ^= 0 THEN DELETE;
  bl_&Outcome. = &Outcome.;
  KEEP ID bl_&Outcome.;
RUN;

DATA Data_Enter; MERGE Data_Enter _Baseline; BY ID; 
  ChangeOutcome = &Outcome. - bl_&Outcome.;
RUN;

PROC UNIVARIATE DATA = Data_Enter NOPRINT;
  VAR ID;
  OUTPUT OUT = _TempID
  MIN = Min
  MAX = Max
;
RUN;

DATA _TempID; SET _TempID;
  CALL SYMPUTX('Min', Min, "G");
  CALL SYMPUTX('Max', Max, "G");
RUN;

DATA _Target;
  IF "&Diag." = "MCI" THEN DO;
    Target = %SYSEVALF(&TargetSampleSize.*&MCIRate.);
  END;
  ELSE DO;
    Target = %SYSEVALF(&TargetSampleSize.*(1-&MCIRate.));
  END;
  Count = 0;
  CALL SYMPUTX("Count", Count, "G");
  CALL SYMPUTX("Target", Target, "G");
RUN;

DATA Data; STOP; RUN;





%MACRO Registry;

%DO i = &Min. %TO &Max.;

%IF &Count. < &Target. %THEN %DO; 

*------------------------------------------------*;
* Step 3: Estimation of decline slope and Enrollment patient   *;
*------------------------------------------------*;

DATA DataAnalysis; SET DataAnalysisALL;
  IF ID ^= &i. THEN DELETE;
RUN;

DATA DataAnalysis; SET DataAnalysis Data_ADNI_&Diag.; RUN;

DATA DataOne; SET Data_Enter;
  IF ID ^= &i. THEN DELETE;
RUN;

ODS OUTPUT SolutionF = _Fixeff;
ODS OUTPUT SolutionR = _Raneff;

PROC MIXED DATA = DataAnalysis METHOD = ml EMPIRICAL;
  CLASS ID bl_Diagnostic(REF="&Diag.");
  MODEL ChangeOutcome =  bl_Diagnostic*Visit_year / NOINT S;
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
  Tilde_Beta = Fixed_effect + Random_effect; * Beta~_{i} の推定値 *;
RUN;

DATA _Est; SET _Est;
  IF Tilde_Beta < 0 THEN DO;
    Cate = -1;
  END;
  ELSE IF Tilde_Beta >= 0 AND Tilde_Beta < 1 THEN DO; Cate = 0; END;
  ELSE IF Tilde_Beta >= 1 AND Tilde_Beta < 2 THEN DO; Cate = 1; END;
  ELSE Cate = 2;
RUN;

PROC UNIVARIATE DATA = TrajALL_&Diag. NOPRINT;
  VAR CateG;
  OUTPUT OUT = _Temp
  MAX = Max;
RUN;

DATA _Temp; SET _Temp;
  CALL SYMPUTX('Ex_CateG_Max', Max, 'G');
RUN;

DATA Est; SET _Est;
  IF Cate >= &Ex_CateG_Max. THEN CateG = &Ex_CateG_Max.;
  ELSE CateG = Cate;
RUN;

DATA _CateG; SET Est; KEEP ID CateG; RUN;

PROC SORT DATA = _CateG; BY ID; RUN;
PROC SORT DATA = DataOne; BY ID; RUN;
PROC SORT DATA = DataAnalysis; BY ID; RUN;

DATA DataOne; MERGE DataOne _CateG; BY ID;
  IF Visit_year = . THEN DELETE;
  CALL SYMPUTX("CateG", CateG, "G");
RUN; 

%IF &CateG. >= 0 %THEN %DO;
  %IF &CateG. < &CutOffCateG. %THEN %DO;
  DATA Data; SET Data DataOne; RUN;
  DATA _TempCount; SET Data(WHERE = (Visit_year = 0)); RUN;
  DATA _Count; SET _TempCount NOBS = NOBS;
    CALL SYMPUTX("Count", NOBS, "G");
  RUN;
  %END;
%END;

%END;

%END;

%MEND;
%Registry;





*-----------------------------------------------*;
* Step 4: Prediction for the changes of cognitive test score *;
*-----------------------------------------------*;

%DO Traj = 1 %TO 3; 

DATA Data_DisTime; STOP; RUN;

%MACRO Temp;

PROC FREQ DATA = Data; 
  TABLE Visit_year;
  ODS OUTPUT OneWayFreqs = Visit;
RUN;

DATA _Visit;  SET Visit;
  KEEP F_Visit_year;
RUN;

DATA _Visit; SET _Visit NOBS = NOBS;
  CALL SYMPUTX('NObs_Visit', NOBS,'G');
RUN;

PROC TRANSPOSE DATA = _Visit OUT = _Visit1; VAR F_Visit_year;RUN;

DATA _Visit2; LENGTH COL $250; SET _Visit1;
  COL = COL1;
  %DO V = 2 %TO &NObs_Visit.;
   COL = catx("," , COL, COL&V.) ;
  %END;
RUN;

DATA _Visit2; SET _Visit2;
  CALL SYMPUTX('Visit', COL,'G');
RUN;

PROC UNIVARIATE DATA = Data NOPRINT;
  VAR CateG;
  OUTPUT OUT = _InfoCateG
  MAX = MaxCateG;
RUN;

DATA _InfoCateG; SET _InfoCateG;
  CALL SYMPUTX('MaxCateG', MaxCateG, "G");
RUN;

%DO CateG = 0 %TO &MaxCateG. %BY 1;

DATA Traj; SET TrajALL_&Diag.(WHERE = (CateG = &CateG. AND Traj = &Traj.)); RUN;
DATA _Data; SET Data(WHERE = (CateG = &CateG.)); RUN; 
DATA _Data1; SET _Data(WHERE = (Visit_year = 0)); RUN;
DATA _Data1; SET _Data1;
  TempID = _N_;
RUN;
DATA _Data1; SET _Data1 NOBS = NObs;
  CALL SYMPUT('NObs_Data', NObs);
RUN;


DATA _Data4; STOP; RUN;

%DO TempID = 1 %TO &NObs_Data.;

DATA _Data2; SET _Data1;
  CateG = &CateG.;
  IF TempID ^= &TempID. THEN DELETE;
RUN;

DATA Traj; SET Traj NOBS = NObs; 
  CALL SYMPUT('NObs_Traj', NObs);
RUN;

DATA _Data2; SET _Data2;
  DO i = 1 TO &NObs_Traj.;
    OUTPUT;
  END;
RUN;

DATA _Data2; MERGE _Data2 Traj; 
  Dif = Pred - &Outcome.; 
  ABS_Dif = ABS(Dif);
RUN;

PROC SORT DATA = _Data2;
  BY ABS_Dif;
RUN;

DATA _Data2; SET _Data2;
  DisTime = ROUND(DisTime, .01);
  DROP Visit_year;
RUN;

DATA _Data3; SET _Data2;
  BY ID;
  RETAIN BaseYear;
  IF first.id THEN BaseYear = DisTime; 
  DO Visit_year = &Visit.;
    IF DisTime = BaseYear + Visit_year THEN OUTPUT; 
  END;
  DROP &Outcome.;
RUN;

DATA _Data4; SET _Data4 _Data3; RUN;

%END;


PROC SORT DATA = _Data4; BY ID Visit_year; RUN;
PROC SORT DATA = _Data; BY ID Visit_year; RUN;

DATA _Data_DisTime; MERGE _Data4 _Data;
  BY ID Visit_year;
RUN;

DATA Data_DisTime; SET Data_DisTime _Data_DisTime; RUN;

%END;

%MEND;
%Temp;

PROC DATASETS LIB=work NOPRINT; DELETE _:; RUN; QUIT;

DATA _Temp; SET Data_DisTime(WHERE=(Visit_year = 0)); 
  DO Visit_year = &Visit.;
    OUTPUT;
  END;
  &Outcome. = .;
  KEEP ID Visit_year;
RUN;

PROC SORT DATA = _Temp; BY ID Visit_year; RUN;
PROC SORT DATA = Data_DisTime; BY ID Visit_year; RUN;

DATA Data_Result; MERGE _Temp Data_DisTime;
  BY ID Visit_year;
RUN;

DATA Data_Result; SET Data_Result;
  B = ROUND(BaseYear, 1);
  IF B = &MaxTau. OR B = . THEN Pred = &MaxOutcome.;
RUN;

PROC SORT DATA = Data_Result; BY ID Visit_year; RUN;


DATA Data_Result; SET Data_Result;
  DROP TempID i Dif ABS_Dif BaseYear B;
RUN;



*----------------------------------------------------------------------*;
* Step 5: Stratified randomization based on the predicted change of cognitive test score *;
*----------------------------------------------------------------------*;

DATA Dec; RETAIN ID Enrich; LENGTH Enrich $8; SET Data_Result;
  IF ID = . THEN DELETE;
  IF Visit_year ^= &AnalysisTime. THEN DELETE;
  PredChange = Pred - bl_&Outcome.;

  IF PredChange < 1.5 THEN Enrich = "A";
  ELSE IF PredChange >= 1.5 AND PredChange < 3 THEN Enrich = "B";
  ELSE IF PredChange >= 3 AND PredChange < 4.5 THEN Enrich = "C";
  ELSE Enrich = "D";

  PredChange&Traj. = PredChange;
  Enrich&Traj. = Enrich;

  KEEP ID PredChange&Traj. Enrich&Traj.;
RUN;


PROC SORT DATA = Data; BY ID; RUN;
PROC SORT DATA = Dec; BY ID; RUN;


DATA Data; MERGE Data Dec; 
  BY ID; 
RUN;

%END; 

DATA Data_&Diag.; SET Data; 
  IF ChangeOutcome < 1.5 THEN TrueEnrich = "A";
  ELSE IF ChangeOutcome >= 1.5 AND ChangeOutcome < 3 THEN TrueEnrich = "B";
  ELSE IF ChangeOutcome >= 3 AND ChangeOutcome < 4.5 THEN TrueEnrich = "C";
  ELSE TrueEnrich = "D";
RUN;

DATA Data_Result_&Diag.; SET Data_Result; RUN;
DATA Dec_&Diag.; SET Dec; RUN;


DATA Info_&Diag.; SET Data;
  IF Visit_year ^= &AnalysisTime. THEN DELETE;
RUN;


%MEND;
%Prepare(Diag = MCI);
%Prepare(Diag = AD);

DATA Data; SET Data_MCI Data_AD; 
  IF Enrich1 = Enrich2 = Enrich3 THEN Enrich = Enrich1;
  ELSE Enrich = "Z";
RUN;
DATA _Info; SET Info_MCI Info_AD; 
  Sim = &Sim.;
RUN;

DATA Info; SET Info _Info; RUN;







DATA Data_MMRM; SET Data;
  IF Visit_year > &AnalysisTime. THEN DELETE;
RUN;

PROC FREQ DATA = Data_MMRM;
  TABLE ID*Enrich;
  ODS OUTPUT  CrossTabFreqs = IDEnrich;
RUN;

DATA IDEnrich; SET IDEnrich;
  IF Frequency = 0 THEN DELETE;
  IF Enrich = "" THEN DELETE;
  IF ID = "." THEN DELETE;
  KEEP ID Enrich;
RUN;


PROC SORT DATA = IDEnrich; BY Enrich; RUN;

PROC SURVEYSELECT DATA = IDEnrich
  METHOD = SRS
  REP = 1 
  SAMPRATE = 0.5  
  SEED = &Sim.
  OUT = _Out;
  STRATA Enrich;
RUN;

DATA _Out; SET _Out;
  Arm = 1;
  KEEP ID Arm;
RUN;


PROC SORT DATA = _Out; BY ID; RUN;
PROC SORT DATA = IDEnrich; BY ID; RUN;

DATA Check; MERGE _Out IDEnrich; 
  BY ID; 
  IF Arm = . THEN Arm = 0;
RUN;

PROC FREQ DATA = Check;
  TABLE Arm*Enrich;
  ODS OUTPUT CrossTabFreqs = _SRDist;
RUN;


DATA _SRDist; SET _SRDist;
  Sim = &Sim.;
  IF Arm = . THEN DELETE;
  IF Enrich = "" THEN DELETE;
  KEEP Sim Arm Enrich Frequency ColPercent;
RUN;


DATA SRDist; SET SRDist _SRDist; RUN;

PROC SORT DATA = Data_MMRM; BY ID; RUN;
PROC SORT DATA = _Out; BY ID; RUN;

DATA Data_MMRM_StrataRandom; MERGE Data_MMRM _Out;
  BY ID;
  IF Arm = . THEN Arm = 0;

  IF Arm = 1 THEN DO;
    IF Visit_year = 0.5 THEN ChangeOutcome = ChangeOutcome - 0.2;
    IF Visit_year = 1 THEN ChangeOutcome = ChangeOutcome - 0.4;
    IF Visit_year = 1.5 THEN ChangeOutcome = ChangeOutcome - 0.45;
 END;
RUN;

PROC MIXED DATA = Data_MMRM_StrataRandom EMPIRICAL;
  CLASS Arm Visit_year ID Enrich;
  MODEL ChangeOutcome = bl_&Outcome. Arm Visit_year Arm*Visit_year Enrich/ DDFM = KR;
  LSMEANS Arm*Visit_year;
  ESTIMATE 'Test - Cont at Analysis Time Point'  Arm -1 1
                              Arm*Visit_year 0 0 0 -1 0 0 0 1/ E; 
  REPEATED Visit_year / SUBJECT = ID TYPE = AR(1);
  ODS OUTPUT Estimates = AnalysisResult_SR;
  ODS OUTPUT LSMeans = LSMeans_SR;
RUN;

DATA AnalysisResult_SR; SET AnalysisResult_SR;
  Sim = &Sim.;
  IF Probt < 0.05 THEN Sig = 1;
  ELSE Sig = 0;
RUN;

DATA SR; SET SR AnalysisResult_SR; RUN;


DM 'log' clear; 
DM 'output' clear;


%END;

%MEND;

%Sim;


%MACRO Out(Data);
DATA &Data.; SET &Data.;
  Dif = (Estimate + 0.45)**2;
RUN;

PROC UNIVARIATE DATA = &Data. NOPRINT;
  VAR Estimate Dif Sig;
  OUTPUT OUT = Out_&Data.
  Mean = MEstimate MDif Power;
RUN;

DATA Performance_&Data.; SET Out_&Data.;
  Bias = ROUND(MEstimate + 0.45, .001);
  RMSE = ROUND(SQRT(MDif), .001);
  Power = Power*100;
RUN;

%MEND;
%Out(Data =CR);
%Out(Data =SR);

DATA Performance_CR; RETAIN Design; SET Performance_CR; 
  Design = "Standard";
RUN;

DATA Performance_SR; RETAIN Design; SET Performance_SR; 
  Design = "Proposed";
RUN;

DATA Performance_RCTdesign; SET Performance_CR Performance_SR; RUN;




PROC SORT DATA = Info; BY Sim bl_Diagnostic; RUN;

PROC UNIVARIATE DATA = Info NOPRINT;
  BY Sim bl_Diagnostic;
  VAR ID;
  OUTPUT OUT = NP
  Max = Max;
RUN;

DATA NP1; SET NP;
  IF bl_Diagnostic = "AD" THEN Max = Max - 700;
RUN;

PROC UNIVARIATE DATA = NP1 NOPRINT;
  BY Sim;
  VAR Max;
  OUTPUT OUT = _Number_of_Screening
  Sum = Sum;
RUN;

PROC UNIVARIATE DATA = _Number_of_Screening NOPRINT;
  VAR Sum;
  OUTPUT OUT = _Number_of_Screening
  MEAN = NumScreen;
RUN;

DATA Temp; 
  Design = "Standard";
  NumScreen = 320;
RUN;

DATA _Number_of_Screening; RETAIN Design; SET _Number_of_Screening;
  Design = "Proposed";
RUN;

DATA Number_of_Screening; SET Temp _Number_of_Screening; RUN;


PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE CR SR Performance_RCTdesign Number_of_Screening;
RUN;
QUIT;

PROC COPY IN = Work OUT = Result MEMTYPE = (data); RUN;
  


