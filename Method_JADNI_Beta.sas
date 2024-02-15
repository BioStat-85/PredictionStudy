

* Note 
 The user can execute this program by inputting only "Input Parameters".
*;

* Output Datasets
  Trajectory_JADNI: Predicted trajectories of cognitive test score for moderate, intermediate, and rapid cognitive decline groups
  DeclineSlope_JADNI: Average estimated decline slope during the follow-up period
  Prediction_JADNI: Superposition of the observed cognitive test scores in individuals on long-term cognitive decline trajectory
  PredError_NoAdj_JADNI: Average predictive error in the external validations of the prediction model (No adjustment for Optimism)
  CV_Train_JADNI: Predictive error in the internal validations of the prediction model (Training set)
  CV_Test_JADNI: Predictive error in the internal validations of the prediction model (Test set)
  PredError_CV_Train_JADNI: Average predictive error in the internal validations of the prediction model (Training set)
  PredError_CV_Test_JADNI: Average predictive error in the internal validations of the prediction model (Test set)
  Prediction_EV_JADNI: Superposition of the observed cognitive test scores in individuals on long-term cognitive decline trajectory (External data)
  PredError_EV_JADNI: Average predictive error in the external validations of the prediction model
*;

*****************
* Input Parameters *
*****************;

* Folder pass where this program is saved *;
%LET ProgramFolder = ;

* Folder pass where JADNI data is saved *;
%LET DataFolderJADNI = ;
LIBNAME Data "&DataFolderJADNI.";

* Folder pass where ADNI data is saved *;
%LET DataFolderADNI = ;

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


* Paticipants categorization factor -*
  Decline slope: Beta 
  APOE e4: ProgA
*----------------------------*;
%LET WCate = Beta;

%MACRO Temp;
DATA _ProgFactor;
  %IF &WCate. = ProgA %THEN %DO;
    ProgFactor = "APOE";
    CALL SYMPUTX('ProgFactor', ProgFactor, 'G');
  %END;
RUN;
%MEND;
%Temp;


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


*************;
* Loading Data *;
*************;

%INCLUDE "&ProgramFolder.\JADNI_DataOrigin.sas";



**********************;
* Create Analysis Dataset *;
**********************;

DATA _Data0; LENGTH APOE $5; SET DataOrigin;
  IF bl_&Outcome. = "." THEN DELETE;
  IF bl_Diagnostic = "" THEN DELETE;
  IF AGE  = "." THEN DELETE;
  IF PTGENDER = "" THEN DELETE;
  IF PTEDUCAT = "." THEN DELETE;
  IF APOE4 = "." THEN DELETE;

  IF APOE4 = "positive" THEN APOE = "Posi";
  ELSE APOE = "Nega";

  IF CSF_AB = . AND PIB ="." AND BF227 = "." THEN DELETE;

  IF PIB = "positive" OR PIB = "equivocal" OR BF227 = "positive" THEN PET = 1;
  ELSE PET = 0;

  IF CSF_AB >= 333 OR CSF_AB =. THEN CSFAB = 0;
  ELSE CSFAB = 1;

  IF PET =1 OR CSFAB = 1 THEN AB = "Posi";
  ELSE AB = "Nega";

  KEEP ID Visit_year bl_Diagnostic AGE PTGENDER PTEDUCAT APOE AB 
           MMSE bl_MMSE CDRSB bl_CDRSB ADAS13 bl_ADAS13;
RUN;

DATA _Data0; 
  RETAIN ID Visit_year bl_Diagnostic AGE PTGENDER PTEDUCAT APOE AB 
             MMSE bl_MMSE CDRSB bl_CDRSB ADAS13 bl_ADAS13;
  SET _Data0;
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
DATA DataAnalysis; SET Data(WHERE = (APOE = "Posi")); RUN;
%END;

%IF &AnalysisSet. = ABETA_Posi %THEN %DO;
DATA DataAnalysis; SET Data(WHERE = (AB = "Posi")); RUN;
%END;

%IF &AnalysisSet. = APOENega %THEN %DO;
DATA DataAnalysis; SET Data(WHERE = (APOE = "Nega")); RUN;
%END;

%IF &AnalysisSet. = ABETA_Nega %THEN %DO;
DATA DataAnalysis; SET Data(WHERE = (AB = "Nega")); RUN;
%END;
%MEND;
%Temp;


PROC DATASETS LIB = work NOPRINT; DELETE _:; RUN; QUIT;





*******************************************************;
* Calculate the starting point for estimation of the prediction curve *;
*******************************************************;

DATA _Temp; SET DataAnalysis(WHERE = (Visit_year = 0 AND bl_Diagnostic = "NC")); RUN;

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
  CLASS ID bl_Diagnostic(REF='NC');
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

*
-2<=Tilde_Beta<-1 ⇒ Cate = -2
-1<=Tilde_Beta<0 ⇒ Cate = -1
0<=Tilde_Beta<1 ⇒ Cate = 0
1<=Tilde_Beta<2 ⇒ Cate = 1
*;

DATA _Est; SET _Est;
  IF Tilde_Beta < 0 THEN DO;
    CateG = INT(Tilde_Beta) - 1;
  END;
  ELSE CateG = INT(Tilde_Beta);
RUN;


%MACRO Temp;

%IF &Outcome. = MMSE %THEN %DO;
  PROC SORT DATA = _Est; BY CateG; RUN;
  DATA Est; SET _Est;
    IF CateG = -1 THEN CateG = -2;
    IF CateG = -3 THEN CateG = -4;
    IF CateG < -4 THEN CateG = -6;
  RUN;

  PROC FORMAT;
    VALUE CateGf -2 = 'Moderate' -4 = 'Intermediate' -6 = 'Rapid';
  RUN;
%END;


%IF &Outcome. = CDRSB %THEN %DO;
  PROC SORT DATA =_Est; BY DESCENDING CateG; RUN;
  DATA Est; SET _Est;
    IF CateG > 2 THEN CateG = 2;
  RUN;
 
  PROC FORMAT;
    VALUE CateGf 0 = 'Moderate' 1 = 'Intermediate' 2 = 'Rapid';
  RUN;
%END;


%IF &Outcome. = ADAS13 %THEN %DO;
  PROC SORT DATA =_Est; BY DESCENDING CateG; RUN;
  DATA Est; SET _Est;
    %DO i = 0 %TO 20 %BY 3;
      IF CateG = &i. THEN CateG = CateG+2;
    %END;
    %DO i = 1 %TO 20 %BY 3;
      IF CateG = &i. THEN CateG = CateG+1;
    %END;
    IF CateG > 8 THEN CateG = 8;
  RUN;

  PROC FORMAT;
    VALUE CateGf 2 = 'Moderate' 5 = 'Intermediate' 8 = 'Rapid';
  RUN;
%END;

%MEND;
%Temp;


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
* Estimation of parameter γ *;
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




* Step IV: Superposition of the observed cognitive test scores in individuals on long-term cognitive decline trajectory *;

DATA _Est; SET Est;
  KEEP ID CateG;
RUN;

PROC SORT DATA = _Est; BY ID; RUN;
PROC SORT DATA = Data; BY ID; RUN;
DATA Data; MERGE Data _Est; BY ID; RUN;

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

    IF DisTime = BaseYear + Visit_year THEN OUTPUT; * 疾患進行時間を算出 *;
  END;
  DROP &Outcome.;
RUN;

DATA _Data4; SET _Data4 _Data3; RUN;

%END;


PROC SORT DATA = _Data4; BY ID Visit_year; RUN;
PROC SORT DATA = _Data; BY ID Visit_year; RUN;

DATA _Data_DisTime; MERGE _Data4 _Data;
  BY ID Visit_year;
  PredError2 =  (Pred - &Outcome.)**2;
RUN;

DATA Traj; SET Traj;
  CateG = &CateG.;
RUN;


DATA TrajALL; SET TrajALL Traj; RUN;

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
  PredImp = Pred;
  IF B = &MaxTau. OR B = . THEN PredImp = &MaxOutcome.; * パターン② *;
  PredErrorImp2 = (PredImp - &Outcome.)**2;
RUN;

PROC SORT DATA = Data_Result; BY ID Visit_year; RUN;





****************************************;
* Prediction Error (No Adjestment for Optimism) *;
****************************************;

PROC SORT DATA = Data_Result; BY CateG ID; RUN;

PROC UNIVARIATE DATA = Data_Result NOPRINT;
  BY CateG ID;
  VAR PredErrorImp2;
  OUTPUT OUT = _MPredResult2_Method2G
  MEAN = Mean_PredError2;
RUN;

PROC UNIVARIATE DATA = _MPredResult2_Method2G NOPRINT;
  BY CateG;
  VAR Mean_PredError2;
  OUTPUT OUT = _PredResult2_Method2G
  MEAN = MPE;
RUN;




DATA PredResult; SET _PredResult2_Method2G; 
  IF CateG = . THEN DELETE;
  AveragePredictiveError = SQRT(MPE);
  AveragePredictiveError = ROUND(AveragePredictiveError, .01);
  FORMAT CateG CateGf.;
  KEEP CateG AveragePredictiveError;
RUN;


DATA Trajectory; RETAIN CateG DisTime Pred; SET TrajALL; RUN;

DATA Prediction_JADNI; SET Data_Result; RUN;
DATA PredError_NoAdj_JADNI; SET PredResult; RUN;



******************************************************************************;
* Relationship between the estimated slopes and the average scores during the follow-up period *;
******************************************************************************;

DATA ForPlot; SET DataAnalysis(WHERE = (Visit_year = 0)); RUN;

DATA _Temp1; SET Gamma_est; 
  KEEP Effect CateG Estimate;
RUN;

PROC TRANSPOSE DATA = _Temp1 OUT = _Temp2;
  BY CateG;
  ID Effect;
RUN;

PROC SORT DATA = ForPlot OUT = _ForPlot; BY CateG; RUN;

PROC UNIVARIATE DATA = _ForPlot NOPRINT;
  VAR CateG;
  OUTPUT OUT = _CateG
  MIN = Min
  MAX = Max
;
RUN;

DATA _CateG; SET _CateG;
  CALL SYMPUT('MinCateG', Min);
  CALL SYMPUT('MaxCateG', Max);
RUN;

%MACRO Temp;
DATA _CateG;
  %IF &Outcome. = MMSE %THEN %DO;
    MinC = &MinCateG.;
	MaxC = -2;
	ByC = 2;
  %END;
  %IF &Outcome. = CDRSB %THEN %DO;
    MinC = 0;
	MaxC = &MaxCateG.;
	ByC = 1;  
  %END;
  %IF &Outcome. = ADAS13 %THEN %DO;
    MinC = 2;
	MaxC = &MaxCateG.;
	ByC = 3;  
  %END;
  CALL SYMPUTX('MinC', MinC, 'G');
  CALL SYMPUTX('MaxC', MaxC, 'G');
  CALL SYMPUTX('ByC', ByC, 'G');
RUN;
%MEND;
%Temp;


%MACRO FigB;

DATA _Temp; STOP; RUN;

%DO C = &MinC. %TO &MaxC. %BY &ByC.;

DATA _ForPlot; SET ForPlot(WHERE = (CateG = &C.)); RUN;

PROC UNIVARIATE DATA = _ForPlot NOPRINT;
  VAR Mean;
  OUTPUT OUT = _Mean
  Max = Max
  Min = Min;
RUN;

DATA _Null_; SET _Mean;
  Max = ROUND(Max) + 2;
  Min = ROUND(Min) - 2;
  CALL SYMPUT('MinMean', Min);
  CALL SYMPUT('MaxMean', Max);
RUN;

DATA _Temp3; SET _Temp2(WHERE = (CateG = &C.));
  DO M = &MinMean. TO &MaxMean. BY 0.01;
  %IF &Outcome. = MMSE %THEN %DO;
    B =RMean*((30-M)*M)/30;
  %END;
  %IF &Outcome. = CDRSB %THEN %DO;
    B =RMean*((M-18)*M)/18;
  %END;
  %IF &Outcome. = ADAS13 %THEN %DO;
    B = RMean*((M-85)*M)/85;
  %END;
    OUTPUT;
  END;
RUN;

DATA _Temp; SET _Temp _Temp3; RUN;

%END;

%MEND;
%FigB;


DATA FP; SET ForPlot _Temp; RUN;

PROC SORT DATA = FP; BY bl_Diagnostic; RUN;

ODS ESCAPECHAR = '^';

%MACRO Temp;
DATA FP1; SET FP;
%IF &Outcome. = MMSE %THEN %DO;
IF CateG >=0 THEN DELETE;
%END;
%IF &Outcome. ^= MMSE %THEN %DO;
IF CateG <0 THEN DELETE;
%END;
SUBJID = ID;
IF ID = . AND CateG = 0 THEN SUBJID = 9000;
IF ID = . AND CateG = 1 THEN SUBJID = 9001;
IF ID = . AND CateG = 2 THEN SUBJID = 9002;
IF ID = . AND CateG = 5 THEN SUBJID = 9005;
IF ID = . AND CateG = 8 THEN SUBJID = 9008;

IF ID = . AND CateG = -2 THEN SUBJID = 8998;
IF ID = . AND CateG = -4 THEN SUBJID = 8996;
IF ID = . AND CateG = -6 THEN SUBJID = 8994;

IF CateG = 0 THEN CateG1 = 1;
IF CateG = 1 THEN CateG1 = 2;
IF CateG = 2 THEN CateG1 = 3;
IF CateG = 5 THEN CateG1 = 6;
IF CateG = 8 THEN CateG1 = 9;

IF CateG = -2 THEN CateG1 = 2;
IF CateG = -4 THEN CateG1 = 4;
IF CateG = -6 THEN CateG1 = 6;
RUN; 

PROC FORMAT;
%IF &Outcome. = MMSE %THEN %DO;
  VALUE CateG1n 2 = '-2(*ESC*){unicode "2264"x}<0' 
                          4 = '-4(*ESC*){unicode "2264"x}<-2' 
                          6 = '<-4';
%END;
%IF &Outcome. = CDRSB %THEN %DO;
  VALUE CateG1n 1 = '0(*ESC*){unicode "2264"x}<1' 
                          2 = '1(*ESC*){unicode "2264"x}<2' 
                          3 = '2(*ESC*){unicode "2264"x}';
%END;
%IF &Outcome. = ADAS13 %THEN %DO;
  VALUE CateG1n 3 = '0(*ESC*){unicode "2264"x}<3'
						  6 = '3(*ESC*){unicode "2264"x}<6'
						  9 = '6(*ESC*){unicode "2264"x}';
%END;

RUN;


PROC SQL;
  CREATE TABLE Value AS SELECT DISTINCT bl_Diagnostic, CateG, SUBJID AS Value FROM FP1;
QUIT;

DATA Value1; SET Value;
  IF bl_Diagnostic ="" THEN Check = 1;
  ELSE Check = 0;
RUN;

DATA ATTRMAP1; SET Value1;
  ID = "PATID"; LENGTH MARKERSYMBOL $20 LINECOLOR $10 MARKERCOLOR $10;
  IF bl_Diagnostic = "NC" THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;
  IF bl_Diagnostic = "MCI" THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;
  IF bl_Diagnostic = "AD" THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END;

%IF &Outcome. = CDRSB %THEN %DO;
  IF Check = 1 AND CateG = 0 THEN DO; LINECOLOR = "BLACK"; MARKERCOLOR = "BLACK"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 1; END;
  IF Check = 1 AND CateG = 1 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 1; END;
  IF Check = 1 AND CateG = 2 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 1; END;
%END;
%IF &Outcome. = ADAS13 %THEN %DO;
  IF Check = 1 AND CateG = 2 THEN DO; LINECOLOR = "BLACK"; MARKERCOLOR = "GREEN"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 1; END;
  IF Check = 1 AND CateG = 5 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "CYAN"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 1; END;
  IF Check = 1 AND CateG = 8 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "VIOLET"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 1; END;
%END;
%IF &Outcome. = MMSE %THEN %DO;
  IF Check = 1 AND CateG = -2 THEN DO; LINECOLOR = "BLACK"; MARKERCOLOR = "BLACK"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 1; END;
  IF Check = 1 AND CateG = -4 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLACK"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 1; END;
  IF Check = 1 AND CateG = -6 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "BLACK"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 1; END;
%END;
RUN;

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


%MACRO Temp;

%MEND;
%Temp;

ODS HTML CLOSE;
ODS GRAPHICS ON / RESET = ALL IMAGENAME = "Decline_and_Mean_&Outcome._JADNI" IMAGEFMT = PNG ATTRPRIORITY = NONE NOBORDER;
GOPTIONS FTEXT = 'Meiryo' FTITLE = 'Meiryo' HTITLE = 2 HTEXT = 2 VSIZE = 15cm HSIZE = 30cm;
ODS LISTING GPATH =  "&ProgramFolder.\&ResultFolder." STYLE = MyJournal3;

PROC SGPANEL DATA = FP1 NOAUTOLEGEND DATTRMAP = ATTRMAP1;
  STYLEATTRS
	WALLCOLOR = WhiteSmoke
  ;
  PANELBY CateG1 / SORT = ASCENDING LAYOUT = COLUMNLATTICE COLUMNS = 3 
                                        HEADERATTRS = (SIZE = 16 Family = Meiryo) NOVARNAME;

  SCATTER X = Mean Y = Tilde_Beta / GROUP = SUBJID ATTRID = PATID 
                                                         MARKERATTRS = (SIZE = 16) NAME = "Plot";
  SERIES X = M Y = B / GROUP = SUBJID ATTRID = PATID LINEATTRS = (THICKNESS = 0.04in) NAME = "Line";

  COLAXIS LABEL = "Average &YLabel." LABELATTRS = (SIZE = 16 Family = Meiryo)
            VALUES = (0 TO &YMAX.)
            VALUEATTRS = (SIZE = 16 Family = Meiryo) OFFSETMIN = 0.1 OFFSETMAX = 0.2
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);
  ROWAXIS LABEL = "Estimated lopes of &YLabel." LABELATTRS = (SIZE = 16 Family = Meiryo)
/*            VALUES = (2 TO -10 BY -2)*/
            VALUEATTRS = (SIZE = 16 Family = Meiryo) OFFSETMIN = 0.1 OFFSETMAX = 0.1
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);
  FORMAT CateG1 CateG1n.;
RUN;


ODS LISTING CLOSE;
ODS GRAPHICS / RESET = ALL;




***************;
* Trajectory plot *;
***************;

DATA _FitTraj; SET TrajAll;
  ID = CateG + 9000;
RUN;

DATA _FitResult; SET Data_Result; 
  KEEP ID DisTime &Outcome. CateG;
RUN;

DATA Fit; SET _FitTraj _FitResult; 
  RENAME ID = SUBJID;
RUN;

DATA _Fit; SET Fit; RUN;
PROC SORT DATA = _Fit; BY SUBJID; RUN;

DATA _Temp; SET Data_Result; RUN;

DATA _Temp1; SET _Temp;
  SUBJID = ID;
  KEEP SUBJID Visit_year;
RUN;

PROC SORT DATA = _Temp1; BY SUBJID; RUN;

DATA _Fit; MERGE _Fit _Temp1; BY SUBJID; RUN;


PROC UNIVARIATE DATA = _Fit NOPRINT;
  VAR CateG;
  OUTPUT OUT = Cate
  MIN = MinCate;
RUN;

DATA Cate; SET Cate;
  CALL SYMPUT('MinCate', MinCate);
RUN;


DATA _Fit1; SET _Fit;
D = ROUND(DisTime, 1); 
IF D = &MaxTau. or DisTime = . THEN DELETE;
IF Pred = . AND &Outcome. ="" THEN DELETE;
&Outcome.1 = INPUT(&Outcome., best32.);
RENAME &Outcome.1 = &Outcome.;
DROP D &Outcome.;
RUN;

PROC SORT DATA = _Fit1; BY DisTime; RUN;

DATA _Z; SET _Fit1;
  IF SUBJID > 8000 THEN DELETE;
RUN;

PROC UNIVARIATE DATA = _Z NOPRINT;
  VAR DisTime;
  OUTPUT OUT = Out
  MAX = Max;
RUN;

DATA Out; SET Out;
  M = ROUND(Max, 1);
  MDisTime = M + 1;
  CALL SYMPUT('MDisTime', MDisTime);
RUN;

PROC UNIVARIATE DATA = _Z NOPRINT;
  VAR &Outcome.;
  OUTPUT OUT = Out1
  Min = Min
  Max = Max
;
RUN;

DATA Out1; SET Out1;
  MMin = ROUND(Min, 1);
  MMax = ROUND(Max, 1);
  MMin = MMin + 1;
  MMax = MMax + 1;
  CALL SYMPUT('MMax', MMax);
  CALL SYMPUT('MMin', MMin);
RUN;


DATA Dig; SET DataAnalysis;
  SUBJID = ID;
  KEEP SUBJID bl_Diagnostic;
RUN;

PROC SORT DATA = Dig; BY SUBJID; RUN;
PROC SORT DATA = _Fit1; BY SUBJID; RUN;

DATA PlotData; MERGE _Fit1 Dig; BY SUBJID; RUN;

PROC SQL;
  CREATE TABLE Value AS SELECT DISTINCT bl_Diagnostic, CateG, SUBJID AS Value FROM PlotData;
QUIT;

DATA Value; SET Value; 
  IF CateG ="." THEN DELETE; 
RUN;

PROC SORT DATA = PlotData; BY CateG; RUN;

DATA _Fit2; SET PlotData;
  BY CateG;
  IF bl_Diagnostic = "" THEN DO;
    IF FIRST.CateG THEN DO;
      X2 = DisTime;
	  Y2 = Pred;
    END;
  END;
RUN;

%MACRO Temp;
DATA ATTRMAP; SET Value;
  ID = "PATID"; LENGTH MARKERSYMBOL $20 LINECOLOR $10 MARKERCOLOR $10;

  %IF &Outcome. = MMSE %THEN %DO;
  IF bl_Diagnostic = "NC" AND CateG = -2 THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;
  IF bl_Diagnostic = "NC" AND CateG = -4 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;
  IF bl_Diagnostic = "NC" AND CateG = -6 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;

  IF bl_Diagnostic = "MCI" AND CateG = -2 THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;
  IF bl_Diagnostic = "MCI" AND CateG = -4 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;
  IF bl_Diagnostic = "MCI" AND CateG = -6 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;

  IF bl_Diagnostic = "AD" AND CateG = -2 THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END;  
  IF bl_Diagnostic = "AD" AND CateG = -4 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END; 
  IF bl_Diagnostic = "AD" AND CateG = -6 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END; 

  IF bl_Diagnostic = "" AND CateG = -2 THEN DO; LINECOLOR = "BLACK"; MARKERCOLOR = "Orange"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 8; END;
  IF bl_Diagnostic = "" AND CateG = -4 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 2; END;
  IF bl_Diagnostic = "" AND CateG = -6 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "GREEN"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 5; END;
  %END;

  %IF &Outcome. = CDRSB %THEN %DO;
  IF bl_Diagnostic = "NC" AND CateG = 0 THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;
  IF bl_Diagnostic = "NC" AND CateG = 1 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;
  IF bl_Diagnostic = "NC" AND CateG = 2 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;

  IF bl_Diagnostic = "MCI" AND CateG = 0 THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;
  IF bl_Diagnostic = "MCI" AND CateG = 1 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;
  IF bl_Diagnostic = "MCI" AND CateG = 2 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;

  IF bl_Diagnostic = "AD" AND CateG = 0 THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END;  
  IF bl_Diagnostic = "AD" AND CateG = 1 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END; 
  IF bl_Diagnostic = "AD" AND CateG = 2 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END; 

  IF bl_Diagnostic = "" AND CateG = 0 THEN DO; LINECOLOR = "BLACK"; MARKERCOLOR = "Orange"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 8; END;
  IF bl_Diagnostic = "" AND CateG = 1 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 2; END;
  IF bl_Diagnostic = "" AND CateG = 2 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "GREEN"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 5; END;
  %END;

  %IF &Outcome. = ADAS13 %THEN %DO;
  IF bl_Diagnostic = "NC" AND CateG = 2 THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;
  IF bl_Diagnostic = "NC" AND CateG = 5 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;
  IF bl_Diagnostic = "NC" AND CateG = 8 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "CircleFilled"; LINEPATTERN = 15; END;

  IF bl_Diagnostic = "MCI" AND CateG = 2 THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;
  IF bl_Diagnostic = "MCI" AND CateG = 5 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;
  IF bl_Diagnostic = "MCI" AND CateG = 8 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "TriangleFilled"; LINEPATTERN = 20; END;

  IF bl_Diagnostic = "AD" AND CateG = 2 THEN DO; LINECOLOR = "Black"; MARKERCOLOR = "Black"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END;  
  IF bl_Diagnostic = "AD" AND CateG = 5 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END; 
  IF bl_Diagnostic = "AD" AND CateG = 8 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "RED"; MARKERSYMBOL = "TriangleDownFilled"; LINEPATTERN = 26; END; 

  IF bl_Diagnostic = "" AND CateG = 2 THEN DO; LINECOLOR = "BLACK"; MARKERCOLOR = "Orange"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 8; END;
  IF bl_Diagnostic = "" AND CateG = 5 THEN DO; LINECOLOR = "BLUE"; MARKERCOLOR = "BLUE"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 2; END;
  IF bl_Diagnostic = "" AND CateG = 8 THEN DO; LINECOLOR = "RED"; MARKERCOLOR = "GREEN"; MARKERSYMBOL = "CIRCLE"; LINEPATTERN = 5; END;
  %END;
RUN;
%MEND;
%Temp;

PROC SORT DATA = ATTRMAP; BY ID; RUN;
PROC SORT DATA = PlotData; BY SUBJID DisTime; RUN;


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
ODS GRAPHICS / RESET = ALL IMAGENAME = "Trajectory_&Outcome._JADNI" IMAGEFMT = PNG NOBORDER;
GOPTIONS FTEXT = 'Meiryo' FTITLE = 'Meiryo' HTITLE = 2 HTEXT = 2 VSIZE = 15cm HSIZE = 30cm  ;
ODS LISTING GPATH = "&ProgramFolder.\&ResultFolder.";

PROC SGPLOT DATA = PlotData NOAUTOLEGEND DATTRMAP = ATTRMAP NOBORDER;
  STYLEATTRS
	WALLCOLOR = WhiteSmoke
  ;
  SERIES Y = &Outcome. X = DisTime / GROUP = SUBJID MARKERS ATTRID = PATID NAME = "Real";
  SERIES Y = Pred X = DisTime / GROUP = SUBJID ATTRID = PATID 
                                                  LINEATTRS=(THICKNESS=0.05in PATTERN=1) NAME = "Pred";

  KEYLEGEND "Pred"/ TITLE = "Decline"
                        TITLEATTRS = (SIZE = 16 Family = Meiryo) 
                        VALUEATTRS = (SIZE = 16 Family = Meiryo)
                        ;

  XAXIS VALUES = (0 TO &MDisTime. BY 1) VALUEATTRS = (SIZE = 16 Family = Meiryo) 
             LABEL = 'Disease progression time (Years)' LABELATTRS = (SIZE = 16 Family = Meiryo)
             OFFSETMIN = 0.04 OFFSETMAX = 0.02
			 DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);
  YAXIS VALUES = (0 TO &YMax.) VALUEATTRS = (SIZE = 16 Family = Meiryo) 
             LABEL = "&YLabel." LABELATTRS = (SIZE = 16 Family = Meiryo)
             OFFSETMIN = 0.05 OFFSETMAX = 0.05
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);
RUN;



**************************************************;
* Average estimated decline slope during the follow-up period *;
**************************************************;

PROC SORT DATA = Est OUT = _Est; BY CateG; RUN;

PROC UNIVARIATE DATA = _Est NOPRINT;
  BY CateG;
  VAR Tilde_Beta;
  OUTPUT OUT = _Temp
  N = N
  MEAN = Mean
  STD = SD
/*  MEDIAN = Med*/
/*  Q1 = Q1*/
/*  Q3 = Q3*/
;
RUN;

DATA _DeclineSlope; LENGTH Outcome $15 Prognostic $15 AnalysisSet $15; SET _Temp;
  Outcome = "&Outcome.";
  AnalysisSet = "&AnalysisSet.";
RUN;

PROC SORT DATA = _DeclineSlope; BY DESCENDING CateG; RUN;

%MACRO Temp;
DATA DeclineSlope; SET _DeclineSlope; 
  Mean = ROUND(Mean, .1);
  SD = ROUND(SD, .1);
  Med = ROUND(Med, .1);
  Q1 = ROUND(Q1, .1);
  Q3 = ROUND(Q3, .1);

  %IF &Outcome = MMSE %THEN %DO;
    IF CateG >=0 THEN DELETE;
  %END;
  %IF &Outcome ^= MMSE %THEN %DO;
    IF CateG <0 THEN DELETE;
  %END;

  Summary = CATT(Mean, ' (', SD, ')');
  FORMAT CateG CateGf.;
  KEEP Outcome AnalysisSet CateG N Summary;
RUN;
%MEND;
%Temp;


PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Trajectory DeclineSlope Prediction_JADNI PredError_NoAdj_JADNI Data;
RUN;
QUIT;



********************************;
* Internal Validation (Cross Validation) *;
********************************;

%INCLUDE "&ProgramFolder.\Method_JADNI_Beta_CV.sas";

PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Trajectory DeclineSlope Prediction_JADNI PredError_NoAdj_JADNI CV_Train_JADNI CV_Test_JADNI 
            PredError_CV_Train_JADNI PredError_CV_Test_JADNI;
RUN;
QUIT;



******************;
* External Validation *;
******************;

LIBNAME Data "&DataFolderADNI.";

%INCLUDE "&ProgramFolder.\Method_JADNI_Beta_EV.sas";

PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Trajectory DeclineSlope Prediction_JADNI PredError_NoAdj_JADNI CV_Train_JADNI CV_Test_JADNI 
            PredError_CV_Train_JADNI PredError_CV_Test_JADNI Prediction_EV_JADNI PredError_EV_JADNI;
RUN;
QUIT;







DATA Trajectory_JADNI; RETAIN Outcome AnalysisSet; SET Trajectory; 
  Outcome = "&Outcome.";
  AnalysisSet = "&AnalysisSet.";
  RENAME DisTime = DiseaseProgressionTime;
  RENAME Pred = PredictedScore;
  FORMAT CateG CateGf.;
RUN;

PROC SORT DATA = Trajectory_JADNI; BY DESCENDING CateG; RUN;

DATA Declineslope_JADNI; SET Declineslope;
  ATTRIB _ALL_  LABEL = " " ;
RUN;

DATA Prediction_JADNI; LENGTH ID Visit_year 8 bl_Diagnostic $4 &Outcome. bl_&Outcome. CateG DisTime Pred 8;
SET Prediction_JADNI;
  RENAME DisTime = DiseaseProgressionTime;
  RENAME Pred = PredictedScore;
  Pred = ROUND(Pred, 1);
  KEEP ID Visit_year bl_Diagnostic &Outcome. bl_&Outcome. CateG DisTime Pred;
  FORMAT CateG CateGf.;
RUN; 

DATA PredError_NoAdj_JADNI; RETAIN Outcome AnalysisSet; SET PredError_NoAdj_JADNI;
  Outcome = "&Outcome.";
  AnalysisSet = "&AnalysisSet.";
RUN;

PROC SORT DATA = PredError_NoAdj_JADNI; BY DESCENDING CateG; RUN;

DATA CV_Train_JADNI; RETAIN Rep CateG AveragePredictiveError; SET CV_Train_JADNI;
  FORMAT CateG CateGf.;
RUN;
  
PROC SORT DATA = CV_Train_JADNI; BY REP DESCENDING CateG; RUN;

DATA CV_Test_JADNI; RETAIN Rep CateG AveragePredictiveError; SET CV_Test_JADNI;
  FORMAT CateG CateGf.;
RUN;
  
PROC SORT DATA = CV_Test_JADNI; BY REP DESCENDING CateG; RUN;

DATA PredError_CV_Train_JADNI; RETAIN Outcome AnalysisSet; SET PredError_CV_Train_JADNI;
  Outcome = "&Outcome.";
  AnalysisSet = "&AnalysisSet.";
  FORMAT CateG CateGf.;
RUN;

PROC SORT DATA = PredError_CV_Train_JADNI; BY DESCENDING CateG; RUN;


DATA PredError_CV_Test_JADNI; RETAIN Outcome AnalysisSet; SET PredError_CV_Test_JADNI;
  Outcome = "&Outcome.";
  AnalysisSet = "&AnalysisSet.";
  FORMAT CateG CateGf.;
RUN;

PROC SORT DATA = PredError_CV_Test_JADNI; BY DESCENDING CateG; RUN;



DATA Prediction_EV_JADNI; LENGTH ID Visit_year 8 bl_Diagnostic $4 &Outcome. bl_&Outcome. CateG DisTime Pred 8;
SET Prediction_EV_JADNI;
  RENAME DisTime = DiseaseProgressionTime;
  RENAME Pred = PredictedScore;
  Pred = ROUND(Pred, 1);
  KEEP ID Visit_year bl_Diagnostic &Outcome. bl_&Outcome. CateG DisTime Pred;
  FORMAT CateG CateGf.;
RUN; 

DATA PredError_EV_JADNI; RETAIN Outcome AnalysisSet; SET PredError_EV_JADNI;
  Outcome = "&Outcome.";
  AnalysisSet = "&AnalysisSet.";
RUN;

PROC SORT DATA = PredError_EV_JADNI; BY DESCENDING CateG; RUN;


PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Trajectory_JADNI DeclineSlope_JADNI Prediction_JADNI PredError_NoAdj_JADNI CV_Train_JADNI CV_Test_JADNI 
            PredError_CV_Train_JADNI PredError_CV_Test_JADNI Prediction_EV_JADNI PredError_EV_JADNI;
RUN;
QUIT;

PROC COPY IN = Work OUT = Result MEMTYPE = (data); RUN;


