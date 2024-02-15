
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

DATA DataAnalysis; SET Data; RUN;

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
-2<=Tilde_Beta<-1 Ë Cate = -2
-1<=Tilde_Beta<0 Ë Cate = -1
0<=Tilde_Beta<1 Ë Cate = 0
1<=Tilde_Beta<2 Ë Cate = 1
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
%END;


%IF &Outcome. = CDRSB %THEN %DO;
  PROC SORT DATA =_Est; BY DESCENDING Cate; RUN;
  DATA Est; SET _Est;
    IF CateG > 2 THEN CateG = 2;
  RUN;
%END;


%IF &Outcome. = ADAS13 %THEN %DO;
  PROC SORT DATA =_Est; BY DESCENDING Cate; RUN;
  DATA Est; SET _Est;
    %DO i = 0 %TO 20 %BY 3;
      IF CateG = &i. THEN CateG = CateG+2;
    %END;
    %DO i = 1 %TO 20 %BY 3;
      IF CateG = &i. THEN CateG = CateG+1;
    %END;
    IF CateG > 8 THEN CateG = 8;
  RUN;
%END;

%MEND;
%Temp;


PROC UNIVARIATE DATA = Trajectory NOPRINT;
  VAR CateG;
  OUTPUT OUT = _Temp
  MIN = Min
  MAX = Max;
RUN;

DATA _Temp; SET _Temp;
  CALL SYMPUTX('Ex_CateG_Min', Min, 'G');
  CALL SYMPUTX('Ex_CateG_Max', Max, 'G');
RUN;

%MACRO Temp;
DATA Est; SET Est;
  %IF &Outcome. = MMSE %THEN %DO;
    IF CateG <= &Ex_CateG_Min. THEN CateG = &Ex_CateG_Min.;
    ELSE CateG = CateG;
  %END;
  %ELSE %DO;
    IF CateG >= &Ex_CateG_Max. THEN CateG = &Ex_CateG_Max.;
    ELSE CateG = CateG;
  %END;
RUN;
%MEND;
%Temp;

DATA _CateG; SET Est; KEEP ID CateG; RUN;

PROC SORT DATA = _CateG; BY ID; RUN;
PROC SORT DATA = Data; BY ID; RUN;
PROC SORT DATA = DataAnalysis; BY ID; RUN;

DATA Data; MERGE Data _CateG; BY ID; RUN;
DATA DataAnalysis; MERGE DataAnalysis _CateG; BY ID; RUN;

PROC DATASETS LIB=work NOPRINT; DELETE _:; RUN; QUIT;



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




%DO CateG = &MinC. %TO &MaxC. %BY &ByC.;


DATA Traj; SET Trajectory(WHERE = (CateG = &CateG.)); RUN;
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
  PredError2 =  (Pred - &Outcome.)**2;
RUN;

DATA Traj; SET Traj;
  CateG = &CateG.;
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
  PredImp = Pred;
  IF B = &MaxTau. OR B = . THEN PredImp = &MaxOutcome.;
  PredErrorImp2 = (PredImp - &Outcome.)**2;
RUN;

PROC SORT DATA = Data_Result; BY ID Visit_year; RUN;





****************;
* Prediction Error *;
****************;

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


%MACRO Temp;

%IF &Outcome. = MMSE %THEN %DO;
PROC FORMAT;
  VALUE CateGf -2 = 'Moderate' -4 = 'Intermediate' -6 = 'Rapid';
RUN;
%END;

%IF &Outcome. = CDRSB %THEN %DO;
PROC FORMAT;
  VALUE CateGf 0 = 'Moderate' 1 = 'Intermediate' 2 = 'Rapid';
RUN;
%END;

%IF &Outcome. = ADAS13 %THEN %DO;
PROC FORMAT;
  VALUE CateGf 2 = 'Moderate' 5 = 'Intermediate' 8 = 'Rapid';
RUN;
%END;

%MEND;
%Temp;

DATA PredResult; SET _PredResult2_Method2G; 
  IF CateG = . THEN DELETE;
  AveragePredictiveError = SQRT(MPE);
  AveragePredictiveError = ROUND(AveragePredictiveError, .01);
  FORMAT CateG CateGf.;
  KEEP CateG AveragePredictiveError;
RUN;


DATA Prediction_EV_ADNI; SET Data_Result; RUN;
DATA PredError_EV_ADNI; SET PredResult; RUN;



