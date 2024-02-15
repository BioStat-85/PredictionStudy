
OPTIONS NONOTES NOSOURCE DKRICOND = NOWARN;
ODS LISTING CLOSE;
ODS HTML CLOSE;


%MACRO CV;

*--------------*;
* Random sampling *;
*---------------*;

PROC FREQ DATA = Data;
  TABLE ID;
  ODS OUTPUT OneWayFreqs = DataID;
RUN;

DATA DataID; SET DataID; KEEP ID; RUN;

PROC SURVEYSELECT DATA = DataID 
  METHOD = SRS 
  REP = 1 
  RATE = 0.80  
  SEED = &Seed. 
  OUT = _Out;
RUN;

DATA _Out; SET _Out;
  Check = 1;
  KEEP ID Check;
RUN;

PROC SORT DATA = Data; BY ID; RUN;
PROC SORT DATA = _Out; BY ID; RUN;

DATA Data; MERGE Data _Out;
  BY ID;
  IF Check = . THEN Check = 0;
RUN;


DATA Data_Train; SET Data;
  IF Check ^= 1 THEN DELETE;
RUN;

DATA Data_Test; SET Data;
  IF Check ^= 0 THEN DELETE;
RUN;

DATA DataAnalysis_Train; SET Data_Train;
  IF Visit_year > &Y. THEN DELETE;
RUN;

DATA DataAnalysis_Train; SET Data_Train; RUN;
DATA DataAnalysis_Test; SET Data_Test; RUN;


*******************************************************;
* Calculate the starting point for estimation of the prediction curve *;
*******************************************************;

DATA _Temp; SET DataAnalysis_Train(WHERE = (Visit_year = 0 AND bl_Diagnostic = "NC")); RUN;

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

%MACRO Temp(Data);

ODS OUTPUT SolutionF = _Fixeff;
ODS OUTPUT SolutionR = _Raneff;

PROC MIXED DATA = DataAnalysis_&Data. METHOD = ml EMPIRICAL;
  CLASS ID bl_Diagnostic(REF='NC'); 
  MODEL DifOutcome =  bl_Diagnostic*Visit_year / NOINT S; 
  RANDOM Visit_year / TYPE = UN SUBJECT = ID S;
RUN;

DATA _Fixeff ; SET _Fixeff; 
  Fixed_effect = Estimate;
  KEEP bl_Diagnostic Fixed_effect;
RUN;;

DATA _Disease_id; SET DataAnalysis_&Data.(WHERE = (Visit_year = 0));
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

DATA _Est_&Data.; MERGE _Fixeff1 _Raneff1;
  BY ID; 
  Tilde_Beta = Fixed_effect + Random_effect; * Beta~_{i} ÇÃêÑíËíl *;
RUN;





*------------------------------------------*;
* Modeling individual inclinations and outcome means *;
*-------------------------------------------*;

*
-2<=Tilde_Beta<-1 ÅÀ Cate = -2
-1<=Tilde_Beta<0 ÅÀ Cate = -1
0<=Tilde_Beta<1 ÅÀ Cate = 0
1<=Tilde_Beta<2 ÅÀ Cate = 1
*;

DATA _Est_&Data.; SET _Est_&Data.;
  IF Tilde_Beta < 0 THEN DO;
    CateG = INT(Tilde_Beta) - 1;
  END;
  ELSE CateG = INT(Tilde_Beta);
RUN;


%MACRO Temp1;

%IF &Outcome. = MMSE %THEN %DO;
  PROC SORT DATA = _Est; BY CateG; RUN;
  DATA Est_&Data.; SET _Est_&Data.;
    IF CateG = -1 THEN CateG = -2;
    IF CateG = -3 THEN CateG = -4;
    IF CateG < -4 THEN CateG = -6;
  RUN;
%END;


%IF &Outcome. = CDRSB %THEN %DO;
  PROC SORT DATA =_Est_&Data.; BY DESCENDING CateG; RUN;
  DATA Est_&Data.; SET _Est_&Data.;
    IF CateG > 2 THEN CateG = 2;
  RUN;
%END;


%IF &Outcome. = ADAS13 %THEN %DO;
  PROC SORT DATA =_Est_&Data.; BY DESCENDING CateG; RUN;
  DATA Est_&Data.; SET _Est_&Data.;
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
%Temp1;

DATA _CateG; SET Est_&Data.; KEEP ID CateG; RUN;

PROC SORT DATA = _CateG; BY ID; RUN;
PROC SORT DATA = Data_&Data.; BY ID; RUN;
PROC SORT DATA = DataAnalysis_&Data.; BY ID; RUN;

DATA Data_&Data.; MERGE Data_&Data. _CateG; BY ID; RUN;
DATA DataAnalysis_&Data.; MERGE DataAnalysis_&Data. _CateG; BY ID; RUN;

%MEND;
%Temp(Data = Train);
%Temp(Data = Test);


PROC DATASETS LIB=work NOPRINT; DELETE _:; RUN; QUIT;




* Step II: Modeling relationship between cognitive decline rate and average scores *;

*-----------------------*;
* Estimation of average score *;
*-----------------------*;

PROC UNIVARIATE DATA = DataAnalysis_Train NOPRINT;
  BY ID;
  VAR &Outcome.;
  OUTPUT OUT = Mean_out
  MEAN = Mean;
RUN;

PROC SORT DATA = DataAnalysis_Train; BY ID; RUN;
PROC SORT DATA = Mean_out; BY ID; RUN;
PROC SORT DATA = Est_Train; BY ID; RUN;

DATA DataAnalysis_Train; MERGE DataAnalysis_Train Mean_out Est_Train;
  BY ID;
RUN; 



*----------------------*;
* Estimation of parameter É¡ *;
*----------------------*;
%MACRO Temp2;

DATA DataAnalysis_Train; SET DataAnalysis_Train;
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
%Temp2;


DATA _Temp; SET DataAnalysis_Train(WHERE=(Visit_year=0)); RUN;

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
  CALL SYMPUTX('NObs_Visit', NOBS, 'G');
RUN;


PROC TRANSPOSE DATA = _Visit OUT = _Visit1; VAR F_Visit_year;RUN;

%MACRO Temp3;
DATA _Visit2; LENGTH COL $250; SET _Visit1;
  COL = COL1;
  %DO V = 2 %TO &NObs_Visit.;
   COL = catx("," , COL, COL&V.) ;
  %END;
RUN;
%MEND;
%Temp3;

DATA _Visit2; SET _Visit2;
  CALL SYMPUTX('Visit', COL, 'G');
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


%MACRO Temp4;
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
%Temp4;

DATA TrajALL; STOP; RUN;
DATA Data_DisTime_Train; STOP; RUN;
DATA Data_DisTime_Test; STOP; RUN;

%MACRO Temp5;

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

%MACRO Predict(Data);

DATA _Est_&Data.; SET Est_&Data.;
  KEEP ID CateG;
RUN;

PROC SORT DATA = _Est_&Data.; BY ID; RUN;
PROC SORT DATA = Data_&Data.; BY ID; RUN;

DATA Data_&Data.; MERGE Data_&Data. _Est_&Data.; BY ID; RUN;
DATA _Data_&Data.; SET Data_&Data.(WHERE = (CateG = &CateG.)); RUN;
DATA _Data1_&Data.; SET _Data_&Data.(WHERE = (Visit_year = 0)); RUN;
DATA _Data1_&Data.; SET _Data1_&Data.;
  TempID = _N_;
RUN;

DATA _Data1_&Data.; SET _Data1_&Data. NOBS = NObs;
  CALL SYMPUT('NObs_Data', NObs);
RUN;


DATA _Data4_&Data.; STOP; RUN;

%DO TempID = 1 %TO &NObs_Data.;

DATA _Data2_&Data.; SET _Data1_&Data.;
  CateG = &CateG.;
  IF TempID ^= &TempID. THEN DELETE;
RUN;


DATA Traj; SET Traj NOBS = NObs; 
  CALL SYMPUT('NObs_Traj', NObs);
RUN;


DATA _Data2_&Data.; SET _Data2_&Data.;
  DO i = 1 TO &NObs_Traj.;
    OUTPUT;
  END;
RUN;

DATA _Data2_&Data.; MERGE _Data2_&Data. Traj; 
  Dif = Pred - &Outcome.; * tau_{i0] ÇãÅÇﬂÇÈÇΩÇﬂÇÃçÏã∆ *;
  ABS_Dif = ABS(Dif);
RUN;

PROC SORT DATA = _Data2_&Data.;
  BY ABS_Dif;
RUN;

DATA _Data2_&Data.; SET _Data2_&Data.;
  DisTime = ROUND(DisTime, .01);
  DROP Visit_year;
RUN;

DATA _Data3_&Data.; SET _Data2_&Data.;
  BY ID;
  RETAIN BaseYear;
  IF first.id THEN BaseYear = DisTime;
  DO Visit_year = &Visit.;
    IF DisTime = BaseYear + Visit_year THEN OUTPUT;
  END;
  DROP &Outcome.;
RUN;

DATA _Data4_&Data.; SET _Data4_&Data. _Data3_&Data.; RUN;

%END;

PROC SORT DATA = _Data4_&Data.; BY ID Visit_year; RUN;
PROC SORT DATA = _Data_&Data.; BY ID Visit_year; RUN;

DATA _Data_DisTime_&Data.; MERGE _Data4_&Data. _Data_&Data.;
  BY ID Visit_year;
  PredError2 =  (Pred - &Outcome.)**2;
RUN;

DATA Traj; SET Traj;
  CateG = &CateG.;
RUN;


DATA TrajALL; SET TrajALL Traj; RUN;

DATA Data_DisTime_&Data.; SET Data_DisTime_&Data. _Data_DisTime_&Data.; RUN;

%MEND;
%Predict(Data = Train);
%Predict(Data = Test);

%END;

%MEND;
%Temp5;

PROC DATASETS LIB=work NOPRINT;
  DELETE _:;
RUN;
QUIT;


%MACRO Temp6(Data);

DATA _Temp_&Data.; SET Data_DisTime_&Data.(WHERE=(Visit_year = 0)); 
  DO Visit_year = &Visit.;
    OUTPUT;
  END;
  &Outcome. = .;
  KEEP ID Visit_year;
RUN;

PROC SORT DATA = _Temp_&Data.; BY ID Visit_year; RUN;
PROC SORT DATA = Data_DisTime_&Data.; BY ID Visit_year; RUN;

DATA Data_Result_&Data.; MERGE _Temp_&Data. Data_DisTime_&Data.;
  BY ID Visit_year;
RUN;

DATA Data_Result_&Data.; SET Data_Result_&Data.;
  B = ROUND(BaseYear, 1);
  PredImp = Pred;
  IF B = &MaxTau. OR B = . THEN PredImp = &MaxOutcome.; 
  PredErrorImp2 = (PredImp - &Outcome.)**2;
RUN;

PROC SORT DATA = Data_Result_&Data.; BY ID Visit_year; RUN;





****************;
* Prediction Error *;
****************;

DATA Data_Result; SET Data_Result_&Data.; RUN;
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

DATA PredResult_&Data.; SET _PredResult2_Method2G; 
  SMPE = SQRT(MPE);
  MPE = ROUND(MPE, .01);
  SMPE = ROUND(SMPE, .01);
RUN;


%MEND;
%Temp6(Data = Train);
%Temp6(Data = Test);

%MEND;


%MACRO Roop;

DATA CV_Train; STOP; RUN;
DATA CV_Test; STOP; RUN;

%DO SEED = 1 %TO 1000;
%CV;

PROC FREQ DATA = TrajALL;
  TABLE CateG;
  ODS OUTPUT OneWayFreqs = _Temp;
RUN;

DATA _Temp; SET _Temp;
  IF Frequency ^= . THEN Frequency = 1;
  KEEP CateG Frequency;
RUN;

PROC SORT DATA = PredResult_Train; BY CateG; RUN;
PROC SORT DATA = PredResult_Test; BY CateG; RUN;
PROC SORT DATA = _Temp; BY CateG; RUN;

DATA PredResult_Train; MERGE PredResult_Train _Temp; BY CateG; 
  IF Frequency = . THEN Frequency = 0;
  Rep = &Seed.;
RUN;

DATA PredResult_Test; MERGE PredResult_Test _Temp; BY CateG; 
  IF Frequency = . THEN Frequency = 0;
  Rep = &Seed.;
RUN;


DATA CV_Train; SET CV_Train PredResult_Train; RUN;
DATA CV_Test; SET CV_Test PredResult_Test; RUN;

DM LOG 'CLEAR';
DM OUTPUT 'CLEAR';

PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Trajectory Prediction_JADNI PredError_NoAdj_JADNI CV_Train CV_Test DataOrigin;
RUN;

%END;

%MEND;
%Roop;

PROC SORT DATA = CV_Train; BY CateG; RUN;
PROC SORT DATA = CV_Test; BY CateG; RUN;

PROC UNIVARIATE DATA = CV_Train NOPRINT;
  BY CateG;
  VAR SMPE;
  OUTPUT OUT = Sum_CV_Train
  MEAN =  M_SMPE;
RUN;


PROC UNIVARIATE DATA = CV_Test NOPRINT;
  BY CateG;
  VAR SMPE;
  OUTPUT OUT = Sum_CV_Test
  MEAN =  M_SMPE;
RUN;



DATA CV_Train_JADNI; SET CV_Train;
  RENAME SMPE = AveragePredictiveError;
  KEEP CateG SMPE Freq Rep;
RUN;

DATA CV_Test_JADNI; SET CV_Test;
  RENAME SMPE = AveragePredictiveError;
  KEEP CateG SMPE Freq Rep;
RUN;

DATA PredError_CV_Train_JADNI; SET Sum_CV_Train;
  RENAME M_SMPE = AveragePredictiveError;
  ATTRIB _ALL_ LABEL = " " ;
RUN;

DATA PredError_CV_Test_JADNI; SET Sum_CV_Test;
  RENAME M_SMPE = AveragePredictiveError;
  ATTRIB _ALL_ LABEL = " " ;
RUN;
