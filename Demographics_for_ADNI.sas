
* Note 
 The user can execute this program by inputting only "Input Parameters".
*;

* Output Datasets
  Demo_table_all: Participantsf demographics of ADNI cohort
  Demo_table_ab_or_apoe: Participantsf demographics of ADNI cohort by ABeta and APOE e4 status
  Demo_table_follow: Follow-up period for each cognitive function test score in ABeta-positive participants of ADNI cohort
*;


*****************;
* Input Parameters *;
*****************;

* Folder pass where this program is saved *;
%LET ProgramFolder = ;

* Folder pass where ADNI data is saved *;
%LET DataFolderADNI = ;
LIBNAME Data "&DataFolderADNI.";

* Folder pass where J-ADNI data is saved *;
%LET DataFolderJADNI = ;

* ADNI DataSet *;
%LET ADNIData = ;

* Folder name to save the results *;
%LET ResultFolder = ;
%LET rc=%SYSFUNC(DCREATE(&ResultFolder., &ProgramFolder.\));
LIBNAME Result "&ProgramFolder.\&ResultFolder.";

* Enter the number of outcome measurement time points after baseline to define the analysis population *;
%LET ExNumOut = 1;

* Maximum years of predictive curve for outcome *;
%LET MaxTau = 50; 



***************************************************
***** The user does not need to modify following codes. *****
***************************************************;



*************;
* Loading Data *;
*************;

%INCLUDE "&ProgramFolder.\ADNI_DataOrigin.sas";



**********************;
* Create Analysis Dataset *;
**********************;

DATA Data; SET DataOrigin;
  IF bl_Diagnostic ="CN" THEN bl_Diagnostic ="NC";
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
RUN;



**************************************;
* Participantsf demographics of ADNI cohort *;
**************************************;

DATA Demo_Table; STOP; RUN;

%MACRO Summary;

%IF &Dig. = ALL %THEN %DO;
DATA TempData; SET Data(WHERE=(Visit_year = 0)); RUN;
%END;

%ELSE %DO;
DATA TempData; SET Data(WHERE=(Visit_year = 0 AND bl_Diagnostic = "&Dig.")); RUN;
%END;


DATA Demo; STOP; RUN;

%MACRO Demo(Var, No);
PROC UNIVARIATE DATA = TempData NOPRINT;
  VAR &Var.;
  OUTPUT OUT=_Temp
  N=N
  MEAN=Mean
  STD=SD  
  MEDIAN=Med
  Q1=Q1
  Q3=Q3;
RUN;

DATA _Temp; RETAIN N Mean SD Med Min Max; SET _Temp; 
  Mean=ROUND(Mean, .1);
  SD=ROUND(SD, .1);
  Var1=CATT(Mean, ' } ', SD);
  Var2=CATT(Med, ' (', Q1, ' ,', Q3, ')');
  KEEP N Var1 Med Var2;
RUN;

DATA _Temp; RETAIN N Var1 Med Var2; SET _Temp; RUN;

PROC TRANSPOSE DATA=_Temp OUT=_Temp1;
  VAR N Var1 Med Var2;
RUN;

DATA _Temp1; 
  LENGTH Var $20 _Name_ $20; 
  RETAIN Var; 
  SET _Temp1;

  Var="&Var.";
  IF _Name_="Var1" THEN _Name_="•½‹Ï’l } SD";
  IF _Name_="Med" THEN _Name_="’†‰›’l";
  IF _Name_="Var2" THEN _Name_="Q1, Q3";

  RENAME _Name_=Stat;
  RENAME COL1=Value;
  ATTRIB _ALL_ LABEL="";

  No = &No.;

  DROP _LABEL_;

RUN;

DATA Demo; SET Demo _Temp1; RUN;

PROC DATASETS LIB = work NOPRINT;
  DELETE _:;
RUN;
QUIT;

%MEND;

%MACRO DemoF(Var, No);

PROC FREQ DATA = TempData;
  TABLE &Var.;
  ODS OUTPUT OneWayFreqs=_Temp;
RUN;

DATA _Temp; LENGTH Var $20; SET _Temp;
  Var="&Var.";
  Percent=ROUND(Percent, .1);
  Value=CATT(Frequency, ' (', Percent, '%)');

  RENAME &Var.= Stat;

  No = &No.;

  KEEP Var Value &Var. No;

RUN;

DATA Demo; SET Demo _Temp; RUN;

PROC DATASETS LIB=work NOPRINT;
  DELETE _:;
RUN;
QUIT;

%MEND;

%DemoF(Var = PTGENDER, No = 1);
%Demo(Var = AGE, No = 2);
%Demo(Var = PTEDUCAT, No = 3);
%DemoF(Var = APOE, No = 4);
%DemoF(Var = AB, No = 5);
%DemoF(Var = bl_Diagnostic, No = 6);
%Demo(Var = MMSE, No = 7);
%Demo(Var = CDRSB, No = 8);
%Demo(Var = ADAS13, No = 9);

PROC SORT DATA = Demo; BY No; RUN;

DATA Demo1; SET Demo;
  Value_&Dig. = Value;
  IF Var = "AGE" AND Stat ^= "Q1, Q3" THEN DELETE;
  IF Var = "PTGENDER" AND Stat ^= "Male" THEN DELETE;
  IF Var = "PTEDUCAT" AND Stat ^= "Q1, Q3" THEN DELETE;
  IF Var = "APOE" AND Stat ^= "Posi" THEN DELETE;
  IF Var = "AB" AND Stat ^= "Posi" THEN DELETE;
  IF Var = "MMSE" AND Stat ^= "Q1, Q3" THEN DELETE;
  IF Var = "CDRSB" AND Stat ^= "Q1, Q3" THEN DELETE;
  IF Var = "ADAS13" AND Stat ^= "Q1, Q3" THEN DELETE;
RUN;

DATA Demo_Table; MERGE Demo_Table Demo1; 
  BY No;
RUN;

%MEND;

DATA Demo_Table; STOP; RUN;

%LET Dig = ALL;
%LET No = 1;
%Summary;

DATA Demo_Table; MERGE Demo_Table Demo1; RUN;

%LET Dig = NC;
%LET No = 2;
%Summary;

%LET Dig = SMC;
%LET No = 3;
%Summary;

%LET Dig = EMCI;
%LET No = 4;
%Summary;

%LET Dig = LMCI;
%LET No = 5;
%Summary;

%LET Dig = AD;
%LET No = 6;
%Summary;

DATA Demo_Table_ALL; SET Demo_Table;
  DROP Value;
RUN;


*************************************************************;
* Participantsf demographics of ADNI cohort by AƒÀ and APOE ?4 status. *;
*************************************************************;

DATA Demo_Table; STOP; RUN;

%MACRO Summary(AnalysisSet, No);

%IF &AnalysisSet. = ALL %THEN %DO;
DATA _Data; SET Data; RUN;
%END;

%IF &AnalysisSet. = APOEPosi %THEN %DO;
DATA _Data; SET Data(WHERE = (APOE = "Posi")); RUN;
%END;

%IF &AnalysisSet. = ABETA_Posi %THEN %DO;
DATA _Data; SET Data(WHERE = (AB = "Posi")); RUN;
%END;

%IF &AnalysisSet. = APOENega %THEN %DO;
DATA _Data; SET Data(WHERE = (APOE = "Nega")); RUN;
%END;

%IF &AnalysisSet. = ABETA_Nega %THEN %DO;
DATA _Data; SET Data(WHERE = (AB = "Nega")); RUN;
%END;


DATA TempData; SET _Data(WHERE=(Visit_year = 0)); RUN;
DATA Demo; STOP; RUN;

%MACRO Demo(Var, No);
PROC UNIVARIATE DATA = TempData NOPRINT;
  VAR &Var.;
  OUTPUT OUT=_Temp
  N=N
  MEAN=Mean
  STD=SD  
  MEDIAN=Med
  Q1=Q1
  Q3=Q3;
RUN;

DATA _Temp; RETAIN N Mean SD Med Min Max; SET _Temp; 
  Mean=ROUND(Mean, .1);
  SD=ROUND(SD, .1);
  Var1=CATT(Mean, ' } ', SD);
  Var2=CATT(Med, '(', Q1, ' ,', Q3, ')');
  KEEP N Var1 Med Var2;
RUN;

DATA _Temp; RETAIN N Var1 Med Var2; SET _Temp; RUN;

PROC TRANSPOSE DATA=_Temp OUT=_Temp1;
  VAR N Var1 Med Var2;
RUN;

DATA _Temp1; 
  LENGTH Var $20 _Name_ $20; 
  RETAIN Var; 
  SET _Temp1;

  Var="&Var.";
  IF _Name_="Var1" THEN _Name_="•½‹Ï’l } SD";
  IF _Name_="Med" THEN _Name_="’†‰›’l";
  IF _Name_="Var2" THEN _Name_="Q1, Q3";

  RENAME _Name_=Stat;
  RENAME COL1=Value;
  ATTRIB _ALL_ LABEL="";

  No = &No.;

  DROP _LABEL_;

RUN;

DATA Demo; SET Demo _Temp1; RUN;

PROC DATASETS LIB = work NOPRINT;
  DELETE _:;
RUN;
QUIT;

%MEND;

%MACRO DemoF(Var, No);

PROC FREQ DATA = TempData;
  TABLE &Var.;
  ODS OUTPUT OneWayFreqs=_Temp;
RUN;

DATA _Temp; LENGTH Var $20; SET _Temp;
  Var="&Var.";
  Percent=ROUND(Percent, .1);
  Value=CATT(Frequency, ' (', Percent, '%)');

  RENAME &Var.= Stat;

  No = &No.;

  KEEP Var Value &Var. No;

RUN;

DATA Demo; SET Demo _Temp; RUN;

PROC DATASETS LIB=work NOPRINT;
  DELETE _:;
RUN;
QUIT;

%MEND;

%DemoF(Var = PTGENDER, No = 1);
%Demo(Var = AGE, No = 2);
%Demo(Var = PTEDUCAT, No = 3);
%DemoF(Var = APOE, No = 4);
%DemoF(Var = AB, No = 5);
%DemoF(Var = bl_Diagnostic, No = 6);
%Demo(Var = MMSE, No = 7);
%Demo(Var = CDRSB, No = 8);
%Demo(Var = ADAS13, No = 9);

PROC SORT DATA = Demo; BY No; RUN;

DATA Demo1; SET Demo;
  Value_&AnalysisSet. = Value;
  IF Var = "AGE" AND Stat ^= "Q1, Q3" THEN DELETE;
  IF Var = "PTGENDER" AND Stat ^= "Male" THEN DELETE;
  IF Var = "PTEDUCAT" AND Stat ^= "Q1, Q3" THEN DELETE;
  IF Var = "APOE" AND Stat ^= "Posi" THEN DELETE;
  IF Var = "AB" AND Stat ^= "Posi" THEN DELETE;
  IF Var = "MMSE" AND Stat ^= "Q1, Q3" THEN DELETE;
  IF Var = "CDRSB" AND Stat ^= "Q1, Q3" THEN DELETE;
  IF Var = "ADAS13" AND Stat ^= "Q1, Q3" THEN DELETE;
RUN;

DATA Demo_Table; MERGE Demo_Table Demo1; 
  BY No;
RUN;

%MEND;

%Summary(AnalysisSet = ALL, No = 1);
DATA Demo_Table; MERGE Demo_Table Demo1; RUN;
%Summary(AnalysisSet = ABETA_Posi, No =2);
%Summary(AnalysisSet = ABETA_Nega, No =3);
%Summary(AnalysisSet = APOEPosi, No =4);
%Summary(AnalysisSet = APOENega, No =5);

DATA Demo_Table_AB_or_APOE; SET Demo_Table;
  DROP Value;
RUN;





**********************************************************************;
* Follow-up period for each cognitive function test score in AƒÀ-positive participants *;
**********************************************************************;

DATA Demo_Table; STOP; RUN;

%MACRO Summary(Outcome, No);

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

  IF bl_PIB > 1.5 OR bl_AV45 > 1.11 OR .< bl_ABETA < 192 THEN AB = "Posi";
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


DATA Data; SET Data(WHERE = (AB = "Posi")); RUN;

PROC UNIVARIATE DATA = Data NOPRINT;
  VAR Visit_year;
  BY ID;
  OUTPUT OUT = _Follow
  MAX = Followup;
RUN;

DATA Data; MERGE Data _Follow; BY ID; RUN;

DATA DataAnalysis; SET Data; RUN;



DATA TempData; SET Data(WHERE=(Visit_year = 0)); RUN;
DATA Demo; STOP; RUN;

%MACRO Demo(Var, No);

PROC UNIVARIATE DATA = TempData NOPRINT;
  VAR &Var.;
  OUTPUT OUT=_Temp
  N=N
  MEAN=Mean
  STD=SD  
  MEDIAN=Med
  Q1=Q1
  Q3=Q3;
RUN;

DATA _Temp; RETAIN N Mean SD Med Min Max; SET _Temp; 
  Mean=ROUND(Mean, .1);
  SD=ROUND(SD, .1);
  Var1=CATT(Mean, ' } ', SD);
  Var2=CATT(Med, '(', Q1, ' ,', Q3, ')');
  KEEP N Var1 Med Var2;
RUN;

DATA _Temp; RETAIN N Var1 Med Var2; SET _Temp; RUN;

PROC TRANSPOSE DATA=_Temp OUT=_Temp1;
  VAR N Var1 Med Var2;
RUN;

DATA _Temp1; 
  LENGTH Var $20 _Name_ $20; 
  RETAIN Var; 
  SET _Temp1;

  Var="&Var.";
  IF _Name_="Var1" THEN _Name_="•½‹Ï’l } SD";
  IF _Name_="Med" THEN _Name_="’†‰›’l";
  IF _Name_="Var2" THEN _Name_="Q1, Q3";

  RENAME _Name_=Stat;
  RENAME COL1=Value;
  ATTRIB _ALL_ LABEL="";

  No = &No.;

  DROP _LABEL_;

RUN;

DATA Demo; SET Demo _Temp1; RUN;

PROC DATASETS LIB = work NOPRINT;
  DELETE _:;
RUN;
QUIT;

%MEND;

%Demo(Var = Followup, No = 10);

PROC SORT DATA = Demo; BY No; RUN;

DATA Demo1; SET Demo;
  Value_&Outcome. = Value;
  IF Var = "Followup" AND Stat ^= "Q1, Q3" AND Stat ^= "N" THEN DELETE;
RUN;

DATA Demo_Table; MERGE Demo_Table Demo1; 
  BY No;
RUN;

%MEND;
%Summary(Outcome = MMSE, No = 1);
DATA Demo_Table; MERGE Demo_Table Demo1; RUN;
%Summary(Outcome = ADAS13, No =2);
%Summary(Outcome = CDRSB, No =3);

DATA Demo_Table_Follow; SET Demo_Table;
  DROP Value;
RUN;



%MACRO FigA(AnalysisSet, Outcome);


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

DATA _Data0; MERGE _Data0 _N;
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

DATA DataAnalysis; SET DataAnalysis;
  IF bl_Diagnostic = "NC" THEN Sort = 1;
  IF bl_Diagnostic = "SMC" THEN Sort = 2;
  IF bl_Diagnostic = "EMCI" THEN Sort = 3;
  IF bl_Diagnostic = "LMCI" THEN Sort = 4;
  IF bl_Diagnostic = "AD" THEN Sort = 5;
RUN;

PROC SORT DATA = DataAnalysis; BY Sort; RUN;

ODS GRAPHICS ON / RESET = ALL IMAGENAME = "&Outcome._ADNI" IMAGEFMT = PNG ATTRPRIORITY = NONE NOBORDER;
GOPTIONS FTEXT = 'Meiryo' FTITLE = 'Meiryo' HTITLE = 2 HTEXT = 2 VSIZE = 15cm HSIZE = 30cm  ;
ODS HTML GPATH = "&ProgramFolder.\&ResultFolder." STYLE = MyJournal3;

%IF &Outcome. = MMSE %THEN %DO;

PROC SGPANEL DATA = DataAnalysis NOAUTOLEGEND;
  STYLEATTRS
/*    DATACONTRASTCOLORS = (Cyan Olive Salmon)*/
	DATACONTRASTCOLORS = (Black Black Black Black Black)
    DATASYMBOLS = (Circle Triangle Square Diamond TriangleDown)
	DATALINEPATTERNS = (1 2 4 5 8)
	WALLCOLOR = WhiteSmoke
  ;
  PANELBY bl_Diagnostic / SORT = DATA LAYOUT = COLUMNLATTICE COLUMNS = 5 
                                        HEADERATTRS = (SIZE = 16 Family = Meiryo) NOVARNAME;
  SERIES X = Visit_year Y = MMSE / GROUP = bl_Diagnostic MARKERS LINEATTRS = (THICKNESS= 1) MARKERATTRS = (SIZE = 15);

  COLAXIS LABEL = "Follow-up (year)" LABELATTRS = (SIZE = 16 Family = Meiryo)
            VALUE = (0 TO 15 BY 5) VALUEATTRS = (SIZE = 16 Family = Meiryo) OFFSETMIN = 0.1 OFFSETMAX = 0.3
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);

  ROWAXIS LABEL = "MMSE" LABELATTRS = (SIZE = 16 Family = Meiryo)
            VALUE = (0 TO 30 BY 5) VALUEATTRS = (SIZE = 16 Family = Meiryo) 
            OFFSETMIN = 0.01 OFFSETMAX = 0.05
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);
RUN;

%END;


%IF &Outcome. = CDRSB %THEN %DO;

PROC SGPANEL DATA = DataAnalysis NOAUTOLEGEND;
  STYLEATTRS
/*    DATACONTRASTCOLORS = (Cyan Olive Salmon)*/
	DATACONTRASTCOLORS = (Black Black Black Black Black)
    DATASYMBOLS = (Circle Triangle Square Diamond TriangleDown)
	DATALINEPATTERNS = (1 2 4 5 8)
	WALLCOLOR = WhiteSmoke
  ;
  PANELBY bl_Diagnostic / SORT = DATA LAYOUT = COLUMNLATTICE COLUMNS = 5 
                                        HEADERATTRS = (SIZE = 16 Family = Meiryo) NOVARNAME;
  SERIES X = Visit_year Y = CDRSB / GROUP = bl_Diagnostic MARKERS LINEATTRS = (THICKNESS= 1) MARKERATTRS = (SIZE = 15);

  COLAXIS LABEL = "Follow-up (year)" LABELATTRS = (SIZE = 16 Family = Meiryo)
            VALUE = (0 TO 15 BY 5) VALUEATTRS = (SIZE = 16 Family = Meiryo) OFFSETMIN = 0.1 OFFSETMAX = 0.3
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);

  ROWAXIS LABEL = "CDR-SOB" LABELATTRS = (SIZE = 16 Family = Meiryo)
            VALUE = (0 TO 18 BY 3) VALUEATTRS = (SIZE = 16 Family = Meiryo) 
            OFFSETMIN = 0.01 OFFSETMAX = 0.05
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);
RUN;

%END;



%IF &Outcome. = ADAS13 %THEN %DO;

PROC SGPANEL DATA = DataAnalysis NOAUTOLEGEND;
  STYLEATTRS
/*    DATACONTRASTCOLORS = (Cyan Olive Salmon)*/
	DATACONTRASTCOLORS = (Black Black Black Black Black)
    DATASYMBOLS = (Circle Triangle Square Diamond TriangleDown)
	DATALINEPATTERNS = (1 2 4 5 8)
	WALLCOLOR = WhiteSmoke
  ;
  PANELBY bl_Diagnostic / SORT = DATA LAYOUT = COLUMNLATTICE COLUMNS = 5 
                                        HEADERATTRS = (SIZE = 16 Family = Meiryo) NOVARNAME;
  SERIES X = Visit_year Y = ADAS13 / GROUP = bl_Diagnostic MARKERS LINEATTRS = (THICKNESS= 1) MARKERATTRS = (SIZE = 15);

  COLAXIS LABEL = "Follow-up (year)" LABELATTRS = (SIZE = 16 Family = Meiryo)
            VALUE = (0 TO 15 BY 5) VALUEATTRS = (SIZE = 16 Family = Meiryo) OFFSETMIN = 0.1 OFFSETMAX = 0.3
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);

  ROWAXIS LABEL = "ADAS-Cog" LABELATTRS = (SIZE = 16 Family = Meiryo)
            VALUE = (0 TO 75 BY 15 85) VALUEATTRS = (SIZE = 16 Family = Meiryo) 
            OFFSETMIN = 0.01 OFFSETMAX = 0.05
            DISPLAY = (NOLINE NOTICKS) GRID GRIDATTRS = (COLOR = WHITE);
RUN;

%END;

%MEND;
%FigA(AnalysisSet=ABETA_Posi, Outcome=MMSE);
%FigA(AnalysisSet=ABETA_Posi, Outcome=CDRSB);
%FigA(AnalysisSet=ABETA_Posi, Outcome=ADAS13);

DATA Demo_table_all_ADNI; SET Demo_table_all; RUN;
DATA Demo_table_ab_or_apoe_ADNI; SET Demo_table_ab_or_apoe; RUN;
DATA Demo_table_follow_ADNI; SET Demo_table_follow; RUN;

PROC DATASETS LIB = WORK NOLIST MEMTYPE = DATA;
  SAVE Demo_table_all_ADNI Demo_table_ab_or_apoe_ADNI Demo_table_follow_ADNI;
RUN;

PROC COPY IN = Work OUT = Result MEMTYPE = (data); RUN;
