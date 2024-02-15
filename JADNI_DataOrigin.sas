
PROC COPY IN = Data OUT = Work; 
  SELECT jadni;
RUN;

PROC SORT DATA = Jadni OUT = _DataOrigin;
  BY id;
RUN;

%MACRO DataImp(File, Out);

PROC IMPORT DATAFILE = "&DataFolderJADNI.\&File."
  OUT = Work.&Out.
  DBMS = DLM 
  REPLACE;

  DELIMITER = '09'x;
  
  GETNAMES = YES;
  GUESSINGROWS = 10029;
RUN;

%MEND;

%DataImp(File = CSF_J-ADNI.tsv, Out = _CSF);
%DataImp(File = PETQC_J-ADNI.tsv, Out = _PET);

DATA CSF; SET _CSF;
  IF VISCODE ^= "BL" THEN DELETE;
  KEEP ID CSF_AB;
RUN;

DATA PET; SET _PET;
  IF PET_QC_BF227__VISIT_ID ^= "BL Scan" THEN DELETE;
  IF PET_QC_PIB__VISIT_ID ^= "BL Scan" THEN DELETE;
  BF227 = PET_QC_BF227__INTERPRETATION;
  PIB = PET_QC_PIB__INTERPRETATION;
  KEEP ID  BF227 PIB;
RUN;

DATA DataOrigin; MERGE _DataOrigin CSF PET;
  BY ID;
  IF PIB = "" THEN PIB = ".";
  IF BF227 ="" THEN BF227 = ".";
RUN;

DATA DataOrigin; SET DataOrigin;
  RENAME MMSE = MMSE;
  RENAME bl_MMSE = bl_MMSE;
  RENAME CDR_total = CDRSB;
  RENAME bl_CDR_total = bl_CDRSB;
  RENAME ADAS_mod = ADAS13;
  RENAME bl_ADAS_mod = bl_ADAS13;

  RENAME bl_AGE = AGE;
  RENAME GENDER = PTGENDER;
  RENAME eduperiod = PTEDUCAT;
  RENAME apoe_diag = APOE4;

  RENAME PID = ID;

  Visit_year1 = visit_year;
  DROP visit_year;
  RENAME Visit_year1 = Visit_year;
RUN;

/*PROC DATASETS LIB=WORK nolist memtype=data;*/
/*    SAVE DataOrigin;*/
/*RUN;*/
/*QUIT;*/
