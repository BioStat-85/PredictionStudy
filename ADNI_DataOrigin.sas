
DATA _DataOrigin;
  LENGTH VAR1-VAR27 $10000;
  INFILE "&DataFolderADNI.\&ADNIData." DSD MISSOVER LRECL = 30000 FIRSTOBS = 2;
  INPUT VAR1-VAR27;
RUN;

DATA _DataOrigin; SET _DataOrigin;
  IF VAR13 = "<200" THEN VAR13 = "200";
  IF VAR13 = ">1700" THEN VAR13 = "1700";
  IF VAR22 = "<200" THEN VAR22 = "200";
  IF VAR22 = ">1700" THEN VAR22 = "1700";

  IF VAR14 = "<80" THEN VAR14 = "80";
  IF VAR14 = ">1300" THEN VAR14 = "1300";
  IF VAR23 = "<80" THEN VAR23 = "80";
  IF VAR23 = ">1300" THEN VAR23 = "1300";

  IF VAR15 = "<8" THEN VAR15 = "8";
  IF VAR15 = ">120" THEN VAR15 = "120";
  IF VAR24 = "<8" THEN VAR24 = "8";
  IF VAR24 = ">120" THEN VAR24 = "120";

DATA _DataOrigin; SET _DataOrigin;
  ID = INPUT(VAR1, best32.);
  VISCODE = VAR2;
  M = INPUT(VAR3, best32.);
  Visit_year = M/12;
  bl_Diagnostic = VAR4;
  Diagnostic = VAR5;
  AGE = INPUT(VAR6, best32.);
  PTGENDER = VAR7;
  PTEDUCAT = INPUT(VAR8, best32.);
  APOE4 = INPUT(VAR9, best32.);
  bl_FDG = INPUT(VAR10, best32.);
  bl_PIB = INPUT(VAR11, best32.);
  bl_AV45 = INPUT(VAR12, best32.);
  bl_ABETA = INPUT(VAR13, best32.);
  bl_TAU = INPUT(VAR14, best32.);
  bl_PTAU = INPUT(VAR15, best32.);
  bl_CDRSB = INPUT(VAR16, best32.);
  bl_ADAS13 = INPUT(VAR17, best32.);
  bl_MMSE = INPUT(VAR18, best32.);
  FDG = INPUT(VAR19, best32.);
  PIB = INPUT(VAR20, best32.);
  AV45 = INPUT(VAR21, best32.);
  ABETA = INPUT(VAR22, best32.);
  TAU = INPUT(VAR23, best32.);
  PTAU = INPUT(VAR24, best32.);
  CDRSB = INPUT(VAR25, best32.);
  ADAS13 = INPUT(VAR26, best32.);
  MMSE = INPUT(VAR27, best32.);

  DROP VAR1-VAR27 bl_CDRSB bl_ADAS13 bl_MMSE;
RUN;

DATA _Temp; SET _DataOrigin(WHERE = (Visit_year = 0));
  bl_CDRSB = CDRSB;
  bl_ADAS13 = ADAS13;
  bl_MMSE = MMSE;
  KEEP ID bl_CDRSB bl_ADAS13 bl_MMSE;
RUN;

PROC SORT DATA = _DataOrigin; BY ID; RUN;
PROC SORT DATA = _Temp; BY ID; RUN;

DATA DataOrigin; MERGE _DataOrigin _Temp; BY ID; RUN;

PROC DELETE DATA = WORK.JADNI WORK.CSF WORK.PET; RUN;
PROC DATASETS LIB = work NOPRINT; DELETE _:; RUN; QUIT;
