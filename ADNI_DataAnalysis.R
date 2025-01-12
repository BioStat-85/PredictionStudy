
library(readr)
library(dplyr)

# SiteFolder <- 
# SiteData <- 

# Read the data
DataOrigin <- read_csv(
  file.path(DataFolderADNI, ADNIData),
  col_names = FALSE,
  col_types = cols(.default = col_character()),
  skip = 1
)

# Assign column names
colnames(DataOrigin) <- paste0("VAR", 1:27)

# Replace specific values in the data
DataOrigin <- DataOrigin %>%
  mutate(
    VAR13 = ifelse(VAR13 == "<200", "200", ifelse(VAR13 == ">1700", "1700", VAR13)),
    VAR22 = ifelse(VAR22 == "<200", "200", ifelse(VAR22 == ">1700", "1700", VAR22)),
    VAR14 = ifelse(VAR14 == "<80", "80", ifelse(VAR14 == ">1300", "1300", VAR14)),
    VAR23 = ifelse(VAR23 == "<80", "80", ifelse(VAR23 == ">1300", "1300", VAR23)),
    VAR15 = ifelse(VAR15 == "<8", "8", ifelse(VAR15 == ">120", "120", VAR15)),
    VAR24 = ifelse(VAR24 == "<8", "8", ifelse(VAR24 == ">120", "120", VAR24))
  )

# Convert variables and create new columns
DataOrigin <- DataOrigin %>%
  mutate(ID = as.numeric(VAR1),
    VISCODE = VAR2,
    M = as.numeric(VAR3),
    Visit_year = M / 12,
    bl_Diagnostic = VAR4,
    Diagnostic = VAR5,
    AGE = as.numeric(VAR6),
    PTGENDER = VAR7,
    PTEDUCAT = as.numeric(VAR8),
    APOE4 = as.numeric(VAR9),
    bl_FDG = as.numeric(VAR10),
    bl_PIB = as.numeric(VAR11),
    bl_AV45 = as.numeric(VAR12),
    bl_ABETA = as.numeric(VAR13),
    bl_TAU = as.numeric(VAR14),
    bl_PTAU = as.numeric(VAR15),
    bl_CDRSB = as.numeric(VAR16),
    bl_ADAS13 = as.numeric(VAR17),
    bl_MMSE = as.numeric(VAR18),
    FDG = as.numeric(VAR19),
    PIB = as.numeric(VAR20),
    AV45 = as.numeric(VAR21),
    ABETA = as.numeric(VAR22),
    TAU = as.numeric(VAR23),
    PTAU = as.numeric(VAR24),
    CDRSB = as.numeric(VAR25),
    ADAS13 = as.numeric(VAR26),
    MMSE = as.numeric(VAR27)) %>%
  select(-starts_with("VAR"), -bl_CDRSB, -bl_ADAS13, -bl_MMSE)

# Create _Temp dataset for baseline values
Temp <- DataOrigin %>%
  filter(M == 0) %>%
  select(ID, bl_CDRSB = CDRSB, bl_ADAS13 = ADAS13, bl_MMSE = MMSE)


D <- DataOrigin[order(DataOrigin$ID, DataOrigin$M), ]
T <- Temp[order(Temp$ID), ]
T2 <- T %>% filter(if_any(c(bl_CDRSB, bl_ADAS13, bl_MMSE), ~!is.na(.)))


# Merge datasets by ID
DataOrigin <- merge(D, T2, by = "ID", all = TRUE)
rm(list = c("D", "T", "T2", "Temp"))

# Create Analysis Dataset
Data0 <- DataOrigin %>%
  mutate(
    bl_Diagnostic = ifelse(bl_Diagnostic == "CN", "NC", bl_Diagnostic),
    APOE = ifelse(APOE4 >= 1, "Posi", "Nega"),
    # AB = ifelse(bl_PIB > 1.5 | bl_AV45 > 1.11 | (bl_ABETA > . & bl_ABETA < 192), "Posi", "Nega"),
    AB = case_when(bl_PIB > 1.5 | bl_AV45 > 1.11 | (bl_ABETA >=0 & bl_ABETA < 192) ~ "Posi", TRUE ~ "Nega"),
    DifOutcome = !!as.name(Outcome) - !!as.name(paste0("bl_", Outcome))
  ) %>%
  filter(
    !is.na(!!as.name(paste0("bl_", Outcome))),
    !is.na(bl_Diagnostic),
    !is.na(AGE),
    !is.na(PTGENDER),
    !is.na(PTEDUCAT),
    !is.na(APOE4),
    !is.na(bl_PIB) | !is.na(bl_AV45) | !is.na(bl_ABETA)
  )

# PROC UNIVARIATEに相当する処理（N_Outを計算）
N_Output <- Data0 %>%
  filter(
    !is.na(!!as.name(Outcome))
  )


# Calculate number of Outcome observations per ID
N_Out <- N_Output %>%
  group_by(ID) %>%
  summarise(N_Out = n(), .groups = 'drop')


# Merge datasets and filter based on ExNumOut
Data00 <- merge(Data0, N_Out, by = "ID", all = TRUE)

ExNumOut <- 1

Data000 <- Data00 %>%
  filter(N_Out > ExNumOut + 1)

AnalysisSet <- "ABETA_Posi"

# Subset data based on AnalysisSet
Data_ADNI <- Data000 %>%
  filter(
    (AnalysisSet == "APOEPosi" & APOE == "Posi") |
      (AnalysisSet == "ABETA_Posi" & AB == "Posi") |
      (AnalysisSet == "APOENega" & APOE == "Nega") |
      (AnalysisSet == "ABETA_Nega" & AB == "Nega") |
      (AnalysisSet == "ALL")
  )

rm(list = c("DataOrigin", "Data0", "Data00", "Data000", "N_Out", "N_Output"))

DataSite <- read_csv(
  file.path(SiteFolder,SiteData),
  col_names = FALSE,
  col_types = cols(.default = col_character()),
  skip = 1
)

DataSite <- DataSite %>%
  mutate(ID = as.numeric(X1),
         Site = X2,
         M = as.numeric(X3),
  )

Data_ADNI <- Data_ADNI %>%
  inner_join(DataSite, by = c("ID", "M"))

