
rm(list = ls())

# Required libraries
library(dplyr)
library(tidyr)
library(lme4)
library(mclust)
library(deSolve)
library(purrr)
library(writexl)
library(survey)


# Input Parameters
ProgramFolder <- "" #Location where this program is saved
Outcome <- "" # MMSE or CDRSB or ADAS13
DataFolderADNI <- "" #Location where ADNI data is saved
ADNIData <- "" # ADNI data file

MaxTau <-  # prediction period (set to 60 in the paper)


####################
# Import ADNI Data #
####################

source(file.path(ProgramFolder, "ADNI_DataAnalysis.R"))

SiteSummary <- Data_ADNI %>%
  distinct(ID, .keep_all = TRUE) %>%
  group_by(Site) %>%
  summarize(
    Count = n(),
    .groups = "drop"
  )

print(SiteSummary)

SiteSummary <- SiteSummary[order(-SiteSummary$Count), ]
SiteSummary$Group <- NA
group_sums <- rep(0, 5)

for (i in 1:nrow(SiteSummary)) {
  group_idx <- which.min(group_sums)
  SiteSummary$Group[i] <- group_idx
  group_sums[group_idx] <- group_sums[group_idx] + SiteSummary$Count[i]
}

Data_ADNI <- Data_ADNI %>%
  inner_join(SiteSummary, by = "Site")

PredALL <- data.frame()

for (seed in 1:5) {

Data <- Data_ADNI
DataAnalysis <- Data

Data_Train <- Data %>% filter(Group != seed)
Data_Test <- Data %>% filter(Group == seed)

Temp <- Data_Train %>%
  filter(Visit_year == 0, bl_Diagnostic == "NC")

F_BaseOutcome <- function(Outcome) {
  
  if (Outcome == "MMSE") {
    stats <- summary(Temp[[Outcome]])
    Max <- stats["Max."]
    if (Max == 30) {
      Max <- 29
    }
    BaseOutcome <- Max
  }
  
  else if (Outcome == "CDRSB") {
    stats <- summary(Temp[[Outcome]])
    Min <- stats["Min."]
    if (Min == 0) {
      Min <- 0.5
    }
    BaseOutcome <- Min
  }
  
  else if (Outcome == "ADAS13") {
    stats <- summary(Temp[[Outcome]])
    Min <- stats["Min."]
    if (Min == 0) {
      Min <- 1
    }
    BaseOutcome <- Min
  }
  
  return(BaseOutcome)
}

BaseOutcome <- F_BaseOutcome(Outcome)

model <- lmer(DifOutcome ~  0 + bl_Diagnostic:Visit_year + (0 + Visit_year|ID), 
              data = Data_Train, 
              control = lmerControl(optim="Nelder_Mead"),
              REML = FALSE
              )

fixed_effects <- data.frame(estimate = fixef(model))

list_bl_Diagnostic_df <- data.frame(bl_Diagnostic = sort(unique(DataAnalysis$bl_Diagnostic)))
fixed_effect <- cbind(list_bl_Diagnostic_df, fixed_effects)
colnames(fixed_effect) <- c("bl_Diagnostic", "Fixed_Effect")
random_effects <- ranef(model)$ID
random_effect <- data.frame(ID = rownames(random_effects), Random_Effect = random_effects[, "Visit_year"])


Disease_id <- Data_Train[Data_Train$Visit_year == 0, c("bl_Diagnostic", "ID")]
Disease_id <- Disease_id[order(Disease_id$bl_Diagnostic), ]
fixed_effect <- fixed_effect[order(fixed_effect$bl_Diagnostic), ]
fixed_effect1 <- merge(Disease_id, fixed_effect, by = "bl_Diagnostic")

fixed_effect1 <- fixed_effect1[order(fixed_effect1$ID), ]
random_effect <- random_effect[order(random_effect$ID), ]

Est <- merge(fixed_effect1, random_effect, by = "ID")
Est$Tilde_Beta <- Est$Fixed_Effect + Est$Random_Effect

rm(list = c("Disease_id", "fixed_effect", "fixed_effects", "fixed_effect1", 
            "list_bl_Diagnostic_df", "model", "random_effect", "random_effects", "Temp"))



Mean_out <- Data_Train %>%
  group_by(ID) %>%
  summarise(Mean = mean(!!sym(Outcome), na.rm = TRUE)) %>%
  arrange(ID)

Mean_out <- Mean_out[order(Mean_out$ID), ]
Data_Train <- Data_Train[order(Data_Train$ID), ]
Est <- Est[order(Est$ID), ]

Data_Train <- merge(Data_Train, Est, by = "ID")
Data_Train <- merge(Data_Train, Mean_out, by = "ID")


Temp <- subset(Data_Train, Visit_year == 0)


F_clusterdata <- function(Outcome) {
  if (Outcome == "MMSE") {
    Temp <- subset(Temp, subset = Tilde_Beta < 0)
  }
  
  else {
    Temp <- subset(Temp, subset = Tilde_Beta > 0)
  }
  
  return(Temp)
}

Temp <- F_clusterdata(Outcome)
Temp1 <- subset(Temp, select = c(ID, Mean, Tilde_Beta))

Temp1$GMM_Cluster <- NA

if (Outcome == "MMSE") {
  Temp1$GMM_Cluster <- ifelse(Temp1$Tilde_Beta < -4, 3,
                        ifelse(Temp1$Tilde_Beta >= -4 & Temp1$Tilde_Beta < -2, 2,
                               ifelse(Temp1$Tilde_Beta >= -2 & Temp1$Tilde_Beta < 0, 1, NA)))
} else if (Outcome == "CDRSB") {
  Temp1$GMM_Cluster <- ifelse(Temp1$Tilde_Beta >= 2, 3,
                        ifelse(Temp1$Tilde_Beta >= 1 & Temp1$Tilde_Beta < 2, 2,
                               ifelse(Temp1$Tilde_Beta >= 0 & Temp1$Tilde_Beta < 1, 1, NA)))
} else if (Outcome == "ADAS13") {
  Temp1$GMM_Cluster <- ifelse(Temp1$Tilde_Beta >= 6, 3,
                        ifelse(Temp1$Tilde_Beta >= 3 & Temp1$Tilde_Beta < 6, 2,
                               ifelse(Temp1$Tilde_Beta >= 0 & Temp1$Tilde_Beta < 3, 1, NA)))
}
Cluster <- subset(Temp1, select = c(ID, GMM_Cluster))

Data_Train <- merge(Data_Train, Cluster, by = "ID")

rm(list = c("Cluster", "Temp", "Temp1"))

model <- lmer(DifOutcome ~  0 + bl_Diagnostic:Visit_year + (0 + Visit_year|ID), 
              data = Data_Test, 
              control = lmerControl(optim="Nelder_Mead"),
              REML = FALSE
)

fixed_effects <- data.frame(estimate = fixef(model))

list_bl_Diagnostic_df <- data.frame(bl_Diagnostic = sort(unique(DataAnalysis$bl_Diagnostic)))
fixed_effect <- cbind(list_bl_Diagnostic_df, fixed_effects)
colnames(fixed_effect) <- c("bl_Diagnostic", "Fixed_Effect")
random_effects <- ranef(model)$ID
random_effect <- data.frame(ID = rownames(random_effects), Random_Effect = random_effects[, "Visit_year"])


Disease_id <- Data_Test[Data_Test$Visit_year == 0, c("bl_Diagnostic", "ID")]
Disease_id <- Disease_id[order(Disease_id$bl_Diagnostic), ]
fixed_effect <- fixed_effect[order(fixed_effect$bl_Diagnostic), ]
fixed_effect1 <- merge(Disease_id, fixed_effect, by = "bl_Diagnostic")

fixed_effect1 <- fixed_effect1[order(fixed_effect1$ID), ]
random_effect <- random_effect[order(random_effect$ID), ]

Est <- merge(fixed_effect1, random_effect, by = "ID")
Est$Tilde_Beta <- Est$Fixed_Effect + Est$Random_Effect

rm(list = c("Disease_id", "fixed_effect", "fixed_effects", "fixed_effect1", 
            "list_bl_Diagnostic_df", "model", "random_effect", "random_effects"))

Mean_out <- Data_Test %>%
  group_by(ID) %>%
  summarise(Mean = mean(!!sym(Outcome), na.rm = TRUE)) %>%
  arrange(ID)

Mean_out <- Mean_out[order(Mean_out$ID), ]
Data_Test <- Data_Test[order(Data_Test$ID), ]
Est <- Est[order(Est$ID), ]

Data_Test <- merge(Data_Test, Est, by = "ID")
Data_Test <- merge(Data_Test, Mean_out, by = "ID")


Temp <- subset(Data_Test, Visit_year == 0)


F_clusterdata <- function(Outcome) {
  if (Outcome == "MMSE") {
    Temp <- subset(Temp, subset = Tilde_Beta < 0)
  }
  
  else {
    Temp <- subset(Temp, subset = Tilde_Beta > 0)
  }
  
  return(Temp)
}

Temp <- F_clusterdata(Outcome)
Temp1 <- subset(Temp, select = c(ID, Mean, Tilde_Beta))

Temp1$GMM_Cluster <- NA

if (Outcome == "MMSE") {
  Temp1$GMM_Cluster <- ifelse(Temp1$Tilde_Beta < -4, 3,
                              ifelse(Temp1$Tilde_Beta >= -4 & Temp1$Tilde_Beta < -2, 2,
                                     ifelse(Temp1$Tilde_Beta >= -2 & Temp1$Tilde_Beta < 0, 1, NA)))
} else if (Outcome == "CDRSB") {
  Temp1$GMM_Cluster <- ifelse(Temp1$Tilde_Beta >= 2, 3,
                              ifelse(Temp1$Tilde_Beta >= 1 & Temp1$Tilde_Beta < 2, 2,
                                     ifelse(Temp1$Tilde_Beta >= 0 & Temp1$Tilde_Beta < 1, 1, NA)))
} else if (Outcome == "ADAS13") {
  Temp1$GMM_Cluster <- ifelse(Temp1$Tilde_Beta >= 6, 3,
                              ifelse(Temp1$Tilde_Beta >= 3 & Temp1$Tilde_Beta < 6, 2,
                                     ifelse(Temp1$Tilde_Beta >= 0 & Temp1$Tilde_Beta < 3, 1, NA)))
}
Cluster <- subset(Temp1, select = c(ID, GMM_Cluster))

Data_Test <- merge(Data_Test, Cluster, by = "ID")

rm(list = c("Cluster", "Temp", "Temp1"))


F_RMean <- function(Data_Train, Outcome) {
  if (Outcome == "MMSE") {
    Data_Train$RMean <- ((30 - Data_Train$Mean) * Data_Train$Mean) / 30
  } else if (Outcome == "CDRSB") {
    Data_Train$RMean <- ((Data_Train$Mean - 18) * Data_Train$Mean) / 18
  } else if (Outcome == "ADAS13") {
    Data_Train$RMean <- ((Data_Train$Mean - 85) * Data_Train$Mean) / 85
  }
  return(Data_Train)
}

Data_Train <- F_RMean(Data_Train, Outcome)



Temp <- Data_Test %>% filter(Visit_year == 0)

Cate_summary <- Temp %>% 
  summarise(Min = min(GMM_Cluster), Max = max(GMM_Cluster))
Min_Cate <- Cate_summary$Min
Max_Cate <- Cate_summary$Max

# Frequency table
Visit_freq <- Data_Test %>% count(Visit_year)
NObs_Visit <- nrow(Visit_freq)
F_Visit_year <- Visit_freq$Visit_year

# Transpose data and concatenate columns
Visit1 <- data.frame(t(F_Visit_year))
Visit2 <- data.frame(COL = paste(Visit1[1, ], collapse = ","))

# Assign concatenated values to Visit
Visit <- Visit2$COL



Temp <- Data_Train %>% filter(Visit_year == 0)
# Sort data by CateG
Temp <- Temp %>% arrange(GMM_Cluster)





Gamma_Est_list <- list()
TrajALL_list <- list()

for(i in 1:1){
  set.seed(i)
  
  Temp_Sample <- Temp
  
  Gamma_Est <- lmer(Tilde_Beta ~ 0 + (0 + RMean|GMM_Cluster), 
                    data = Temp_Sample, 
                    REML = TRUE, 
                    control = lmerControl(optCtrl = list(maxfun = 50000)))
  
  Gamma_Ests <- ranef(Gamma_Est)$GMM_Cluster
  Gamma_Est <- data.frame(GMM_Cluster = rownames(Gamma_Ests), 
                          Estimate = Gamma_Ests[, "RMean"])
  
  Gamma_Est_list[[i]] <- Gamma_Est

F_MaxOutcome <- function(Outcome) {
  if (Outcome == "MMSE") {
    return(30)
  } else {
    return(0)
  }
}

MaxOutcome <- F_MaxOutcome(Outcome)

TrajALL <- data.frame()
Data_DisTime <- data.frame()

for (CateG in seq(Min_Cate, Max_Cate, 1)) {
  
  Gamma_Est_Temp <- Gamma_Est %>% filter(GMM_Cluster == !!CateG)
  
  prog_func <- function(t, c, parm) {
    gam <- parm
    r <- if (Outcome == "MMSE") {
      gam*c*(30-c)/30
    } else if (Outcome == "CDRSB") {
      gam*c*(c-18)/18
    } else if (Outcome == "ADAS13") {
      gam*c*(c-85)/85
    }
    return(list(r))
  }
  
  
  # Initial condition
  init <- BaseOutcome
  time <- seq(0, MaxTau, by = 1/12)
  parm <- Gamma_Est_Temp$Estimate
  
  ode_res <- ode(
    y = init,
    times = time,
    func = prog_func,
    parms = parm,
    method = 'lsoda'
  )
  
  Traj <- data.frame(DisTime = ode_res[, "time"], Pred = ode_res[, 2])
  
  if (Outcome == "MMSE") {
    Traj$Pred[Traj$Pred > MaxOutcome] <- MaxOutcome
  } else {
    Traj$Pred[Traj$Pred < MaxOutcome] <- MaxOutcome
  }

  Data_Temp <- Data_Test %>% filter(GMM_Cluster == !!CateG)

  Data_Temp1 <- Data_Temp %>% 
    filter(Visit_year == 0) %>% 
    mutate(TempID = row_number())
  
  NObs_Data <- nrow(Data_Temp1)

  Data_Temp4 <- data.frame()

  for (TempID in 1:NObs_Data) {
    
    Data_Temp2 <- Data_Temp1 %>% 
      filter(TempID == !!TempID) %>% 
      mutate(GMM_Cluster = CateG)

    NObs_Traj <- nrow(Traj)
    
    Data_Temp2 <- Data_Temp2 %>% slice(rep(1:n(), each = NObs_Traj))
    Data_Temp2 <- bind_cols(Data_Temp2, Traj)
    
    Data_Temp2 <- Data_Temp2 %>%
      mutate(Dif = Pred - get(Outcome),
             ABS_Dif = abs(Dif)) %>%
      arrange(ABS_Dif)
    
    Data_Temp2 <- Data_Temp2 %>%
      mutate(DisTime = round(DisTime, 2)) %>%
      select(-Visit_year)

    Data_Temp2 <- Data_Temp2 %>%
      mutate(BaseYear = first(DisTime))
        
    BY <- first(Data_Temp2$BaseYear)
    Visit_values <- as.numeric(unlist(strsplit(Visit, ",")))
    Visit_values1 <- Visit_values + BY
    
    Data_Temp3 <- Data_Temp2 %>%
       filter(DisTime %in% Visit_values1)
    
    Visit_df <- data.frame(Visit_year = Visit_values)
    
    Data_Temp3 <- bind_cols(Data_Temp3, Visit_df)
    
    Data_Temp4 <- bind_rows(Data_Temp4, Data_Temp3)
  }
  
  Data_Temp5 <- Data_Temp4 %>% 
    arrange(ID, Visit_year) %>%
    select(c("ID", "Visit_year", "BaseYear","DisTime", "Pred"))
    
  Data_Temp <- Data_Temp %>% arrange(ID, Visit_year)
  
  Data_DisTime_Temp <- merge(Data_Temp5, Data_Temp, by = c("ID", "Visit_year"))
  Data_DisTime_Temp <- Data_DisTime_Temp %>%
    mutate(PredError2 = (Pred - get(Outcome))^2)
  
  Traj <- Traj %>% mutate(CateG = CateG)
  TrajALL <- bind_rows(TrajALL, Traj)
  Data_DisTime <- bind_rows(Data_DisTime, Data_DisTime_Temp)
}

TrajALL_list[[i]] <- TrajALL

}

Result_Gamma_Est <- do.call(rbind, lapply(seq_along(Gamma_Est_list), function(i) {
  cbind(Gamma_Est_list[[i]], Seed = i)  # ?V?[?h???????ǉ?
}))

Result_TrajALL <- do.call(rbind, lapply(seq_along(TrajALL_list), function(i) {
  cbind(TrajALL_list[[i]], Seed = i)  # ?V?[?h???????ǉ?
}))


Temp <- Data_DisTime %>%
  filter(Visit_year == 0) %>%
  slice(rep(1:n(), each = length(Visit_values))) %>%
  mutate(Visit_year = rep(Visit_values, times = n() / length(Visit_values)),
         !!Outcome := NA) %>%
  select(ID, Visit_year)

Data_DisTime <- Data_DisTime %>%
  arrange(ID, Visit_year)

Data_Result <- full_join(Temp, Data_DisTime, by = c("ID", "Visit_year"))

Data_Result <- Data_Result %>%
  mutate(B = round(BaseYear, 1),
         PredImp = if_else(B == MaxTau | is.na(B), MaxOutcome, Pred),
         PredErrorImp2 = (PredImp - get(Outcome))^2)

Data_Result <- Data_Result %>% arrange(GMM_Cluster, ID)

MPredResult2_Method2G <- Data_Result %>%
  group_by(GMM_Cluster, ID) %>%
  summarise(Mean_PredError2 = mean(PredErrorImp2, na.rm = TRUE))

PredResult2_Method2G <- MPredResult2_Method2G %>%
  group_by(GMM_Cluster) %>%
  summarise(MPE = mean(Mean_PredError2, na.rm = TRUE))

PredResult <- PredResult2_Method2G %>%
  filter(!is.na(GMM_Cluster)) %>%
  mutate(AveragePredictiveError = sqrt(MPE),
         AveragePredictiveError = round(AveragePredictiveError, 2)) %>%
  select(GMM_Cluster, AveragePredictiveError)

PredResult$Seed <- seed

PredALL <- bind_rows(PredALL, PredResult)

keep_objects <- c("Data_ADNI", "PredALL", "Outcome", "MaxTau", "seed")
all_objects <- ls()
remove_objects <- setdiff(all_objects, keep_objects)
rm(list = remove_objects)


}

Result_CrossSiteValidation <- PredALL %>%
  group_by(GMM_Cluster) %>%
  summarise(mean = mean(AveragePredictiveError, na.rm = TRUE))

print(Result_CrossSiteValidation)
  