# load data
load("pbpEjectSZ.RData")

# load packages
packages <- c(
  "tidyverse", "lfe", "mgcv", "parallel", "splancs", 
  "PBSmapping", "readxl", "ggeffects", "dplyr", 
  "emmeans", "lme4", "car", "report", 
  "margins", "tidyr"
)

install_and_load <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
  }
  library(package_name, character.only = TRUE)
}

lapply(packages, install_and_load)






dataMod1 <- pbpEjectSZ %>% dplyr::select(calledStrike, UMPIRENAME, fielder_2,
                                         matchup.batter.fullName, matchup.pitcher.fullName,
                                         Ejection, EjectedTeam, cs_prob,
                                         about.halfInning, result.homeScore, result.awayScore, Year, Temperature, Attendance)



##### original model 1
model1_m4.glmer = glmer(calledStrike ~ cs_prob + about.halfInning + Ejection + EjectedTeam + Ejection*EjectedTeam + 
                          (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod1, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 

summary(model1_m4.glmer)






##### Robustness Check 1: model 1 with linear probability model
results_rob1 <- felm(calledStrike ~ cs_prob + about.halfInning + Ejection + EjectedTeam + Ejection*EjectedTeam | 
                     UMPIRENAME + fielder_2 + matchup.batter.fullName + matchup.pitcher.fullName, 
                   data = dataMod1)

# Summary of the model
summary(results_rob1)






#####  Robustness Check 2: model 1 with year FE
model1_m4.glmer_Rob2 = glmer(calledStrike ~ cs_prob + about.halfInning + Ejection + EjectedTeam + Ejection*EjectedTeam + Year +
                          (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod1, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 

summary(model1_m4.glmer_Rob2)





#####  Robustness Check 3: model 1 with additional controls
model1_m4.glmer_Rob3 = glmer(calledStrike ~ cs_prob + about.halfInning + Ejection + EjectedTeam + Ejection*EjectedTeam + 
                          Year + Attendance + Temperature +
                          (1|UMPIRENAME) + (1|fielder_2) + 
                          (1|matchup.batter.fullName) + (1|matchup.pitcher.fullName), 
                        data = dataMod1, family = binomial, 
                        control=glmerControl(optimizer="bobyqa"),nAGQ = 0) 

summary(model1_m4.glmer_Rob3)



