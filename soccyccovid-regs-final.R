  rm(list = ls()) #clear list
  
  #automatic installation of required packages
  packages <- c("xlsx","calibrate","stargazer","sandwich","lmtest","getopt","CausalGAM","ggplot2","reshape2","xts",
                "lattice","gridExtra","gtable","plm","lfe","lmtest","car","tis","foreign","MASS","quantreg","ggrepel",
                "dplyr","stringr","datasets","rio","psych","systemfit","MatchIt","CRTgeeDR","eurostat","plyr","zoo","ggthemes",
                "robumeta","metafor","dplyr","clubSandwich","Hmisc","metafor","pracma","pkgs","broom","sjPlot", "here", "data.table")
  
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  ipak(packages)
  
  #load packages
  library(xlsx) #Excel-Paket laden
  library(calibrate) #Laden des Pakets, das f??r Datenbeschriftung n??tig ist
  library (stargazer) #Laden des Pakets, mit dem R-Regressionsoutput in Latex-Tabellen ??bergef??hrt werden kann
  library(sandwich)
  library(lmtest)
  library(getopt)
  library(CausalGAM)
  library(ggplot2)
  library(reshape2)
  library(xts)
  library(lattice)
  library(gridExtra)
  library(gtable)
  library(plm)
  library(lfe)
  library(lmtest)
  library(car)
  library(tis)
  library(foreign)
  library(MASS)
  library(quantreg)
  library(ggrepel)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(datasets)
  library(rio)
  library(psych)
  library(systemfit)
  library(foreign)
  library(MatchIt)
  library(CRTgeeDR)
  library(eurostat)
  library(plyr)
  library(zoo)
  library(ggthemes)
  library("robumeta")
  library("metafor")
  library("dplyr")
  library(clubSandwich)
  library(Hmisc)
  library(metafor)
  library(pracma)
  library(broom)
  library(sjPlot)
  library(here)
  library(data.table)
  
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  #Load data
  dat <- fread(here("data_socexpcovid_final.csv"))
  
  #full sample
  dat_full <- subset(dat, year %in% c('1995', '1996', '1997','1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012', '2013', '2014', '2015','2016', '2017', '2018', '2019', '2020', '2021'))
  
  #Instrumental variables
  library(AER)
  library(ivpack)

  #subsamples
  #continential
  dat_continental <- subset(dat_full, Continental %in% c('1'))
  dat_nordic <- subset(dat_full, Nordic %in% c('1'))
  dat_southern <- subset(dat_full, Southern %in% c('1'))
  dat_eastern <- subset(dat_full, Eastern %in% c('1'))
  
  #IVREG
  #Regressions including Covid-19 dummy
  #Social protection
  reg_v1_SOCIAL_IV_COVID <- ivreg(SOCIAL ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_SOCIAL_IV_COVID)
  summary(reg_v1_SOCIAL_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_SOCIAL_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")

  #weak instruments test
  summary(reg_v1_SOCIAL_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  
  #Sickness and disability
  reg_v1_SICK_IV_COVID <- ivreg(SICK ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_SICK_IV_COVID)
  summary(reg_v1_SICK_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_SICK_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")
  
  #Old age
  reg_v1_OLD_IV_COVID <- ivreg(OLD ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_OLD_IV_COVID)
  summary(reg_v1_OLD_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_OLD_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")
  
  #Survivors
  reg_v1_SURV_IV_COVID <- ivreg(SURV ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_SURV_IV_COVID)
  summary(reg_v1_SURV_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_SURV_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")
  
  #Family and children
  reg_v1_FAMI_IV_COVID <- ivreg(FAMI ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_FAMI_IV_COVID)
  summary(reg_v1_FAMI_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_FAMI_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")

  #Unemployment
  reg_v1_UNEM_IV_COVID <- ivreg(UNEM ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_UNEM_IV_COVID)
  summary(reg_v1_UNEM_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_UNEM_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")

  #weak instruments test
  summary(reg_v1_UNEM_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  
  #Housing
  reg_v1_HOUS_IV_COVID <- ivreg(HOUS ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_HOUS_IV_COVID)
  summary(reg_v1_HOUS_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_HOUS_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")
  
  #other social exclusion
  reg_v1_EXCL_IV_COVID <- ivreg(EXCL ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_EXCL_IV_COVID)
  summary(reg_v1_EXCL_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_EXCL_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")
  
  #R&D social expenditure
  reg_v1_RDSO_IV_COVID <- ivreg(RDSO ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_RDSO_IV_COVID)
  summary(reg_v1_RDSO_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_RDSO_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")
  
  #other social protection
  reg_v1_OTHS_IV_COVID <- ivreg(OTHS ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_OTHS_IV_COVID)
  summary(reg_v1_OTHS_IV_COVID, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_OTHS_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")
  
  #preparation for tables
  ses.reg_v1_SOCIAL_IV_COVID <- list(coef_test(reg_v1_SOCIAL_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_SOCIAL_IV_COVID <- list(coef_test(reg_v1_SOCIAL_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_SOCIAL_IV_COVID <- list(coef_test(reg_v1_SOCIAL_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_SICK_IV_COVID <- list(coef_test(reg_v1_SICK_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_SICK_IV_COVID <- list(coef_test(reg_v1_SICK_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_SICK_IV_COVID <- list(coef_test(reg_v1_SICK_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_OLD_IV_COVID <- list(coef_test(reg_v1_OLD_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_OLD_IV_COVID <- list(coef_test(reg_v1_OLD_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_OLD_IV_COVID <- list(coef_test(reg_v1_OLD_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_SURV_IV_COVID <- list(coef_test(reg_v1_SURV_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_SURV_IV_COVID <- list(coef_test(reg_v1_SURV_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_SURV_IV_COVID <- list(coef_test(reg_v1_SURV_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_FAMI_IV_COVID <- list(coef_test(reg_v1_FAMI_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_FAMI_IV_COVID <- list(coef_test(reg_v1_FAMI_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_FAMI_IV_COVID <- list(coef_test(reg_v1_FAMI_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_UNEM_IV_COVID <- list(coef_test(reg_v1_UNEM_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_UNEM_IV_COVID <- list(coef_test(reg_v1_UNEM_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_UNEM_IV_COVID <- list(coef_test(reg_v1_UNEM_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_HOUS_IV_COVID <- list(coef_test(reg_v1_HOUS_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_HOUS_IV_COVID <- list(coef_test(reg_v1_HOUS_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_HOUS_IV_COVID <- list(coef_test(reg_v1_HOUS_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_EXCL_IV_COVID <- list(coef_test(reg_v1_EXCL_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_EXCL_IV_COVID <- list(coef_test(reg_v1_EXCL_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_EXCL_IV_COVID <- list(coef_test(reg_v1_EXCL_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_RDSO_IV_COVID <- list(coef_test(reg_v1_RDSO_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,3]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_RDSO_IV_COVID <- list(coef_test(reg_v1_RDSO_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,4]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_RDSO_IV_COVID <- list(coef_test(reg_v1_RDSO_IV_COVID, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")[,6]) # heteroskedasticity-robust p-val
  
  #stargazer table
  stargazer(reg_v1_SOCIAL_IV_COVID, reg_v1_SICK_IV_COVID, reg_v1_OLD_IV_COVID, reg_v1_FAMI_IV_COVID, reg_v1_UNEM_IV_COVID, reg_v1_HOUS_IV_COVID, t=list(unlist(tvals.reg_v1_SOCIAL_IV_COVID), unlist(tvals.reg_v1_SICK_IV_COVID), unlist(tvals.reg_v1_OLD_IV_COVID), unlist(tvals.reg_v1_FAMI_IV_COVID), unlist(tvals.reg_v1_UNEM_IV_COVID), unlist(tvals.reg_v1_HOUS_IV_COVID)), se=list(unlist(ses.reg_v1_SOCIAL_IV_COVID), unlist(ses.reg_v1_SICK_IV_COVID), unlist(ses.reg_v1_OLD_IV_COVID), unlist(ses.reg_v1_FAMI_IV_COVID), unlist(ses.reg_v1_UNEM_IV_COVID), unlist(ses.reg_v1_HOUS_IV_COVID)), pvals=list(unlist(pvals.reg_v1_SOCIAL_IV_COVID), unlist(pvals.reg_v1_SICK_IV_COVID), unlist(pvals.reg_v1_OLD_IV_COVID), unlist(pvals.reg_v1_FAMI_IV_COVID), unlist(pvals.reg_v1_UNEM_IV_COVID), unlist(pvals.reg_v1_HOUS_IV_COVID)))
  
  #GMM
  #Regressions including Covid-19 dummy
  #Social protection
  reg_v1_SOCIAL_GMM_COVID <- pgmm(SOCIAL ~ lag(SOCIAL) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_SOCIAL_GMM_COVID)
  
  #Sickness and disability
  reg_v1_SICK_GMM_COVID <- pgmm(SICK ~ lag(SICK) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_SICK_GMM_COVID)
  
  #Old age
  reg_v1_OLD_GMM_COVID <- pgmm(OLD ~ lag(OLD) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_OLD_GMM_COVID)

  #Survivors
  reg_v1_SURV_GMM_COVID <- pgmm(SURV ~ lag(SURV) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_SURV_GMM_COVID)
  
  #Family and children
  reg_v1_FAMI_GMM_COVID <- pgmm(FAMI ~ lag(FAMI) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_FAMI_GMM_COVID)
  
  #Unemployment
  reg_v1_UNEM_GMM_COVID <- pgmm(UNEM ~ lag(UNEM) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_UNEM_GMM_COVID)
  
  #Housing
  reg_v1_HOUS_GMM_COVID <- pgmm(HOUS ~ lag(HOUS) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_HOUS_GMM_COVID)
  
  #other social exclusion
  reg_v1_EXCL_GMM_COVID <- pgmm(EXCL ~ lag(EXCL) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_EXCL_GMM_COVID)
 
  #R&D social expenditure
  reg_v1_RDSO_GMM_COVID <- pgmm(RDSO ~ lag(RDSO) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_RDSO_GMM_COVID)
  
  #other social protection
  reg_v1_OTHS_GMM_COVID <- pgmm(OTHS ~ lag(OTHS) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_OTHS_GMM_COVID)
  coeftest(reg_v1_SOCIAL_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]
  
  #preparation for tables
  ses.reg_v1_SOCIAL_GMM_COVID <- list(coeftest(reg_v1_SOCIAL_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_SOCIAL_GMM_COVID <- list(coeftest(reg_v1_SOCIAL_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_SOCIAL_GMM_COVID <- list(coeftest(reg_v1_SOCIAL_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_SICK_GMM_COVID <- list(coeftest(reg_v1_SICK_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_SICK_GMM_COVID <- list(coeftest(reg_v1_SICK_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_SICK_GMM_COVID <- list(coeftest(reg_v1_SICK_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_OLD_GMM_COVID <- list(coeftest(reg_v1_OLD_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_OLD_GMM_COVID <- list(coeftest(reg_v1_OLD_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_OLD_GMM_COVID <- list(coeftest(reg_v1_OLD_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_SURV_GMM_COVID <- list(coeftest(reg_v1_SURV_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_SURV_GMM_COVID <- list(coeftest(reg_v1_SURV_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_SURV_GMM_COVID <- list(coeftest(reg_v1_SURV_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_FAMI_GMM_COVID <- list(coeftest(reg_v1_FAMI_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_FAMI_GMM_COVID <- list(coeftest(reg_v1_FAMI_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_FAMI_GMM_COVID <- list(coeftest(reg_v1_FAMI_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_UNEM_GMM_COVID <- list(coeftest(reg_v1_UNEM_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_UNEM_GMM_COVID <- list(coeftest(reg_v1_UNEM_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_UNEM_GMM_COVID <- list(coeftest(reg_v1_UNEM_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_HOUS_GMM_COVID <- list(coeftest(reg_v1_HOUS_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_HOUS_GMM_COVID <- list(coeftest(reg_v1_HOUS_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_HOUS_GMM_COVID <- list(coeftest(reg_v1_HOUS_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_EXCL_GMM_COVID <- list(coeftest(reg_v1_EXCL_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_EXCL_GMM_COVID <- list(coeftest(reg_v1_EXCL_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_EXCL_GMM_COVID <- list(coeftest(reg_v1_EXCL_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  ses.reg_v1_RDSO_GMM_COVID <- list(coeftest(reg_v1_RDSO_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,2]) #heteroskedasticity-robust standard errors
  tvals.reg_v1_RDSO_GMM_COVID <- list(coeftest(reg_v1_RDSO_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,3]) #heteroskedasticity-robust t-values "group" ("time") accounts for serial (cross-sectional) correlation.
  pvals.reg_v1_RDSO_GMM_COVID <- list(coeftest(reg_v1_RDSO_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))[,4]) # heteroskedasticity-robust p-val
  
  #IV dynamic
  #Unemployment
  reg_v1_UNEM_IV_COVID_dynamic <- ivreg(UNEM ~ lag(UNEM) + OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(UNEM) + lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_full)
  summary(reg_v1_UNEM_IV_COVID_dynamic)
  summary(reg_v1_UNEM_IV_COVID_dynamic, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_UNEM_IV_COVID_dynamic, vcov = "CR0", cluster = dat_full$ccode, test = "naive-t")
  
  #GMM
  #Unemployment
  reg_v1_UNEM_GMM_COVID <- pgmm(UNEM ~ lag(UNEM) + COVID*OG  + lag(PDEBT) + ELEC | lag(SOCIAL,2) + lag(SOCIAL,3) + COVID*lag(OG) + COVID*lag(OG,2)  + lag(PDEBT) + ELEC, index=c("ccode", "year"), effect="individual", model="onestep", transformation="ld", data=dat_full)
  summary(reg_v1_UNEM_GMM_COVID)
  coeftest(reg_v1_UNEM_GMM_COVID, vcov.=function(x) vcovHC(x, type="sss"))
  
  #IV continental
  #Unemployment
  reg_v1_UNEM_IV_COVID_continental <- ivreg(UNEM ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_continental)
  summary(reg_v1_UNEM_IV_COVID_continental)
  summary(reg_v1_UNEM_IV_COVID_continental, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_UNEM_IV_COVID_continental, vcov = "CR0", cluster = dat_continental$ccode, test = "naive-t")
  
  #IV nordic
  #Unemployment
  reg_v1_UNEM_IV_COVID_nordic <- ivreg(UNEM ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_nordic)
  summary(reg_v1_UNEM_IV_COVID_nordic)
  summary(reg_v1_UNEM_IV_COVID_nordic, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_UNEM_IV_COVID_nordic, vcov = "CR0", cluster = dat_nordic$ccode, test = "naive-t")
  
  #IV southern
  #Unemployment
  reg_v1_UNEM_IV_COVID_southern <- ivreg(UNEM ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID + lag(OG,3)*FINCRI + USGAP*FINCRI + lag(PDEBT) + ELEC + factor(ccode), data=dat_southern)
  summary(reg_v1_UNEM_IV_COVID_southern)
  summary(reg_v1_UNEM_IV_COVID_southern, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_UNEM_IV_COVID_southern, vcov = "CR0", cluster = dat_southern$ccode, test = "naive-t")
  
  #IV eastern
  #Unemployment
  reg_v1_UNEM_IV_COVID_eastern <- ivreg(UNEM ~ OG*COVID  + lag(PDEBT) + ELEC + factor(ccode) | lag(OG)*COVID + USGAP*COVID  + lag(PDEBT) + ELEC + factor(ccode), data=dat_eastern)
  summary(reg_v1_UNEM_IV_COVID_eastern)
  summary(reg_v1_UNEM_IV_COVID_eastern, vcov = sandwich, df = Inf, diagnostics = TRUE)
  coef_test(reg_v1_UNEM_IV_COVID_eastern, vcov = "CR0", cluster = dat_eastern$ccode, test = "naive-t")
  