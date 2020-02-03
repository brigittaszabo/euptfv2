# --------------------------------------------------------------------------
#        euptf ver 2.0
#             Szabo (Toth), B.; Weber, T.K.D.; Weynants, M.
#            
#             TEST the random forest
#            
#             author of script: Brigitta Szabo <toth.brigitta@agrar.mta.hu> 
#                 first version: 25.09.2018.
#                 last changes: 03.02.2020.                    
#  --------------------------------------------------------------------------

source("setupRF_MVG.r")

# Calculate performance of MVG prediction based on K-h
# 1. predict K based on predicted MVG parameters for those head values for which there was measured theta
# 2. calculate residuals between predicted and measured K

# modify setupRF_KS_VG_MVG_TB.R according to parameter to calculate
setwd("D:/ownCloud/data/euptf_v2")
source("setupRF_KS_VG_MVG_TB.R")

# load data with measured THETA-h pairs
load("D:/ownCloud/data/euptf_v2/data/VG_hydi.rdata")
str(VG.hydi)
summary(VG.hydi)
str(RETfilt)
meas_T_h <- RETfilt[, c(1:3)]

# load data with measured K-h pairs
load("D:/ownCloud/data/euptf_v2/data/MV_hydi.rdata")
str(MV.hydi)
summary(MV.hydi)
str(CONDfilt)
meas_K_h <- CONDfilt[, c(1:3)] 


#load predicted MVG parameters (all input combination)
load("D:/ownCloud/data/euptf_v2/results/MV/MVG_log10THR_1_TEST_RES_200.rdata")

SAMPLE_ID <- lapply(Test.res.L, "[", c("SAMPLE_ID"))

log10thr_1 <- lapply(Test.res.L, "[", c("M_50"))
sapply(log10thr_1, summary)

thr <- mapply(function(x) (10^(x))-1,
            x = log10thr_1,
            SIMPLIFY = FALSE)
sapply(thr, summary)


load("D:/ownCloud/data/euptf_v2/results/MV/MVG_THS_TEST_RES_200.rdata")
ths <- lapply(Test.res.L, "[", c("M_50"))
sapply(ths, summary)

load("D:/ownCloud/data/euptf_v2/results/MV/MVG_log10ALP_TEST_RES_200.rdata")
log10alp <- lapply(lapply(Test.res.L, FUN = '[', "M_50"), FUN = data.frame)
alp <- mapply(function(x) 10^(x),
              x = log10alp,
              SIMPLIFY = FALSE)
sapply(alp, summary)

load("D:/ownCloud/data/euptf_v2/results/MV/MVG_log10N_1_TEST_RES_200.rdata")
log10n_1 <- lapply(Test.res.L, "[", c("M_50"))
n <- mapply(function(x) (10^(x))+1,
            x = log10n_1,
            SIMPLIFY = FALSE)
sapply(n, summary)

# m=1-1/n
m <- mapply(function(x) 1-1/x,
            x = n,
            SIMPLIFY = FALSE)
sapply(m, summary)

load("D:/ownCloud/data/euptf_v2/results/MV/MVG_L_TEST_RES_200.rdata")
L <- lapply(Test.res.L, "[", c("M_50"))
sapply(L, summary)

load("D:/ownCloud/data/euptf_v2/results/MV/MVG_log10K0_TEST_RES_200.rdata")
log10K0 <- lapply(lapply(Test.res.L, FUN = '[', "M_50"), FUN = data.frame)
K0 <- mapply(function(x) 10^(x),
              x = log10K0,
              SIMPLIFY = FALSE)
sapply(K0, summary)


interim.L  <- mapply(cbind, "SAMPLE_ID" = SAMPLE_ID, "thr" = thr, "ths" = ths, "alp" = alp, "n" = n, "m" = m, "L" = L, "K0" = K0, SIMPLIFY = FALSE)
sapply(interim.L, summary)

Test.par.L <- lapply(interim.L, setNames, c("SAMPLE_ID",  "thr", "ths", "alp", "n", "m", "L", "K0"))
sapply(Test.par.L, summary)

save(Test.par.L, file = file.path(path_out, data.predict, paste(data.predict, "_PAR_200",  ".rdata", sep = "")))



# add measured K-h data based on SAMPLE_ID
func <- function(x,y){merge(x, y, by.x=names(x)[1], by.y=names(y)[1])}
Test.par_K_h.L <- lapply(Test.par.L, func, meas_K_h)

save(Test.par_K_h.L, file = file.path(path_out, data.predict, paste(data.predict, "_PAR_K_HEAD_200",  ".rdata", sep = "")))


# calculate K based on predicted MVG parameters and head
SAMPLE_ID <- lapply(Test.par_K_h.L, FUN = '[[', "SAMPLE_ID")
thr <- lapply(lapply(Test.par_K_h.L, FUN = '[', "thr"), FUN = data.frame)
ths <- lapply(lapply(Test.par_K_h.L, FUN = '[', "ths"), FUN = data.frame)
alp <- lapply(lapply(Test.par_K_h.L, FUN = '[', "alp"), FUN = data.frame)
n <- lapply(lapply(Test.par_K_h.L, FUN = '[', "n"), FUN = data.frame)
m <- lapply(lapply(Test.par_K_h.L, FUN = '[', "m"), FUN = data.frame)
L <- lapply(lapply(Test.par_K_h.L, FUN = '[', "L"), FUN = data.frame)
K0 <- lapply(lapply(Test.par_K_h.L, FUN = '[', "K0"), FUN = data.frame)
head <- lapply(lapply(Test.par_K_h.L, FUN = '[', "HEAD"), FUN = data.frame)
K <- lapply(lapply(Test.par_K_h.L, FUN = '[', "K"), FUN = data.frame)

# VG function
VG_fun <-function(thr, ths, alp, head, n)
{
  thr+((ths-thr)/((1+((alp*head)^n))^(1-(1/n))))
}
# calculate predicted THETA from predicted VG parameters
THETA_pred <- mapply(`VG_fun`, thr, ths, alp, head, n, SIMPLIFY = FALSE)


# MV function
Se_fun <-function(THETA_pred, thr, ths)
{
      (THETA_pred - thr)/(ths-thr)
}
# calculate Se from predicted VG parameters 
Se <- mapply(`Se_fun`, THETA_pred, thr, ths, SIMPLIFY = FALSE)

K_fun <-function(K0, Se, L, m)
{
      K0 * Se^L * (1-(1-Se^(1/m))^m)^2
}
# calculate predicted K from predicted MVG parameters
K_pred <- mapply(`K_fun`, K0, Se, L, m, SIMPLIFY = FALSE)
sapply(K_pred, summary)
sapply(K, summary)

# calculate residuals for THETA
RES_THETA <- mapply(`-`, THETA, THETA_pred, SIMPLIFY = FALSE)
sapply(RES_THETA, summary)

# calculate residuals for log10(K)
log10K <- mapply(function(x) log10(x),
              x = K,
              SIMPLIFY = FALSE)
sapply(log10K, summary)

log10K_pred <- mapply(function(x) log10(x),
                 x = K_pred,
                 SIMPLIFY = FALSE)
sapply(log10K_pred, summary)

RES_log10K <- mapply(`-`, log10K, log10K_pred, SIMPLIFY = FALSE)
sapply(RES_log10K, summary)



interim.L <- mapply(cbind, Test.par_K_h.L, "log10K" =log10K, "log10K_pred" = log10K_pred, "RES_log10K" = RES_log10K, SIMPLIFY = FALSE)
Test.res.L_par_K <- lapply(interim.L, setNames, c("SAMPLE_ID",  "thr", "ths", "alp", "n", "m", "L", "K0", "HEAD", "K", "log10K", "log10K_pred", "RES_log10K"))

# save predicted parameters, meaured theta-h pairs and predited theta
save(Test.res.L_par_K, file = file.path(path_out, data.predict, paste(data.predict, "_PAR_log10K_RES_200",  ".rdata", sep = "")))

# save only id, res, head
interim.L <- mapply(cbind, "SAMPLE_ID" = SAMPLE_ID, "head"=head, "RES_log10K" = RES_log10K, SIMPLIFY = FALSE)
Test.res.L <- lapply(interim.L, setNames, c("SAMPLE_ID", "head", "RES_log10K"))
save(Test.res.L, file = file.path(path_out, data.predict, paste(data.predict, "_TEST_RES_200",  ".rdata", sep = "")))


# CREATE DATAFRAME TO STORE RESULTS
nl <- length(Test.res.L_par_K)
test.stat.df <- data.frame(Reduce(rbind, lapply(Test.res.L_par_K, gofFun, y0 =  "log10K", yhat0 = "log10K_pred")), row.names = 1:nl)
# add names of input combinations
test.stat.df[,"VAR"]       <- sapply( sel.L, paste, collapse = " ")


#
###         OUTPUT
#
#


# SAVE STATS RESULTS 
save(test.stat.df, file = file.path(path_out, data.predict, paste(data.predict, "_TEST_STATS_200",  ".rdata", sep = "")))


