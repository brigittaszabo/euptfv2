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

source("setupRF_VG.r")

# Calculate performance of VG prediction based on theta
# 1. predict theta based on predicted VG parameters for those head values for which there was measured theta
# 2. calculate residuals between predicted and measured theta


# load data with measured THETA-h pairs
load("D:/ownCloud/data/euptf_v2/data/VG_hydi.rdata")
str(VG.hydi)
summary(VG.hydi)
str(RETfilt)
meas_T_h <- RETfilt[, c(1:3)]


#load predicted VG parameters (all input combination)
load("D:/ownCloud/data/euptf_v2/results/VG/MVG_log10THR_1_TEST_RES_200.rdata")

SAMPLE_ID <- lapply(Test.res.L, "[", c("SAMPLE_ID"))

log10thr_1 <- lapply(Test.res.L, "[", c("M_50"))
sapply(log10thr_1, summary)

thr <- mapply(function(x) (10^(x))-1,
            x = log10thr_1,
            SIMPLIFY = FALSE)
sapply(thr, summary)


load("D:/ownCloud/data/euptf_v2/results/VG/MVG_THS_TEST_RES_200.rdata")
ths <- lapply(Test.res.L, "[", c("M_50"))
sapply(ths, summary)

load("D:/ownCloud/data/euptf_v2/results/VG/MVG_log10ALP_TEST_RES_200.rdata")
log10alp <- lapply(lapply(Test.res.L, FUN = '[', "M_50"), FUN = data.frame)
alp <- mapply(function(x) 10^(x),
              x = log10alp,
              SIMPLIFY = FALSE)
sapply(alp, summary)

load("D:/ownCloud/data/euptf_v2/results/VG/MVG_log10N_1_TEST_RES_200.rdata")
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

interim.L  <- mapply(cbind, "SAMPLE_ID" = SAMPLE_ID, "thr" = thr, "ths" = ths, "alp" = alp, "n" = n, "m" = m, SIMPLIFY = FALSE)
sapply(interim.L, summary)

Test.par.L <- lapply(interim.L, setNames, c("SAMPLE_ID",  "thr", "ths", "alp", "n", "m"))
sapply(Test.par.L, summary)

save(Test.par.L, file = file.path(path_out, data.predict, paste(data.predict, "_PAR_200",  ".rdata", sep = "")))



# add measured THETA-h data based on SAMPLE_ID
func <- function(x,y){merge(x, y, by.x=names(x)[1], by.y=names(y)[1])}
Test.par_t_h.L <- lapply(Test.par.L, func, meas_T_h)
Test.par_t_h.L <- lapply(Test.par_t_h.L, setNames, c("SAMPLE_ID",  "thr", "ths", "alp", "n", "m", "head", "THETA"))

save(Test.par_t_h.L, file = file.path(path_out, data.predict, paste(data.predict, "_PAR_THETA_HEAD_200",  ".rdata", sep = "")))


# calculate theta based on predicted VG parameters and head
SAMPLE_ID <- lapply(Test.par_t_h.L, FUN = '[[', "SAMPLE_ID")
thr <- lapply(lapply(Test.par_t_h.L, FUN = '[', "thr"), FUN = data.frame)
ths <- lapply(lapply(Test.par_t_h.L, FUN = '[', "ths"), FUN = data.frame)
alp <- lapply(lapply(Test.par_t_h.L, FUN = '[', "alp"), FUN = data.frame)
n <- lapply(lapply(Test.par_t_h.L, FUN = '[', "n"), FUN = data.frame)
head <- lapply(lapply(Test.par_t_h.L, FUN = '[', "head"), FUN = data.frame)
THETA <- lapply(lapply(Test.par_t_h.L, FUN = '[', "THETA"), FUN = data.frame)

# function
VG_fun <-function(thr, ths, alp, head, n)
{
  thr+((ths-thr)/((1+((alp*head)^n))^(1-(1/n))))
}
# calculate predicted THETA from predicted VG parameters 
THETA_pred <- mapply(`VG_fun`, thr, ths, alp, head, n, SIMPLIFY = FALSE)

# calculate residuals
RES <- mapply(`-`, THETA, THETA_pred, SIMPLIFY = FALSE)
sapply(RES, summary)

interim.L <- mapply(cbind, Test.par_t_h.L, "THETA_pred" = THETA_pred, "RES" = RES, SIMPLIFY = FALSE)
Test.res.L_par_theta <- lapply(interim.L, setNames, c("SAMPLE_ID",  "thr", "ths", "alp", "n", "m", "head", "THETA", "THETA_pred", "RES"))

# save predicted parameters, meaured theta-h pairs and predited theta
save(Test.res.L_par_theta, file = file.path(path_out, data.predict, paste(data.predict, "_PAR_THETA_RES_200",  ".rdata", sep = "")))


# CREATE DATAFRAME TO STORE RESULTS
test.stat.df <- data.frame(Reduce(rbind, lapply(Test.res.L_par_theta, gofFun, y0 =  "THETA", yhat0 = "THETA_pred")), row.names = 1:nl)
# add names of input combinations
test.stat.df[,"VAR"]       <- sapply( sel.L, paste, collapse = " ")

# SAVE STATS RESULTS 
save(test.stat.df, file = file.path(path_out, data.predict, paste(data.predict, "_TEST_STATS_200",  ".rdata", sep = "")))


