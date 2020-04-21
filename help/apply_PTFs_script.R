# --------------------------------------------------------------------------
#        euptf ver 2.0
#             Szabo (Toth), B.; Weber, T.K.D.; Weynants, M.
#            
#             Apply PTFs on new dataset
#            
#             author of script: Brigitta Szabo <toth.brigitta@agrar.mta.hu> 
#                 first version: 21.04.2020.
#                 last changes: 21.04.2020.                    
#  --------------------------------------------------------------------------


library(ranger)
library(parallel)
library(repmis)
library (readr)


# load data with predictors -> now it is a sample dataset
urlfile <- "https://raw.githubusercontent.com/TothSzaboBrigitta/euptfv2/master/help/data_sample.csv"
new_data <- read_csv2(url(urlfile))

# check available predictors in your dataset
names(new_data)

# find which PTFs are recommended for given set of available predictors
urlfile <- "https://raw.githubusercontent.com/TothSzaboBrigitta/euptfv2/master/suggested_PTFs/list_of_final_PTFs.csv"
list_PTFs <- read_csv2(url(urlfile))
View(list_PTFs)
# available predictors in "new_data": USSAND USSILT USCLAY DEPTH_M OC BD CACO3 PH_H2O CEC
# row 32 includes recommended PTFs


### a) predict response without uncertainty ----
## prediction of field capacity (FC) with PTF07

# load prediction algorithm
source_data("https://github.com/TothSzaboBrigitta/euptfv2/blob/master/suggested_PTFs/FC_EUHYDI/FC_PTF07.rdata?raw=True")

FC <- predict(FC_PTF07,
              data=new_data,
              type = "response",
              num.threads = detectCores()-1)
FC <- FC$predictions

# add predicted values to the input dataset
new_data_pred <- cbind(new_data, FC)



### b) predict response with uncertainty ----
## prediction of Mualem-van Genuchten parameters (MVG) with PTF27

# load all prediction algorithms
source_data("https://github.com/TothSzaboBrigitta/euptfv2/blob/master/suggested_PTFs/MV_EUHYDI/MVG_THS_PTF27.rdata?raw=True")
source_data("https://github.com/TothSzaboBrigitta/euptfv2/blob/master/suggested_PTFs/MV_EUHYDI/MVG_log10THR_1_PTF27.rdata?raw=True")
source_data("https://github.com/TothSzaboBrigitta/euptfv2/blob/master/suggested_PTFs/MV_EUHYDI/MVG_log10ALP_PTF27.rdata?raw=True")
source_data("https://github.com/TothSzaboBrigitta/euptfv2/blob/master/suggested_PTFs/MV_EUHYDI/MVG_log10N_1_PTF27.rdata?raw=True")
source_data("https://github.com/TothSzaboBrigitta/euptfv2/blob/master/suggested_PTFs/MV_EUHYDI/MVG_L_PTF27.rdata?raw=True")
source_data("https://github.com/TothSzaboBrigitta/euptfv2/blob/master/suggested_PTFs/MV_EUHYDI/MVG_log10K0_PTF27.rdata?raw=True")


MVG_THS <- predict(MVG_THS_PTF27,
                   data=new_data,
                   type = "quantiles",
                   quantiles = c(0.05, 0.5, 0.95),
                   seed = 123,
                   num.threads = detectCores()-1)
MVG_THS <- as.data.frame(MVG_THS$predictions)


MVG_log10THR_1 <- predict(MVG_log10THR_1_PTF27,
                          data=new_data,
                          type = "quantiles",
                          quantiles = c(0.05, 0.5, 0.95),
                          seed = 123,
                          num.threads = detectCores()-1)
MVG_log10THR_1 <- as.data.frame(MVG_log10THR_1$predictions)
trans_log10_1 <- function(x) {(10^(x))-1}
MVG_THR <- sapply(MVG_log10THR_1[, c(1:3)], trans_log10_1)


MVG_log10ALP <- predict(MVG_log10ALP_PTF27,
                        data=new_data,
                        type = "quantiles",
                        quantiles = c(0.05, 0.5, 0.95),
                        seed = 123,
                        num.threads = detectCores()-1)
MVG_log10ALP <- as.data.frame(MVG_log10ALP$predictions)
trans_log10 <- function(x) {10^(x)}
MVG_ALP <- sapply(MVG_log10ALP[, c(1:3)], trans_log10)


MVG_log10N_1 <- predict(MVG_log10N_1_PTF27,
                        data=new_data,
                        type = "quantiles",
                        quantiles = c(0.05, 0.5, 0.95),
                        seed = 123,
                        num.threads = detectCores()-1)
MVG_log10N_1 <- as.data.frame(MVG_log10N_1$predictions)
trans_log10_p1 <- function(x) {(10^(x))+1}
MVG_N <- sapply(MVG_log10N_1[, c(1:3)], trans_log10_p1)


calc_m <- function(x) {1-1/x}
MVG_M <- sapply(as.data.frame(MVG_N[, c(1:3)]), calc_m)


MVG_log10K0 <- predict(MVG_log10K0_PTF27,
                       data=new_data,
                       type = "quantiles",
                       quantiles = c(0.05, 0.5, 0.95),
                       seed = 123,
                       num.threads = detectCores()-1)
MVG_log10K0 <- as.data.frame(MVG_log10K0$predictions)
trans_log10 <- function(x) {10^(x)}
MVG_K0 <- sapply(MVG_log10K0[, c(1:3)], trans_log10)


MVG_L <- (predict(MVG_L_PTF27,
                 data=new_data,
                 type = "quantiles",
                 quantiles = c(0.05, 0.5, 0.95),
                 seed = 123,
                 num.threads = detectCores()-1))
MVG_L <- as.data.frame(MVG_L$predictions)

pred <- cbind(MVG_THS, MVG_THR, MVG_ALP, MVG_N, MVG_M, MVG_K0, MVG_L)
names(pred)[c(1:3)] <- paste("MVG_THS_", names((pred)[c(1:3)]), sep="")
names(pred)[c(4:6)] <- paste("MVG_THR_", names((pred)[c(4:6)]), sep="")
names(pred)[c(7:9)] <- paste("MVG_ALP_", names((pred)[c(7:9)]), sep="")
names(pred)[c(10:12)] <- paste("MVG_N_", names((pred)[c(10:12)]), sep="")
names(pred)[c(13:15)] <- paste("MVG_M_", names((pred)[c(13:15)]), sep="")
names(pred)[c(16:18)] <- paste("MVG_K0_", names((pred)[c(16:18)]), sep="")
names(pred)[c(19:21)] <- paste("MVG_L_", names((pred)[c(19:21)]), sep="")

# add predicted values to the input dataset
new_data_pred <- cbind(new_data, pred)