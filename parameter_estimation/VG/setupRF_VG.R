library(caret)
library(parallel)
library(ranger)
library(agricolae)
library(reshape2)
library(dplyr)


# 0 NOTES -----------------------------------------------------------------
####################################################################################
#
#     Setup script for tuneRF.R, buildfinalRF.R, and testRF.R 
#
####################################################################################


rm(list = ls()); gc()
setwd("D:/ownCloud/data/euptf_v2")
path <- "D:/ownCloud/data/euptf_v2"

# SELECT RESPONSE VARIABLE
data.predict <- c("VG")
var.predict <- c("MVG_THS")
# var.predict <- c("MVG_log10THR_1")
# var.predict <- c("MVG_log10ALP")
# var.predict <- c("MVG_log10N_1")

# PATH FOR THE OUTPUT
path_out <- file.path(path, "results/")

# LOAD DATA
load(file.path(path,"data","data_ptf_depth_pF2.rdata"))

# transform non normally distributed variables
data.ptf$MVG_log10ALP <- log10(data.ptf$MVG_ALP)
data.ptf$MVG_log10N_1 <- log10((data.ptf$MVG_N)-1)
data.ptf$MVG_log10THR_1 <- log10((data.ptf$MVG_THR)+1)

# CREATE POSSIBLE EXPLANATORY COMBINATIONS
In_min <- c("USSAND","USSILT","USCLAY","DEPTH_M")
In_add <- c("OC","BD","CACO3","PH_H2O","CEC")

# CREAT TOTAL COMBINATIONS OF INPUT SELECTIONS
sel.L <- do.call("c", lapply(seq_along(In_add), function(i) combn(In_add, i, FUN = list)))
sel.L <- c(list(In_min),mapply(c,rep(list(In_min),length(sel.L)),sel.L))
nl <- length(sel.L)
IDs <- 1:nl

all.rowFun <- function(mat){sapply(1:nrow(mat), function(x){all(mat[x,])})}

nvar <- length(var.predict)

gofFun <- function(data, y0, yhat0){          # rescheck
      
      y <- data[[y0]]
      yhat <- data[[yhat0]]

      # calculate vectors of residuals
      RES <- y -  yhat # residuals of THETA and log10Kh
      
      res <- list()
      
      m = length(RES);
      res$ME   = mean(RES);                            # mean (weighted) error
      res$MAE  = mean(abs(RES));                       # mean average (weighted) error
      res$MSE  = sum(RES^2)/m                          # mean squared (weighted) error
      res$RMSE = sqrt(res$RSS/m);                      # root mean squared (weighted) error
      res$R2 = 1-sum((y-yhat)^2)/sum((y-mean(y))^2);   # Eq. 1 of Kvalseth (1985) 39:4(1, pp. 279-285, The American Statistician, 
      res$N = m                          
      # returns
      
      return(unlist(res))
      
}
