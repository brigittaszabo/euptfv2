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
var.predict <- c("log10KS")
data.predict <- c("KS")

# PATH FOR THE OUTPUT
path_out <- file.path(path, "results/")

# LOAD DATA
load(file.path(path,"data","data_ptf_depth_pF2.rdata"))

# transform non normally distributed variables
data.ptf$log10KS <- log10(data.ptf$KS)

# CREATE POSSIBLE EXPLANATORY COMBINATIONS
In_min <- c("USSAND","USSILT","USCLAY","DEPTH_M")
In_add <- c("OC","BD","CACO3","PH_H2O","CEC")

# CREATE COMBINATIONS OF INPUT SELECTIONS
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
      res$RSS  = t(RES)%*%RES;                         # sum of squared (weighted) residuals
      res$RMSE = sqrt(res$RSS/m);                      # root mean squared (weighted) error
      res$R2 = 1-sum((y-yhat)^2)/sum((y-mean(y))^2);   # Eq. 1 of Kvalseth (1985) 39:4(1, pp. 279-285, The American Statistician, 
      res$N = m                          
      # Number of THETA-H-pairs
      
      # th$AIC  = -2 * log(th$rss/m) + 2 * n;
      # th$AICc = ifelse((m-n-1)<=0,NaN,th$AIC + 2*n*(n+1)/(m-n-1));
      # th$BIC  = -2 * log(th$rss/m)+n*log(m);

      # returns
      
      return(unlist(res))
      
}




