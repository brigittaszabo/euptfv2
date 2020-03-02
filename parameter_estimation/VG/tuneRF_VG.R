# --------------------------------------------------------------------------
#        euptf ver 2.0
#             Szabo (Toth) B.; Weber, T.K.D.; Weynants, M.
#            
#             TUNE the random forest
#            
#             author of script: Tobias KD Weber <tobias.weber@posteo.de>
#                               Brigitta Szabo <toth.brigitta@agrar.mta.hu>
#                 first version: 20.06.2018.
#                 last changes: 03.02.2020.            
#  --------------------------------------------------------------------------

source("setupRF_VG.r")

# 1 INITIALISE AND RUN -------------------------------------------------------------
####################################################################################
# 
# 
#                  INITIALISE  |  GENERAL PROJECT OPTIONS 
# 
# 
####################################################################################

for (j in 1:nvar) {
      
      if(!dir.exists(file.path(getwd(),"results", data.predict[j]))) {dir.create(file.path(getwd(),"results", data.predict[j]))}
      # 
      CV.rf.L <- rep( list(vector()), nl ) 
      ## RUN
      for (i in 1:nl){
            
            # prepare data sel selector
            if(sum(c("CACO3","PH_H2O","CEC")%in%sel.L[[i]]) == 0){
                  
                  sel.data <- paste("test", data.predict, sep = "")
              
            } else {
                  sel.data <- paste("test", data.predict, "chem", sep = "")
            }
            
            # SELECT THE REQUIRED DATASET FOR THE DIFFERENT SUBSETS SPECIFIED IN SEL.V
            tune.dat <- data.ptf[all.rowFun(!is.na(data.ptf[ , c(var.predict[j],sel.L[[i]]) ])) & !data.ptf[ ,sel.data], ]
            
            ###################################################
            #                                                 #
            #           SETUP THE RANDOM FOREST PART          #
            #                                                 #
            ###################################################
            
            
            #           TRAIN AND CONTROL
            tc <- trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 10)
            
            #           GRIDDING
            rfGrid <- expand.grid(mtry = seq(2, length(sel.L[[i]]), 1), 
                                  splitrule = c("variance", "extratrees", "maxstat"),
                                  min.node.size = 10)
            
            set.seed(1253)
            
            #           RUNNING THE MODEL
            CV.rf.L[[i]] <- train(as.formula(paste(var.predict[j], paste(sel.L[[i]], collapse = " + "), sep = " ~ ")),
                                  data = tune.dat, 
                                  method = "ranger",
                                  importance = "impurity",
                                  trControl = tc,
                                  tuneGrid = rfGrid,
                                  num.trees = 200,
                                  verbose = TRUE,
                                  num.threads = (detectCores()-4))
            
            # remove unused variables
            rm(tc, rfGrid, sel.data, tune.dat)
            
      } # end of i loop
      

      
      # 2 OUTPUT OF RESULTS ---------------------------------------------
      ###################################################################
      #                                                                 #
      #                                                                 #
      #                     OUTPUT  |  STATISTICS                       #
      #                                                                 #
      #                                                                 #
      ###################################################################
      
      #
      ###         STATISTICS
      #
      #
      
      # CREATE DATAFRAME TO STORE RESULTS
      tune.res.df <- data.frame(matrix(NA, nrow = nl, ncol = 7)); colnames(tune.res.df) <- c("ID", "NoSamples", "RMSE", "R2", "VAR", "RMSE.full", "R2.full")
      
      # PUT RESULTS INTO DATAFRAME
      tune.res.df[,"ID"]        <- IDs
      tune.res.df[,"NoSamples"] <- sapply(lapply(CV.rf.L, FUN = '[[', "finalModel"), FUN = "[[",  "num.samples") # Number of Samples
      tune.res.df[,"RMSE.full"] <- sqrt(sapply(lapply(CV.rf.L, FUN = '[[', "finalModel"), FUN = "[[",  "prediction.error")) # RMSE
      tune.res.df[,"R2.full"]   <- sapply(lapply(CV.rf.L, FUN = '[[', "finalModel"), FUN = "[[",  "r.squared")  # R2
      tune.res.df[,"VAR"]       <- sapply( sel.L, paste, collapse = " ")
      tune.res.df[,"RMSE"]      <- signif(tune.res.df[,"RMSE.full"],3 )           # rounded RMSE
      tune.res.df[,"R2"]        <- signif(tune.res.df[,"R2.full"],3 )             # rounded R2
      
      #
      ###         VARIABLE IMPORTANCE
      #
      #
      
      # EXTRACT RESULTS OF VARIABLE IMPORTANCE
      varImp.L <- sapply(lapply(CV.rf.L, FUN = '[[', "finalModel"), FUN = "[[",  "variable.importance")
      n.max <- sapply(varImp.L, max)
      varImp.norm.L <- lapply(Map(`/`, lapply(varImp.L, t), n.max), t)
      
      #
      ###         OUTPUT
      #
      #
      
      # OUTPUT OF THE TUNED/TRAINED MODEL
      save(CV.rf.L, file = file.path(path_out,data.predict[j],paste(var.predict[j], "_TUNE_200", ".rdata", sep = "")))
      
      # SAVE STATS RESULTS 
      save(tune.res.df, file = file.path(path_out,data.predict[j], paste(var.predict[j], "_TUNE_STATS_200",  ".rdata", sep = "")))
      
      # SAVE VARIMP RESULTS 
      save(varImp.norm.L, file = file.path(path_out,data.predict[j], paste(var.predict[j],"_TUNE_varImp_200", ".rdata", sep = "")))
      
} # end of j loop
