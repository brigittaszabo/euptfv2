# --------------------------------------------------------------------------
#        euptf ver 2.0
#             Szabo (Toth), B.; Weber, T.K.D.; Weynants, M.
#            
#             TEST the random forest
#            
#             author of script: Tobias KD Weber <tobias.weber@posteo.de> 
#                 first version: 24.06.2018.
#                 last changes: 03.02.2020.                    
#  --------------------------------------------------------------------------

source("setupRF_VG.r")

for(j in 1:nvar){
      
      # LOAD Built.rf.L | THE BUILT MODEL
      load(file = file.path(path_out, data.predict[j], paste(var.predict[j], "_BUILT_200", ".rdata", sep = "")))
      
      Test.rf.L  <- rep( list(vector()), nl) 
      Test.OBS.L <- rep( list(vector()), nl) 
      
      for (i in 1:nl){
            
            if(sum(c("CACO3","PH_H2O","CEC")%in%sel.L[[i]]) == 0){
                  
                  sel.data <- paste("test", data.predict, sep = "")
                  
            } else {
                  sel.data <- paste("test", data.predict, "chem", sep = "")
            }
            
            # SELECT THE REQUIRED DATASET FOR THE DIFFERENT SUBSETS SPECIFIED IN SEL.V !!!!!  CAREFUL NO !data.ptf, here
            Test.OBS.L[[i]] <- data.ptf[all.rowFun(!is.na(data.ptf[ , c(var.predict[j], sel.L[[i]]) ])) & data.ptf[ ,sel.data], ]
            
            Built.rf <- Built.rf.L[[i]]
            
            ### Predict on test set ----
            Test.rf.L[[i]] <- predict(Built.rf, 
                                      data = Test.OBS.L[[i]], 
                                      predict.all = FALSE, 
                                      num.trees = Built.rf$num.trees, 
                                      type = "quantiles", 
                                      quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), 
                                      seed = 11, 
                                      num.threads = (detectCores()-1), verbose = TRUE)
            
            # remove unused variables
            # rm(test.THS, sel.data)
         
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
      
      
      # Test.res.L <- rep(list(vector()), nl) 
      
      Pred.L <- lapply(Test.rf.L, FUN = '[[', "predictions")
      Pred.L <- lapply(Pred.L, FUN = data.frame)
      Pred.L <- lapply(Pred.L, setNames, c("L_5", "L_25", "M_50", "L_75", "U_95"))
    
      M50 <- lapply(lapply(Pred.L, FUN = '[', "M_50"), FUN = data.frame)
      OBS <- lapply(Test.OBS.L, FUN = '[[', var.predict[j])
      RES <- mapply(`-`, M50, OBS, SIMPLIFY = FALSE)
      SAMPLE_ID <- lapply(Test.OBS.L, FUN = '[[', "SAMPLE_ID")
      
      interim.L <- mapply(cbind, Pred.L, "OBS" = OBS, "RES" = RES, "SAMPLE_ID" = SAMPLE_ID, SIMPLIFY = FALSE)
      Test.res.L <- lapply(interim.L, setNames, c("L_5", "L_25", "M_50", "L_75", "U_95", "OBS", "RES", "SAMPLE_ID"))
      rm(interim.L, M50, OBS, RES, SAMPLE_ID)  # 
      
      
      # CREATE DATAFRAME TO STORE RESULTS
      test.stat.df <- data.frame(Reduce(rbind, lapply(Test.res.L, gofFun, y0 =  "OBS", yhat0 = "M_50")), row.names = 1:nl)
      
      
      #
      ###         OUTPUT
      #
      #
      
  
      # SAVE STATS RESULTS 
      save(test.stat.df, file = file.path(path_out, data.predict[j], paste(var.predict[j], "_TEST_STATS_200",  ".rdata", sep = "")))
      
      # save raw output of RF predictions on test sets
      save(Test.rf.L, file = file.path(path_out, data.predict[j], paste(var.predict[j], "_Test_200", ".rdata", sep = "")))

      # save residuals, all quantiles, observed value and SAMPLE_ID in one dataframe
      save(Test.res.L, file = file.path(path_out, data.predict[j], paste(var.predict[j], "_TEST_RES_200", ".rdata", sep = "")))
      
} # end of j loop

