# --------------------------------------------------------------------------
#        euptf ver 2.0
#             Szabo (Toth), B; Weber, T.K.D.; Weynants, M.
#            
#             BUILD the random forest
#            
#             author of script: Tobias KD Weber <tobias.weber@posteo.de> 
#                 first version: 20.06.2018.
#                 last changes: 03.02.2020.             
#  --------------------------------------------------------------------------


source("setupRF_VG.r")

# 1 INITIALISE -------------------------------------------------------------
####################################################################################
# 
# 
#                  INITIALISE  |  GENERAL PROJECT OPTIONS 
# 
# 
####################################################################################

for (j in 1:nvar) {
      
      # LOAD THE TRAIN RESULT
      load(file = file.path(path_out, data.predict[j], paste(var.predict[j], "_TUNE_200", ".rdata", sep = "")))
      
      # CREATE LIST TO STORE RESULTS
      Built.rf.L <- rep( list(vector()), nl)
      
      for (i in 1:nl){
            
            if(sum(c("CACO3","PH_H2O","CEC")%in%sel.L[[i]]) == 0){
                  
                  sel.data <- paste("test", data.predict, sep = "")
                  
            } else {
                  sel.data <- paste("test", data.predict, "chem", sep = "")
            }
            
            # SELECT THE REQUIRED DATASET FOR THE DIFFERENT SUBSETS SPECIFIED IN SEL.V
            build.dat <- data.ptf[all.rowFun(!is.na(data.ptf[ , c(var.predict[j],sel.L[[i]]) ])) & !data.ptf[ ,sel.data], ]
            
            # INTERIM LIST
            CV.rf <- CV.rf.L[[i]]
            
            Built.rf.L[[i]] <- ranger(as.formula(paste(var.predict[j], paste(sel.L[[i]], collapse = " + "), sep = " ~ ")),
                                      data = build.dat,
                                      mtry = CV.rf$bestTune$mtry,
                                      splitrule = CV.rf$bestTune$splitrule,
                                      num.trees = 200,
                                      quantreg = TRUE,
                                      min.node.size = 10,
                                      importance = "impurity",
                                      keep.inbag = TRUE,
                                      seed = 0,
                                      num.threads = detectCores()-1)
            
            # remove unused variables
            rm(CV.rf, sel.data, build.dat)
      }
      
      save(Built.rf.L, file = file.path(path_out, data.predict[j], paste(var.predict[j], "_BUILT_200", ".rdata", sep = "")))
      
      
      # 2 OUTPUT OF RESULTS ---------------------------------------------
      ###################################################################
      #                                                                 #
      #                                                                 #
      #                     OUTPUT  |  STATISTICS                       #
      #                                                                 #
      #                                                                 #
      ###################################################################
      
      #
      ###         VARIABLE IMPORTANCE
      #
      #
      
      # EXTRACT RESULTS OF VARIABLE IMPORTANCE
      varImp.L <- sapply(Built.rf.L, FUN = "[[",  "variable.importance")
      n.max <- sapply(varImp.L, max)
      varImp.norm.L <- lapply(Map(`/`, lapply(varImp.L, t), n.max), t)
      
      save(varImp.norm.L, file = file.path(path_out,data.predict[j], paste(var.predict[j],"_BUILT_varImp_200", ".rdata", sep = "")))
} # end of j loop
