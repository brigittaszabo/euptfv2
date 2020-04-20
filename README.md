# euptfv2
**Codes for the updated European hydraulic pedotransfer functions (euptfv2)**

This repository includes the R scripts used to derive the updated European prediction algorithms to compute soil hydraulic parameters from easily available soil properties, aka pedotransfer functions (PTFs). The new algorithms, based on random forests (RF), lead to significantly better predictions and provide built-in prediction uncertainty computation. The influence of predictor variables on predicted soil hydraulic properties is explored.
A website (https://ptfinterface.rissac.hu) facilitates easy application of the updated prediction algorithms.

For each point or parameter PTF, the workflow is to run the R scripts in the following order:
1. setupRF.R:				loading data, define path, input variables and function to compute performance of the PTFs;
2. tuneRF.R:				parameter tuning of the random forest;
3. buildfinalRF.R:	building final random forest, and finally
4. testRF.R: 				compute performance of the final random forest on the test set.
The above scripts are included separately for point and parameter estimations.

Under the suggested_PTFs folder:
- the list of suggested PTFs by predictor variables is included (list_of_final_PTFs.xlsx) and
- final PTFs (random forest algorithms) by predicted soil hydraulic parameters are available in RData format.


*Please cite as:*

Szabó, B., Weynants, M. and Weber, T. K. D. (2020) Updated European hydraulic pedotransfer functions with communicated uncertainties in the predicted variables (euptfv2). Submitted to Geoscientific Model Development.
