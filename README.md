# euptfv2
**Updated European hydraulic pedotransfer functions with communicated uncertainties in the predicted variables (euptfv2)**

This repository includes the R scripts used to derive the updated European prediction algorithms to compute soil hydraulic parameters from easily available soil properties, aka pedotransfer functions (PTFs). The new algorithms, based on random forests (RF), lead to significantly better predictions and provide built-in prediction uncertainty computation. The influence of predictor variables on predicted soil hydraulic properties is explored.
A website (https://ptfinterface.rissac.hu) and R package euptf2 facilitate easy application of the updated prediction algorithms.

For each point or parameter PTF, the workflow is to run the R scripts in the following order: setupRF.R, tuneRF.R, buildfinalRF.R, and finally testRF.R

*Please cite as:*

Szabó, B., Weynants, M. and Weber, T. K. D. (2020) Updated European hydraulic pedotransfer functions with communicated uncertainties in the predicted variables (euptfv2). Submitted to Geoscientific Model Development.
