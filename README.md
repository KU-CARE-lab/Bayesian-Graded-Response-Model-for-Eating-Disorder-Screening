##################### Data

Our current study protocol requires a signed data use agreement prior to release. Please email the corresonding author at kforbush@ku.edu.

#####################  model fit

The folder "model fit" contains R files for fitting the Bayesian-GRM, testing model fit, and performing cross-validation.
1. Fitting the Bayesian-GRM to training data (Section 3.1): "Bayesian_GRM_scoff.R" and "Bayesian_GRM_base.R".
2. Predicting individual ED risk using fitted Bayesian-GRM (Section 3.2): "ED_risk_estimate_scoff.R" and "ED_risk_estimate_base.R".
3. Cross-validation using out-of-sample method (Section 3.3): "cross_validation_scoff.R" and "cross_validation_base.R".
4. Figure 2 (posterior predictive comparison): plotted by "posterior_predictive_scoff.R" and "posterior_predictive_base.R".
5. Figure 3 (cross-validation results): plotted by "plot_cross_validation.R".

#####################  hypotheses 1 and 2

The folder "hypothesis_1_and_2" contains R files used in Hypotheses 1 (Accuracy of ED risk estimation, Section 4.2) and 2 (Characterization of item-level discriminability, Section 4.3).
1. Figure 4 (posterior distributions of person parameters): plotted by "plot_theta_ED_risk_scoff.R" and "plot_theta_ED_risk_base.R".
2. Table 4 (mean squared errors): calculated by "MSE.R".
3. Figure 5 (Association between person-parameter and ED risk): plotted by "ED_diagnosis_prediction.R".
4. Figure 6 (posterior distributions of discrimination parameters): plotted by "plot_alpha.R".
5. Figure 7 (individuals with high screening scores but low ED risk estimates): plotted by "GRM_aggregation_comparison.R".

#####################  bootstrap study

The folder "bootstrap" contains R code used in the bootstrap study (Section 4.4).
1. Performing the bootstrap: "Bayesian_GRM_scoff_boot.R" and "Bayesian_GRM_base_boot.R".
2. Predicting individual ED risk: "ED_risk_estimate_scoff_boot.R" and "ED_risk_estimate_base_boot.R".
3. Table 5 (MSE): "MSE.R".
4. Figure 8, and figures in Supplement 4: ""


