clear all
import delimited "C:\Users\Miki\Documents\2018_NYU GPH\Applied Practice\APE_Clean\SVI_ACS_500Cities_TrimmedJoinCovid.csv"

*run alphas for social/econ variables, standardized
alpha ep_pov ep_unemp ep_nohsdp ep_age65 ep_disabl ep_sngpnt ep_minrty ep_limeng ep_munit ep_mobile ep_crowd ///
ep_noveh ep_groupq ep_uninsur occupation_perc_management occupation_perc_service occupation_perc_sales ///
occupation_perc_natural occupation_perc_production perc_male, item std

*run alphas for health status, standardized
alpha arthritis_crudep bphigh_crudeprev cancer_crudeprev ///
casthma_crudepre chd_crudeprev copd_crudeprev csmoking_crudepr ///
diabetes_crudepr obesity_crudepre stroke_crudeprev , item std

*run alphas for build environment, standardized
alpha landusejoin_sum1 landusejoin_sum23 landusejoin_sum4 landusejoin_sum5 landusejoin_sum6 landusejoin_sum7 ///
landusejoin_sum8 landusejoin_sum9 landusejoin_sum10 landusejoin_sum11 , item std

*factor analysis with selected social/econ variables: no hs dp, minority, management, service
factor ep_nohsdp ep_minrty occupation_perc_management occupation_perc_service

*test rotations
rotate, promax
rotate, varimax  
*** as expected, promax explains high proportion of variance, makes sense with high corr
*** loading cutoff of .4 leaves 3 components in factor 1 and 2 components in 2

*factor analysis with selected health variables: bp, chd, copd, diabetes, stroke
factor bphigh_crudeprev chd_crudeprev copd_crudeprev ///
diabetes_crudepr stroke_crudeprev

*test rotations
rotate, promax
*** loading cutoff of .4 produces unique loadings for factors 1 & 2 (eliminates factor 3)

* throw them all together? 
factor ep_nohsdp ep_minrty occupation_perc_management occupation_perc_service bphigh_crudeprev chd_crudeprev ///
copd_crudeprev diabetes_crudepr stroke_crudeprev

*test rotations
rotate, promax
*** loading cutoff of 0.5 produces unique loadings for factors 1 & 2; no loadings on factors 3-5)

*regression with weighted outcomes
*august case rate
regress wt_aug6_case_rate ep_nohsdp ep_minrty occupation_perc_management occupation_perc_service bphigh_crudeprev chd_crudeprev ///
copd_crudeprev diabetes_crudepr stroke_crudeprev
*aug death rate
regress wt_aug6_death_rate ep_nohsdp ep_minrty occupation_perc_management occupation_perc_service bphigh_crudeprev chd_crudeprev ///
copd_crudeprev diabetes_crudepr stroke_crudeprev
*aug positive tests
regress wt_aug6_positive ep_nohsdp ep_minrty occupation_perc_management occupation_perc_service bphigh_crudeprev chd_crudeprev ///
copd_crudeprev diabetes_crudepr stroke_crudeprev

*may case rate
regress wt_may18_case_rate ep_nohsdp ep_minrty occupation_perc_management occupation_perc_service bphigh_crudeprev chd_crudeprev ///
copd_crudeprev diabetes_crudepr stroke_crudeprev
*may death rate
regress wt_may18_death_rate ep_nohsdp ep_minrty occupation_perc_management occupation_perc_service bphigh_crudeprev chd_crudeprev ///
copd_crudeprev diabetes_crudepr stroke_crudeprev
*may positive tests
regress wt_may18_positive ep_nohsdp ep_minrty occupation_perc_management occupation_perc_service bphigh_crudeprev chd_crudeprev ///
copd_crudeprev diabetes_crudepr stroke_crudeprev
