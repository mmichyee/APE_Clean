import delimited "C:\Users\Miki\Documents\2018_NYU GPH\Applied Practice\APE_Clean\SVI_ACS_500Cities_TrimmedJoinShort.csv"

*run alphas for social/econ variables, standardized
alpha ep_pov ep_unemp ep_nohsdp ep_age65 ep_disabl ep_sngpnt ep_minrty ep_limeng ep_munit ep_mobile ep_crowd ///
ep_noveh ep_groupq ep_uninsur acsst5y2018s2401_occupation_perc v27 v28 v29 v30 acsst5y2018s0101_agesex_perc_mal, item std

*run alphas for health status, standardized
alpha _cities_cttract_arthritis_crudep _cities_cttract_bphigh_crudeprev _cities_cttract_cancer_crudeprev ///
_cities_cttract_casthma_crudepre _cities_cttract_chd_crudeprev _cities_cttract_copd_crudeprev _cities_cttract_csmoking_crudepr _cities_cttract_diabetes_crudepr _cities_cttract_obesity_crudepre _cities_cttract_stroke_crudeprev , item std

*run alphas for build environment, standardized
alpha landusejoin_sum1 landusejoin_sum23 landusejoin_sum4 landusejoin_sum5 landusejoin_sum6 landusejoin_sum7 ///
landusejoin_sum8 landusejoin_sum9 landusejoin_sum10 landusejoin_sum11 , item std

*factor analysis with selected social/econ variables: no hs dp, minority, management, service
factor ep_nohsdp ep_minrty acsst5y2018s2401_occupation_perc v27

*test rotations
rotate, promax
rotate, varimax  
*** as expected, promax explains high proportion of variance, makes sense with high corr
*** loading cutoff of .4 leaves 3 components in factor 1 and 2 components in 2

* factor analysis with selected health variables: bp, chd, copd, diabetes, stroke
factor _cities_cttract_bphigh_crudeprev _cities_cttract_chd_crudeprev _cities_cttract_copd_crudeprev ///
_cities_cttract_diabetes_crudepr _cities_cttract_stroke_crudeprev

*test rotations
rotate, promax
*** loading cutoff of .4 produces unique loadings for factors 1 & 2 (eliminates factor 3)

* throw them all together? 
factor ep_nohsdp ep_minrty acsst5y2018s2401_occupation_perc v27 _cities_cttract_bphigh_crudeprev _cities_cttract_chd_crudeprev ///
_cities_cttract_copd_crudeprev _cities_cttract_diabetes_crudepr _cities_cttract_stroke_crudeprev

*test rotations
rotate, promax
*** loading cutoff of 0.5 produces unique loadings for factors 1 & 2; no loadings on factors 3-5)

