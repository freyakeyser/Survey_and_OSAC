# RESTRATIFICATION STEPS:

To restratify a bank using a domain estimator, you must:

1) Update the following files be appending the new data, with the appropriate start year for the change
#           paste(direct,"data/Survey_data/survey_information.csv",sep="")
#           paste(direct,"Data/Maps/approved/Survey/survey_detail_polygons.csv")
#           paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv")
*** The script Simple_restratification_of_SB_pkg_sp.R may be useful! ***

2) In SurveySummary_data.r, you must revise near line 418 to apply assign.strata() function correctly to get old and new strata assigned. 
	You must also revise near line 730 to implement survey.dat.restrat() function instead of survey.dat. 
	Currently, only Sable is set up for restratification of the years prior to 2018.

3) In survey.dat.restrat.r, you must add the name of the bank you are restratifying to the error message catches and if statements. 
	You should also go through this function stepwise to make sure everything works for your new bank, as we have not conducted 
	tests on banks other than Sable. 
	survey.dat.restrat function applies PEDstrata::Domain.estimates() to data prior to the year of restratification. 

Any questions, refer to FK. I have a local & private Github record of the changes I made.