# survey.dat.restrat.r
################################################################################################################
####  Created by FK in May 2018.  This function uses Domain.estimates to restratify Sable Bank, and then PEDstrata 
#### to get the stratified mean and variance (CV) from the survey data. This mirrors survey.dat.r as of May 2018
#### up to the point where the domain estimator is calculated. Then it's all downhill from there...
####
# Update history
# May 2018: FK created this based on survey.dat, but with domain estimation. When more banks need restratifying, you must edit
#           Survey.Summary_data.r to bring that bank to this script instead of using survey.dat. This method also requires that
#           all new strata coordinates and areas be appended to survey_information.csv, survey_detail_polygons.csv and 
#           survey_boundary_polygons.csv, with the startyear column filled in for the year of the change. DO NOT DELETE 
#           OLD STRATA INFORMATION from any of these files as this is used to restratify historical tow data.
################################################################################################################

#####################################  Function Summary ########################################################
####  
##  This function is used within these files:(a.k.a "dependent files") 
##  1: "SurveySummary_data.r"
##
###############################################################################################################

###############################################################################################################
## This function needs these functions to work (a.k.a. "support files")
# 
#    
#      
##
###############################################################################################################

###############################################################################################################
# source(".../Assessment_fns/Survey/survey.dat.restrat.r") ok

# Arguments
#shf:          Shell height frequency data
#htwt.fit:     Shell height Meat Weight model fit
#years:        Years of interest, not required.
#RS:           Size a scallop becomes a recruit:  Defalut = 80
#CS:           Size a scallop becomes commercial:  Default = 100
#bk:           The bank of interest.  Default = "Sab".  Currently this should only run for Sab because it's the only restratified bank.
#areas:        An object with the strata numbers/names and the area covered by each stratum.          
#mw.par:       How is meat weight to be calculated.  Default = 'annual', options are ('fixed' or 'annual') alternatively
#              some variant of "CF" is used if the meat weight is being calculated from condition factor.  Need to have
#              mw.par = column name that includes CF data for this option to work properly.
#err:          What CV to calculate. Default ='str' for stratified design, "rnd" will calculate the random survey design CV.
# user.bins:   Calculate the biomass of specified user bins, these bins will have had to already been calculated with the surv.by.tow function
#              for this to work!!  Default is NULL which doesn't do any calculations.  If not null user.bins should look something like 
#              user.bins <- c(50,70,90,110)
###############################################################################################################

survey.dat.restrat <- function(shf, htwt.fit, years, RS=80, CS=100, bk="Sab", areas,  mw.par='annual',err='str',user.bins = NULL) {

  if(!bnk=="Sab") print("You wound up in survey.dat.restrat even though your bank shouldn't be restratified. How did you get here? 
                        Please return to SurveySummary_data.r")
  if(bnk=="Sab"){
# load the PEDstrata package, note this is a locally developed package not available from R repositories
require(PEDstrata)  || stop("PEDstrata package required please obtain a copy and install this locally developed package")
require(survey)     || stop("survey package required please install package and try again")
require(splancs)    || stop("splancs package required please install package and try again")
# This is silly, but for the below code to work we need to increase the RS/CS by 5
RS <- RS + 5
CS <- CS + 5

# If years is not supplied than obtain from the data
if(missing(years)==T) years<-sort(unique(shf$year))

# Sable was restratified prior to 2018 survey due to the creation of the Western Emerald Bank Conservation Area. 
# It was decided at February 2018 Survey Working Group meeting to remove WEBCA from Sable strata, therefore, a domain estimator
# is required to convert pre-2018 survey data to the new strata. For this reason, we must break this script into a pre and post 
# re-stratification sections.
# 1900 has been set as the start year for all offshore strata up to 2018, since these are the strata to be used for all data < 2018.

# Create strata object for PEDstrata package, includes Strata and number of towable units in that strata.
# Yes I know Strata and strata.id are the same, but Domainestimates.R uses both columns, and I'm rolling with the punches here.
HSIstrata.obj <- data.frame(Strata=areas[,1], strata.id=areas[,1], NH=areas[,2], startyear=areas[,3])[order(areas[,1]),]
if(length(unique(HSIstrata.obj$startyear))==2){
strata.obj <- HSIstrata.obj[HSIstrata.obj$startyear == min(unique(HSIstrata.obj$startyear)),]
domain.obj <- HSIstrata.obj[HSIstrata.obj$startyear == max(unique(HSIstrata.obj$startyear)),]

# also need to make the strata names different from each other. So I'm just making up a new convention for the new strata.
domain.obj$Strata <- paste0(domain.obj$Strata, "_2.0")
domain.obj$strata.id <- paste0(domain.obj$strata.id, "_2.0")

# and you gotta do this in your actual tow data too. Everything has to match.
shf$Strata_ID_new  <- paste0(shf$Strata_ID_new, "_2.0")
}

if(!length(unique(HSIstrata.obj$startyear))==2){
  "Houston you have a problem. You either don't need to restratify this bank at all, and therefore shouldn't be in the survey.dat.restrat function,
  or you have to re-write this to accommmodate more than 1 restratification. Sorry that you have to go through that, cuz even doing this much was hard.
  Take a walk to clear your mind and prepare yourself for some coding!"
}

# Output the object to screen and determine the number of towable units for this bank.
print(strata.obj)
print(domain.obj)
N.tu <- HSIstrata.obj$NH

# for easier indexing of shell height bins in shf
bin <- as.numeric(substr(names(shf),2,nchar(names(shf))))

# And make some more objects if we have user specified bins...
if(!is.null(user.bins))
{
  # Get the names for the user bins and good names for the final results...
  bnames <- paste0("bin_lt_",user.bins[1])
  mean.names <- paste0("mean_lt_",user.bins[1])
  CV.names <- paste0("CV_lt_",user.bins[1])
  for(p in 1:length(user.bins)+1) 
  {
    if(p > 1 && p < length(user.bins)+1) 
    {
      bnames[p] <- paste0("bin_",user.bins[p-1],"-",user.bins[p])
      mean.names[p] <- paste0("mean_",user.bins[p-1],"-",user.bins[p])
      CV.names[p] <- paste0("CV_",user.bins[p-1],"-",user.bins[p])
    } # end if(p > 1 && p < length(user.bins)+1) 
    
    if(p == length(user.bins)+1) 
    {
      bnames[p] <- paste0("bin_",user.bins[p-1],"_plus")
      mean.names[p] <- paste0("mean_",user.bins[p-1],"_plus")
      CV.names[p] <- paste0("CV_",user.bins[p-1],"_plus")
    } # end if(p == length(user.bins)+1) 
  } # End for(p in 1:length(user.bins)+1) 
  bnames <- c(bnames,paste0(bnames,"_bm"))
  mean.names <- c(mean.names,paste0(mean.names,"_bm"))
  CV.names <- c(CV.names,paste0(CV.names,"_bm"))
  tmp <- as.data.frame(matrix(NA,ncol = (length(CV.names) + length(mean.names)+1) ,nrow=length(years)))
  names(tmp) <- c(mean.names,CV.names,"year")
  tmp$year <- years
} # end if(!is.null(user.bins))

# intialize objects for upcoming for loop.
w.yst <- matrix(NA, length(years), 40)
n.yst <- w.yst
n.stratmeans <-list(NULL)
w.stratmeans <-list(NULL)
strat.res <- data.frame(year=years)
Strata.obj <- NULL
mw <- NULL

#objects for domain estimation
scall.est.w.IPR <- NULL
scall.est.w.IR <- NULL
scall.est.w.I <- NULL
scall.est.n.IPR <- NULL
scall.est.n.IR <- NULL
scall.est.n.I <- NULL
out.domain <- data.frame(YEAR=years,BANK=bnk,
                  yst.w.IPR=rep(NA,length(years)),
                  var.yst.w.IPR=rep(NA,length(years)),
                  yst.w.IR=rep(NA,length(years)),
                  var.yst.w.IR=rep(NA,length(years)),
                  yst.w.I=rep(NA,length(years)),
                  var.yst.w.I=rep(NA,length(years)),
                  yst.n.IPR=rep(NA,length(years)),
                  var.yst.n.IPR=rep(NA,length(years)),
                  yst.n.IR=rep(NA,length(years)),
                  var.yst.n.IR=rep(NA,length(years)),
                  yst.n.I=rep(NA,length(years)),
                  var.yst.n.I=rep(NA,length(years)),
                  descrip=rep('domain',length(years))) 


# If CS and RS are just one value turn them into a vector the same length as the number of years of data.
if(length(CS) == 1)	CS <- rep(CS, length(years))
if(length(RS) == 1)	RS <- rep(RS, length(years))

# For loop to do the calculations of meat weight for non-restratified banks. Domain estimation happens in here too!
m=0 # indexing variable
for(i in 1:length(years))
{
  m=m+1
  
  # Set the bins
  mw.bin<-seq(5,200,5)
  # select the current years data.
  ann.dat<-subset(shf,year==years[i])
  # Use the MW-SH model fit to calculate the meat weight, assumes that year was a random effect in the model
  # Remember mw is in grams here.
  # FK had to specify htwt.fit <- SpatHtWt.fit[[bnk]]??
  if(mw.par=='annual') mw[[i]] <- matrix(exp(log(seq(2.5,200,5))*htwt.fit$b[i]+log(htwt.fit$a[i])),
                                         nrow(ann.dat),40,byrow=T,dimnames=list(ann.dat$tow,mw.bin))
  # Use the MW-SH model fit to calculate the meat weight, assumes that year was not included in the model
  # Remember mw is in grams here.
  
  if(mw.par=='fixed') mw[[i]]<-matrix(exp(log(seq(2.5,200,5))*htwt.fit$B+htwt.fit$A),nrow(ann.dat),40,
                                      byrow=T,dimnames=list(ann.dat$tow,mw.bin))
  # DK Note:  So as this was it would overwright the calculations from mw.par=="annual" but this
  # would actually cause an error if ever this was specified as annual
  # Use some other data to estimate Meat Weight, Condition factor generally used for this option.
  # Remember mw is in grams here.
  
  if(mw.par !='annual' && mw.par !='fixed') mw[[i]]<-sweep(matrix((seq(2.5,200,5)/100)^3,nrow(ann.dat),
                                                                  40,byrow=T,dimnames=list(ann.dat$tow,mw.bin)),1,FUN='*',ann.dat[,mw.par])
print("Careful, you didn't specify the location for prediction of CF so I have picked mean depth, lat, and lon between 2005 and 2014 be sure this is how this has been done in the past!")

num <- data.frame(subset(shf, year==years[i], which(bin==5):which(bin==200)), 
                  STRATA.ID.NEW=shf$Strata_ID_new[shf$year==years[i]], 
                  STRATA.ID.OLD=shf$Strata_ID_old[shf$year==years[i]])

# Remove rows with strata ID's which are NA's
num<-na.omit(num)

# Add up the numbers of Scallops in each size category.
num$pre <- rowSums(num[, which(mw.bin==5):(which(mw.bin==RS[i])-1)],na.rm=T)
num$rec <- rowSums(num[, which(mw.bin==RS[i]):(which(mw.bin==CS[i])-1)],na.rm=T)
num$com <- rowSums(num[, which(mw.bin==CS[i]):which(mw.bin==200)],na.rm=T)

# Make a dataframe with the biomasses for each bin and tow, add the strata ID's as well
# This is in grams per tow
w <- data.frame(subset(shf, year==years[i], which(bin==5):which(bin==200))*mw[[i]], 
                STRATA.ID.NEW=shf$Strata_ID_new[shf$year==years[i]],
                STRATA.ID.OLD=shf$Strata_ID_old[shf$year==years[i]])

# Remove any rows in which the strata is NA
w<-na.omit(w)
# Add up the biomass of Scallops in each size category, again this is in grams per tow
w$pre <- rowSums(w[, which(mw.bin==5):(which(mw.bin==RS[i])-1)],na.rm=T)
w$rec <- rowSums(w[, which(mw.bin==RS[i]):(which(mw.bin==CS[i])-1)],na.rm=T)
w$com <- rowSums(w[, which(mw.bin==CS[i]):which(mw.bin==200)],na.rm=T)

# The proportion of towable area in each strata.
# run HSIstrata.obj to remind yourself of the years for each
pstrat_new <- as.numeric(N.tu[c(1,3,5,7,9)]/sum(N.tu[c(1,3,5,7,9)]))
pstrat_old <- as.numeric(N.tu[c(2,4,6,8,10)]/sum(N.tu[c(2,4,6,8,10)]))

# restratification occurs for all years < 2018
if(years[i]<max(unique(HSIstrata.obj$startyear))){
# get domaine estimator for biomasses in each size category - Domain.estimates(data, Strata, Domain, strata.obj, domain.obj, Nd = NULL
source("E:/INSHORE SCALLOP/BoF/Assessment_fns/SFA29W/Domainestimates.R")
scall.dom.w.IPR <- Domain.estimates(w$pre, w$STRATA.ID.OLD, w$STRATA.ID.NEW, strata.obj, domain.obj)
scall.dom.w.IR <- Domain.estimates(w$rec, w$STRATA.ID.OLD, w$STRATA.ID.NEW, strata.obj, domain.obj)
scall.dom.w.I <- Domain.estimates(w$com, w$STRATA.ID.OLD, w$STRATA.ID.NEW, strata.obj, domain.obj)

scall.dom.n.IPR <- Domain.estimates(num$pre, num$STRATA.ID.OLD, num$STRATA.ID.NEW, strata.obj, domain.obj)
scall.dom.n.IR <- Domain.estimates(num$rec, num$STRATA.ID.OLD, num$STRATA.ID.NEW, strata.obj, domain.obj)
scall.dom.n.I <- Domain.estimates(num$com, num$STRATA.ID.OLD, num$STRATA.ID.NEW, strata.obj, domain.obj)

scall.est.w.IPR[[m]] <- c(YR=years[i], bank=bnk, scall.dom.w.IPR)
scall.est.w.IR[[m]] <- c(YR=years[i], bank=bnk, scall.dom.w.IR)
scall.est.w.I[[m]] <- c(YR=years[i], bank=bnk, scall.dom.w.I)

scall.est.n.IPR[[m]] <- c(YR=years[i], bank=bnk, scall.dom.n.IPR)
scall.est.n.IR[[m]] <- c(YR=years[i], bank=bnk, scall.dom.n.IR)
scall.est.n.I[[m]] <- c(YR=years[i], bank=bnk, scall.dom.n.I)

scall.sum.w.IPR <- summary.domain.est(scall.dom.w.IPR)
scall.sum.w.IR <- summary.domain.est(scall.dom.w.IR)
scall.sum.w.I <- summary.domain.est(scall.dom.w.I)

scall.sum.n.IPR <- summary.domain.est(scall.dom.n.IPR)
scall.sum.n.IR <- summary.domain.est(scall.dom.n.IR)
scall.sum.n.I <- summary.domain.est(scall.dom.n.I)

out.domain[m,seq(3, 13, 2)] <- as.numeric(c(scall.sum.w.IPR[[2]][2],
                                     scall.sum.w.IR[[2]][2],
                                     scall.sum.w.I[[2]][2],
                                     scall.sum.n.IPR[[2]][2],
                                     scall.sum.n.IR[[2]][2],
                                     scall.sum.n.I[[2]][2]))
out.domain[m,seq(4, 14, 2)] <- as.numeric(c(scall.sum.w.IPR[[2]][3],
                                     scall.sum.w.IR[[2]][3],
                                     scall.sum.w.I[[2]][3],
                                     scall.sum.n.IPR[[2]][3],
                                     scall.sum.n.IR[[2]][3],
                                     scall.sum.n.I[[2]][3]))

scall.levels.w.IPR <- with(scall.dom.w.IPR,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
scall.levels.w.IR <- with(scall.dom.w.IR,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
scall.levels.w.I <- with(scall.dom.w.I,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
scall.levels.n.IPR <- with(scall.dom.n.IPR,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
scall.levels.n.IR <- with(scall.dom.n.IR,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))
scall.levels.n.I <- with(scall.dom.n.I,data.frame(ybd=(unlist(ybd)),var.ybd=(unlist(var.ybd)),var.diffdomain=(unlist(var.diffdomain)),se.ybd=(unlist(se.ybd)) ))

scall.levels.w.IPR$LEVEL <- row.names(scall.levels.w.IPR)
scall.levels.w.IR$LEVEL <- row.names(scall.levels.w.IPR)
scall.levels.w.I$LEVEL <- row.names(scall.levels.w.IPR) 
scall.levels.n.IPR$LEVEL <- row.names(scall.levels.w.IPR)
scall.levels.n.IR$LEVEL <- row.names(scall.levels.w.IPR)
scall.levels.n.I$LEVEL <- row.names(scall.levels.w.IPR)

scall.levels.w.IPR$YEAR <- rep(years[i],dim(scall.levels.w.IPR)[1])
scall.levels.w.IR$YEAR <- rep(years[i],dim(scall.levels.w.IR)[1])
scall.levels.w.I$YEAR <- rep(years[i],dim(scall.levels.w.I)[1])
scall.levels.n.IPR$YEAR <- rep(years[i],dim(scall.levels.n.IPR)[1])
scall.levels.n.IR$YEAR <- rep(years[i],dim(scall.levels.n.IR)[1])
scall.levels.n.I$YEAR <- rep(years[i],dim(scall.levels.n.I)[1])

scall.levels.w.IPR$BANK <- rep(bnk,dim(scall.levels.w.IPR)[1])
scall.levels.w.IR$BANK <- rep(bnk,dim(scall.levels.w.IR)[1])
scall.levels.w.I$BANK <- rep(bnk,dim(scall.levels.w.I)[1])
scall.levels.n.IPR$YEAR <- rep(bnk,dim(scall.levels.n.IPR)[1])
scall.levels.n.IR$YEAR <- rep(bnk,dim(scall.levels.n.IR)[1])
scall.levels.n.I$YEAR <- rep(bnk,dim(scall.levels.n.I)[1])

scall.levels.w.IPR[[m]] <- scall.levels.w.IPR
scall.levels.w.IR[[m]] <- scall.levels.w.IR
scall.levels.w.I[[m]] <- scall.levels.w.I
scall.levels.n.IPR[[m]] <- scall.levels.n.IPR
scall.levels.n.IR[[m]] <- scall.levels.n.IR
scall.levels.n.I[[m]] <- scall.levels.n.I

out.domain ### THIS CONTAINS STRATIFIED ESTIMATES. NO NEED TO RUN PEDSTRATA AGAIN!

# So we can throw the appropriate biomass values into Strata.obj$I, IR, and IPR
Strata.obj$I[[i]] <- out.domain$yst.w.I[i]
Strata.obj$IR[[i]] <- out.domain$yst.w.IR[i]
Strata.obj$IPR[[i]] <- out.domain$yst.w.IPR[i]

# So we can throw the appropriate count values into Strata.obj$N, NR, and NPR
Strata.obj$N[[i]] <- out.domain$yst.n.I[i]
Strata.obj$NR[[i]] <- out.domain$yst.n.IR[i]
Strata.obj$NPR[[i]] <- out.domain$yst.n.IPR[i]

# total number of tows
strat.res$n[i] <- sum(scall.dom.w.I$nh)

# summary of stratified design, returns a number of useful survey design results and optimization summaries.
I.tmp <- summary(scall.dom.w.I)
# Convert to Biomass estiamte for the bank in tonnes


}

if(years[i] == max(unique(HSIstrata.obj$startyear)) | years[i] > max(unique(HSIstrata.obj$startyear))) {
  
# Strata calculations for biomass for commerical size Scallops
Strata.obj$I[[i]] <- PEDstrata(w, HSIstrata.obj,'STRATA.ID',w$com)
# total number of tows
strat.res$n[i] <- sum(Strata.obj$I[[i]]$nh)
# summary of stratified design, returns a number of useful survey design results and optimization summaries.
I.tmp <- summary(Strata.obj$I[[i]],effic=T)
# Convert to Biomass estiamte for the bank in tonnes

strat.res$I[i] <- I.tmp$yst * sum(N.tu[c(1,3,5,7,9)])/10^6			#g to t
# Calculate the CV, 'str' is the stratified CV, the 'ran' option gives the random design CV.
if(err=='str') strat.res$I.cv[i] <- I.tmp$se.yst / I.tmp$yst
# Note here that the variance from the summary is more like a variance of an s.e. rather than a variance of a s.d.
if(err=='ran') strat.res$I.cv[i] <- sqrt(I.tmp$var.ran) / I.tmp$yst

# Strata calculations for biomass for recruit sized Scallops
Strata.obj$IR[[i]]<-PEDstrata(w, HSIstrata.obj,'STRATA.ID',w$rec)
IR.tmp <- summary(Strata.obj$IR[[i]],effic=T)
strat.res$IR[i] <- IR.tmp$yst* sum(N.tu)/10^6			#g to t
if(err=='str')  strat.res$IR.cv[i] <- IR.tmp$se.yst / IR.tmp$yst
# Note here that the variance from the summary is more like a variance of an s.e. rather than a variance of a s.d.
if(err=='ran') strat.res$IR.cv[i] <- sqrt(IR.tmp$var.ran) / IR.tmp$yst

# Strata calculations for biomass for pre-recruit sized Scallops
Strata.obj$IPR[[i]]<-PEDstrata(w, HSIstrata.obj,'STRATA.ID',w$pre)
IPR.tmp <- summary(Strata.obj$IPR[[i]],effic=T)
strat.res$IPR[i] <- IPR.tmp$yst* sum(N.tu)/10^6			#g to t
if(err=='str') strat.res$IPR.cv[i] <- IPR.tmp$se.yst /  IPR.tmp$yst
if(err=='ran') strat.res$IPR.cv[i] <- sqrt(IPR.tmp$var.ran) /  IPR.tmp$yst


# Strata calculations for abundance for commerical size Scallops
Strata.obj$N[[i]]<-PEDstrata(num, HSIstrata.obj,'STRATA.ID',num$com)
N.tmp <- summary(Strata.obj$N[[i]],effic=T)
strat.res$N[i] <- N.tmp$yst * sum(N.tu)/10^6			#in millions
if(err=='str') strat.res$N.cv[i] <- N.tmp$se.yst / N.tmp$yst
if(err=='ran') strat.res$N.cv[i] <- sqrt(N.tmp$var.ran) / N.tmp$yst


# Strata calculations for abundance for recruit size Scallops
Strata.obj$NR[[i]]<-PEDstrata(num, HSIstrata.obj,'STRATA.ID',num$rec)
NR.tmp <- summary(Strata.obj$NR[[i]],effic=T)
strat.res$NR[i] <- NR.tmp$yst* sum(N.tu)/10^6			#in millions
if(err=='str')  strat.res$NR.cv[i] <- NR.tmp$se.yst / NR.tmp$yst
if(err=='ran') strat.res$NR.cv[i] <- sqrt(NR.tmp$var.ran) / NR.tmp$yst

# Strata calculations for abundance for pre-recruit size Scallops
Strata.obj$NPR[[i]]<-PEDstrata(num, HSIstrata.obj,'STRATA.ID',num$pre)
NPR.tmp <- summary(Strata.obj$NPR[[i]],effic=T)
strat.res$NPR[i] <- NPR.tmp$yst * sum(N.tu)/10^6			#in millions
if(err=='str') strat.res$NPR.cv[i] <- NPR.tmp$se.yst / NPR.tmp$yst
if(err=='ran') strat.res$NPR.cv[i] <- sqrt(NPR.tmp$var.ran) / NPR.tmp$yst

# Average weight of fully recruited scallop by year
strat.res$w.bar[i] <- sum(w.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)]) /
  sum(n.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])							

# Average shell height of fully recruited scallop by year
strat.res$l.bar[i] <- sum((n.yst[i,]*seq(2.5,200,5))[which(mw.bin==CS[i]):which(mw.bin==200)]) / 
  sum(n.yst[i,which(mw.bin==CS[i]):which(mw.bin==200)])
strat.res$l.k[i] <- sum((n.yst[i,]*seq(2.5,200,5))[which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)]) / 
  sum(n.yst[i,which(mw.bin==RS[i]):which(mw.bin==CS[i]-5)])

# Calculate the mean abundance and mean biomass (grams) per tow (for each NEW strata. (ybar_h)
n.stratmeans[[i]] <- with(num, sapply(1:40, function(x){tapply(num[,x],STRATA.ID.NEW,mean)}))
w.stratmeans[[i]] <- with(w, sapply(1:40, function(x){tapply(w[,x],STRATA.ID.NEW,mean)}))

#Multiply the mean abundance(biomass) in each shell height category in a strata by the proportion of towable area
#in that strata.  Sum this product for each strata resulting in an estimate of total abundance (biomass) for each
#shell height category in a given year. (ybar_st)
if(is.null(nrow(n.stratmeans[[i]]))) n.yst[i,] <- n.stratmeans[[i]]
if(!is.null(nrow(n.stratmeans[[i]]))) n.yst[i,] <- apply(sapply(1:nrow(n.stratmeans[[i]]), function(x){n.stratmeans[[i]][x,] * pstrat[x]}),1,sum)
#  Now multiply by the total bank area to determine the survey estimated abundance(biomass).
# The abundance is actual numbers 
n.Yst <- n.yst[i,] * sum(N.tu) 
if(is.null(nrow(w.stratmeans[[i]])))  w.yst[i,] <- w.stratmeans[[i]]
if(!is.null(nrow(w.stratmeans[[i]]))) w.yst[i,] <- apply(sapply(1:nrow(w.stratmeans[[i]]), function(x){w.stratmeans[[i]][x,] * pstrat[x]}),1,sum)
w.Yst <- w.yst[i,] * sum(N.tu)
}

# By this point, we should have matching df's whether it's pre re-stratification or post, so we can go back to treating them the same way from here on.


}

}