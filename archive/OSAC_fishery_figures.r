################################################################################################################
##### ####################  The fishery summary plots necessary for Pre-OSAC and OSAC ###################################
###########################  November 18, 2015 - DK
################################################################################################################
####
#Commented, checked  and revised by DK March 31, 2016

###############################################################################################################
## This function needs these local functions to work (a.k.a. "support files"), 
# see "Source_relation_OSAC_fishery_summary.docx" for complete details
#  1: fishsum.plt.r
#  2: gridPlot.r
#  3: fishery.dat.r
#  4: ScallopMap.r
##
###############################################################################################################

### Function Argument list
# fish.dat: The fishery data, likely extracted from the survey summary results but could be called stand alone, default is blank
# max.date: The last date for which QA/QC log data exists.  Default = format(Sys.time(), "%Y-%m-%d"),
# years:    The years of fishery data to plot.  Default = 1981:format(Sys.time(), "%Y"),
# bnk:      The bank(s) from which to extract these data.  Default = c("Ban","Mid","Sab","Ger","BBs","BBn","GBa","GBb"),
# log.ts:   Do you want to log transform the survey time series data.  (T/F) default = F.
# grids:    What size should the catch areas be for the spatial plot.  Default = 1/60 which is a 1 minute grid size.
# fun:      The function to use for calculations in grids, see gridPlot for options.  Default = sum.
# lvl:      What are the levels for the spatial plot.  Default is c(10,50,100,500,1000,5000,10000,50000), which will produce
#           bins for total catch of 10kg-50kg... up to 50,000 kg+
# poly.brd: For the spatial plot if you want a border around each cell specify the color here.  Default = NULL (no border)
#add.titles:Add titles to the figures.  (T/F) default = T.
# dirct:    The directory to save the figures and source the functions from.Default =Y:/Offshore scallop/Assessment/Assessment_fns/
# save.fig: Do you want to save the figure as image files, or just plot them to screen. (T/F) Default = T which plots to the
#           directory pulled from here- paste(dirct,"Presentations/",yr,"/OSAC/",bnk[i],"/Fishery_summary.png",sep="")
#           you will need to create the directory for each year/bank combination if it does not already exist.


####################  The fishery summary plots necessary for Pre-OSAC and OSAC ###################################

fishery_figures <- function(fish.dat, max.date = format(Sys.time(), "%Y-%m-%d"),years = 1981:format(Sys.time(), "%Y"),
                            bnk = c("Ban","Mid","Sab","Ger","BBs","BBn","GBa","GBb"),log.ts =F, grids = 1/60, fun = sum,
                            lvl = c(10,50,100,500,1000,5000,10000,50000),add.titles = T, poly.brd = NULL,
                            dirct = "Y:/Offshore scallop/Assessment/",save.fig = T)
{
  # Load required packages and local functions.
  
  source(paste(dirct,"Assessment_fns/Fishery/fishsum.plt.r",sep="")) #Source1
  source(paste(dirct,"Assessment_fns/Survey_and_OSAC/gridPlot.r",sep="")) #Source2
  source(paste(dirct,"Assessment_fns/Fishery/fishery.dat.r",sep="")) #Source3 
  source(paste(dirct,"Assessment_fns/Maps/ScallopMap.r",sep="")) #Source3 
  
  require(PBSmapping)
  require(RColorBrewer)
  # Also bring in the seedboxes for the spatial plots
  #Read1 All the seedboxes ever.
  seedboxes <-read.csv(paste(direct,"Data/Maps/approved/Fishing_Area_Borders/Seed_boxes.csv",sep=""),
                       stringsAsFactors = F,header=T)
  seedboxes$Closed <- as.Date(seedboxes$Closed)
  seedboxes$Open <- as.Date(seedboxes$Open)
  # Dump the commments they are just messy..
  seedboxes <- seedboxes[,-grep("comment",names(seedboxes))]
  # Read2 Get the survey boundary polygons for all banks.
  survey.bound.polys<-read.csv(paste(direct,"Data/Maps/approved/Survey/survey_boundary_polygons.csv",sep=""),
                               header=T,stringsAsFactors = F)
  
  # Now subset the fishery data if we need to remove some data (usually b/c recent data is not QA/QC ready) ...
  fish.dat <- subset(fish.dat, date < max.date)
  
  # Just how fishery.dat is structured you don't want to use the subset data (it is looking for BBs data too
  # DK note this is something that should be fixed up in the code, but not right now!
  polys.dat <- NULL
  cpue.ts <- NULL
  # Let's make this run for all the banks at once.
  for(i in 1:length(bnk))
  {
    # Get the CPUE for the bank
    cpue.ts[[bnk[i]]]<-fishery.dat(fish.dat,bk=bnk[i],yr=years,method='jackknife',direct=dirct) 			
    # Also bring in the seedboxes for the spatial plots later
    # If Effort = 0 than to be consistent with CPUE I think this should be NA rather than 0
    # and it should not be in the plot.
    cpue.ts[[bnk[i]]]$effort[cpue.ts[[bnk[i]]]$effort==0] <- NA
    # If we don't have any fishing this year we need to add the NA's to the data, 
    # must be a better way than this ugly loop but it works...
    if(max(cpue.ts[[bnk[i]]]$year,na.rm=T) < yr)
    {
      pad = yr - max(cpue.ts[[bnk[i]]]$year,na.rm=T)
      # Fill the cpue.ts object with the year, 0's, and NA's as per other years without data.
      for(j in 1:pad) cpue.ts[[bnk[i]]] <- rbind(cpue.ts[[bnk[i]]],c(yr-pad+j,c(rep(NA,ncol(cpue.ts[[bnk[i]]])-1))))
    } # end if(max(cpue.ts$year < yr))
    
    ###########
    # plot the time seris of catch, CPUE and effort.  Save the image to the presentation directory or just plot it to the window.
    if(save.fig==F) windows(11,8.5)
    if(save.fig==T)
    {
      png(paste(dirct,yr,"/Presentations/OSAC/",bnk[i],"/Fishery_summary.png",sep=""),units="in",width = 8.5, height = 11,
          res=420,bg = "transparent")
    } # end if(save.fig==T)
    fishsum.plt(cpue.ts[[bnk[i]]],years=years,add.title = add.titles,cx.mn=3,bnk=bnk[i],logged=log.ts,lwd=12)
    # If we aren't using all of the data, AND we are looking at the current years data, we need to annotate the Catch plot.
    #if(max.date != format(Sys.time(), "%Y-%m-%d") && format(max.date,"%Y") == format(Sys.time(), "%Y"))
    #  {
    #    mtext(paste("(Note: ",format(max.date,"%Y")," data is preliminary and includes up to ",
    #                format(max.date-1,"%B %d %Y"),")",sep=""),side=1,outer=T,line=4,adj=0.8)
    #  }  
    if(save.fig==T) dev.off()
    ###########
    
    # Set the date for the most recent data.
    #yr <- format(max.date, "%Y")
    # We create the spatial exploitation map if we have the most recent years data
    bnk.fish.dat <- subset(fish.dat, bank== bnk[i] & date >= as.Date(paste(yr,"-01-01",sep="")),
                           select = c('ID','lon','lat','pro.repwt'))
    # For some reason the latitude was being turned into a character instead of a number, can't figure why
    bnk.fish.dat$lon <- as.numeric(bnk.fish.dat$lon)
    bnk.fish.dat$lat <- as.numeric(bnk.fish.dat$lat)
    names(bnk.fish.dat)[1:4]<-c("EID","X","Y","Z")
    
    # Make sure we have data from the current year
    if(nrow(bnk.fish.dat) > 0)
    {
    # Remove any NA's from the data and give a warning...
    if(any(is.na(bnk.fish.dat)==T)) 
      {
    message(paste("Heads up", bnk[i], "has NA's in it, check your data!! I will remove and continue calculations for now"))
    bnk.fish.dat <- na.omit(bnk.fish.dat)
      } # if(any(is.na(bnk.fish.dat)==T)) 
   
   
      # If the current bank has a seedbox then grab it so we can plot it later
      if(nrow(subset(seedboxes,Bank==bnk[i] & Open >= paste(yr,"-01-01",sep="")))>0)
      {
        boxes <- as.PolySet(subset(seedboxes,Bank==bnk[i] & Open >= paste(yr,"-01-01",sep="")),projection = "LL")
      } # end if(bnk[i] == "BBn" || bnk[i] == "GBa")
      
      # Get the survey boundary polygon for the bank 
      bnk.survey.bound.poly <- subset(survey.bound.polys,label==bnk[i])
      
      # Set the levels, might need to think a bit about these!
      lvls=lvl
      #Get the total removals from each 1 minute cell within the bank for the levels (10 kg to 50 tonnes!)
      bnk.polys <- gridPlot(bnk.fish.dat,bnk.survey.bound.poly,lvls,border=poly.brd,FUN=fun,grid.size=grids)
      
      ##########
      # Plot the spatial distribution of catch for each bank and save the image (or just plot to a window if you prefer)
      if(save.fig==F) windows(11,8.5)
      if(save.fig==T)
      {
        png(paste(dirct,yr,"/Presentations/OSAC/",bnk[i],"/Spatial_fishery.png",sep=""),units="in",width = 11, height = 8.5,
            res=420,bg = "transparent")
      } # end if(save.fig==T) 
      if(add.titles == T) titl <- paste("Spatial Distribution of Catch (",bnk[i],"-",yr,")",sep="")
      if(add.titles== F) titl <- c("")
      ScallopMap(bnk[i],poly.lst=bnk.polys[1:2],poly.border=bnk.polys[[2]]$border,xlab="",ylab="",
                 title=titl, bathy.source="quick",
                 plot.bathy = T,plot.boundries = T,boundries="offshore",direct=dirct,cex.mn=2,dec.deg = F)
      tlvls<-lvls/1000
      legend("bottomleft",c(paste(tlvls[-length(tlvls)],'-',tlvls[-1],sep=''),paste(tlvls[length(tlvls)],'+',sep='')),
             fill=bnk.polys[[3]],title='Catch (t)',inset=0.02,bg='white',box.col='white')
      
      # Add the survey boxes.
      if(nrow(subset(seedboxes,Bank==bnk[i] & Open >= paste(yr,"-01-01",sep="")))>0) addPolys(subset(boxes),lty=2)
      # If we aren't using all of the data, AND we are looking at the current years data, we could annotate the plot.
      # But we decided against that so maybe make this an option someday!
      #if(max.date != format(Sys.time(), "%Y-%m-%d") && format(max.date,"%Y") == format(Sys.time(), "%Y"))
      #{
      #  mtext(paste("(Note: ",format(max.date,"%Y")," data is preliminary and includes up to ",
      #              format(max.date-1,"%B %d %Y"),")",sep=""),side=1,outer=F,line=4)
      #}  
      
      
      if(save.fig==T) dev.off()
      ##########
      polys.dat[[bnk[i]]] <- bnk.polys
    } # end      if(nrow(bnk.fish.dat) > 0)
 
  } # end for(i in 1:length(bnk))
  return(list(polys.dat,cpue.ts))
  
} # End function
