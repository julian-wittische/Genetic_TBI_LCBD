###Ryan 2018 october
###script import summary_popAlltime.csv (CDmetaPOP simulation output)
###part 1: find peak timesteps and valley timesteps accroding to initial population size of timestep
###part 2: import peak populations and valley populations 
###part 3: convert into csv format

# parameters to change:
# line 20: see comment at line 19 and 20
# line 28: directory of the scenario where all replicats located 
# Notice: the metadatpks.csv and metadatvals.csv will be stored inside the directory of the replicat.

library(devtools);
library(adegenet);
library(pcadapt);
library(pegas);
library(sp);
library(hierfstat);

### a 'peak' is defined as a local maximal with "accu" points either side of it being smaller than it. 
accu<-2 #10year senario use 2; 20year use 5; 40year use 10;
nbr.loc<-100
#decide regular sampling plan: sample size of peak populations, % of valley populations
#pkSample<-200 #this is the sample size
#valSample<-10 #this is percentage 10 means 10%

for(j in 0:19){
  
  directory<-paste("F:/Ryan(simulations)/simulation_grid_nolimit/10y_nolimit/10y_21x21_3x3_dis199_k30/replicat",j,sep = "")
  setwd(directory)
  
  #############################################################
  #############################################################
  ####### part 1: import summary_popAlltime.csv ###############
  #############################################################
  #############################################################
  
  
  #load simulation data, here we use summary_popAllTime.csv
  summary_pop<-read.table("summary_popAllTime.csv",sep=",",header=TRUE,row.names = NULL)
  
  
  
  
  #extract timestep and population size
  popSummary<-summary_pop[,c(1,4)] #column 1 is timestep, column 4 is the N_Initial
  
  popu<-popSummary[-seq(2,nrow(popSummary),2),,drop=FALSE] #use only the data of the even number timesteps
  write.table(popu[,2],file="indivs.csv",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
  pop<-read.table("indivs.csv",sep="|")
  popSize<-NULL #vector that has the population size of all timesteps
  for(i in 1:nrow(popu)){
    popSize<-c(popSize,pop[i,1])
  }
  
  plot(popSize,type="o")
  
  #chosen timesteps (even timesteps)
  write.table(popu[,1],file="time.csv",sep=",",quote=FALSE,row.names=FALSE,col.names=FALSE)
  time<-read.table("time.csv")
  timeSteps<-NULL #vector that has the population size of all timesteps
  for(i in 1:nrow(popu)){
    timeSteps<-c(timeSteps,time[i,1])
  }
  
  ##############################################################
  ##############################################################
  #### part 2: find peaks and valleys and convert###############
  ##############################################################
  ##############################################################
  
  
  #########################################
  ####*****find peaks fonction***##########
  ###notice:a 'peak' is defined as a local maxima with m points either side of it being smaller than it. 
  ###hence, the bigger the parameter m, the more stringent is the peak funding procedure
  find_peaks <- function (x, m = accu ){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
      z <- i - m + 1
      z <- ifelse(z > 0, z, 1)
      w <- i + m + 1
      w <- ifelse(w < length(x), w, length(x))
      if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    })
    pks <- unlist(pks)
  }
  
  ####*****find_peaks(-x) can be used to find valleys****######
  ############################################################
  find_valleys <- function (x,m = accu){
    vals<-find_peaks(-x,m)
    vals
  }
  
  pks<-find_peaks(popSize)
  vals<-find_valleys(popSize)
  
  
  #function that delete fake pks (which cause by 2 continuous kmax or k=0 in the k string)
  #remove pk if it is in the m steps following the previous peak.
  truepks<-function(pks,m = accu){
    fakepks<-NULL
    for (i in 2:length(pks)){
      ss<-pks[i]-pks[i-1]
      if (ss<=m) {
        fakepks<-c(fakepks,i)
      }
    }
    if(length(fakepks)>0){
      pks<-pks[-fakepks]
      pks
    }else{
      pks
    }
    #pks <-pks[(length(pks)-10):(length(pks)-1)]
  }
  
  pks<-truepks(pks,m = accu)
  vals<-truepks(vals,m = accu)
  
  #if population grows continuously before 1st peak, we consider the 1 genaration is the first valleys
  if(length(vals)<10){
    vals<-c(1,vals)
    print(vals)
  }
  
  pkststeps<-timeSteps[pks]
  valststeps<-timeSteps[vals]
  
  
  ##################################################################################
  ####*****fonction that returns file names of the chosen peaks and valleys****#####
  ##################################################################################
  find_files <- function(x){
    t<-NULL
    for (i in 1:length(x))
      t <- paste("ind",x,".csv",sep = "")
    return(t)
  }
  ####*****end of the fonction****####
  
  ##return the file names of the peaks and valleys
  
  pksfiles<-find_files(pkststeps) ##peaks to be imported and converted into genind for future tests
  valsfiles<-find_files((valststeps))  ##valleys to be imported and converted into genind for future tests
  
  print(paste("the last 10 peaks are:"))
  print(timeSteps[pks])
  print(paste("the last 10 valleys are:"))
  print(timeSteps[vals])
  
  
  ################################2018_06-08######################################################
  
  #############################################################
  #############################################################
  ### part 3: export matadatvals.csv and metadatpks.csv  ######
  #############################################################
  #############################################################
  
  ###metadata pks #########
  metadatpks<-data.frame()
  for (i in 1:length(pkststeps)){
    GRID<-read.csv(pksfiles[i], sep = ",",header = TRUE)
    
    individu<-cbind(pkststeps[i],GRID)
    metadatpks<-rbind(metadatpks,individu)
    
  }
  
  pksFileNam<-paste("metadatpks",j,".csv",sep="")
  
  write.table(metadatpks,file=pksFileNam,row.names = FALSE, col.names = TRUE,sep=",",quote=FALSE)
  
  #######metadata valleys #######
  metadatvals<-data.frame()
  for (i in 1:length(valststeps)){
    GRID<-read.csv(valsfiles[i], sep = ",",header = TRUE)
    
    individu<-cbind(valststeps[i],GRID)
    metadatvals<-rbind(metadatvals,individu)
  }
  
  valsFileNam<-paste("metadatvals",j,".csv",sep="")
  
  write.table(metadatvals,file=valsFileNam,row.names = FALSE, col.names = TRUE,sep=",",quote=FALSE)

} #end of the 'for' loop
