## clear working space
rm(list=ls())

## Auto install missing package required 
list.of.packages <- c("lattice", "latticeExtra",'zoo','plyr',
                      'doBy','directlabels')#,'checkpoint')

new.packages <- list.of.packages[!(list.of.packages %in% 
                                   installed.packages()[,"Package"])
                                ]
if(length(new.packages)){
	install.packages(new.packages)
	}

## Load packages
lapply(list.of.packages,
       library,
       character.only = TRUE)


#checkpoint("2015-07-25") ## May need to use if using desktop instead of server

## set working directorate (need to change if data are located at
##  a different folder)
wd <- 'C:/Users/zhangw/Desktop/exercise/'
wd <- '/Users/xingmowang/Desktop/mbie/'
setwd(wd)

#################### read data  ##################
## read main data
data_main <- read.table("MainHeader.txt",
                        header = TRUE,
                        sep = "|"
                        )

## check if all respondents are unique
check_no_MainResponse <- length(unique(data_main$SurveyResponseID))

if(check_no_MainResponse!=nrow(data_main)){
  stop('Survey respondents are not unique')
}

## read Itinerary data
data_it <- read.table("ItineraryPlaces.txt",
                      header = TRUE,
                      sep = "|"
                      )
check_no_ItResponse <- length(unique(data_it$SurveyResponseID))

## check if any respondents are missing -- mismatch between the two dataset
if(check_no_MainResponse!=check_no_ItResponse){
  warning('Number of respondents do not match between the two datasets')
  warning(paste('Main data has',check_no_MainResponse,'unique respondents.'))
  warning(paste('Itinerary data has',check_no_ItResponse,'unique respondents.'))
}

################  Clear and format data ###############
## Change the class of Qtr from factor to time
data_main$Qtr <- as.matrix(data_main$Qtr) ## to char
data_main$Qtr <- as.yearqtr(data_main$Qtr,'%Y %q') ## to time class

## Not if all respondents are given to me so need to check this
## Looks like all repondents are given to me as there are around
## 1000 to 2500 respondents quarterly or 5k to 10k annually
tmp <- xtabs(~Qtr, data_main)
plot(tmp,type='l')

############## Aggregate main data to Annual ############
### year ended Mar is used here as ChCh earthquake happend on Feb 2011
## And the more recent data is March 2015.
## Year ended Mar data can better reflect the impacts from the earthquake

data_main$YearEndMar <- ifelse(substr(data_main$Qtr,6,7)%in%c('Q2','Q3','Q4'),
                         floor(as.numeric(data_main$Qtr))+1,
                         as.numeric(data_main$Qtr)
                        )

### aggregate to Year Ended March data
data_mainA <- ddply(data_main,
                    c(.(SurveyResponseID),
                      .(YearEndMar),
                      .(Airport),
                      .(POV)
                      ),
                     function(df){
                     	PopulationWeight <- sum(df$PopulationWeight,na.rm=T)
                     	WeightedSpend <- sum(df$WeightedSpend,na.rm=T)
                     	tmp <- cbind(PopulationWeight,WeightedSpend)
                     	colnames(tmp) <- c('PopulationWeight','WeightedSpend')
                     	return(tmp)
                     },
                     .progress = 'text'
                   )

### Aggregate to Annual data by Respondents to get 'population data'
data_mainAA <- ddply(data_mainA,
                    c(.(YearEndMar),
                      .(Airport),
                      .(POV)
                      ),
                     function(df){
                     	PopulationWeight <- sum(df$PopulationWeight,na.rm=T)
                     	WeightedSpend <- sum(df$WeightedSpend,na.rm=T)
                     	tmp <- cbind(PopulationWeight,WeightedSpend)
                     	colnames(tmp) <- c('PopulationWeight','WeightedSpend')
                     	return(tmp)
                     },
                     .progress = 'text'
                   )

## Aggregate by Airport
data_mainAAAP <- ddply(data_mainAA,
                    c(.(YearEndMar),
                      .(POV)
                      ),
                     function(df){
                     	PopulationWeight <- sum(df$PopulationWeight,na.rm=T)
                     	WeightedSpend <- sum(df$WeightedSpend,na.rm=T)
                     	tmp <- cbind(PopulationWeight,WeightedSpend)
                     	colnames(tmp) <- c('PopulationWeight','WeightedSpend')
                     	return(tmp)
                     },
                     .progress = 'text'
                   )
data_mainAAAP$Airport <- 'New Zealand'
data_mainAAAP <- data_mainAAAP[,colnames(data_mainAA)]

## Aggregate by POV
data_mainAAPOV <- ddply(data_mainAA,
                    c(.(YearEndMar),
                      .(Airport)
                      ),
                     function(df){
                     	PopulationWeight <- sum(df$PopulationWeight,na.rm=T)
                     	WeightedSpend <- sum(df$WeightedSpend,na.rm=T)
                     	tmp <- cbind(PopulationWeight,WeightedSpend)
                     	colnames(tmp) <- c('PopulationWeight','WeightedSpend')
                     	return(tmp)
                     },
                     .progress = 'text'
                   )
data_mainAAPOV$POV <- 'All'
data_mainAAPOV <- data_mainAAPOV[,colnames(data_mainAA)]

##Aggregate by Air and POV
data_mainAAAPPOV <- ddply(data_mainAA,
                    c(.(YearEndMar)
                      ),
                     function(df){
                     	PopulationWeight <- sum(df$PopulationWeight,na.rm=T)
                     	WeightedSpend <- sum(df$WeightedSpend,na.rm=T)
                     	tmp <- cbind(PopulationWeight,WeightedSpend)
                     	colnames(tmp) <- c('PopulationWeight','WeightedSpend')
                     	return(tmp)
                     },
                     .progress = 'text'
                   )
data_mainAAAPPOV$Airport <- 'New Zealand'
data_mainAAAPPOV$POV <- 'All'
data_mainAAAPPOV <- data_mainAAAPPOV[,colnames(data_mainAA)]

### Pull all Repondents aggregated data together
data_mainAA <- rbind.data.frame(data_mainAA,
                                data_mainAAAP,
                                data_mainAAPOV,
                                data_mainAAAPPOV)

## remove transitional data.frame
rm(list=c('data_mainAAAP','data_mainAAPOV','data_mainAAAPPOV'))

################ Merge both data ###################
data_all <- merge(data_main,data_it,by = c('SurveyResponseID'),all=TRUE)



