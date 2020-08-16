# Homework assignment for Coursera Data Science Specialization Course
# R Programming course- Week 2

#Part 1

pollutantmean <- function(directory,pollutant,id=1:332){
        # 'directory' is a character vector of length 1 indicating the location
        ## of .csv files
        
        # 'pollutant' is a character vector of length 1 indicating the name of
        ## the pollutant for which this function calculates the mean value
        
        # 'id' is an integer vector indicating which monitors will be used in 
        ## calculating the mean for the indicated pollutant
        
        # Returns the mean value for the pollutant across all monitors indicated
        
        #################
        origDir<-getwd()
        # navigate to the correct directory
        ##directory<-"C:/Users/S421596/Documents/Personal/DataScienceSpecializationCourse/specdata"
        setwd('..')
        setwd(directory)
        #get the list of files in the directory
        fileNames<-list.files()
        
        # Read in the first file 
        data=read.csv(fileNames[1])
        
        # Use which pollutant to identify the column number you'll work with
        colNum<-match(pollutant,names(data))
        rm(data)
        
        #Now get the mean value and number of observations for the pollutant of 
        ## interest for each .csv file, ignoring NA values. Since we know the 
        ## number of files we are going to look at from the 'id' variable, we 
        ## can make a matrix to hold those values. This will allow us to close 
        ## each .csv file after we're done with it to preserve memory
        obsData<-matrix(data=NA,length(id),2) #a matrix of NA's so I can tell if I messed up

        idx<-1:length(id)
        for(i in idx){
                data<-read.csv(fileNames[id[i]])#read in the data
                #take the correct pollutant column and remove NA's
                polTemp<-data[,colNum]
                polTemp<-polTemp[!is.na(polTemp)]
                goodVals<-c(mean(polTemp),length(polTemp))
                obsData[i,]<-goodVals
        }
        #go back to original directory
        setwd(origDir)
        
        #Now you have the mean and sample number for each .csv file. Now you can
        ## find the grand mean
        if(nrow(obsData)==1){
                obsData[1]
        }
        else{
                grandSum<-obsData[,1]*obsData[,2]
                #remove any NA values
                obsData<-obsData[!is.na(grandSum),1:2]
                grandSum<-sum(grandSum[!is.na(grandSum)])
                totObs<-sum(obsData[,2])
                grandSum/totObs
        }
        

        
}