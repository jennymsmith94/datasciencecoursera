# Homework assignment for Coursera Data Science Specialization Course
# R Programming course- Week 2

#Part 3

corr <- function(directory,threshold=0){
        # 'directory' is a character vector of length 1 indicating the location
        ## of .csv files
        
        # 'threshold' is a numeric vecotr of length 1 indicating the number of
        ## complete cases required to compute the correlation between nitrate 
        ## and sulfate. The default is 0.
        
        #Returns a numeric vector of correlations
        ## DON'T ROUND
        ###################################
        # get current directory
        origDir<-getwd()
        
        #Get the number of complete cases for each monitor
        numCases<-complete(directory)
        #Narrow down the list to cases that meet criteria
        numCases<-numCases[numCases$nobs>=threshold,]
        
        #navigate to proper directory
        setwd('..')
        setwd(directory)
        #get the list of files in the directory
        fileNames<-list.files()
        
        if(length(numCases$id>0)){
                #create a storage variable to hold the pollutant info based on number of
                ##correlation coefficients we will be generating (number of monitors)
                storData<-c(1,length(numCases$id))
                
                #cycle through list of monitors to add the info to the storage variable
                for(i in 1:length(numCases$id)){
                        data<-read.csv(fileNames[numCases$id[i]])#read in the data
                        data<-data[complete.cases(data),]#only get the complete cases
                        #compute the correlation
                        corTemp<-cor(data$sulfate,data$nitrate)
                        #add data to storage variable
                        storData[i]<-corTemp
                        
                }
                storData=storData[!is.na(storData)]
                setwd(origDir)
        }
        else{
                storData<-numeric()
        }
        storData
}
        