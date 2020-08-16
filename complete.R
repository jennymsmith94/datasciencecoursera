# Homework assignment for Coursera Data Science Specialization Course
# R Programming course- Week 2

#Part 2
complete <- function(directory,id=1:332){
        # 'directory' is a character vector of length 1 indicating the location
        ## of .csv files
        
        # 'id' is an integer vector indicating which monitors will be used in 
        ## calculating the mean for the indicated pollutant
        
        # Returns the number of complete cases for the monitors indicated
        ## A dataframe in the form:
        ## id  nobs
        ## 1   117
        ## 2   1041
        ##  ...
        
        #################
        origDir<-getwd()
        # navigate to the correct directory
        ##directory<-"C:/Users/S421596/Documents/Personal/DataScienceSpecializationCourse/specdata"
        setwd('..')
        setwd(directory)
        #get the list of files in the directory
        fileNames<-list.files()
        
        # set up dataframe based on number of monitors
        d=data.frame(id=id,nobs=0)
        
        # cycle through each id and get the number of complete entries
        idx<-1:length(id)
        for(i in idx){
                data<-read.csv(fileNames[id[i]])#read in the data
                data1<-data[complete.cases(data),]
                d$nobs[i]<-nrow(data1)
        }
        d
        
}