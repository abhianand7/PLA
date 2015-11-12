#remove previous workspace
rm(list = ls())

#include required Packages
require(XML)

#Obtain file location dynamically from a Folder
filenames <- list.files("/home/techtron/Career Graph/FFCS_Data",pattern="*.html",full.names=TRUE)

#Initialize Mark Sheet
mark <-NULL

#Access each File at time and make it into Mark Sheet
for(i in 1:NROW(filenames)){
	newmark <- as.data.frame(readHTMLTable(filenames[i],which=1,
		header=FALSE,
		stringsAsFactors=FALSE))
	newmark <- newmark[(newmark[,1]	== '1'|
			newmark[,1] == '2'|
			newmark[,1] == '3'|
			newmark[,1] == '4'|
			newmark[,1] == '5'|
			newmark[,1] == '6'|
			newmark[,1] == '7'|
			newmark[,1] == '8'|
			newmark[,1] == '9'|
			newmark[,1] == '10'),]
	newmark <- newmark[,c(3,7)]
	mark
	newmark				
	mark <- rbind(mark,newmark)
}

#Remove All NA
mark <- mark[(mark[,2] != 'N/A'),]

#Column And Row Names
names(mark) <- c("Course Name","Marks Obtained")
rownames(mark) <- c(1:NROW(mark))

#Convert Marks Obtained from Char to Numeric
ffcs[,2] <-as.numeric(ffcs[,2])

#Plot the Graph of the Mark Sheet
