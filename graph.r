#remove previous workspace
rm(list = ls())

#include required Packages
require(XML)
require(stringr)
require(xlsx)

#Obtain file location dynamically from a Folder
filenames <- list.files("/home/techtron/Career_Graph/git_project/FFCS_Data",pattern="*.html",full.names=TRUE)

#Initialize
mark <-NULL
mark_mean <- NULL
cfall = 1
cwinter = 2
sem <- NULL
#Access each File at time and make it into Mark Sheet
for(i in 1:NROW(filenames)){
	header <- str_replace_all(filenames[i],
				"(.+?)Data/(.+?).html",
				replacement="\\2")
#For Semester Numbering	
	number <- str_replace_all(filenames[i],"(.+?)Data/(.+?)20(.+?).html",replacement="\\2")
	
	if(number == "fall"){
		newname <- sprintf("%d",cfall)
		cfall = cfall + 2
	}
	else if(number == "winter"){
		newname <- sprintf("%d",cwinter)
		cwinter = cwinter + 2
	}
	sem <- rbind(sem,c(newname,header))

#For Mark Sheet				
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
	
	#Remove All NA
	newmark <- newmark[(newmark[,2] != 'N/A'),]
	
	#Convert Marks Obtained from Char to Numeric
	newmark[,2] <-as.numeric(newmark[,2])
	#Convert Mean Marks from Char to Numeric
	mark_mean[,2] <- as.numeric(mark_mean[,2])

	#Calculate Mean Semester-wise and Add to Mean Mark Sheet
	newmean <- mean(newmark[,2])
	
	#Binding it with Original sheets
	mark_mean <- rbind(mark_mean,c(header,newmean))
	mark <- rbind(mark,newmark)
	
}

#Convert Mean Mark from Matrix format to Data Frame
mark_mean <- as.data.frame(mark_mean,stringsAsFactors=FALSE)
sem <- as.data.frame(sem,stringsAsFactors=FALSE)

#Combining semester numbering to mean sheet
mark_mean <- cbind(sem[,1],mark_mean)

#Sorting Mean Sheet as Sem Numbering
mark_mean <- mark_mean[order(mark_mean[,1]),]

#Convert Mean Marks from Char to Numeric
mark_mean[,3] <- as.numeric(mark_mean[,3])

mark <- mark[,c(1,NCOL(mark))]
mark_mean <- mark_mean[,c(2,(NCOL(mark_mean)))]

#Column And Row Names
names(mark) <- c("Course Name","Marks Obtained")
rownames(mark) <- c(1:NROW(mark))
names(mark_mean) <- c("Semester","Mean")

#Plot the Graph

#midpoints1 <- barplot(mark[,2],ylim=c(0,100),xlim=c(0,NROW(mark)),width=.8)
#text(midpoints1,45, labels=mark[,1],las=2,srt=90)

plot(mark_mean[,2],col="red",lwd = 3,type = "b",main = "Performance",xlab = "Semester",ylab = "Mean Marks per Semester")

#Export Sheet to XLSX file format
wb = createWorkbook()
marksheet <- createSheet(wb,sheetName="Marks Obtained")
meanmarksheet <- createSheet(wb,sheetName="Semester-wise Mean Marks")
addDataFrame(mark,sheet=marksheet)
addDataFrame(mark_mean,sheet=meanmarksheet)
saveWorkbook(wb,"CareerGraph.xlsx")


