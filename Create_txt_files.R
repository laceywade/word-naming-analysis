### this script takes a csv file outputted by the Word Naming Game Task (run on PCIbexFarm) 
### and creates individual txt files for each word elicited, to be used for Forced Alignment

library(stringr)

### Load in the csv file from Ibex
ibex.full<-read.csv(file.choose(), header=T)
### Keep only lines with filenames
ibex<-ibex.full[ibex.full$Parameter=="Filename",]
### Create filenames
ibex$files<-str_replace(ibex$filename, ".wav", ".txt") #If your soundfiles are saved as something other than .wav, adjust this here

### Create tab-delimited transcript files in working directory
for (row in 1:nrow(ibex)) {
  file <- paste(ibex[row, "files"])
  id <- paste(ibex[row, "id"])
  word <- ibex[row, "word"]
  transcription <- paste("the word is", word)
### Choose whether you want txt files compatible with FAVE-Align or P2FA 
  contents <- data.frame(cbind(id, "0.1", "100000", transcription)) # for use with FAVE-Align
# contents <- transcription # uncomment for use with P2FA
   write.table(contents, file=file, sep="\t", col.names=FALSE, quote=FALSE)
  }
