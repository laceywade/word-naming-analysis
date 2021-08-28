library(stringr)

### Load in the "Cleaned_Results_date.csv" file from Ibex
ibex.full<-read.csv(file.choose(), header=T)
### Keep only lines with filenames
ibex<-ibex.full[ibex.full$Parameter=="Filename",]
### Create filenames
ibex$files<-str_replace(ibex$filename, ".wav", ".txt") #If your soundfiles are save as something other than .wav, adjust this here

### Create tab-delimited transcript files
for (row in 1:nrow(ibex)) {
  file <- paste(ibex[row, "files"])
  id <- paste(ibex[row, "id"])
  word <- ibex[row, "word"]
  transcription <- paste("the word is", word)
  contents <- data.frame(cbind(id, "0.1", "100000", transcription))
   write.table(contents, file=file, sep="\t", col.names=FALSE, quote=FALSE)
  }