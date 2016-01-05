#3cases
# 1.less than 20 occurrances : http://www.ncbi.nlm.nih.gov/pubmed/?term=(heart+failure)+AND+prevacid
  #cue : does not contain word 'of'
# 2.more than 20 occurances  : http://www.ncbi.nlm.nih.gov/pubmed/?term=(heart+failure)+AND+cozaar
  #cue : does contain word 'of'
# 3.not found                : http://www.ncbi.nlm.nih.gov/pubmed/?term=(heart+failure)+AND+cdsjkds 
  #cue : contains 'No items found'

library('stringr')
library('dplyr')

options(warn=-1)

df = read.csv("MedicationsToConsider.csv")
medications  = as.character(df$MedicationNames)
allprefix = c('(heart+failure)','(readmission)','(chf)','(length+of+stay)','(mortality)', '(one+year+mortality)','(twelve+month+mortality)','(seven+day+length+of+stay)','(thirty+day+readmission)','(30+day+readmission)')
#allprefix = c('(heart+failure)','(readmission)','(chf)')
#prefix = "http://www.ncbi.nlm.nih.gov/pubmed/?term=(heart+failure)+AND+"

#final containers
heartfailurecountarray = c()
readmissioncountarray = c()
chfcountarray = c()
lengthofstaycountarray = c()
mortalitycountarray = c()
oneyearmortalitycountarray = c()
twelvemonthmortalitycountarray = c()
sevendaylengthofstaycountarray = c()
thirtydayreadmissioncountarray = c()
thirtyday30readmissioncountarray = c()

#loop through all prefixes
for(p in 1:length(allprefix))
{
 #prefix here
 prefix = paste0("http://www.ncbi.nlm.nih.gov/pubmed/?term=",allprefix[p],"+AND+")

 #counters
 medicationsarray = c()
 mylookuptable = c()

 for(i in 1:length(medications))
 #for(i in 1:5)
 {
  cat(paste("p==",p,"i==",i,"\n"))
 
  medication = word(medications[i],1)
 
  #cat(paste("medication==",medication,"\n"))
  #cat(paste("mylookuptable==\n"))
  #print(mylookuptable)
  if(medication %in% mylookuptable)
   next;
  
  mylookuptable = c(mylookuptable,medication)
 
  url = paste0(prefix,medication)
  print(url);
 
  page = readLines(url)
 
  #Case 3 : not found
  if(length(grep("No items found",page)) > 0)
  {
   print(0)
   count = 0
  }else#Case 1 OR Case 2
  {
   line = page[grep("result_count",page)]
   matches <- regmatches(line, gregexpr("[[:digit:]]+", line))
   matches = as.numeric(unlist(matches))
  
   #Case 2 : more than 20 occurances
   if(length(grep("of",line)) > 0)
   {
    print(matches[6])
    count = matches[6]
   }else #Case 1 : less than 20 occurances
   {
    count = matches[4];
    print(matches[4])
   } 
  }
  
  medicationsarray = c(medicationsarray,medication)

  switch(p, 
  '1'={
  heartfailurecountarray = c(heartfailurecountarray,count)
  },
  '2'={
  readmissioncountarray = c(readmissioncountarray,count)
  },
  '3'={
  chfcountarray = c(chfcountarray,count)
  },
  '4'={
  lengthofstaycountarray = c(lengthofstaycountarray,count)
  },
  '5'={
  mortalitycountarray = c(mortalitycountarray,count)
  },
  '6'={
  oneyearmortalitycountarray = c(oneyearmortalitycountarray,count)
  },
  '7'={
  twelvemonthmortalitycountarray = c(twelvemonthmortalitycountarray,count)
  },
  '8'={
  sevendaylengthofstaycountarray = c(sevendaylengthofstaycountarray,count)
  },
  '9'={
  thirtydayreadmissioncountarray = c(thirtydayreadmissioncountarray,count)
  },
  '10'={
  thirtyday30readmissioncountarray = c(thirtyday30readmissioncountarray,count)
  },
  {
   print('default')
  }
 )

 }
}

save.image("MyImage.RData")
 
df = data.frame(medicationsarray,heartfailurecountarray,readmissioncountarray,chfcountarray,lengthofstaycountarray,mortalitycountarray,oneyearmortalitycountarray,twelvemonthmortalitycountarray,sevendaylengthofstaycountarray,thirtydayreadmissioncountarray,thirtyday30readmissioncountarray)
#print(df)

for(i in 2:ncol(df))
{
 narows = which(is.na(df[,i]))
 df[narows,i] = 0
}

COMBINEDCOUNT = rowSums(df[,2:ncol(df)])
df$COMBINEDCOUNT = COMBINEDCOUNT

df = arrange(df,desc(COMBINEDCOUNT))

write.table(df, "domainexpert_AND_NIH", sep="\t")
write.csv(df,'output.csv',row.names=FALSE)
saveRDS(df,'output.RDS')
