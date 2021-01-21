##Henry Droese, Dec. 2020 -Rev.3
##Created for DME Web Analytics Department
##Keyword Finding Script for Post-processing Google Analytics results for keywords

#---
#TO RUN:
#Import data into RStudio
#Run through entire script to make sure functions are declared
#Run the skimfxn with the input of topsearch (1-150 terms)

#!Matrix input from excel/CSV must be [Terms,Number Searches] for proper results to show/script to work
#Consult example of input in order to make sure your input is correct
#Script/fxns built to be modular and adaptable
#---

#Pre-process the matrix
AllNames <- DecALLDATA[,1] #Takes the names from matrix, turn into separate matrix   ---DEC DATA
TNames <- t(AllNames) #Transpose into vector
CNames <- tolower(TNames) #Make all terms lowercase to increase homogenous search

##can use grep()!

#FXN to search for a specific term, and return the overall sum of all assocated counts
searchfxn <- function(term){
  termCol <- grep(term, CNames) #searches cleaned terms for TERM
  totalCol <- DecALLDATA[termCol,2] #compares found TERMS w/ totals    ---DEC DATA
  SumTerms <- sum(totalCol) #sum totals
  ListF <- cbind(term,SumTerms) #add term to first column in display result
  colnames(ListF) <- NULL
  return(ListF) 
}

#Take first 200 Searches, to use to compare to ALL terms
topsearch <- CNames[1:200]

#While loop to achieve this, kinda sloppy, may refine later with apply()
skimfxn <- function(Searches){
  i <- 1  
  outMatrix <- 0
  outTemp <- 0
  
  while (i < 201){
    outTemp <- searchfxn(Searches[i]) #use search fxn on i term of 150
    outMatrix <- rbind(outMatrix,outTemp)
    i = i+1    
  }
  return(outMatrix)
}

#At end, manual post-processing may be needed to eliminate distinct/similar results from top 150
#In doing so, do NOT sum similar term sums, as each term similarity sum is representative of overall
#i.e. would over-inflate representation of term
