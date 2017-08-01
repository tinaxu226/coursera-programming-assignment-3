best <- function(state, outcome) {
#read the data and extract the needed five columns
dataout <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(dataout)
dout <- as.data.frame(cbind(dataout[, 2],   # hospital
                            dataout[, 7],   # state
                            dataout[, 11],  # heart attack
                            dataout[, 17],  # heart failure
                            dataout[, 23]), # pneumonia
                      stringsAsFactors = FALSE)
colnames(dout) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")

#check state and outcome is valid and return the outcome
if(!state %in% dout[, "state"]){
  stop('invalid state')
} else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
  stop('invalid outcome')
} else {
  st <- dout[which(dout[, "state"] == state),]  # extracting data for the called state
  ot <- as.numeric(st[, eval(outcome)])
  min_val <- min(ot, na.rm = TRUE)
  result  <- st[, "hospital"][which(ot == min_val)]
  output  <- result[order(result)]
}
return(output)
}

#test function
best("SC", "heart attack")  