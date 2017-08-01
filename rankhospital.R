rankhospital <- function(state, outcome, rank = "best"){
  ## Read outcome data
  dataout <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  dout   <- as.data.frame(cbind(dataout[, 2],  # hospital
                              dataout[, 7],  # state
                              dataout[, 11],  # heart attack
                              dataout[, 17],  # heart failure
                              dataout[, 23]), # pneumonia
                        stringsAsFactors = FALSE)
  colnames(dout) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome is valid and return output
  if (!state %in% dout[, "state"]) {
    stop('invalid state')
  } else if (!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  } else if (is.numeric(rank)) {
    st <- dout[which(dout[, "state"] == state),]  # extracting data for the called state
    st[, eval(outcome)] <- as.numeric(st[, eval(outcome)])
    st <- st[order(st[, eval(outcome)], st[, "hospital"]), ]
    output <- st[, "hospital"][rank]
  } else if (!is.numeric(rank)){
    if (rank == "best") {
      output <- best(state, outcome)
    } else if (rank == "worst") {
      st <- dout[which(dout[, "state"] == state),]
      st[, eval(outcome)] <- as.numeric(st[, eval(outcome)])
      st <- st[order(st[, eval(outcome)], st[, "hospital"], decreasing = TRUE), ]
      output <- st[, "hospital"][1]
    } else {
      stop('invalid rank')
    }
  }
  return(output)
}

#test function
rankhospital("NC", "heart attack", "worst")
