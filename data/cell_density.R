
funny_math <- function(samp_vec, doubling=2, operator)
{  if(operator=="add"){
  return(sum(samp_vec + doubling))
  }
else if(operator == "subtract") {
  return(sum(samp_vec - doubling))
  }
  return (NA)
}


cell_density <- vector(mode="numeric", length=100)
for(time in 1:100){
  cell_density (time) <- time^2
}

cell_density <- (1:100)*2