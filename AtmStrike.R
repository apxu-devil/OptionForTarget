
AtmStrike = function(strikes, price, index=F){
  
  moneyness = abs(strikes - price)
  
  centre_index = which.min(moneyness)
  
  if(index) return(centre_index)
  
  return(strikes[centre_index])
  
}

