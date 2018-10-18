#
# Функция для приложения: выгрузка страйка-премии опциона
#
#
GetStrikePremiumTable = function(symbol, expiry_date, market_price_type, opt_right){
  
  if(!require('quantmod')) install.packages('quantmod')
  
  # Загружаем опционную доску выбранного актива и даты экспирации;
  option_chain = getOptionChain(symbol, Exp=expiry_date)
  
  # Выбираем столбцы Примия и Страйк;
  opt_rights = c("call", "put")
  opt_board_n = which( opt_rights %in% opt_right)
  option_chain_short = option_chain[[opt_board_n]]
  
  # Рыночная цена - середина
  if(market_price_type=="mid"){
    
    option_chain_short$Mid = (option_chain_short$Ask + option_chain_short$Bid)/2
    # option_chain_short = option_chain_short[, c("Strike", "Mid", "Ask", "Bid")]
  }
  
  # Рыночная цена - цена последней сделки
  # if(market_price_type=="last")
  #   option_chain_short = option_chain_short[, c("Strike", "Last", "Ask", "Bid")]
  # 
  # names(option_chain_short) = c("Strike", "Premium", "Ask", "Bid")
  
  return(option_chain_short)
}
