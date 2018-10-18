#
# Расчёт прибыли спреда опционов.
# Возвращает датафрейм Страйк-Прибыль.
# 
# boardм - датафрейм с колонками Strike и Premium
# target - целевой уровень по базовому активу
# opt_type - call или put
# ba_price - цена базового актива
# bank_rate - безрисковая ставка депозита
# exp_date - дата исполнения
#

# setdiff(c('a', 'b'), names(xx)) %>% length

# CalcProfitForStrike(data.frame(Strike=c(1,2), Premium=c(0.1, 0.2)), 100, 'p')

# # test:
# board = option_chain_short
# target = 185
# opt_type = "call"
# ba_price = 176 
# bank_rate = 0.045
# exp_date = expiry_date
# atm_only=T
# market_price = F - if TRUE, price options at Ask and Bid; if FALSE - price options at Premium
# 
# 
# profit_table = CalcProfitForSpread(board, target, opt_type, ba_price, bank_rate, exp_date)
# 
# profit_table = profit_table %>% dplyr::select(Strike1, Strike2, Return) %>% 
#   #filter(Strike <= symbol_price) %>%
#   mutate(Return = round(Return * 100, 2))
# 
# gg1 = ggplot(data=profit_table, aes(x=Strike2, y=Strike1, fill=Return)) + geom_tile() +
#   scale_fill_gradient2(low = "white", high = "green", mid = "white", name="Return, %") +
#   geom_text(aes(label=Return), size=3)




CalcProfitForSpread = function(board, target, opt_type, ba_price=0, bank_rate=NULL, exp_date=NULL, atm_only=T, market_price = F){
  
  if(!require('dplyr')) install.packages('dplyr')
  if(!require('scales')) install.packages('scales')
  
  # Проверяем наличие колонок Strike и Premium
  columns_required = c('Strike', 'Premium')
  columns_present = names(board)
  columns_missing = setdiff(columns_required, columns_present)
  
  if(length(columns_missing)>0)
    stop(paste0("Can't find required columns in dataframe: ", columns_missing))
  
  
  # Если известна цена базового актива, определяем центральный страйк и за базовую премию принимаем премию центр. страйка
  # Если цена базовго актива не известна, базовая премия равна средней премии опциона в серии
  if(ba_price>0)
  {
    atm_index = AtmStrike( board$Strike, ba_price, T )
    atm_premium = board$Premium[ atm_index ]
    atm_strike = AtmStrike( board$Strike, ba_price, F )
    
  }  else {
    atm_premium = mean(board$Premium, na.rm = T)
    atm_index = which.min(abs(board$Premium - atm_premium))
    atm_strike = board$Strike[atm_index]
    
  }
  
  # Если нет цены актива, текущая цена = центральный страйк
  if(ba_price==0) ba_price=atm_strike
  
  # Define numeric var for option type
  # TODO: rename to callput
  opt_type_num = ifelse(opt_type=="call", 1, -1) 
  
  # Select only OTM strikes for long option in streads
  otm_strikes = board$Strike
  otm_strikes = otm_strikes[ (opt_type_num*otm_strikes >= opt_type_num*ba_price) & (opt_type_num*otm_strikes <= opt_type_num*target)]
 
   # Empty table for all profits
  profit_table = NULL 
  
  
 for (strike1 in otm_strikes){  # calc profit for each pair strike1 (long) / strike2 (short)
  
   if (market_price == F){
     prem1 = board$Premium[board$Strike==strike1] # Long option premium
     prem2 = board$Premium
     
   } else {
     
     prem1 = board$Ask[board$Strike==strike1]
     prem2 = board$Bid
   }
  
  profit_df = board %>% 
    mutate(Strike1=strike1, 
           Strike2=Strike, 
           Prem1=prem1, 
           Prem2=prem2, 
           PremSprd=Prem1-Prem2)
  
  # Select strikes2 only before target
  profit_df = profit_df %>% filter(Strike2*opt_type_num <= target*opt_type_num)
  
  # If target price before Strike2 (short option) then calc profit at target price, else - calc profit at Strike2 price
  if(opt_type_num==1)
    profit_df = profit_df %>% mutate(RealTarget = pmin(Strike2, target))
  else 
    profit_df = profit_df %>% mutate(RealTarget = pmax(Strike2, target))
  
  
  profit_df = profit_df %>% 
    mutate( Revenue = (RealTarget-Strike1) * opt_type_num ) %>% 
    mutate( Revenue = pmax(0, Revenue) ) %>%
    mutate( Profit = Revenue - PremSprd ) %>%
    mutate( Q =  atm_premium / PremSprd ) %>%
    mutate( Profitability = Profit / PremSprd ) %>%
    mutate( ROR = Q*Profit / ba_price)   # Доходность опциона относительно текущей цены базового актива (Колич * Прибыль / Цена_актива)

  # Calc spread Return for a given bank rate as a budget limit
  if( !is.null(bank_rate) & !is.null(exp_date) ){
    
    exp_date = as.Date(exp_date)
    exp_days = as.numeric(exp_date - Sys.Date())
    exp_years = exp_days / 365
    
    profit_df = profit_df %>% mutate( Return = (bank_rate*exp_years)/(PremSprd/ba_price)*(Revenue/ba_price)  )
    
    # profit_at_strike$Return = percent(profit_at_strike$Return)
  }
  
  # Merge tables for various strike1 results
  if(!is.null(profit_table))
    profit_table = rbind(profit_table, profit_df)
  else
    profit_table = profit_df

 }  
  
  # Оставляем страйки только до цели (после цели фин.рез всё равно 0)
  profit_table = profit_table %>% filter(Q>0)

  
  return(profit_table)
  
}
