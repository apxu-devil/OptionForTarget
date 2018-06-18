#
# Расчёт прибыли спреда опционов.
# Возвращает датафрейм Страйк-Прибыль.
# 
# df_strike_premiumм - датафрейм с колонками Strike и Premium
# target - целевой уровень по базовому активу
# opt_type - call или put
# ba_price - цена базового актива
# bank_rate - безрисковая ставка депозита
# exp_date - дата исполнения
#

# setdiff(c('a', 'b'), names(xx)) %>% length

# CalcProfitForStrike(data.frame(Strike=c(1,2), Premium=c(0.1, 0.2)), 100, 'p')

# test:
df_strike_premium = option_chain_short
target = 200
opt_type = "call"
ba_price = 176

CalcProfitForSpread = function(df_strike_premium, target, opt_type, ba_price=0, bank_rate=NULL, exp_date=NULL, atm_only=T){
  
  if(!require('dplyr')) install.packages('dplyr')
  if(!require('scales')) install.packages('scales')
  
  # Проверяем наличие колонок Strike и Premium
  columns_required = c('Strike', 'Premium')
  columns_present = names(df_strike_premium)
  columns_missing = setdiff(columns_required, columns_present)
  
  if(length(columns_missing)>0)
    stop(paste0("Can't find required columns in dataframe: ", columns_missing))
  
  
  # Если известна цена базового актива, определяем центральный страйк и за базовую премию принимаем премию центр. страйка
  # Если цена базовго актива не известна, базовая премия равна средней премии опциона в серии
  if(ba_price>0)
  {
    atm_index = AtmStrike(df_strike_premium$Strike, ba_price, T)
    atm_premium = df_strike_premium$Premium[atm_index]
    atm_strike = AtmStrike(df_strike_premium$Strike, ba_price, F)
  }  else {
    atm_premium = mean(df_strike_premium$Premium, na.rm = T)
    atm_index = which.min(abs(df_strike_premium$Premium - atm_premium))
    atm_strike = df_strike_premium$Strike[atm_index]
  }
  
  
  opt_type_num = ifelse(opt_type=="call", 1, -1)
  
# for (strike1 in df_strike_premium$Strike){  # calc profit for each pair strike1 (long) / strike2 (short)
  
  strike1 = 176
  
  prem1 = df_strike_premium$Premium[df_strike_premium$Strike==strike1] # Long option premium
  
  profit_df = df_strike_premium
  profit_df = profit_df %>% 
    mutate(Strike1=strike1, Strike2=Strike, Prem1=prem1, Prem2=Premium, PremSprd=Prem1-Prem2)
  
  
  if(opt_type_num==1){
    profit_df = profit_df %>% mutate(RealTarget = pmin(Strike2, target))
  } else {
    profit_df = profit_df %>% mutate(RealTarget = pmax(Strike2, target))
  }
  

  
  profit_df = profit_df %>% 
    mutate( Revenue = (RealTarget-Strike1) * opt_type_num ) %>% 
    mutate( Revenue = pmax(0, Revenue) ) %>%
    mutate( Profit = Revenue - PremSprd ) %>%
    mutate( Q =  atm_premium / PremSprd ) %>%
    mutate( Profitability = Profit / PremSprd ) 
  
  
#}

  #select(Strike, Profit_per_dollar)
  
  # Доходность опциона относительно текущей цены базового актива (Колич * Прибыль / Цена_актива)
  #if(ba_price>0){
  
  #profit_at_strike = profit_at_strike %>% mutate(Strike_ror = Q * Profit /  ba_price)
  
  if( !is.null(bank_rate) & !is.null(exp_date) ){
    
    exp_date = as.Date(exp_date)
    exp_days = as.numeric(exp_date - Sys.Date())
    exp_years = exp_days / 365
    
    profit_at_strike = profit_at_strike %>% mutate(Return =  Profitability * bank_rate * exp_years) #Q * Profit
    # profit_at_strike$Return = percent(profit_at_strike$Return)
  }
  #}
  
  
  # Оставляем страйки только до цели (после цели фин.рез всё равно 0)
  if(opt_type_num==1)
    profit_at_strike = profit_at_strike %>% filter(Strike<=target)
  else
    profit_at_strike = profit_at_strike %>% filter(Strike>=target)
  
  return(profit_at_strike)
  
}