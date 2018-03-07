
#
# Расчёт прибыли на целевом уровне для всех опционов по страйкам.
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

CalcProfitForStrike = function(df_strike_premium, target, opt_type, ba_price=0, bank_rate=NULL, exp_date=NULL){
  
  if(!require('dplyr')) install.packages('dplyr')
  if(!require('scales')) install.packages('scales')
  
  # Проверяем наличие колонок Strike и Premium
  columns_required = c('Strike', 'Premium')
  columns_present = names(df_strike_premium)
  columns_missing = setdiff(columns_required, columns_present)
  
  if(length(columns_missing)>0)
    stop(paste0("Can't find required columns in dataframe: ", columns_missing))
  
  
  # Если известна цена базовго актива, определяем центральный страйк серии
  # Если цена базовго актива не известна, заменяем её средней премией опциона в серии
  if(ba_price>0)
  {
    atm_index = AtmStrike(df_strike_premium$Strike, ba_price, T)
    atm_premium = df_strike_premium$Premium[atm_index]
  } 
  else 
  {
    atm_premium = mean(df_strike_premium$Premium, na.rm = T)
  }
  
  
  opt_type_num = ifelse(opt_type=="call", 1, -1)
  
  profit_at_strike = df_strike_premium %>% 
    mutate( Revenue = (target-Strike) * opt_type_num ) %>% 
    mutate( Revenue = pmax(0, Revenue) ) %>%
    mutate( Profit = Revenue - Premium ) %>%
    mutate( Q =  atm_premium / Premium ) %>%
    mutate( Profitability = Profit / Premium )
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
