# Расчёт прибыли на каждом страйке для разных целей
# 
# Диапазон целей target:
# Если target - вектор - тогда рассчёт прибыли будет по этим значениям; 
# Если target - число - тогда фукнция формирует вектор из 10 значений в диапазоне от ba_price до target
# Диапазон разбиваем на 20 точек
# Для каждой из 20 точек считаем прибыль по страйку с пом. фукнции CalcProfitForStrike():
#   Для каждого значения цели из диапазона запускаем функцию CalcProfitForStrike
#   Добавляем колонку со значением цели
#   Полученные на каждой итерации таблицы склеиваем по страйку в одну таблицу:
#   Если результирующая таблица нуль, она равна таблице Страйк-Прибыль
#   В противном случае - совмещаем результирующую таблицу и последнюю таблицу Страй-Прибыль по колонкам
# 
#   (Если будут пустые значения на страйках, где нет расчёов прибыли, заменяем их на 0 - xxx[is.na(xxx)] = 0)


# target = seq(from = 100, to=200, length.out = 19)


CalcProfitForTargets = function(df_strike_premium, target, opt_type, ba_price=0, bank_rate=NULL, exp_date=NULL){
  
  # Если target - вектор - тогда рассчёт прибыли будет по этим значениям; 
  # Если target - число - тогда фукнция формирует вектор из 10 значений в диапазоне от ba_price до target
  if(length(target) == 1)
    targets = seq(from = target, to = ba_price, length.out = 10)
  else
    targets = target
  
  profit_table = NULL
  
  #   Для каждого значения цели из диапазона запускаем функцию CalcProfitForStrike
  for (tgt in targets) {
    
    profits = CalcProfitForStrike(df_strike_premium, tgt, opt_type, ba_price, bank_rate, exp_date)
    profits$Target = tgt
    
    if(!is.null(profit_table))
      profit_table = rbind(profit_table, profits)
    else
      profit_table = profits
    
  }
  
  profit_table$Return = round(profit_table$Return, 4)
  profit_table$Profitability = round(profit_table$Profitability, 2)
  profit_table$Q = round(profit_table$Q, 2)
  
  return(profit_table)
}
