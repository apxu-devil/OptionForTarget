#UTF-8
if(!require('quantmod')) install.packages('quantmod')
require(dplyr)
require(ggplot2)
# Безрисковая ставка депозита
bank_ror = 0.045

# Задаём символ базового актива;
symbol = "QQQ"

# Задаём Цель по цене безового актива;
target = 160

# Загружаем текущую цену базового актива;
symbol_price = getQuote(symbol)$Last

# Загружаем доступные даты экспирации символа (Символ);


expiry_dates = GetExpiryDates(symbol)
expiry_dates
expiry_date = expiry_dates[5]

# Определяем направление движения к цели
opt_rights = c("call", "put")
opt_right = ifelse(target>symbol_price, opt_rights[1], opt_rights[2])

# Выбираем тип цены для определения премии
market_price_types = c("mid", "last")
market_price_type = market_price_types[2]


option_chain_short = GetStrikePremiumTable(symbol, expiry_date, market_price_type, opt_right)

# Прибыль по спредам
# test:


profit_table = CalcProfitForSpread(option_chain_short, target, opt_right, symbol_price, bank_ror, expiry_date, market_price = F)

profit_table = profit_table %>% dplyr::select(Strike1, Strike2, Return) %>% 
  mutate(Return = round(Return * 100, 2))

ggplot(data=profit_table, aes(x=Strike2, y=Strike1, fill=Return)) + geom_tile() +
  scale_fill_gradient2(low = "white", high = "green", mid = "white", name="Return, %") +
  geom_text(aes(label=Return), size=3)



#atm_index = AtmStrike(option_chain_short$Strike, symbol_price, T)

profit_for_targets = CalcProfitForTargets(option_chain_short, target, opt_right, symbol_price, 0.05, expiry_date)
profit_for_targets = profit_for_targets %>% dplyr::select(Strike, Target, Return) %>% 
  filter(Strike<=symbol_price) %>%
  mutate(Return = round(Return * 100, 2))

profit_for_targets %>% top_n(., 5, Return)

require(scales)
gg1 = ggplot(data=profit_for_targets, aes(x=Target, y=Strike, fill=Return)) + geom_tile() +
  geom_text(aes(label=Return), size=3) + 
  scale_fill_gradient2(low = "white", high = "green", mid = "white")

require(plotly)
ggplotly(gg1)



# Рассчитываем прибыль для каждого страйка на уровне Цели (табл:страйк,премия; цель как число, кол/пут как фактор/текст) -> Страйк~Прибыль;
profit_for_strike = CalcProfitForStrike(option_chain_short, target, opt_right, symbol_price, 0.05, expiry_date)



# Площать треугольника прибыли
require(ggplot2)
profit_square = profit_for_strike %>% mutate(Sqr = (Profit*Q)*abs(Strike-target))
qplot(x = Strike, y = Sqr, data=profit_square)


# Рисуем график Страйк~Прибыль;
require(ggplot2)
gg = ggplot(data=profit_for_strike, aes(x=Strike, y=Return), color='red') + geom_point()
gg + geom_vline(xintercept = symbol_price, show.legend = T) + 
  geom_text(data = profit_for_strike %>% filter(Return>0.04),aes(label=paste0(round(Return*100, 2),"%" ), vjust=-1, hjust=1 ), size=3)

# Интерактивный график - способ 1
require(ggiraph)

gg <- ggplot(profit_for_strike, aes(x = Strike, y = Profit_per_dollar, tooltip = Profit_per_dollar ) ) + 
  geom_point_interactive(size=3)

library(rvg)
mytheme_main <- theme( panel.background = element_blank(), 
                       panel.grid.major = element_line(colour = "#dddddd"), 
                       axis.ticks = element_line(colour = "#dddddd") )

ggiraph(code = {print(gg)})

# Интерактивный график - способ 2
require(plotly)
ggplotly(gg)




