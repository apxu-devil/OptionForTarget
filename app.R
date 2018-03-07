#setwd('C:/Users/Andreal/Documents/GitHub/OptionForTarget')
#print(getwd())
source("CalcProfitForStrike.R", local = T, echo = F)
source('AtmStrike.R', local = T)
source('GetExpiryDates.R', local = T)
source('GetStrikePremiumTable.R', local = T)
source('CalcProfitForTargets.R', local = T)

library(shiny)
library(ggplot2)
library(DT)
require(scales)
require(ggiraph)
require(quantmod)
require(plotly)

# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Сравнение прибыли опционов с разными страйками"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         textInput(inputId = "txt_symbol",label = "Тикер базового актива (только NYSE)"),
         textOutput(outputId = "prnt_cur_price"), br(),
         uiOutput(outputId = "list_exp_dates"), 
         textOutput(outputId = "period_days"), br(),
         numericInput(inputId = "num_target", label = "Цель по базовому активу", value = 0),
         textOutput(outputId = "target_prc"), br(),
         selectInput("market_price_type", label = "Цена опционов:",choices = c("mid", "last"),selected = "last", multiple = F ),
         numericInput("bank_rate", label = "Безрисковая ставка, % годовых", value = 5, min = 0, max = 1, step = 0.1),
         textOutput(outputId = "period_rate"),br(),
         actionButton(inputId = "btn_apply", label = "Apply")
      ),
      
      # Основная панель с результатами
      mainPanel(
         # plotOutput("plot_strike_profit"),
         h2("Зависимость доходности от страйка"),
         plotlyOutput("plot_ly"), br(),
         h2("Страйки с максимальной доходностью"),
         DT::dataTableOutput("DT_strike_profit"), br(),
         h2("Зависимость доходности от цели и страйка"),
         plotlyOutput("profit_heatmap")
         
      )
   )
)


##### SERVER #####

server <- function(input, output) {
  
  # Загружаем цену базового актива, если есть тикер
  symbol_price = reactive({
    
    validate(need(input$txt_symbol!="", "Enter NYSE ticker") )
    
    symbol = input$txt_symbol
    price = getQuote(symbol)$Last
    
    price
  })
  
  #
  # Выводим цену базового актива
  #
  output$prnt_cur_price = renderText({
    
    paste0("Текущая цена: ", symbol_price(), "\n")
  })
  
  #
  # Количество дней в инвест. периоде
  #
  output$period_days = renderText({
    
    validate( need(input$list_exp_dates!="", "") )
    
    exp_date = as.Date(input$list_exp_dates)
    exp_days = as.numeric(exp_date - Sys.Date())
    
    paste0("Период: ", exp_days, " дней \n")
  })
  
  
  #
  # Процентная ставка на период
  #
  output$period_rate = renderText({
    
    validate( need(input$list_exp_dates!="", "") )
    validate( need(input$bank_rate!="", "") )
    
    exp_date = as.Date(input$list_exp_dates)
    exp_days = as.numeric(exp_date - Sys.Date())
    period_rate = round(input$bank_rate / 365 * exp_days, 2)
    
    paste0(period_rate, "% на период \n")
  })
  
  
#
# Если указан целевой уровень, рассчитываем расстояние в % до него от текущей цены
#
  output$target_prc = renderText({
    
    validate( need(symbol_price()!="", "") ) # Только если есть цена базового актива
    
    if(input$num_target >0){
      
      price = symbol_price()
      target = isolate(input$num_target)
      target_prc = round((target/price - 1)*100, 0)
      
      paste0(target_prc, "% до цели \n")
    }
  })
  
  
# 
# Выпадающий список дат исполнения: формируется после ввода тикера
#
  output$list_exp_dates = renderUI({
    
    validate( need(input$txt_symbol!="", "Enter NYSE ticker") ) # Если тикер не указан, выводим сообщение
    
    symbol = input$txt_symbol
    exp_dates = GetExpiryDates(symbol)
  
    selectInput("list_exp_dates", label = "Expiration dates", choices = exp_dates, multiple = F)
  })  
  
  
#
# Расчитываем значения прибыли по страйку при нажатии кнопки
#
  strike_profit_table = reactive({
    
    # При нажатии кнопки
    input$btn_apply
    
    # Определяем входные параметры функции расчёта из элементов упрвления
    symbol = isolate(input$txt_symbol)
    target = isolate(input$num_target)
    symbol_price = isolate(symbol_price())
    expiry_date = isolate(input$list_exp_dates)
    market_price_type = isolate(input$market_price_type)
    bank_rate = isolate(input$bank_rate)/100
    
    # Определяем направление движения к цели
    opt_right = ifelse(target>symbol_price, "call", "put")
    
    # Доска опционов только с колонками Страйк - Прибыль
    option_chain_short = GetStrikePremiumTable(symbol, expiry_date, market_price_type, opt_right)
    
    profit_for_targets = CalcProfitForTargets(option_chain_short, target, opt_right, symbol_price, bank_rate, expiry_date)
    profit_for_targets
  })
  

#
# Таблица 5 лучших при
#
  output$DT_strike_profit = DT::renderDataTable(DT::datatable({

    profit_for_strike = strike_profit_table()
    target = isolate(input$num_target)
    
    profit_for_strike = profit_for_strike %>% filter(Target==target)
    profit_for_strike = profit_for_strike %>% top_n(5, Return)
    
    profit_for_strike$Return = percent(profit_for_strike$Return)
    
    profit_for_strike %>% select(Strike, Premium, Q, Profit, Return)

  }
  # ,extensions = 'Buttons', options = list(
  #   dom = 'Bfrtip',
  #   buttons = list(list(extend = 'colvis', columns = c(2, 3, 4)))
  #     )
    ) 
  )

  
  
  output$plot_ly = renderPlotly({
    
    profit_for_strike = strike_profit_table() 
    
    target = isolate(input$num_target)
    symbol_price = isolate(symbol_price())
    
    profit_for_strike = profit_for_strike %>% filter(Target==target)
    profit_for_strike$Return = profit_for_strike$Return * 100
    
    gg = ggplot(data=profit_for_strike, aes(x=Strike, y=Return)) + geom_point() + #+ scale_y_continuous(labels=percent) +
      geom_vline(xintercept = symbol_price, show.legend = T) + ylab("Return, %")
    
    # Add top 5 profit points
     top_strikes = profit_for_strike %>% top_n(5, Return)
     top_strikes = top_strikes %>% mutate(Return_prc = paste0(round(Return*100, 2),"%" ))
     
     gg = gg + geom_point(data = top_strikes, color="green")
    
    ggplotly(gg)
  })
  
  
  
  output$profit_heatmap = renderPlotly({
    
    profit_for_targets = strike_profit_table()
    
    symbol_price = isolate(symbol_price())
    
    profit_for_targets = profit_for_targets %>% dplyr::select(Strike, Target, Return) %>% 
      filter(Strike <= symbol_price) %>%
      mutate(Return = round(Return * 100, 2))
    
    gg1 = ggplot(data=profit_for_targets, aes(x=Target, y=Strike, fill=Return)) + geom_tile() +
      geom_text(aes(label=Return), size=3) + 
      scale_fill_gradient2(low = "white", high = "green", mid = "white", name="Return, %")
    
    ggplotly(gg1)
    
    
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

