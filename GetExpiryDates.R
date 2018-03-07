# 
# Загрузка доступных дат экспирации (базовый актив как текст):
#
GetExpiryDates = function(symbol, ...){
  
  if(!require('quantmod')) install.packages('quantmod')
  
  if(is.null(symbol) || symbol==""){
    message("Error: no symbol!")
    return(0)
    
  }
  
  # Указываем неверную дату экспирации, чтобы получить соотв. ошибку
  err = tryCatch({
    getOptionChain(symbol, Exp = "1900-01-01")
    
  },   
  error = function(e)e
  )
  
  # Действительно ли вернулась ошибка при загрузки доски?
  if( length(which(class(err) %in% "error")) ){
    
    # Разделяем строку по двоеточию
    message_split = unlist(strsplit(err$message, ":"))
    
    # Если первая часть сообщения ошибки соответствует неверной дате экспирации
    if(message_split[1]=="Provided expiry date(s) not found. Available dates are"){
      
      # Превращаем строку в массив и конвертируем в даты
      dates_from_message = unlist(strsplit(message_split[2], ","))
      
      # Пробуем преобразовать сообщение в даты
      dates_from_message = try({as.Date(dates_from_message)}, silent=T)
      
      if(class(dates_from_message)=="try-error"){
        warning("Can't convert dates to date format")
        return(0)
        
      } else 
        return(dates_from_message)
      
    } else {
      warning( paste0("Can't load chain for ", symbol) )
      return(0)
    }
    
  } else {
    
    warning("Chain loaded! :)")
    return(0)
  }
  return(0)
}

#