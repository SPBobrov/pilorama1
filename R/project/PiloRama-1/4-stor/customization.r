ПалитраПокупатели <- structure(c(
  "#dddddd", "#1be19f", "#156079", "#5fe12e", "#9d222e", "#27cae6", 
  "#6294ce", "#42952e", "#dddddd", "#ae783e", "#ff0087", "#6294ce", "#E3201B", "#f4bb8f", "#cf80dd", "#6e390d", 
  "#add51f", "#7212ff", "#6a4763", "#f35ff9", "#f3d426", "#f57685", "#89975b", "#fd5917"
  ), names = c("na", "Ашот", "Балтстарк", "Бетулекс", "Валерьян", "Изометрика", "Кондор", "Лесторг", "не указан", "Петрович", 
  "СЗДК", "Стройка", "Schweighofer", "Миша Ростов", "Кокк", "Корфа", "Орман", "", "", "", 
  "", "", "", ""))

ПалитраДерево <- structure(c(
  "#dddddd", "#fffa00", "#ffcc99", "#1dd300", "#ff4000", "#6d9c00", "#ff0000", "#a8f000", "#dddddd", 
  "#fed766", "#7bc043"
  ), names = c("na", "береза", "береза(горб)", "хвоя", "осина", "ель", "дрова", "сосна", "", "", 
  ""))

library(ggtext)


as_theme_month = list(
  theme_minimal(
            base_family = "PT Sans Narrow Bold",
    
           ),
     theme(  
      
      legend.position="bottom",
    text=element_text(size=18)
     
     ),
     
  theme(axis.text.x = element_text(angle = 90, vjust = 0,  size=12)),
  xlab(""),
  ylab(""),
  
  scale_x_date(date_breaks = "1 day",
    date_minor_breaks = "1 day",
    date_labels = "%d %b %a", 
    expand = c(0,0)),
    
  geom_vline(xintercept = ДАТА_ОТЧЕТА, colour="blue", linetype = "longdash", size=1, alpha=0.5) 
           
 )

as_theme = list(
  theme_minimal(
    base_family = "PT Sans Narrow Bold",
    
  ),
  theme(  
    
    legend.position="bottom",
    text=element_text(size=18)
    
  ),
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0,  size=12)),
  xlab(""),
  ylab("")
  
)






