ГрафикиПоУчастку <- function(pCaption = "Оптимизатор", pВХОД = ОПТИМИЗАТОР_ВХОД, pВЫХОД = ОПТИМИЗАТОР_ВЫХОД) {
	pВЫХОД <- subset(x = pВЫХОД, subset = (Дата >= ДАТА_ОТЧЕТА_СТАРТ) & (Дата <= ДАТА_ОТЧЕТА_ФИНИШ) & (Дата <= ДАТА_ОТЧЕТА))
	
	for(i in 1:nrow(ЗаменаПокупателей)){
		from<-ЗаменаПокупателей$from[i]
		to<-ЗаменаПокупателей$to[i]
		pВЫХОД$Покупатель[pВЫХОД$Покупатель==from]<-to
	}
	
	pВЫХОД <- local({
		x <- pВЫХОД
		y <- pВЫХОД
		y[is.na(x[, "Как покинул склад"]), "Как покинул склад"] <- with(x[is.na(x[, "Как покинул склад"]), , drop=FALSE], "")
		y[is.na(x[, "Покупатель"]), "Покупатель"] <- with(x[is.na(x[, "Покупатель"]), , drop=FALSE], "не указан")
		y[is.na(x[, "Сечение"]), "Сечение"] <- with(x[is.na(x[, "Сечение"]), , drop=FALSE], "не указано")
		y[is.na(x[, "Состояние"]), "Состояние"] <- with(x[is.na(x[, "Состояние"]), , drop=FALSE], "")
		y
	})
	
	pВЫХОД_ПО_ПОКУПАТЕЛЯМ <- aggregate(formula = м3 ~ Дата + Покупатель + Состояние, data = pВЫХОД, FUN = sum, na.action = na.omit, na.rm = FALSE)
	
	pВЫХОД_ПО_СЕЧЕНИЯМ_СОРТАМ <- aggregate(formula = м3 ~ Дата + Сечение + сорт, data = pВЫХОД, FUN = sum, na.action = na.omit, na.rm = FALSE)
	
	pВЫХОД_ПО_ДАТАМ <- aggregate(formula = м3 ~ Дата, data = pВЫХОД, FUN = sum, na.action = na.omit, na.rm = FALSE)
	
	pВЫХОД_ПО_СЕЧЕНИЯМ <- aggregate(formula = м3 ~ Дата + Сечение, data = pВЫХОД, FUN = sum, na.action = na.omit, na.rm = FALSE)
	
	
	G_pВЫХОД_ПО_ПОКУПАТЕЛЯМ<-ggplot(data=pВЫХОД_ПО_ПОКУПАТЕЛЯМ, 
	aes(x=Дата, y=м3))+
	as_theme_month+
	
	geom_col(aes(fill=Покупатель, group=Покупатель), size=1.2,  position=position_stack())+
	geom_text( direction="x", size=4, position=position_stack(vjust = 0.5), hjust="center", aes(label=round(м3), group=Покупатель))+
	geom_label( data=pВЫХОД_ПО_ДАТАМ, size=6, aes(x=Дата, y=м3, label=round(м3)), vjust=-0.0)+
	
	ggtitle(paste(pCaption, ", выход по дням (м3)"),
		subtitle=paste(round(sum(filter(pВЫХОД_ПО_ПОКУПАТЕЛЯМ, Дата<=ДАТА_ОТЧЕТА)$м3)),"м3", "на", ДАТА_ОТЧЕТА)
	)+
	
	
	expand_limits(y = seq(0, 50, by = 5))+
	
	scale_fill_manual(values=ПалитраПокупатели)
	
	G_pВЫХОД_ПО_СЕЧЕНИЯМ<-ggplot(data=pВЫХОД_ПО_СЕЧЕНИЯМ, 
	aes(x=Дата, y=м3, fill=Сечение, group=м3))+
	as_theme_month+
	geom_col(position=position_stack(), color="white")+
	#
	#geom_text(  angle=0, aes(label=Сечение), size=4,  hjust="outward", position=position_stack(vjust = .1, ))+
	geom_text( lineheight=1, size=4, position=position_stack(vjust = 0.5), hjust="center", 	aes(label=paste(Сечение, ifelse(round(м3)>1,paste("\n" ,round(м3),"куб.м."),""))))+
	#geom_label( size=6, position=position_stack(vjust = 0), hjust="center", 	aes(label=round(куб.м.)))+
	
	ggtitle(paste(pCaption, "выход по сечениям"))+
	
	theme(legend.position = "NA")
	
	pВХОД <- subset(x = pВХОД, subset = (`Дата покидания склада` >= ДАТА_ОТЧЕТА_СТАРТ) & (`Дата покидания склада` <= ДАТА_ОТЧЕТА_ФИНИШ) & (`Дата покидания склада` <= ДАТА_ОТЧЕТА))
	
	for(i in 1:nrow(ЗаменаПокупателей)){
		from<-ЗаменаПокупателей$from[i]
		to<-ЗаменаПокупателей$to[i]
		pВХОД$Покупатель[pВХОД$Покупатель==from]<-to
	}
	
	pВХОД <- local({
		x <- pВХОД
		y <- pВХОД
		y[is.na(x[, "Покупатель"]), "Покупатель"] <- with(x[is.na(x[, "Покупатель"]), , drop=FALSE], "не указан")
		y[is.na(x[, "Сечение"]), "Сечение"] <- with(x[is.na(x[, "Сечение"]), , drop=FALSE], "не указано")
		y[is.na(x[, "Состояние"]), "Состояние"] <- with(x[is.na(x[, "Состояние"]), , drop=FALSE], "")
		y[is.na(x[, "порода"]), "порода"] <- with(x[is.na(x[, "порода"]), , drop=FALSE], "не указана")
		y
	})
	
	pВХОД_ПО_ПОРОДАМ <- aggregate(formula = м3 ~ `Дата покидания склада` + порода, data = pВХОД, FUN = sum, na.action = na.omit, na.rm = FALSE)
	
	pВХОД_ПО_ДАТАМ <- aggregate(formula = м3 ~ `Дата покидания склада`, data = pВХОД, FUN = sum, na.action = na.omit, na.rm = FALSE)
	
	pВХОД_ПО_ПОРОДАМ_ИТОГ <- aggregate(formula = м3 ~ порода, data = pВХОД, FUN = sum, na.action = na.omit, na.rm = FALSE)
	
	library("ggtext")
	library("ggpmisc")
	
	G_pВХОД<-ggplot(data=pВХОД_ПО_ПОРОДАМ, 
	aes(x=`Дата покидания склада`, y=м3, fill=порода, group=порода))+
	as_theme_month+
	geom_col(, size=1.2,  position=position_stack())+
	#geom_smooth(data=pВХОД_ПО_ДАТАМ)+
	geom_text(  lineheight=1, angle=0, aes(label=paste(round(м3))), size=5,  hjust=0.5, position=position_stack(vjust = .5, ))+
	#geom_text( data=pВХОД_ПО_ДАТАМ, size=7, aes(x=`Дата покидания склада`, y=м3, label=round(м3)), vjust=-0.2, fill="white")+
	
	
	ggtitle(paste(pCaption, "вход  (в м3)"),
	subtitle=paste(
	 round(sum(filter(pВХОД_ПО_ДАТАМ, `Дата покидания склада`<=ДАТА_ОТЧЕТА)$м3)),"м3", "на", ДАТА_ОТЧЕТА)
	)+
	
	scale_fill_manual(values=ПалитраДерево)
	
	G_ВходВыход<-ggplot(data=pВХОД, 
	aes(x=`Дата покидания склада`, y=м3, label=paste(литера,Номер, round(м3,1)), fill=м3))+
	as_theme_month+
	
	
	geom_col(width=0.45, color="white", size=1.2,  position=position_stack())+
	geom_text(  lineheight=1, angle=0, , size=3,  hjust=0.5, position=position_stack(vjust = .5, ))+
	
	geom_col( width=0.45, data=pВЫХОД, aes(x=Дата+0.45, y=1*м3), color="white", size=1.2,  position=position_stack())+
	geom_text(data=pВЫХОД, aes(x=Дата+0.45, y=1*м3),  lineheight=1, angle=0, , size=3,  hjust=0.5, position=
	
	position_stack(vjust = .45, ))+
	theme(  
		legend.position="none"
	)+
	
	scale_fill_gradient(low = "yellow", high = "orange", na.value = NA)+
	
	
	
	ggtitle(paste(pCaption, "вход и выход (в м3)"),
	subtitle=paste(
	 round(sum(filter(pВХОД, `Дата покидания склада`<=ДАТА_ОТЧЕТА)$м3)),"м3", "на", ДАТА_ОТЧЕТА)
	)
	#+
	#facet_wrap(facets=.~round(..x../7), ncol=1, scales="free_x")
	
	#aes(label=paste(round(м3)))
	
	return(list(G_ВЫХОД_ПО_ПОКУПАТЕЛЯМ = G_pВЫХОД_ПО_ПОКУПАТЕЛЯМ, G_ВХОД = G_pВХОД, G_ВходВыход = G_ВходВыход, G_Сечения <- G_pВЫХОД_ПО_СЕЧЕНИЯМ ))
}
