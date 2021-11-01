#Proposta de gráfico para avaliar a variável por P021_1
#Função #porcentagem (T,F) #tipo ("group" ou "stack")
grafico_barra_var_por_P0021_1<-function(var,tipo,porcentagem){ 
  
  data<-frequencia_duas_cat(dados_mod,var,funcao_label(var),3)
  data<-data[-nrow(data),]  
  titlex<-c("Frequência")
  if(porcentagem==T){
    data[,2]<-as.numeric(data[,2])
    data[,4]<-as.numeric(data[,4])
    data[,3]<-data[,2]/(data$Concorrente+data$Principal)*100
    data[,5]<-data[,4]/(data$Concorrente+data$Principal)*100
    data[,2]<-data[,3]
    data[,4]<-data[,5]
    titlex<-c("(%)")
  }
plot_ly(data, y = data[,1], x = as.numeric(data[,2]), type = 'bar', name = 'Concorrente', marker = list(color = 'rgb(49,130,189)')
        ,orientation = 'h')%>% 
  add_trace(x = as.numeric(data[,4]), name = 'Principal', marker = list(color = 'rgb(204,204,204)'))%>% 
  layout(title = paste0(colnames(data)[1]," por Supermercado"),
         
    xaxis = list(title = titlex, tickangle = -45),
                      yaxis = list(title = ""),
                      margin = list(b = 100),
                      barmode = tipo)
}

#Rodando e Salvando
saveWidget(grafico_barra_var_por_P0021_1("P009","group",F), "temp.html")
webshot("temp.html",vwidth = 698,vheight = 373,
        "F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Gráficos/Extra/Proposta de Gráfico - Analisando var por P2021_1 (1).png"
)
saveWidget(grafico_barra_var_por_P0021_1("P009","stack",T), "temp.html")
webshot("temp.html",vwidth = 698,vheight = 373,
        "F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Gráficos/Extra/Proposta de Gráfico - Analisando var por P2021_1 (2).png"
)
#################################################################################
#################################################################################
#################################################################################
#Também sei fazer mapas
dados_mod$LATITUDE<-as.numeric(str_replace(dados$LATITUDE,",","."))
dados_mod$LONGITUDE<-as.numeric(str_replace(dados$LONGITUDE,",","."))
dados_mod
#VENDO RESPONDENTES POR REGIÃO (leaflet)
mapa<-leaflet(dados_mod) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)
saveWidget(mapa, "temp.html")
webshot("temp.html",vwidth = 698,vheight = 373,
        "F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Gráficos/Extra/Mapa_Resposta.png"
)

#VENDO LOCALIZAÇÃO POR SEXO (leaflet)
pal <- colorFactor(c("navy", "red"), domain = c("000Feminino", "000Masculino"))
mapa<-leaflet(dados_mod) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(P012 == "ship", 6, 10),
    color = ~pal(P012),
    stroke = FALSE, fillOpacity = 0.5
  )
saveWidget(mapa, "temp.html")
webshot("temp.html",vwidth = 698,vheight = 373,
        "F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Gráficos/Extra/Mapa_Resposta por Sexo.png"
)

#VENDO RESPONDENTES POR REGIÃO (Plotly)
dados_mod %>%
  plot_ly(
    lat = ~LATITUDE,
    lon = ~LONGITUDE,
    marker = list(color = "red"),
    type = 'scattermapbox',
    hovertext = ~dados_mod$id)%>%
  layout(
    mapbox = list(
      style = 'open-street-map',
      zoom =2.5,
      center = list(lon = -49, lat = -16)))

