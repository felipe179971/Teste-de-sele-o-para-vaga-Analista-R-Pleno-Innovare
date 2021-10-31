#Carregando os Pacotes####
library("xlsx")
library("dplyr")
library("ISwR")
library("stringr")
library("kableExtra")
library("formattable")
library("RcmdrMisc")
library("ggplot2")
library("ggforce")

packages<-c("xlsx","dplyr","ISwR","stringr","kableExtra","formattable",
            "RcmdrMisc","ggplot2","ggforce","leaflet")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


#Importando os dados####
dados<- xlsx::read.xlsx("F:/Teste de seleção para vaga Analista R Pleno Innovare/Dados.xlsx",sheetIndex = 1)
label<- xlsx::read.xlsx("F:/Teste de seleção para vaga Analista R Pleno Innovare/Dados.xlsx",sheetIndex = 2)
#Tornando apenas a primeira letra maiúscula####
#dados_mod<-data.frame(apply(dados, 2,str_to_title))
formatando_texto<-function(data){
  data<-data.frame(apply(data, 2,str_to_title))
  
  for(i in 1:length(data)){
    data[[i]]<-data[[i]]%>%
      str_replace("^ ","")%>%#começa com espaço
      str_replace("/ ","/")%>%
      str_replace(" Em "," em ")%>%
      str_replace(" A "," a ")%>%
      str_replace(" E "," e ")%>%
      str_replace(" Do "," do ")%>%
      str_replace(" Dos "," dos ")%>%
      str_replace(" Da "," da ")%>%
      str_replace(" Das "," das ")%>%
      str_replace(" De "," de ")%>%
      str_replace(" Que "," que ")%>%
      str_replace(" Por "," por ")%>%
      str_replace(" No "," no ")%>%
      str_replace(" São "," são ")%>%
      str_replace(" Mais "," mais ")%>%
      str_replace(" Os "," os ")%>%
      str_replace(" Um "," um ")%>%
      str_replace(" Ou "," ou ")%>%
      str_replace(" Sem "," sem ")%>%
      str_replace(" Tem "," tem ")%>%
      str_replace(" Ser "," ser ")%>%
      str_replace(" Com "," com ")%>%
      str_replace(" No "," no ")%>%
      str_replace(" Ao "," ao ")%>%
      str_replace(" Nos "," nos ")%>%
      str_replace(" Na "," na ")%>%
      str_replace(" Nas "," nas ")%>%
      str_replace(" A "," a ")%>%
      str_replace(" Nesta "," nesta ")%>%
      str_replace("Cientifico","Científico")%>%
      str_replace("Tecnico","Técnico")%>%
      str_replace(" Anos a "," a ")%>%
      str_replace("Cartao","Cartão")%>%
      str_replace("Credito","Crédito")%>%
      str_replace("Debito","Débito")%>%
      str_replace("a Vista","à Vista")%>%
      str_replace("Créditoto","Crédito")%>%
      str_replace("Alimentacao","Alimentação")%>%
      str_replace("(As)","(as)")%>%
      str_replace("Pre-Datado","Pré-Datado")%>%
      str_replace("Tiquete","Tíquete")%>%
      str_replace("^$","NA")%>%
      str_replace("^Nr$","NA")%>%
      str_replace("^Ns$","NA")%>%
      str_replace("^Nsa$","NA")%>%
      
      str_replace("^<NA>$","NA")
    na_if(data[[i]], "NA")
    #Colocando 000 no começo (para quando quero fixar ou não a ordem)
    data[[i]]<-paste0("000",data[[i]])
    data[[i]]<- data[[i]]%>%str_replace("^000NA$","99-NA")
    #Arrumando casos especiais e ordenando, caso acho adequado
    if(i==4){
      data[[i]]<-ifelse(data[[i]]=="000Até um 1 Sm","01-Até um SM",
                        ifelse(data[[i]]=="000Mais de 1 a 2 Sm"|data[[i]]=="000Mais de R$ 1.045,00 a R$ 2.090,00","02-Mais de 1 a 2 SM",
                               ifelse(data[[i]]=="000Mais de 2 a 5 Sm"|data[[i]]=="000Mais de R$ 2.090,00 a R$ 5.225,00","03-Mais de 2 a 5 SM",
                                      ifelse(data[[i]]=="000Mais de 5 a 10 Sm"|data[[i]]=="000Mais de R$ 5.225,00 a R$ 10.450,00","04-Mais de 5 a 10 SM",
                                             ifelse(data[[i]]=="000Mais de 10 a 20 Sm"|data[[i]]=="000Mais de R$ 10.450,00 a R$ 20.900,00","05-Mais de 10 a 20 SM",
                                                    ifelse(data[[i]]=="000Mais de 20 Sm"|data[[i]]=="000Mais de R$ 20.900,00","06-Mais de 20 SM",
                                                           data[[i]]
                                                    ))))))
      data[[i]]<-data[[i]]%>%str_replace(" SM"," Salário Mínimo")
    }
    if(i==5){
      data[[i]]<-ifelse(data[[i]]=="000Primeiras Séries do Ensino Fundamental (Primário – 1ª a 5ª Séries)"|data[[i]]=="000Primeiras Séries do Fund.","01-Primeiras Séries do Ensino Fundamental (Primário – 1ª a 5ª Séries)",
                        ifelse(data[[i]]=="000Últimas Séries do Ensino Fundamental (Ginásio – 6ª a 9ª Séries)"|data[[i]]=="000Últimas Séries do Fund.","02-Últimas Séries do Ensino Fundamental (Ginásio – 6ª a 9ª Séries)",
                               ifelse(data[[i]]=="000Ensino Médio (Colegial, Científico, Técnico)"|data[[i]]=="000Médio","03-Ensino Médio (Colegial, Científico, Técnico)",
                                      ifelse(data[[i]]=="000Superior","04-Ensino Superior",
                                             ifelse(data[[i]]=="000Superior (Considerar Também Pós-Graduação)","05-Ensino Superior (Considerando Também Pós-Graduação)",
                                                    data[[i]]
                                             )))))
    }
    if(i==6){
      data[[i]]<-data[[i]]%>%
        str_replace("000Em Ambos, Supermercados Atacarejo/Atacadão","000Em Ambos: Supermercados e Atacarejo/Atacadão")%>%
        str_replace("^000Em Ambos$","000Em Ambos: Supermercados e Atacarejo/Atacadão")%>%
        str_replace("000Apenas Supermercados","000Apenas em Supermercados")
    }
    #if(i %in% c(1,2,3,9,15)){
    #   data[[i]]<-str_sub(data[[i]], start = 4)
    #}
    data[[i]]<-data[[i]]%>%
      str_replace("000Não Faz","01-Não Faz")%>%
      str_replace("000Às Vezes","02-Às Vezes")%>%
      str_replace("000Sempre","03-Sempre")%>%
      str_replace("000Sempre","03-Sempre")
    if(i==11){
      data[[i]]<-data[[i]]%>%
        str_replace("000Uma Vez a Cada 10 Dias","01-Uma Vez a Cada 10 Dias")%>%
        str_replace("000Uma Vez a Cada 15 Dias","02-Uma Vez a Cada 15 Dias")%>%
        str_replace("000Uma Vez Por Mês","03-Uma Vez Por Mês")%>%
        str_replace("000Uma Vez Por Semana","04-Uma Vez Por Semana")%>%
        str_replace("000Todos os Dias","09-Todos os Dias")%>%
        str_replace("000Outra Frequência","10-Outra Frequência")
      data[[i]]<-ifelse(data[[i]]=="000No Mínimo Uma Vez Por Semana (Pelo Menos Uma Vez Por Semana)","05-No Mínimo Uma Vez Por Semana (Pelo Menos Uma Vez Por Semana)",
                        ifelse(data[[i]]=="000No Mínimo Uma Vez a Cada 10 Dias (Pelo Menos Uma Vez A Cada 10 Dias)","06-No Mínimo Uma Vez a Cada 10 Dias (Pelo Menos Uma Vez A Cada 10 Dias)",
                               ifelse(data[[i]]=="000No Mínimo Uma Vez a Cada 15 Dias (Pelo Menos Uma Vez A Cada 15 Dias) Ou","07-No Mínimo Uma Vez a Cada 15 Dias (Pelo Menos Uma Vez A Cada 15 Dias)",
                                      ifelse(data[[i]]=="000No Mínimo Uma Vez Por Mês (Pelo Menos Uma Vez Por Mês)","08-No Mínimo Uma Vez Por Mês (Pelo Menos Uma Vez Por Mês)",
                                             data[[i]]
                                      ))))
    }
    if(i==12){
      data[[i]]<-data[[i]]%>%
        str_replace("000Um","01-Um")%>%
        str_replace("000Dois","02-Dois")%>%
        str_replace("000Três","03-Três")%>%
        str_replace("000Quatro","04-Quatro")%>%
        str_replace("000Mais de Quatro","05-Mais de Quatro")
    }
    if(i %in% c(17,21)){
      data[[i]]<-data[[i]]%>%
        str_replace("000À Pé","01-À Pé")%>%
        str_replace("000Ônibus","02-Ônibus")%>%
        str_replace("000Táxi","03-Táxi")%>%
        str_replace("000Automóvel","04-Automóvel")%>%
        str_replace("000Serviços de Entrega","05-Serviços de Entrega")%>%
        str_replace("000Outro","06-Outro")
    }
    if(i %in% c(18,22)){
      data[[i]]<-data[[i]]%>%
        str_replace("Super[.]","Supermercado")%>%
        str_replace("Supermerc[.]","Supermercado")%>%
        str_replace("Cred[.]","Crédito")%>%
        str_replace("Créd[.]","Crédito")%>%
        str_replace("Cartão Crédito","Cartão de Crédito")%>%
        str_replace("^000Com ","000")%>%
        str_replace("000Outro","05-Outro")%>%
        str_replace("000","002")%>%
        str_replace("^000","02-")%>%
        str_replace("002Dinheiro","01-Dinheiro")%>%
        str_replace("^002","03-")
    }
    data[[i]]<-data[[i]]%>%
      str_replace("^000Péssimo$","01-Péssimo")%>%   
      str_replace("^000Ruim$","02-Ruim")%>%
      str_replace("^000Regular$","03-Regular")%>%
      str_replace("^000Bom$","04-Bom")%>%
      str_replace("^000Ótimo$","05-Ótimo")
    if(i==46){
      data[[i]]<-data[[i]]%>%
        str_replace("000Com Certeza Não Comprará nesta Loja","01-Com Certeza Não Comprará nesta Loja")%>%
        str_replace("000É mais Certo que Não Compre nesta Loja","02-É mais Certo que Não Compre nesta Loja")%>%
        str_replace("000Depende","03-Depende")%>%
        str_replace("000É mais Certo que Compre nesta Loja","04-É mais Certo que Compre nesta Loja")%>%
        str_replace("000Com Certeza Comprará nesta Loja","05-Com Certeza Comprará nesta Loja")
    }
    if(i==48){
      data[[i]]<-data[[i]]%>%
        str_replace("000Não Sabe","01-Não Sabe")%>%
        str_replace("000Supermercado","02-Supermercado")%>%
        str_replace("000Atacarejo","03-Atacarejo")%>%
        str_replace("000Depende","04-Depende")%>%
        str_replace("000Não Sabe","05-Não Sabe")
    }
  }
  data
}
dados_mod<-formatando_texto(dados)
label<-formatando_texto(label)
label$variavel<-str_sub(label$variavel,start = 4)
label$label<-str_sub(label$label,start = 4)


##Funções####
#Tabela de Frequência Simples
#Personalizando a tabela
tabela<-function(data,simples,subcapion){
  if(simples==F){
    #removendo o total para fazer os gráficos
    auxiliar<-data[-nrow(data),]
    alinhar<-c()
    i<-2
    while (i<=ncol(auxiliar)-1) {
      auxiliar[,i]<-color_bar("lightgreen")(data[-nrow(data),i])
      alinhar[i]<-c("r")
      i<-i+1
      auxiliar[,i]<- color_tile("white", "orange")(data[-nrow(data),i])
      alinhar[i]<-c("c")
      i<-i+1
    }
    alinhar[1]<-c("l")
    alinhar<-paste0(alinhar,collapse="")
    titulo<-c(paste0("Tabela com Gráficos",subcapion))
    
    data<-rbind(auxiliar,data[nrow(data),])
  }else{
    data<-data
    alinhar<-paste0("l",paste0(rep("c",ncol(data)-1),collapse=""))
    titulo<-c(paste0("Tabela Simples",subcapion))
  }
  tabela<-kbl(data,escape = F,align=alinhar,caption = titulo) %>%
    kable_classic_2(full_width = F) %>%
    column_spec(2, width = "2cm")%>%
    column_spec(1, bold = F,border_right=T)%>%
    row_spec(0,bold = T,background=c("#D1D1D1"),align = "c")%>%
    row_spec(nrow(data),bold = T,align="c",hline_after=T,background=c("#D1D1D1"))
  if(colnames(data)[1]=="Renda familiar mensal (%)"){
    tabela<-tabela%>%
      footnote(general = "Considerando o salário mínimo de 2019 (R$1.045)."
      )
    tabela
  }else{
    tabela
  }
}
#####
frequencia_simples<-function(data,variavel,nome_variavel,subcapion){
  #Frequencia
  a<-data.frame(table(data[[variavel]]))
  #Removendo o prefixo das ordens
  a[,1]<-str_sub(a[,1], start = 4)
  #Colocando as porcentagens
  a<-cbind(a,round(data.frame(prop.table(table(data[[variavel]])))[,2],5)*100)
  #Nomeando as colunas
  colnames(a)<-c(nome_variavel,"Frequência","(%)")
  #Total
  a<-rbind(a,c("TOTAL",round(colSums(a[,c(2:3)]))))
  #tabela personalizada
  print(tabela(a,T,subcapion))
  print(tabela(a,F,subcapion))
  #imprimindo a tabela simples
  print(a)
}
#Função que pega o label da variável
funcao_label<-function(quero){
  as.character(label%>%filter(variavel==quero))[2]
}
#Tabela de Frequência Duas variáveis
tabela2<-function(data,simples){
  nomes<-colnames(data)[-1]
  i<-2
  while(i<=ncol(data)){
    colnames(data)[i]<-c("Frequência")
    i<-i+1
    colnames(data)[i]<-c("(%)")
    i<-i+1
  }
  #Tabela personalizada para P021_1 devido ao "add_header_above"
  tabela(data,simples,": Supermercado principal (%)")%>%
    add_header_above(c("-" = 1, "Concorrente" = 2, "Principal" = 2),bold = T,
                     background=c("#D1D1D1"),align = "c")
}
frequencia_duas_cat<-function(data,variavel1,nome_variavel1){
  #Farei apenas para P021_1 por causa da tabela, mas essa função faz livre
  variavel2<-c("P021_1")
  nome_variavel2<-c("Supermercado principal (%)")
  #Frequência
  a<-as.table(ftable(cbind(data[variavel1],data[variavel2])))
  #(%) Por coluna
  percents_col<-colPercents(a,digits=1)[-c(nrow(a)+1,nrow(a)+2),]
  #Arrumando do jeito que quero imprimir
  b<-matrix(nrow = nrow(a),ncol =ncol(a)*2+1)
  i<-2;j<-1
  while(i<=ncol(a)*2+1){
    b[,i]<-a[,j]
    i<-i+1
    b[,i]<-percents_col[,j]
    i<-i+1;j<-j+1
  }
  b<-data.frame(apply(b, 2,as.numeric))
  
  #Colocando a primeira coluna com os nomes
  b[,1]<-str_sub(row.names(a), start = 4)
  #Nome das colunas
  colnames(b)[1]<-nome_variavel1
  i<-2;j<-1
  while(i<=ncol(a)*2+1){
    colnames(b)[i]<-str_sub(colnames(a)[j],start=4)
    i<-i+1
    colnames(b)[i]<-paste0(str_sub(colnames(a)[j],start=4),"(%)")
    i<-i+1;j<-j+1
  }
  #Totais
  b<-rbind(b,c("TOTAL",round(colSums(b[,-1]))))
  rm(a,i,j,percents_col)
  #tabela personalizada
  print(tabela2(b,T))
  print(tabela2(b,F))
  #imprimindo a tabela simples
  print(b)
} 
#Formatando a tabela para os gráficos de perfil####
tabela_como_quero<-function(var,nome){
  a<-frequencia_simples(dados_mod,var,funcao_label(var),"")
  a<-a[-nrow(a),]
  a$Frequência<-as.numeric(a$Frequência)
  a
}
Renda<-tabela_como_quero("P009")
Grau<-tabela_como_quero("P010")
Genero<-tabela_como_quero("P012")
Idade<-tabela_como_quero("P013")
#########################################################
dados$P015<-as.numeric(dados$P015)
dados$P021_1<-as.factor(dados$P021_1)
#Média de P015 
#Verificando dois possíveis Outliers 
#[ver com quem coletou vara verificar se digitou certo e etc]
#[por hora, irei remover]
ggplot(dados, aes(x=P015, y=id)) + 
  geom_point()
#Verificando de novo
#[gastar 15.00 é plausível, manterei]
#[mas sempre bom verificar com a fonte da base]
#Médias
medias<-dados%>%filter(P015<3e+06)%>%
  group_by(P021_1)%>%
  summarise(`Média`=round(mean(P015),1))
medias$P021_1<-as.character(medias$P021_1)

media<-dados%>%filter(P015<3e+06)%>%
  summarise(x=mean(P015))
media<-as.character(round(media,2))

media<-rbind(medias,c("Geral",media));rm(medias)
colnames(media)[1]<-c("")  
#Box-Plot
dados%>%filter(P015<3e+06)%>%
  ggplot( aes(x=P021_1, y=P015, fill=P021_1)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#69b3a2", "grey")) +
  scale_alpha_manual(values=c(1,0.1)) +
  theme(legend.position = "none") +
  xlab("Supermercado principal (%)")+
  ylab("Gasto total mensal em supermercados (%)")+
  facet_zoom(ylim = c(0, 1500),zoom.size=2)

dados%>%filter(P015<3e+06)%>%ggboxplot(x = "P021_1", y = "P015",
                                       color = "P021_1", palette =c("#00AFBB", "#E7B800", "#FC4E07"),
                                       add = "jitter", shape = "P021_1")

#########################################################
tabela_como_quero<-function(var,nome){
  a<-frequencia_simples(dados_mod,var,funcao_label(var),"")
  a<-a[-nrow(a),]
  #colnames(a)[1]<-c("var")
  #a$condition<-nome
  a$Frequência<-as.numeric(a$Frequência)
  a
}
Renda<-tabela_como_quero("P009","Renda")
Grau<-tabela_como_quero("P010","Escolaridade")
Grau[,1]<-c("Primeiras Séries do Ensino Fundamental \n (Primário – 1ª a 5ª Séries)",
            "Últimas Séries do Ensino Fundamental \n (Ginásio – 6ª a 9ª Séries)",
            "Ensino Médio \n (Colegial, Científico, Técnico)",
            "Ensino Superior",
            "Ensino Superior \n (Considerando Também Pós-Graduação)")
Genero<-tabela_como_quero("P012","Gênero")
Idade<-tabela_como_quero("P013","Idade")
#P009
##Gráfico de barra

Grau %>%
  arrange(desc(`Frequência`))

grafico_barra<-function(data){
  legenda<-paste0(data$Frequência," (",round(as.numeric(data$`(%)`),2),"%)")
  data%>%
    ggplot(aes(x=`Frequência`, y=reorder(data[,1], -`Frequência`),label=legenda)) +
    geom_bar(stat="identity", fill="steelblue")+
    #geom_text(aes(label=legenda),hjust=0, vjust=-0, size=3.5)+
    theme_classic()+
    xlim(0, max(data$Frequência)+100)+
    ggtitle(colnames(data)[1]) +
    xlab("Frequência") + ylab("")+
    theme(axis.title.x = element_text(color = "#C0C0C0", size = 12, face = "bold"),
          axis.text.y = element_text(colour = "#373737",face = "bold",size=12),
          plot.title = element_text(hjust = 0.5,colour = "black",face = "bold",size=20))+
    geom_text(hjust = 0, nudge_x = 0.05,fontface = "bold",colour ="#7C7979")
  
}
grafico_barra(Idade)
###EXTRAS
#MAPA
dados_mod$LATITUDE<-as.numeric(str_replace(dados$LATITUDE,",","."))
dados_mod$LONGITUDE<-as.numeric(str_replace(dados$LONGITUDE,",","."))
dados_mod
#VENDO RESPONDENTES POR REGIÃO
leaflet(dados_mod) %>% addTiles() %>% addMarkers(
  clusterOptions = markerClusterOptions()
)

pal <- colorFactor(c("navy", "red"), domain = c("000Feminino", "000Masculino"))
#VENDO LOCALIZAÇÃO POR SEXO
leaflet(dados_mod) %>% addTiles() %>%
  addCircleMarkers(
    radius = ~ifelse(P012 == "ship", 6, 10),
    color = ~pal(P012),
    stroke = FALSE, fillOpacity = 0.5
  )


salvar_graficos_png<-function(data,nome){
  png(paste0(file="F:/Teste de seleção para vaga Analista R Pleno Innovare/Gráficos/",nome,".png"),
      width = 966, height = 343)
  print(grafico_barra(data))
  dev.off()
}

salvar_graficos_png(Idade,"grafico_Idade")
###########################
#Pie bar####
library(plotly)

USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

fig <- plot_ly(data, labels = ~Categorie, values = ~X1960, type = 'pie')
fig <- fig %>% layout(title = 'United States Personal Expenditures by Categories in 1960',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig
data<-Renda

plot_ly(data, labels = ~data[,1], values = ~`(%)`, type = 'pie',
        marker = list(colors = c('rgb(211,94,96)',"rgb(211,94,96)", 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)'),
                      line = list(color = '#FFFFFF', width = 1))) %>% 
  layout(title = colnames(data)[1],
  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



plotly_IMAGE(p, width = 500, height = 500, format = "png", scale = 2,
             out_file = "F:/Documentos/test.png")



grafico_barra<-function(data){
  legenda<-paste0(data$Frequência," (",round(as.numeric(data$`(%)`),2),"%)")
  data%>%
    ggplot(aes(x=`Frequência`, y=reorder(data[,1], -`Frequência`),label=legenda)) +
    geom_bar(stat="identity", fill="steelblue")+
    #geom_text(aes(label=legenda),hjust=0, vjust=-0, size=3.5)+
    theme_classic()+
    xlim(0, max(data$Frequência)+100)+
    ggtitle(colnames(data)[1]) +
    xlab("Frequência") + ylab("")+
    theme(axis.title.x = element_text(color = "#C0C0C0", size = 12, face = "bold"),
          axis.text.y = element_text(colour = "#373737",face = "bold",size=12),
          plot.title = element_text(hjust = 0.5,colour = "black",face = "bold",size=20))+
    geom_text(hjust = 0, nudge_x = 0.05,fontface = "bold",colour ="#7C7979")
  
}