dados$P015<-as.numeric(dados$P015)
dados$P021_1<-as.factor(dados$P021_1)
#Verificando dois possíveis Outliers 
#[ver com quem coletou para verificar se digitou certo e etc]
#[por hora, irei remover. Deixei R$15.000 por ser plausível]
ggplot(dados, aes(x=P015, y=id)) + 
  geom_point()

#Box-Plot salvo na pasta (Gosto mais do plotly, mas esse zoom é uma função muito boa para gráfico estático)

png(file="F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Gráficos/P015 por P021_1/ggplot.png",
    width = 698, height = 373)
dados%>%filter(P015<3e+06)%>%
  ggplot( aes(x=P021_1, y=P015, fill=P021_1)) + 
  geom_boxplot() +
  scale_fill_manual(values=c("#69b3a2", "grey")) +
  scale_alpha_manual(values=c(1,0.1)) +
  theme(legend.position = "none") +
  xlab("Supermercado principal (%)")+
  ylab("Gasto total mensal em supermercados (%)")+
  facet_zoom(ylim = c(0, 1500),zoom.size=2)
dev.off()

#Plotly (gráfico que mais gosto)
Concorrente<-dados%>%filter(P015<3e+06)%>%filter(P021_1=="Concorrente")%>%select(P015)
Principal<-dados%>%filter(P015<3e+06)%>%filter(P021_1=="Principal")%>%select(P015)
Geral<-dados%>%filter(P015<3e+06)%>%select(P015)

a<-plot_ly(y = Geral$P015, type = "box", quartilemethod="linear", name="Geral")%>%
  add_trace(y = Principal$P015, quartilemethod="inclusive", name="Principal")%>%
  add_trace(y = Concorrente$P015, quartilemethod="exclusive", name="Concorrente")%>% 
  layout(title = "Comparando a Média da P015 por P021_1")
#Salvando na pasta

saveWidget(a, "temp.html")
webshot("temp.html",vwidth = 698,vheight = 373,
        "F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Gráficos/P015 por P021_1/plotly.png"
)
