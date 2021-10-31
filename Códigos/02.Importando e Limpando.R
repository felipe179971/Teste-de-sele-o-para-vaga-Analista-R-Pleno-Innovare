#Importando os dados####
dados<- xlsx::read.xlsx("F:/Teste de seleção para vaga Analista R Pleno Innovare/Dados.xlsx",sheetIndex = 1)
label<- xlsx::read.xlsx("F:/Teste de seleção para vaga Analista R Pleno Innovare/Dados.xlsx",sheetIndex = 2)
#Tratando
dados_mod<-formatando_texto(dados)
label<-formatando_texto(label)
label$variavel<-str_sub(label$variavel,start = 4)
label$label<-str_sub(label$label,start = 4)



