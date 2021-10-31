#Frequência Simples (salvando como png)
a<-colnames(dados_mod)[-c(1,2,3)]
for (i in 1:length(a)) {
   frequencia_simples(dados_mod,a[i],funcao_label(a[i]),"",1)%>%
    save_kable(file = paste0("F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Tabelas/tabela_",a[i],"(1).png"))
  frequencia_simples(dados_mod,a[i],funcao_label(a[i]),"",2)%>%
    save_kable(file = paste0("F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Tabelas/tabela_",a[i],"(2).png"))
  
}
#Frequência por P021_1 (salvando como png)
a<-colnames(dados_mod)[-c(1,2,3,14)]
for (i in 1:length(a)) {
  frequencia_duas_cat(dados_mod,a[i],funcao_label(a[i]),1)%>%
    save_kable(file = paste0("F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Tabelas/tabela_",a[i],"P021_1(1).png"))
  frequencia_duas_cat(dados_mod,a[i],funcao_label(a[i]),2)%>%
    save_kable(file = paste0("F:/Teste de seleção para vaga Analista R Pleno Innovare/Teste-de-sele-o-para-vaga-Analista-R-Pleno-Innovare/Tabelas/tabela_",a[i],"P021_1(2).png"))
  
}

rm(a)
