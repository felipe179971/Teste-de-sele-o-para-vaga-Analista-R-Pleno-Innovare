##Tabelas####
#P009
Renda<-tabela_como_quero("P009",funcao_label("P009"))
#P0010
Grau<-tabela_como_quero("P010",funcao_label("P010"))
Grau[,1]<-c("Primeiras Séries do Ensino Fundamental \n (Primário – 1ª a 5ª Séries)",
            "Últimas Séries do Ensino Fundamental \n (Ginásio – 6ª a 9ª Séries)",
            "Ensino Médio \n (Colegial, Científico, Técnico)",
            "Ensino Superior",
            "Ensino Superior \n (Considerando Também Pós-Graduação)")
#P0012
Genero<-tabela_como_quero("P012",funcao_label("P012"))
#P013
Idade<-tabela_como_quero("P013",funcao_label("P013"))

##Gráficos de piza e barra sendo salvos
salvar_graficos_plotly_png(Renda,"Renda2")
salvar_graficos_plotly_png(Grau,"Escolaridade2")
salvar_graficos_plotly_png(Genero,"Gênero2")
salvar_graficos_plotly_png(Idade,"Idade2")

