###########  C�LCULO DE DOMIN�NCIA E DISPERS�O

# Define o ano do pleito. Para 2018, alterar o valor.
anoPleito <- "2018"

# Define nome do arquivo de leitura, de acordo com o pleito
arqDominanciaDispersao <- paste ("CalculoDominanciaConcentracaoSemLegenda", ".csv", sep = anoPleito)

# Leitura do arquivo de entrada com as informa��es de vota��o de cada candidato por munic�pio
dfDominanciaDispersao <- read.csv(arqDominanciaDispersao, header=TRUE, sep=",")

# Cria dataframe que armazenar� dados do candidato
dfVotaveis <- data.frame(levels(dfDominanciaDispersao$Votavel))

# Acrescenta colunas para armazenar Dispers�o, Domin�ncia e Regi�o
dfVotaveis <- cbind(dfVotaveis, Dispersao = 0 )
dfVotaveis <- cbind(dfVotaveis, Dominancia = 0 )
dfVotaveis <- cbind(dfVotaveis, Regiao ="BR")

# Transforma em string
dfVotaveis$Regiao <- as.character(dfVotaveis$Regiao)

# Percorre arquivo de entrada para ler todos os registros
for(i in 1:nrow(dfDominanciaDispersao)){
  
  # Identifica o candidato naquela linha
  Votavel <- dfDominanciaDispersao[i,"Votavel"]
 
  # Armazena Total de votos do candidato no munic�pio, total de votos do candidato no pleito
  # e o total de votos para candidatos a deputado federal naquele munic�pio
  TotalVotosDoDepNoMunicipio <- dfDominanciaDispersao[i, "TotalVotosPorMunicipio"]
  TotalVotosDepFed <- dfDominanciaDispersao[i, "TotalVotosDepFed"]
  VotosValidosDepFedMunSemLegenda <- dfDominanciaDispersao[i, "TotalVotosDepFedNoMunicipioSemLegenda"]
  
  # Encontra o �ndice do candidato no dataframe que armazena dos dados dos candidatos
  index <- which(dfVotaveis$levels.dfDominanciaDispersao.Votavel. == Votavel)
  
  # Calcula a domin�ncia e a dispers�o do candidato naquele munic�pio
  Dominancia <- (TotalVotosDoDepNoMunicipio/ VotosValidosDepFedMunSemLegenda) * (TotalVotosDoDepNoMunicipio/TotalVotosDepFed)
  Dispersao <- (((TotalVotosDoDepNoMunicipio)^2)/((TotalVotosDepFed)^2))
  
  # Armazena no dataframe de informa��es dos candidatos a informa��o da itera��o i
  dfVotaveis[index, "Dispersao"] = dfVotaveis[index, "Dispersao"] + Dispersao 
  dfVotaveis[index, "Dominancia"] = dfVotaveis[index, "Dominancia"] + Dominancia 
  dfVotaveis[index, "Regiao"] = as.character(dfDominanciaDispersao[i,"Regiao"])

}

# Aplica o inverso da soma acumulada para cada dispers�o, conforme f�rmula exposta na metodologia do trabalho
for(i in 1:nrow(dfVotaveis)){
  dfVotaveis[i, "Dispersao"] = 1/dfVotaveis[i, "Dispersao"] 
}

# Retira deputados DF que est�o da posi��o 95 a 102 do arquivo de entrada
dfVotaveisCompleto <- dfVotaveis
linhas <- c(95,96,97,98,99,100,101,102)
dfVotaveis<- dfVotaveis[-linhas,]


# CLASSIFICACAO DOS DEPUTADOS
# Cria quartis de acordo com os valores de dispers�o usados por Nelson Carvalho
# Na dispers�o os valores s�o constantes
q1Disp <- 4
q2Disp <- 7
q3Disp <- 11

# Na domin�ncia, o c�lculo � feito a partir da m�dia e de um desvio padr�o para cada lado
q1Dom <- mean(dfVotaveis[,3]) - sd(dfVotaveis[,3])
q2Dom <- mean(dfVotaveis[,3]) 
q3Dom <- mean(dfVotaveis[,3]) + sd(dfVotaveis[,3])

# Cria coluna com classifica��o de cada cidade qto a concentra��o e outra pra domin�ncia
dfVotaveis <- cbind(dfVotaveis, Classif_Conc = 0 )
dfVotaveis$Classif_Conc <- as.character(dfVotaveis$Classif_Conc)
dfVotaveis <- cbind(dfVotaveis, Classif_Dom = 0)
dfVotaveis$Classif_Dom <- as.character(dfVotaveis$Classif_Dom)


# Percorre o dataframe com as informa��es dos deputados para classific�-los
# de acordo com valores de domin�ncia e dispers�o
# aCA Concentra��o Alta
# bCM Concentra��o Moderada
# cDM Dispers�o Moderada
# dDA Dispesrs�o Alta
for(i in 1:nrow(dfVotaveis)){
  # Dispers�o
  if (dfVotaveis[i,2] <= q1Disp){
    dfVotaveis[i,5] = "aCA"
  }else {
    if(dfVotaveis[i,2] <= q2Disp){
      dfVotaveis[i,5] = "bCM"
    }else {
      if (dfVotaveis[i,2] <= q3Disp) {
        dfVotaveis[i,5] = "cDM"
      } else {
        dfVotaveis[i,5] = "dDA"
      }
    }
  }
  
  # Dominancia
  # aSD Sem Domin�ncia
  # bBD Baixa Domin�ncia
  # cDM Domin�ncia M�dia
  # dDA Domin�ncia Alta
  if (dfVotaveis[i,3] <= q1Dom){
    dfVotaveis[i,6] = "aSD"
  }else {
    if(dfVotaveis[i,3] <= q2Dom){
      dfVotaveis[i,6] = "bBD"
    }else {
      if (dfVotaveis[i,3] <= q3Dom) {
        dfVotaveis[i,6] = "cDM"
      } else {
        dfVotaveis[i,6] = "dDA"
      }
    }
  }
}

# Escreve resultado num arquivo
arqDominanciaDispersao <- paste ("Out_DominanciaDispersao", ".csv", sep = anoPleito)
write.csv2(dfVotaveis , arqDominanciaDispersao, row.names = TRUE)
