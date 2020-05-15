###########  CÁLCULO DE DOMINÂNCIA E DISPERSÃO

# Define o ano do pleito. Para 2018, alterar o valor.
anoPleito <- "2018"

# Define nome do arquivo de leitura, de acordo com o pleito
arqDominanciaDispersao <- paste ("CalculoDominanciaConcentracaoSemLegenda", ".csv", sep = anoPleito)

# Leitura do arquivo de entrada com as informações de votação de cada candidato por município
dfDominanciaDispersao <- read.csv(arqDominanciaDispersao, header=TRUE, sep=",")

# Cria dataframe que armazenará dados do candidato
dfVotaveis <- data.frame(levels(dfDominanciaDispersao$Votavel))

# Acrescenta colunas para armazenar Dispersão, Dominância e Região
dfVotaveis <- cbind(dfVotaveis, Dispersao = 0 )
dfVotaveis <- cbind(dfVotaveis, Dominancia = 0 )
dfVotaveis <- cbind(dfVotaveis, Regiao ="BR")

# Transforma em string
dfVotaveis$Regiao <- as.character(dfVotaveis$Regiao)

# Percorre arquivo de entrada para ler todos os registros
for(i in 1:nrow(dfDominanciaDispersao)){
  
  # Identifica o candidato naquela linha
  Votavel <- dfDominanciaDispersao[i,"Votavel"]
 
  # Armazena Total de votos do candidato no município, total de votos do candidato no pleito
  # e o total de votos para candidatos a deputado federal naquele município
  TotalVotosDoDepNoMunicipio <- dfDominanciaDispersao[i, "TotalVotosPorMunicipio"]
  TotalVotosDepFed <- dfDominanciaDispersao[i, "TotalVotosDepFed"]
  VotosValidosDepFedMunSemLegenda <- dfDominanciaDispersao[i, "TotalVotosDepFedNoMunicipioSemLegenda"]
  
  # Encontra o índice do candidato no dataframe que armazena dos dados dos candidatos
  index <- which(dfVotaveis$levels.dfDominanciaDispersao.Votavel. == Votavel)
  
  # Calcula a dominância e a dispersão do candidato naquele município
  Dominancia <- (TotalVotosDoDepNoMunicipio/ VotosValidosDepFedMunSemLegenda) * (TotalVotosDoDepNoMunicipio/TotalVotosDepFed)
  Dispersao <- (((TotalVotosDoDepNoMunicipio)^2)/((TotalVotosDepFed)^2))
  
  # Armazena no dataframe de informações dos candidatos a informação da iteração i
  dfVotaveis[index, "Dispersao"] = dfVotaveis[index, "Dispersao"] + Dispersao 
  dfVotaveis[index, "Dominancia"] = dfVotaveis[index, "Dominancia"] + Dominancia 
  dfVotaveis[index, "Regiao"] = as.character(dfDominanciaDispersao[i,"Regiao"])

}

# Aplica o inverso da soma acumulada para cada dispersão, conforme fórmula exposta na metodologia do trabalho
for(i in 1:nrow(dfVotaveis)){
  dfVotaveis[i, "Dispersao"] = 1/dfVotaveis[i, "Dispersao"] 
}

# Retira deputados DF que estão da posição 95 a 102 do arquivo de entrada
dfVotaveisCompleto <- dfVotaveis
linhas <- c(95,96,97,98,99,100,101,102)
dfVotaveis<- dfVotaveis[-linhas,]


# CLASSIFICACAO DOS DEPUTADOS
# Cria quartis de acordo com os valores de dispersão usados por Nelson Carvalho
# Na dispersão os valores são constantes
q1Disp <- 4
q2Disp <- 7
q3Disp <- 11

# Na dominância, o cálculo é feito a partir da média e de um desvio padrão para cada lado
q1Dom <- mean(dfVotaveis[,3]) - sd(dfVotaveis[,3])
q2Dom <- mean(dfVotaveis[,3]) 
q3Dom <- mean(dfVotaveis[,3]) + sd(dfVotaveis[,3])

# Cria coluna com classificação de cada cidade qto a concentração e outra pra dominância
dfVotaveis <- cbind(dfVotaveis, Classif_Conc = 0 )
dfVotaveis$Classif_Conc <- as.character(dfVotaveis$Classif_Conc)
dfVotaveis <- cbind(dfVotaveis, Classif_Dom = 0)
dfVotaveis$Classif_Dom <- as.character(dfVotaveis$Classif_Dom)


# Percorre o dataframe com as informações dos deputados para classificá-los
# de acordo com valores de dominância e dispersão
# aCA Concentração Alta
# bCM Concentração Moderada
# cDM Dispersão Moderada
# dDA Dispesrsão Alta
for(i in 1:nrow(dfVotaveis)){
  # Dispersão
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
  # aSD Sem Dominância
  # bBD Baixa Dominância
  # cDM Dominância Média
  # dDA Dominância Alta
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
