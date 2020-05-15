###########  NÚMERO MÉDIO DE CANDIDATOS EFETIVOS
###########  1) POR ESTADO

# Define o ano do pleito. Para 2018, alterar o valor.
anoPleito <- "2018"

# Define nome dos arquivos de leitura, de acordo com o pleito
arqEstado <- paste ("TotalVotosDepFedPorEstado", ".csv", sep = anoPleito)
arqDeputado <- paste ("TotalVotosPorDepFed", ".csv", sep = anoPleito)

# Leitura com os arquivos com os dados de votação dos Estados e dos Deputados 
dfEstado <- read.csv(arqEstado, header=TRUE, sep=",")
dfDeputado <- read.csv(arqDeputado, header=TRUE, sep=",")

# Acrescentar uma coluna ao dataframe para armazenar o valor a ser calculado
dfEstado <- cbind(dfEstado, ValorRae = 0 )

# Cálculo do Rae percorrendo arquivo com todos os deputados com votos no Brasil
for(i in 1:nrow(dfDeputado)){
  UF <- dfDeputado[i,"UF"]
  
  # Total de votos do Deputado
  TotalVotos <- dfDeputado[i,"TotalVotos"] 
  
  # Rae no Estado = Rae no Estado +  quadrado do percentual de votos do deputado no total de votos válidos do estado
  dfEstado[which(dfEstado$uf==UF),3] <- dfEstado[which(dfEstado$uf==UF),3] + ((TotalVotos/dfEstado[which(dfEstado$uf==UF),2]) ^ 2)
}

# Cálculo do Rae invertido
dfEstado <- cbind(dfEstado, ValorRaeInvertido = 1/(dfEstado$ValorRae))


###########  1) POR MUNICÍPIO

# Define nome dos arquivos de leitura, de acordo com o pleito
arqMunicipio <- paste ("TbTotalVotosDepFedPorMun", ".csv", sep = anoPleito)
arqMunicipioUF <- paste ("TbMunicipiosUFSemLegenda", ".csv", sep = anoPleito)


# Leitura dos arquivos
dfMunicipio <- read.csv(arqMunicipio, header=TRUE, sep=",")
dfMunicipioUF <- read.csv(arqMunicipioUF, header=TRUE, sep=",")
dfMunicipioUF <- cbind(dfMunicipioUF, ValorRaeMun = 0 )

# Calculo do Rae em cada município, percorrendo dataframe com a votação de cada deputado por município
#número de linhas (i) é o total de deputados votados em cada município
for(i in 1:nrow(dfMunicipio)){
  
  # Recupera o código do município
  Municipio <- dfMunicipio[i,"Municipio"]
  
  # Recupera o total de votos do deputado naquele município
  TotalVotosDepNoMunicipio <- dfMunicipio[i,"TotalVotosPorMunicipio"]
  
  # Recupera o total de votos válido naquele município
  TotalVotosDoMunicipio <- dfMunicipioUF[which(dfMunicipioUF$CodMunTSE==Municipio),3]
  
  # Rae no município = Rae no município + quadrado do percentual de votos do município atribuídos àquele deputado
  dfMunicipioUF[which(dfMunicipioUF$CodMunTSE==Municipio),5] <- dfMunicipioUF[which(dfMunicipioUF$CodMunTSE==Municipio),5] + ((TotalVotosDepNoMunicipio/TotalVotosDoMunicipio) ^ 2)
}

# Rae invertido do Município
dfMunicipioUF <- cbind(dfMunicipioUF, ValorRaeMunInv = 1/(dfMunicipioUF$ValorRaeMun))

# Cria coluna para receber o total de municípios por estado
dfEstado <- cbind(dfEstado, TotalMunicipios = 0)

# Atribui o total de municípios a cada estado
for(i in 1:nrow(dfEstado)){
  dfEstado[i,5] = length(which(dfMunicipioUF$UF==dfEstado[i,1]))
}

# Cria coluna para receber a soma do Indice de Rae calculado para cada município
dfEstado <- cbind(dfEstado, TotalRaePorMunicipio = 0)


# Somar os valores de Rae por municipio do mesmo Estado
for(i in 1:nrow(dfMunicipioUF)){
  dfEstado[which(dfEstado$uf==dfMunicipioUF[i,4]),6] = dfEstado[which(dfEstado$uf==dfMunicipioUF[i,4]),6] + dfMunicipioUF[i,6]
}

# Cria coluna atribuindo a média dos valores de Rae municipal de cada estado
dfEstado <- cbind(dfEstado, CandidatosEfetivosPorMun = dfEstado$TotalRaePorMunicipio/dfEstado$TotalMunicipios)

# Imprime a média do Rae Municipal de cada estado em um arquivo
arqOutput <- paste ("Out_IndiceRaeporEstado", ".csv", sep = anoPleito)
write.csv2(dfEstado$CandidatosEfetivosPorMun , arqOutput, row.names = TRUE)

# Calcula o Rae medio dos municipios no Brasil
SomaRae <- 0
for(i in 1:nrow(dfMunicipioUF)){
  SomaRae <- SomaRae + dfMunicipioUF[i,6]
}
MediaBrasil <- SomaRae/i


# Cria quartis de acordo com os valores de dispersão usados por Nelson Carvalho
q1 <- 4.5
q2 <- 6.4
q3 <- 11.7

# Atribui a cada variavel o total de municípios dentro daquele quartil
conc_alto <- length(which(dfMunicipioUF$ValorRaeMunInv<=q1))
conc_media <- length(which(dfMunicipioUF$ValorRaeMunInv<=q2)) - length(which(dfMunicipioUF$ValorRaeMunInv<=q1))
disp_media <- length(which(dfMunicipioUF$ValorRaeMunInv<=q3)) - length(which(dfMunicipioUF$ValorRaeMunInv<=q2))
disp_alta <- length(which(dfMunicipioUF$ValorRaeMunInv>q3))


# CALCULA A CONCENTRAÇÃO E A DISPERSÃO ELEITORAL DOS MUNICÍPIOS EM CADA ESTADO

# Cria dataframe com a primeira coluna sendo a sigla de cada UF
dfConcentracaoMunicipalnoEstados <- dfEstado[1]

# Cria colunas que serão contadores do total de municípios de cada perfil em cada estado
dfConcentracaoMunicipalnoEstados <- cbind(dfConcentracaoMunicipalnoEstados, conc_alto = 0)
dfConcentracaoMunicipalnoEstados <- cbind(dfConcentracaoMunicipalnoEstados, conc_media = 0)
dfConcentracaoMunicipalnoEstados <- cbind(dfConcentracaoMunicipalnoEstados, disp_media = 0)
dfConcentracaoMunicipalnoEstados <- cbind(dfConcentracaoMunicipalnoEstados, disp_alta = 0)

# Percorre todos os estados
for(i in 1:nrow(dfConcentracaoMunicipalnoEstados)){
  # Sigla do estado
  UF <- dfConcentracaoMunicipalnoEstados[i,1]
  
  # Cria dataframe com todos os municipios do estado que está sendo calculado trazendo a informação de seus RaeInvertidos municipais calculados anteriormente
  tempEstado <- dfMunicipioUF[which(dfMunicipioUF$UF==UF),]
  
  # Atribui para cada faixa de concentração o total de municípios naquele estado específico
  dfConcentracaoMunicipalnoEstados$conc_alto[UF] <- length(which(tempEstado$ValorRaeMunInv<=q1))
  dfConcentracaoMunicipalnoEstados$conc_media[UF] <- length(which(tempEstado$ValorRaeMunInv<=q2)) - length(which(tempEstado$ValorRaeMunInv<=q1))
  dfConcentracaoMunicipalnoEstados$disp_media[UF] <- length(which(tempEstado$ValorRaeMunInv<=q3)) - length(which(tempEstado$ValorRaeMunInv<=q2))
  dfConcentracaoMunicipalnoEstados$disp_alta[UF] <- length(which(tempEstado$ValorRaeMunInv>q3))
}
# Escreve resultado num arquivo
arqOutput <- paste ("Out_ConcentracaoMunicipalnoEstados", ".csv", sep = anoPleito)
write.csv2(dfConcentracaoMunicipalnoEstados , arqOutput, row.names = TRUE)

# Cria coluna com classificação de cada cidade qto a concentração
dfMunicipioUF <- cbind(dfMunicipioUF, Concentracao = 0 )

# Percorre todas as cidades classificando
for(i in 1:nrow(dfMunicipioUF)){
  if (dfMunicipioUF[i,6] <= q1){
    dfMunicipioUF[i,7] = "CA"
  }else {
    if(dfMunicipioUF[i,6] <= q2){
      dfMunicipioUF[i,7] = "CM"
    }else {
      if (dfMunicipioUF[i,6] <= q3) {
        dfMunicipioUF[i,7] = "DM"
      } else {
        dfMunicipioUF[i,7] = "DA"
      }
    }
  }
}
# Escreve resultado em um arquivo
arqOutput <- paste ("TbMunicipiosDispersao", ".csv", sep = anoPleito)
write.csv2(dfMunicipioUF , arqOutput, row.names = TRUE)
