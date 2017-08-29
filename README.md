# PadroesFrequentesMalaria
Esta pasta contém fontes R que utiliza a técnica de padrões frequentes sobre a base de malária
load("basemalaria.RData")

# Duplica o dataset com filtro para pegar s񟳵plas positivas de Malária (RES_EXAM=1 significa exame Negativo)
malariaNovo <- subset(malaria, malaria$RES_EXAM != 1)

# Variável: X
# Transformação: Removida por não ser necessária
malariaNovo$X <- NULL

# Variável: COD_OCUP -> ocupacao
# Transformação: De códigos para descrições
malariaNovo$ocupacao <- ifelse(malariaNovo$COD_OCUP == 1, 'Agricultura',
                        ifelse(malariaNovo$COD_OCUP == 2, 'Pecuária',
                        ifelse(malariaNovo$COD_OCUP == 3, 'Doméstica',
                        ifelse(malariaNovo$COD_OCUP == 4, 'Turismo',
                        ifelse(malariaNovo$COD_OCUP == 5, 'Garimpagem',
                        ifelse(malariaNovo$COD_OCUP == 6, 'Exploração Veg.',
                        ifelse(malariaNovo$COD_OCUP == 7, 'Caça/Pesca',
                        ifelse(malariaNovo$COD_OCUP == 8, 'Constr. Estradas',
                        ifelse(malariaNovo$COD_OCUP == 9, 'Mineração',
                        ifelse(malariaNovo$COD_OCUP == 10, 'Viajante',
                        ifelse(malariaNovo$COD_OCUP == 11, 'Outros',
                        ifelse(malariaNovo$COD_OCUP == 99, 'Ignorado', NA))))))))))))
malariaNovo$ocupacao <- as.factor(malariaNovo$ocupacao)
malariaNovo$COD_OCUP <- NULL

# Variável: DT_EXAME -> mes.exame, ano.exame
# Transformação: Decomposição temporal em mês e ano
malariaNovo$mes.exame <- substr(malariaNovo$DT_EXAME, 6, 7)
malariaNovo$ano.exame <- substr(malariaNovo$DT_EXAME, 1, 4)
malariaNovo$mes.exame <- as.factor(malariaNovo$mes.exame)
malariaNovo$ano.exame <- as.factor(malariaNovo$ano.exame)
malariaNovo$DT_EXAME <- NULL

# Variável: DT_NASCI -> mes.nascimento, ano.nascimento
# Transformação: Decomposição temporal em mês e ano
malariaNovo$mes.nascimento <- substr(malariaNovo$DT_NASCI, 6, 7)
malariaNovo$ano.nascimento <- substr(malariaNovo$DT_NASCI, 1, 4)
malariaNovo$mes.nascimento <- as.factor(malariaNovo$mes.nascimento)
malariaNovo$ano.nascimento <- as.integer(malariaNovo$ano.nascimento)
# Remoção dos outliers
malariaNovo$ano.nascimento[malariaNovo$ano.nascimento %in% boxplot(malariaNovo$ano.nascimento, range = 3)$out] <- NA
malariaNovo$ano.nascimento <- as.factor(malariaNovo$ano.nascimento)
malariaNovo$DT_NASCI <- NULL

# Variável: DT_NOTIF -> mes.notificacao, ano.notificacao
# Transformação: Decomposição temporal em mês e ano
malariaNovo$mes.notificacao <- substr(malariaNovo$DT_NOTIF, 6, 7)
malariaNovo$ano.notificacao <- substr(malariaNovo$DT_NOTIF, 1, 4)
malariaNovo$mes.notificacao <- as.factor(malariaNovo$mes.notificacao)
malariaNovo$ano.notificacao <- as.factor(malariaNovo$ano.notificacao)
malariaNovo$DT_NOTIF <- NULL

# Variável: DT_SINTO -> mes.sintomas, ano.sintomas
# Transformação: Decomposição temporal em mês e ano
malariaNovo$mes.sintomas <- substr(malariaNovo$DT_SINTO, 6, 7)
malariaNovo$ano.sintomas <- substr(malariaNovo$DT_SINTO, 1, 4)
malariaNovo$mes.sintomas <- as.factor(malariaNovo$mes.sintomas)
# Remoção dos outliers
malariaNovo$ano.sintomas[malariaNovo$ano.sintomas %in% boxplot(malariaNovo$ano.sintomas, range = 10)$out] <- NA
malariaNovo$ano.sintomas <- as.factor(malariaNovo$ano.sintomas)
malariaNovo$DT_SINTO <- NULL

# Variável: DT_TRATA -> mes.tratamento, ano.tratamento
# Transformação: Decomposição temporal em mês e ano
malariaNovo$mes.tratamento <- substr(malariaNovo$DT_TRATA, 6, 7)
malariaNovo$ano.tratamento <- substr(malariaNovo$DT_TRATA, 1, 4)
malariaNovo$mes.tratamento <- as.factor(malariaNovo$mes.tratamento)
# Remoção dos outliers
malariaNovo$ano.tratamento[malariaNovo$ano.tratamento %in% boxplot(malariaNovo$ano.tratamento, range = 2)$out] <- NA
malariaNovo$ano.tratamento <- as.factor(malariaNovo$ano.tratamento)
malariaNovo$DT_TRATA <- NULL

# Variável: EXAME. -> tipo.exame
# Transformação: De códigos para descrições
malariaNovo$tipo.exame <- ifelse(malariaNovo$EXAME. == 1, 'Gota espessa/Esfregaço',
                          ifelse(malariaNovo$EXAME. == 2, 'Teste rápido', NA))
malariaNovo$tipo.exame <- as.factor(malariaNovo$tipo.exame)
malariaNovo$EXAME. <- NULL

# Variável: FALCIPARUM -> tratamento.falciparum
# Transformação: De códigos para descrições
malariaNovo$tratamento.falciparum <- ifelse(malariaNovo$FALCIPARUM == 1, 'Sim',
                                     ifelse(malariaNovo$FALCIPARUM == 2, 'Não', NA))
malariaNovo$tratamento.falciparum <- as.factor(malariaNovo$tratamento.falciparum)
malariaNovo$FALCIPARUM <- NULL

# Variável: HEMOPARASI -> resultado.hemoparasitas
# Transformação: De códigos para descrições
malariaNovo$resultado.hemoparasitas <- ifelse(malariaNovo$HEMOPARASI == 1, 'Negativo',
									   ifelse(malariaNovo$HEMOPARASI == 2, 'Trypanosoma sp.',
									   ifelse(malariaNovo$HEMOPARASI == 3, 'Microfilária',
									   ifelse(malariaNovo$HEMOPARASI == 4, 'Trypanosoma sp. + Microfilária',
                                       ifelse(malariaNovo$HEMOPARASI == 9, 'Não pesquisados', NA)))))
malariaNovo$resultado.hemoparasitas <- as.factor(malariaNovo$resultado.hemoparasitas)
malariaNovo$HEMOPARASI <- NULL

# Variável: LOC_INFE -> local.infeccao
# Transformação: Discretização apenas
malariaNovo$local.infeccao <- as.character(malariaNovo$LOC_INFE)
malariaNovo$local.infeccao <- as.factor(malariaNovo$local.infeccao)
malariaNovo$LOC_INFE <- NULL

# Variável: LOC_RESI -> local.residencia
# Transformação: Discretização apenas
malariaNovo$local.residencia <- as.character(malariaNovo$LOC_RESI)
malariaNovo$local.residencia <- as.factor(malariaNovo$local.residencia)
malariaNovo$LOC_RESI <- NULL

# Variável: MUN_INFE -> municipio.infeccao
# Transformação: Discretização apenas
malariaNovo$municipio.infeccao <- as.character(malariaNovo$MUN_INFE)
malariaNovo$municipio.infeccao <- as.factor(malariaNovo$municipio.infeccao)
malariaNovo$MUN_INFE <- NULL

# Variável: MUN_NOTI -> municipio.notificacao
# Transformação: Discretização apenas
malariaNovo$municipio.notificacao <- as.character(malariaNovo$MUN_NOTI)
malariaNovo$municipio.notificacao <- as.factor(malariaNovo$municipio.notificacao)
malariaNovo$MUN_NOTI <- NULL

# Variável: MUN_RESI -> municipio.residencia
# Transformação: Discretização apenas
malariaNovo$municipio.residencia <- as.character(malariaNovo$MUN_RESI)
malariaNovo$municipio.residencia <- as.factor(malariaNovo$municipio.residencia)
malariaNovo$MUN_RESI <- NULL

# Variável: NIV_ESCO -> escolaridade
# Transformação: De códigos para descrições
malariaNovo$escolaridade <- ifelse(malariaNovo$NIV_ESCO == 0, 'Analfabeto',
                            ifelse(malariaNovo$NIV_ESCO == 1, '1ª a 4ª série incompleta do EF',
                            ifelse(malariaNovo$NIV_ESCO == 2, '4ª série completa do EF',
                            ifelse(malariaNovo$NIV_ESCO == 3, '5ª a 8ª série incompleta do EF',
                            ifelse(malariaNovo$NIV_ESCO == 4, 'Ensino fundamental completo',
                            ifelse(malariaNovo$NIV_ESCO == 5, 'Ensino médio incompleto',
                            ifelse(malariaNovo$NIV_ESCO == 6, 'Ensino médio completo',
                            ifelse(malariaNovo$NIV_ESCO == 7, 'Educação superior incompleto',
                            ifelse(malariaNovo$NIV_ESCO == 8, 'Educação superior completa',
                            ifelse(malariaNovo$NIV_ESCO == 10, 'Não se aplica', NA))))))))))
malariaNovo$escolaridade <- as.factor(malariaNovo$escolaridade)
malariaNovo$NIV_ESCO <- NULL

# Variável: NIV_ESCO_1 -> escolaridade.antiga
# Transformação: De códigos para descrições
malariaNovo$escolaridade.antiga <- ifelse(malariaNovo$NIV_ESCO_1 == 1, 'Nenhuma',
                                   ifelse(malariaNovo$NIV_ESCO_1 == 2, 'De 1 a 3 anos',
                                   ifelse(malariaNovo$NIV_ESCO_1 == 3, 'De 4 a 7 anos',
                                   ifelse(malariaNovo$NIV_ESCO_1 == 4, 'De 8 a 11 anos',
                                   ifelse(malariaNovo$NIV_ESCO_1 == 5, 'De 12 anos a mais',
                                   ifelse(malariaNovo$NIV_ESCO_1 == 6, 'Não se aplica',
                                   ifelse(malariaNovo$NIV_ESCO_1 == 9, 'Ignorado', NA)))))))
malariaNovo$escolaridade.antiga <- as.factor(malariaNovo$escolaridade.antiga)
malariaNovo$NIV_ESCO_1 <- NULL

# Variável: PAIS_INF -> pais.infeccao
# Transformação: Discretização apenas
malariaNovo$pais.infeccao <- as.character(malariaNovo$PAIS_INF)
malariaNovo$pais.infeccao <- as.factor(malariaNovo$pais.infeccao)
malariaNovo$PAIS_INF <- NULL

# Variável: PAIS_RES -> pais.residencia
# Transformação: Discretização apenas
malariaNovo$pais.residencia <- as.character(malariaNovo$PAIS_RES)
malariaNovo$pais.residencia <- as.factor(malariaNovo$pais.residencia)
malariaNovo$PAIS_RES <- NULL

# Variável: RACA -> raca
# Transformação: De códigos para descrições
malariaNovo$raca <- ifelse(malariaNovo$RACA == 1, 'Branca',
                    ifelse(malariaNovo$RACA == 2, 'Preta',
                    ifelse(malariaNovo$RACA == 3, 'Amarela',
                    ifelse(malariaNovo$RACA == 4, 'Parda',
                    ifelse(malariaNovo$RACA == 5, 'Indígena', NA)))))
malariaNovo$raca <- as.factor(malariaNovo$raca)
malariaNovo$RACA <- NULL

# Variável: RES_EXAM -> resultado.exame
# Transformação: De códigos para descrições
malariaNovo$resultado.exame <- ifelse(malariaNovo$RES_EXAM == 1, 'Negativa',
                               ifelse(malariaNovo$RES_EXAM == 2, 'Falciparum',
                               ifelse(malariaNovo$RES_EXAM == 3, 'F+FG',
                               ifelse(malariaNovo$RES_EXAM == 4, 'Vivax',
                               ifelse(malariaNovo$RES_EXAM == 5, 'F+V',
                               ifelse(malariaNovo$RES_EXAM == 6, 'V+FG',
                               ifelse(malariaNovo$RES_EXAM == 7, 'FG',
                               ifelse(malariaNovo$RES_EXAM == 8, 'Malariae',
                               ifelse(malariaNovo$RES_EXAM == 9, 'F+M',
                               ifelse(malariaNovo$RES_EXAM == 10, 'Ovale',
                               ifelse(malariaNovo$RES_EXAM == 11, 'Não F', NA)))))))))))
malariaNovo$resultado.exame <- as.factor(malariaNovo$resultado.exame)
malariaNovo$RES_EXAM <- NULL

# Variável: ID_PACIE -> idade.paciente
# Transformação: Conversão para anos e discretização em faixa etária
malariaNovo$idade.paciente <- ifelse(malariaNovo$ID_DIMEA == 'A', malariaNovo$ID_PACIE,
                              ifelse(malariaNovo$ID_DIMEA == 'M', round(malariaNovo$ID_PACIE / 12),
                              ifelse(malariaNovo$ID_DIMEA == 'D', round(malariaNovo$ID_PACIE / 365), NA)))
malariaNovo$idade.paciente <- ifelse(malariaNovo$idade.paciente >= 0 & malariaNovo$idade.paciente <= 14, 'Criança',
                              ifelse(malariaNovo$idade.paciente >= 15 & malariaNovo$idade.paciente <= 29, 'Jovem',
                              ifelse(malariaNovo$idade.paciente >= 30 & malariaNovo$idade.paciente <= 59, 'Adulto',
                              ifelse(malariaNovo$idade.paciente >= 60, 'Idoso', NA))))
malariaNovo$idade.paciente <- as.factor(malariaNovo$idade.paciente)
malariaNovo$ID_PACIE <- NULL

# Variável: ID_DIMEA
# Transformação: Removida por não ser necessária
malariaNovo$ID_DIMEA <- NULL

# Variável: SEM_NOTI -> semana.notificacao
# Transformação: Alteração de nome apenas
malariaNovo$semana.notificacao <- malariaNovo$SEM_NOTI
malariaNovo$SEM_NOTI <- NULL

# Variável: SEXO -> sexo
# Transformação: De códigos para descrições
malariaNovo$sexo <- ifelse(malariaNovo$SEXO == 'M', 'Masculino',
                    ifelse(malariaNovo$SEXO == 'F', 'Feminino',
                    ifelse(malariaNovo$SEXO == 'I', 'Ignorado', NA)))
malariaNovo$sexo <- as.factor(malariaNovo$sexo)
malariaNovo$SEXO <- NULL

# Variável: SINTOMAS -> sintomas
# Transformação: De códigos para descrições
malariaNovo$sintomas <- ifelse(malariaNovo$SINTOMAS == 1, 'Sim',
                        ifelse(malariaNovo$SINTOMAS == 2, 'Não', NA))
malariaNovo$sintomas <- as.factor(malariaNovo$sintomas)
malariaNovo$SINTOMAS <- NULL

# Variável: TIPO_LAM -> tipo.deteccao
# Transformação: De códigos para descrições
malariaNovo$tipo.deteccao <- ifelse(malariaNovo$TIPO_LAM == 1, 'Passiva',
                             ifelse(malariaNovo$TIPO_LAM == 2, 'Ativa',
                             ifelse(malariaNovo$TIPO_LAM == 3, 'LVC', NA)))
malariaNovo$tipo.deteccao <- as.factor(malariaNovo$tipo.deteccao)
malariaNovo$TIPO_LAM <- NULL

# Variável: UF_INFEC -> uf.infeccao
# Transformação: Discretização apenas
malariaNovo$uf.infeccao <- as.character(malariaNovo$UF_INFEC)
malariaNovo$uf.infeccao <- as.factor(malariaNovo$uf.infeccao)
malariaNovo$UF_INFEC <- NULL

# Variável: UF_NOTIF -> uf.notificacao
# Transformação: Discretização apenas
malariaNovo$uf.notificacao <- as.character(malariaNovo$UF_NOTIF)
malariaNovo$uf.notificacao <- as.factor(malariaNovo$uf.notificacao)
malariaNovo$UF_NOTIF <- NULL

# Variável: UF_RESID -> uf.residencia
# Transformação: Discretização apenas
malariaNovo$uf.residencia <- as.character(malariaNovo$UF_RESID)
malariaNovo$uf.residencia <- as.factor(malariaNovo$uf.residencia)
malariaNovo$UF_RESID <- NULL

# Variável: VIVAX -> tratamento.vivax
# Transformação: De códigos para descrições
malariaNovo$tratamento.vivax <- ifelse(malariaNovo$VIVAX == 1, 'Sim',
                                ifelse(malariaNovo$VIVAX == 2, 'Não', NA))
malariaNovo$tratamento.vivax <- as.factor(malariaNovo$tratamento.vivax)
malariaNovo$VIVAX <- NULL

# Variável: pais_infec -> pais.infeccao.top
# Transformação: Remoção da caixa-alta
malariaNovo$pais.infeccao.top <- ifelse(malariaNovo$pais_infec == 'BRASIL', 'Brasil',
                    		     ifelse(malariaNovo$pais_infec == 'GUIANA', 'Guiana',
                    		     ifelse(malariaNovo$pais_infec == 'GUIANA FRANCESA', 'Guiana Francesa',
                    		     ifelse(malariaNovo$pais_infec == 'SURINAME', 'Suriname',
                    		     ifelse(malariaNovo$pais_infec == 'OUTROS', 'Outros', NA)))))
malariaNovo$pais.infeccao.top <- as.factor(malariaNovo$pais.infeccao.top)
malariaNovo$pais_infec <- NULL

# Variável: pos_falci -> positivo.falciparum
# Transformação: De códigos para descrições
malariaNovo$positivo.falciparum <- ifelse(malariaNovo$pos_falci == 0, 'Não',
                                   ifelse(malariaNovo$pos_falci == 1, 'Sim', NA))
malariaNovo$positivo.falciparum <- as.factor(malariaNovo$positivo.falciparum)
malariaNovo$pos_falci <- NULL

# Variável: pos_vivax -> positivo.vivax
# Transformação: De códigos para descrições
malariaNovo$positivo.vivax <- ifelse(malariaNovo$pos_vivax == 0, 'Não',
                                   ifelse(malariaNovo$pos_vivax == 1, 'Sim', NA))
malariaNovo$positivo.vivax <- as.factor(malariaNovo$positivo.vivax)
malariaNovo$pos_vivax <- NULL

# Variável: pos_malaria -> positivo.malaria
# Transformação: De códigos para descrições
malariaNovo$positivo.malaria <- ifelse(malariaNovo$pos_malaria == 0, 'Não',
                                ifelse(malariaNovo$pos_malaria == 1, 'Sim', NA))
malariaNovo$positivo.malaria <- as.factor(malariaNovo$positivo.malaria)
malariaNovo$pos_malaria <- NULL

# Variável: tripanosoma -> positivo.tripanosoma
# Transformação: De códigos para descrições
malariaNovo$positivo.tripanosoma <- ifelse(malariaNovo$tripanosoma == 0, 'Não',
                                    ifelse(malariaNovo$tripanosoma == 1, 'Sim', NA))
malariaNovo$positivo.tripanosoma <- as.factor(malariaNovo$positivo.tripanosoma)
malariaNovo$tripanosoma <- NULL

# Variável: exam_tripanosoma -> exame.tripanosoma
# Transformação: De códigos para descrições
malariaNovo$exame.tripanosoma <- ifelse(malariaNovo$exam_tripanosoma == 0, 'Não',
                                 ifelse(malariaNovo$exam_tripanosoma == 1, 'Sim', NA))
malariaNovo$exame.tripanosoma <- as.factor(malariaNovo$exame.tripanosoma)
malariaNovo$exam_tripanosoma <- NULL

# Variável: cod_ocup
# Transformação: Removida por não ser necessária
malariaNovo$cod_ocup <- NULL

# Variável: ano
# Transformação: Removida por não ser necessária
malariaNovo$ano <- NULL

# Variável: mes
# Transformação: Removida por não ser necessária
malariaNovo$mes <- NULL

# Variável: ano_mes
# Transformação: Removida por não ser necessária
malariaNovo$ano_mes <- NULL

#############################################

# Salvar RData
save(malariaNovo, file = 'malariaNovo.RData')



#############################################

# Importar biblioteca de padrões frequentes
library(arules)

# Criar transações
malariaTrans <- as(malariaNovo[c(1:24,26:39)], "transactions")

# Criar regras
regras <- apriori(malariaTrans, parameter = list(supp = 0.0095, conf = 0.6, minlen = 2, maxlen = 3, target = "rules"), 
                  appearance = list(rhs = c("positivo.falciparum=Sim"), default="lhs"), control = NULL)

# Criar regras (parâmetros diferentes)
regras <- apriori(malariaTrans, parameter = list(supp = 0.015, conf = 0.9, minlen = 2, maxlen = 3, target = "rules"), 
                  appearance = list(rhs = c("positivo.vivax=Sim"), default="lhs"), control = NULL)

# Salvar CSV
write(regras,file = "regras.csv",row.names = FALSE, sep = ",")

# Visualizar regras, suporte, confiança e lift
tabelaRegras<-as(regras, "data.frame")
