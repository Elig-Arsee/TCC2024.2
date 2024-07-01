####################### Regressão Logística Multinomial #######################

# Carregando os pacotes que serão

if(!require(pacman)) install.packages("pacman")
library(pacman)

pacman::p_load(dplyr, car, psych, nnet, AER, lmtest,
               gtsummary, reshape2, ggplot2, DescTools)

pdf("pairs_basecompleta.pdf", width = 20, height = 20)
pairs(base2018)

#Checagem dos pressupostos

## 1. Variável dependente nominal (categorias mutuamente exclusivas)
## 2. Independência das observações (sem medidas repetidas)

## 3. Ausência de multicolinearidade
## Checagem de multicolinearidade
colnames(base2018)
psych::pairs.panels(base2018[1:18, 44:57])
psych::pairs.panels(base2018[1:57])

# Salvar o gráfico em um arquivo PDF já que o plot é muito grande pro visualizador do R
pdf("pairs_panels_plot.pdf", width = 20, height = 20)
pairs.panels(base2018[1:57])

#nenhuma correlação acima de 0.8 ou 0.9

#Checagem de VIF
m <- lm(as.numeric(voto_presidente) ~ localidade + regiao + sexo +
          faixa_etaria + escolaridade + faixa_renda + cor_raca +
          situacao_profissional + avaliacao_atuacao_PoliciaFederal +
          avaliacao_atuacao_GovernoFederal + avaliacao_atuacao_PoderJudiciario +
          avaliacao_atuacao_PartidosPoliticos +
          avaliacao_atuacao_CongressoNacionalCamaraDeputados +
          avaliacao_atuacao_ForcasArmadas + avaliacao_atuacao_MinisterioPublico +
          avaliacao_atuacao_Redeglobo + avaliacao_atuacao_OutrasEmissorasTV +
          nota_satisfacao_PoliticaPublica_ControleCriminalidade + 
          nota_satisfacao_PoliticaPublica_AcessoTratamentoSaude + 
          nota_satisfacao_PoliticaPublica_DiminuicaoDesigualdadeSocial +
          nota_satisfacao_PoliticaPublica_QualidadeEnsinoPublico + 
          nota_satisfacao_PoliticaPublica_AcessoEnsinoProfissionalizante +
          nota_satisfacao_PoliticaPublica_ControleViolenciaPolicial + 
          nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio +
          nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior +
          nota_satisfacao_PoliticaPublica_AcessoCultura + 
          nota_satisfacao_PoliticaPublica_DiminuicaoDesemprego +
          nota_satisfacao_PoliticaPublica_AcessoJustica +
          nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias +
          nota_satisfacao_PoliticaPublica_RespeitoLeis + 
          nota_satisfacao_PoliticaPublica_AcessoRemedios +
          nota_satisfacao_PoliticaPublica_AcessoTratamentoMedicoEspecializado +
          nota_corrupcao_eficiencia_PoliciaFederal +
          nota_corrupcao_eficiencia_MinisterioPublico +
          nota_corrupcao_eficiencia_Judiciario +
          nota_corrupcao_eficiencia_DefensoriaPublica +
          nota_corrupcao_eficiencia_TCU +
          nota_corrupcao_eficiencia_CGU +
          nota_corrupcao_eficiencia_OGU +
          nota_corrupcao_eficiencia_CPIs +
          nota_corrupcao_eficiencia_TSE +
          nota_corrupcao_eficiencia_CNJ +
          opiniao_PrisaoSegundaInstanciaCombateCorrupcao +
          opiniao_DelacaoPremiadaAjudaCorruptos +
          opiniao_ConducaoCoercitivaFereDireitos +
          opiniao_LavaJato + opiniao_LavaJato_CombateCorrupcao +
          interesse_politica + intensidade_acompanha_politica +
          entende_problemas_policos_importantes +
          representacao_partidaria +
          partido_proximo +
          filiacao_partidaria +
          meio_informacao_politica +
          rede_social_informacao_politica +
          acao_decisao_voto, data = base2018)

car::vif(m)

#O vif não consegue calcular porque há variáveis muito correlacionadas
#vamos usar o Stepwise Selection para avaliar e selecionar as melhores variáveis

#Começamos com o modelo sem preditores
modelo_vazio <- lm(formula = as.numeric(voto_presidente) ~ 1, base2018)
summary(modelo_vazio)

#Depois fazemos um modelo com todos os preditores
modelo_completo <- lm(formula= as.numeric(voto_presidente) ~ ., base2018)
summary(modelo_completo)

#Método de seleção com stepwise
modelo_stepwise <- stepAIC(modelo_vazio,
                        scope = list(lower = modelo_vazio, upper = modelo_completo),
                        direction = "both")
summary(modelo_stepwise)

# Imprimindo o AIC do modelo final
cat("AIC do modelo stepwise:", AIC(modelo_stepwise), "\n")

AIC(modelo_stepwise)

#Outra forma de fazer modelo stepwise
library(MASS)
modelo_inicial <- lm(as.numeric(voto_presidente) ~ ., data = base2018)

#ajustando o modelo com base no critério de informação de Akaike (AIC).
modelo_stepwise2 <- stepAIC(modelo_inicial, direction = "both")

#avaliando modelo final
summary(modelo_stepwise2)
cat("AIC do modelo stepwise2:", AIC(modelo_stepwise2), "\n")
#resultado 4002.856 (resultado mais baixo, portanto melhor. Vamos ficar com este modelo)
#O modelo criado foi:
#Call:
#  lm(formula = as.numeric(voto_presidente) ~ regiao + faixa_etaria + 
#       avaliacao_atuacao_ForcasArmadas + avaliacao_atuacao_Redeglobo + 
#       nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio + nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior + 
#       nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias + 
#       nota_corrupcao_eficiencia_DefensoriaPublica + nota_corrupcao_eficiencia_OGU + 
#       nota_corrupcao_eficiencia_CPIs + nota_corrupcao_eficiencia_TSE + 
#       nota_corrupcao_eficiencia_CNJ + opiniao_LavaJato + opiniao_LavaJato_CombateCorrupcao + 
#       interesse_politica + partido_proximo + acao_decisao_voto, 
#     data = base2018)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.3519 -0.4482 -0.1081  0.2779  2.8181
# eu ainda queria ver outras possibilidades, porque não foi incluída nenhuma variável sobre
#meios de comunciação utilizados para se informar sobre política

#Então vamos usar os métodos LASSO e Ridge Regression para ajudando a reduzir a influência
#de variáveis redundantes ou multicolineares. 

### Agora, com as variáveis selecionadas, vamos fazer A regressão logística multinomial
#Começamos gerando uma base que contenha somente as variáveis que vão permanecer no modelo
#para facilitar minha vida
colnames(base2018)

colunas_desejadas <- c("voto_presidente",
                       "regiao",
                       "sexo",
                       "faixa_etaria",
                       "escolaridade",
                       "faixa_renda",
                       "cor_raca",
                       "situacao_profissional",
                       "avaliacao_atuacao_ForcasArmadas",
                       "avaliacao_atuacao_Redeglobo",
                       "nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio",
                       "nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior",
                       "nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias",
                       "nota_corrupcao_eficiencia_DefensoriaPublica",
                       "nota_corrupcao_eficiencia_OGU",
                       "nota_corrupcao_eficiencia_CPIs",
                       "nota_corrupcao_eficiencia_TSE",
                       "nota_corrupcao_eficiencia_CNJ",
                       "opiniao_LavaJato",
                       "opiniao_LavaJato_CombateCorrupcao",
                       "interesse_politica",
                       "partido_proximo",
                       "acao_decisao_voto",
                       "rede_social_informacao_politica")

base2018_selecionado <- base2018[, colunas_desejadas]

# Verificar o novo dataframe
head(base2018_selecionado)
glimpse(base2018_selecionado)

#Checagem dos pressupostos

## 1. Variável dependente nominal (categorias mutuamente exclusivas)
## 2. Independência das observações (sem medidas repetidas)

## 3. Ausência de multicolinearidade
## Checagem de multicolinearidade
colnames(base2018_selecionado)

psych::pairs.panels(base2018_selecionado[1:24])
#quero ver melhor isso vou salvar em pdf
pdf("pairs_panels_plot_base_selecionado.pdf", width = 20, height = 20)
pairs.panels(base2018_selecionado[1:24])


#Checagem de VIF
m <- lm(as.numeric(voto_presidente) ~ ., data = base2018_selecionado)

car::vif(m)
#nenhum valor preocupante (acima de 10)

#4. Independência de alternativas irrelevantes - Teste Hausman-MCFaddem.
# Verifica se caso uma das opções não existisse (voto nulo, branco, haddad ou bolsonaro),
#o coeficiente seria o mesmo. Ou seja, não haveria impacto sobre a decisão do voto.

install.packages("mlogit")#se não tiver pacote instalado precisa instalar.
library(mlogit)

# Modelo com todas as alternativas
modi_ai <- mlogit::mlogit(voto_presidente ~ 1 | regiao + sexo + faixa_etaria + escolaridade +
                            faixa_renda + cor_raca + situacao_profissional +
                            avaliacao_atuacao_ForcasArmadas + avaliacao_atuacao_Redeglobo +
                            nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio +
                            nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior +
                            nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias +
                            nota_corrupcao_eficiencia_DefensoriaPublica +
                            nota_corrupcao_eficiencia_OGU + nota_corrupcao_eficiencia_CPIs +
                            nota_corrupcao_eficiencia_TSE +
                            nota_corrupcao_eficiencia_CNJ +
                            opiniao_LavaJato +
                            opiniao_LavaJato_CombateCorrupcao +
                            interesse_politica +
                            partido_proximo +
                            acao_decisao_voto +
                            rede_social_informacao_politica,
                          data = base2018_selecionado,
                          shape = "wide",
                          reflevel = "Jair Bolsonaro (PSL)")#Categoria de referência

# Modelo excluindo voto em branco
modi_ai2 <- mlogit::mlogit(voto_presidente ~ 1 | regiao + sexo + faixa_etaria + escolaridade +
                             faixa_renda + cor_raca + situacao_profissional +
                             avaliacao_atuacao_ForcasArmadas + avaliacao_atuacao_Redeglobo +
                             nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio +
                             nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior +
                             nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias +
                             nota_corrupcao_eficiencia_DefensoriaPublica +
                             nota_corrupcao_eficiencia_OGU + nota_corrupcao_eficiencia_CPIs +
                             nota_corrupcao_eficiencia_TSE +
                             nota_corrupcao_eficiencia_CNJ +
                             opiniao_LavaJato +
                             opiniao_LavaJato_CombateCorrupcao +
                             interesse_politica +
                             partido_proximo +
                             acao_decisao_voto +
                             rede_social_informacao_politica,
                           data = base2018_selecionado,
                           shape = "wide",
                           reflevel = "Jair Bolsonaro (PSL)",
                           alt.subset = c("Jair Bolsonaro (PSL)", "Fernando Haddad (PT)", "Anulou o voto"))

# Modelo excluindo voto nulo
modi_ai3 <- mlogit::mlogit(voto_presidente ~ 1 | regiao + sexo + faixa_etaria + escolaridade +
                             faixa_renda + cor_raca + situacao_profissional +
                             avaliacao_atuacao_ForcasArmadas + avaliacao_atuacao_Redeglobo +
                             nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio +
                             nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior +
                             nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias +
                             nota_corrupcao_eficiencia_DefensoriaPublica +
                             nota_corrupcao_eficiencia_OGU + nota_corrupcao_eficiencia_CPIs +
                             nota_corrupcao_eficiencia_TSE +
                             nota_corrupcao_eficiencia_CNJ +
                             opiniao_LavaJato +
                             opiniao_LavaJato_CombateCorrupcao +
                             interesse_politica +
                             partido_proximo +
                             acao_decisao_voto +
                             rede_social_informacao_politica,
                           data = base2018_selecionado,
                           shape = "wide",
                           reflevel = "Jair Bolsonaro (PSL)",
                           alt.subset = c("Jair Bolsonaro (PSL)", "Fernando Haddad (PT)", "Votou em branco"))

# Modelo excluindo Fernando Haddad (PT)
modi_ai4 <- mlogit::mlogit(voto_presidente ~ 1 | regiao + sexo + faixa_etaria + escolaridade +
                             faixa_renda + cor_raca + situacao_profissional +
                             avaliacao_atuacao_ForcasArmadas + avaliacao_atuacao_Redeglobo +
                             nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio +
                             nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior +
                             nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias +
                             nota_corrupcao_eficiencia_DefensoriaPublica +
                             nota_corrupcao_eficiencia_OGU + nota_corrupcao_eficiencia_CPIs +
                             nota_corrupcao_eficiencia_TSE +
                             nota_corrupcao_eficiencia_CNJ +
                             opiniao_LavaJato +
                             opiniao_LavaJato_CombateCorrupcao +
                             interesse_politica +
                             partido_proximo +
                             acao_decisao_voto +
                             rede_social_informacao_politica,
                           data = base2018_selecionado,
                           shape = "wide",
                           reflevel = "Jair Bolsonaro (PSL)",
                           alt.subset = c("Jair Bolsonaro (PSL)", "Anulou o voto", "Votou em branco"))

# Comparando modelos para ver se há independência das alternativas irrelevantes
# p precisa ser maior que 0,05
mlogit::hmftest(modi_ai, modi_ai2)
mlogit::hmftest(modi_ai, modi_ai3)
mlogit::hmftest(modi_ai, modi_ai4)


# 5. Construção de modelos de regressão multinomial
library(nnet)
?multinom

# Modelo Satisfação
mod_sat <- multinom(Q12P2.B_Atitude_Turno_2 ~ Q21_Satisfacao_Democracia + P3.4_Avaliacao_Governo_Federal + P3.7_Avaliacao_Partidos_Politicos + P3.8_Avaliacao_Congresso_Nacional_Senado_CamaraDeputados,
                    data = Data2018, model = TRUE)

# Modelo nulo sem nenhum previsor
mod_sat0 <- multinom(Q12P2.B_Atitude_Turno_2 ~ 1, data = Data2018, model = TRUE)

# Ajuste do modelo
# H0: Modelo nulo é igual ao modelo construído. Caso a hipótese seja confirmada,
# significa que o modelo construído é inutil.
# p precisa ser menor que 0,05
Anova <- anova(mod_sat0, mod_sat)

gtsummary::tbl_summary(Anova)


# Verificação do R². Ele informa a proporção da VD que é explicada pelas VI.
R2_Neig_sat <- DescTools::PseudoR2(mod_sat, which = "Nagelkerke")


#Obtenção dos valores de p - por Wald (mesmo tipo do spss)
T_Wald_sat <- lmtest::coeftest(mod_sat)


#Para verificar efeitos globais
car::Anova(mod_sat, type = "II", test = "Nald")


#Para verificar coeficientes do modelo
summary(mod_sat)


#Calcula razão de chance
exp(coef(mod_sat))

#Calcula intervalo de confiança para coeficientes
exp(confint(mod_sat))

#Tabela completa

Tab_Multi_Sat <- gtsummary::tbl_regression(mod_sat, exponentiate = TRUE)

as_gt(Tab_Multi_Sat)



install.packages("pacman", repos="http://cran.rstudio.com/", dependencies=TRUE)

#Gráfico
sjPlot::plot_model(mod_sat)

#Verificar modelo: ficou bem ruim.
summary(predict(mod_sat))

