
##ANÁLISE DE CORRESPONDÊNCIA MÚLTIPLA (MCA) PARA VARIÁVEIS SOCIODEMOGRÁFICAS##
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(factoextra)

mca_socio <- MCA(base2018[, c("localidade",
                              "regiao",
                              "sexo",
                              "faixa_etaria",
                              "escolaridade",
                              "faixa_renda",
                              "cor_raca",
                              "situacao_profissional",
                              "voto_presidente")], graph = FALSE)

#Percentual de explicação das variancias de cada dimensão
G60 <- fviz_screeplot(mca_socio, addlabels = TRUE,
               ggtheme = theme_minimal(),
               ylim = c(0,40), barcolor = "#00798c", barfill = "#00798c" )

ggsave("G60.png", width = 10, height = 10, dpi = 400)

G60

#Variáveis
G61 <- fviz_mca_var(mca_socio, choice = "mca.cor", repel = TRUE, col.var = "#00798c",
             ggtheme = theme_minimal()) + labs(title = "MCA - Variáveis")

ggsave("G61.png", width = 10, height = 10, dpi = 400)

G61

#Categorias
G62 <- fviz_mca_var(mca_socio, col.var = "#00798c", shape.var = 1, repel = TRUE)

ggsave("G62.png", width = 10, height = 10, dpi = 400)

G62

#Cos2 (cosseno ao quadrado: representa a qualidade da representação das variáveis
#no mapa de fatores. Mede o grau de associação entre as categorias das variáveis
#e um determinado eixo)
G63 <- fviz_cos2(mca_socio, choice = "var", axes = 1:2, color = "#00798c",fill = "#00798c") +
  labs(x = "", y = "", title = "")

ggsave("G63.png", width = 10, height = 10, dpi = 400)

G63

#Contribuição (os valores da contribuição para a definição das dimensões)
G64 <- fviz_contrib(mca_socio, choice = "var", axes = 1:2, top = 15,
             ggtheme = theme_minimal(base_size = 18)) + labs(y = "Contribuições (%)")

ggsave("G64.png", width = 10, height = 10, dpi = 400)

G64

# Plot conjunto (gráfico de correspondências: pontos e elipses)
G65 <- fviz_mca_biplot(mca_socio,
                       geom.ind = "point",
                       habillage = "voto_presidente",
                       addEllipses = TRUE,
                       ellipse.level = 0.95,
                       col.var = "Black",
                       palette = paleta,
                       title = "Análise de Correspondência Múltipla (MCA)",
                       legend.title = "Escolha do Eleitor",
                       mean.point = FALSE,
                       repel = TRUE)

ggsave("G65.png", width = 10, height = 10, dpi = 400)

G65

# Plot dos indivíduos
#G58 <- fviz_mca_ind(mca_socio, label = "none", habillage = "voto_presidente",
#                    addEllipses = TRUE, ellipse.level = 0.95, palette = paleta)
#
#ggsave("G58.png", width = 10, height = 10, dpi = 400)
#
#G58

# Plot das variáveis
#G59 <- fviz_mca_var(mca_socio, repel = TRUE, col.var = "#00798c")
#
#ggsave("G59.png", width = 20, height = 20, dpi = 400)
#
#G59
#

#Identificando variáveis mais correlacionadas
#res.desc <- ?dimdesc(mca_socio, axes = c(1,2))
# Descrição da dim 1
#res.desc[[1]]
# Descrição da dim 2
#res.desc[[2]]


#ANÁLISE DE CORRESPONDÊNCIA MÚLTIPLA (MCA) PARA VARIÁVEIS CATEGÓRICAS#
colnames(base2018)
mca_base <- MCA(base2018, quanti.sup = c(19:43), graph = TRUE)

#Percentual de explicação das variancias de cada dimensão
G66 <- fviz_screeplot(mca_base, addlabels = TRUE,
                ggtheme = theme_minimal(),
                ylim = c(0,40), barcolor = "#00798c", barfill = "#00798c" )

ggsave("G66.png", width = 6, height = 4, dpi = 400)

G66

contrib <- mca_base$var$contrib
print(contrib)

#Variáveis
G67 <- fviz_mca_var(mca_base, choice = "mca.cor", repel = TRUE, col.var = "#00798c",
             ggtheme = theme_minimal()) + labs(title = "MCA - Variáveis")

ggsave("G67.png", width = 10, height = 6, dpi = 400)

G67

#coordenadas
#loadings <- fit$loadings

#Categorias
G68 <- fviz_mca_var(mca_base, col.var = "#00798c", shape.var = 1, repel = TRUE)

ggsave("G68.png", width = 10, height = 10, dpi = 400)

G68

#Cos2 (cosseno ao quadrado: representa a qualidade da representação das variáveis
#no mapa de fatores. Mede o grau de associação entre as categorias das variáveis
#e um determinado eixo)
G69 <- fviz_cos2(mca_base, choice = "var", axes = 1:2, color = "#00798c",fill = "#00798c") +
  labs(x = "", y = "", title = "")

ggsave("G69.png", width = 10, height = 10, dpi = 400)

G69

#Contribuição (os valores da contribuição para a definição das dimensões)
G70 <- fviz_contrib(mca_base, choice = "var", axes = 1:2, top = 15,
             ggtheme = theme_minimal(base_size = 18)) + labs(y = "Contribuições (%)")

ggsave("G70.png", width = 10, height = 10, dpi = 400)

G70
# Plot conjunto (gráfico de correspondências: pontos e elipses)
G71 <- fviz_mca_biplot(mca_base,
                       geom.ind = "point",
                       habillage = "voto_presidente",
                       addEllipses = TRUE,
                       ellipse.level = 0.95,
                       col.var = "Black",
                       palette = paleta,
                       title = "Análise de Correspondência Múltipla (MCA)",
                       legend.title = "Escolha do Eleitor",
                       mean.point = FALSE,
                       repel = TRUE)

ggsave("G71.png", width = 10, height = 10, dpi = 400)

G71

#Avaliando a cotribuição das categorias das variáveis em formato de tabela
contrib <- mca_base$var$contrib
print(contrib)

library(openxlsx)
contrib_df <- as.data.frame(contrib)
excel_file <- "contribuicao_variaveis.xlsx"
write.xlsx(contrib_df, file = excel_file, rowNames = TRUE)

#EXCLUINDO VARIÁVEIS COM POUCA CONTRIBUIÇÃO
# Definir o limite para exclusão
limite_baixo <- 0.1

# Identificar variáveis a serem excluídas
variaveis_excluir <- rownames(contrib)[apply(contrib, 1, function(x) all(x < limite_baixo))]

print(variaveis_excluir)

# Excluir variáveis do dataframe original
mca_base_filtrado <- mca_base
mca_base_filtrado$var$coord <- mca_base$var$coord[!rownames(mca_base$var$coord) %in% variaveis_excluir, ]
mca_base_filtrado$var$contrib <- mca_base$var$contrib[!rownames(mca_base$var$contrib) %in% variaveis_excluir, ]

#REFAZENDO A ACM COM A BASE QUE EXCLUIU AS VARIÁVEIS
#converte o mca_base_filtrado em data frame
mca_base_filtrado_df <- as.data.frame(mca_base_filtrado)
glimpse(mca_base_filtrado_df)
View(mca_base_filtrado)

colnames(mca_base_filtrado)

#Percentual de explicação das variancias de cada dimensão
G66 <- fviz_screeplot(mca_base, addlabels = TRUE,
                ggtheme = theme_minimal(),
                ylim = c(0,40), barcolor = "#00798c", barfill = "#00798c" )

ggsave("G66.png", width = 6, height = 4, dpi = 400)

G66

contrib <- mca_base$var$contrib
print(contrib)

#Variáveis
G67 <- fviz_mca_var(mca_base, choice = "mca.cor", repel = TRUE, col.var = "#00798c",
             ggtheme = theme_minimal()) + labs(title = "MCA - Variáveis")

ggsave("G67.png", width = 10, height = 6, dpi = 400)

G67

#coordenadas
#loadings <- fit$loadings

#Categorias
G68 <- fviz_mca_var(mca_base, col.var = "#00798c", shape.var = 1, repel = TRUE)

ggsave("G68.png", width = 10, height = 10, dpi = 400)

G68

#Cos2 (cosseno ao quadrado: representa a qualidade da representação das variáveis
#no mapa de fatores. Mede o grau de associação entre as categorias das variáveis
#e um determinado eixo)
G69 <- fviz_cos2(mca_base, choice = "var", axes = 1:2, color = "#00798c",fill = "#00798c") +
  labs(x = "", y = "", title = "")

ggsave("G69.png", width = 10, height = 10, dpi = 400)

G69

#Contribuição (os valores da contribuição para a definição das dimensões)
G70 <- fviz_contrib(mca_base, choice = "var", axes = 1:2, top = 15,
             ggtheme = theme_minimal(base_size = 18)) + labs(y = "Contribuições (%)")

ggsave("G70.png", width = 10, height = 10, dpi = 400)

G70
# Plot conjunto (gráfico de correspondências: pontos e elipses)
G71 <- fviz_mca_biplot(mca_base,
                       geom.ind = "point",
                       habillage = "voto_presidente",
                       addEllipses = TRUE,
                       ellipse.level = 0.95,
                       col.var = "Black",
                       palette = paleta,
                       title = "Análise de Correspondência Múltipla (MCA)",
                       legend.title = "Escolha do Eleitor",
                       mean.point = FALSE,
                       repel = TRUE)

ggsave("G71.png", width = 10, height = 10, dpi = 400)

G71

#Avaliando a cotribuição das categorias das variáveis em formato de tabela
contrib <- mca_base$var$contrib
print(contrib)
