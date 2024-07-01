#GRÁFICOS EXPLORAÇÃO INICIAL
# defino a minha paleta de cores
paleta <- c("#00798c", "#d1495b", "#edae49",
            "#66a182", "#2e4057", "#996633",
            "#6CB2EB", "#a8dda8", "#95a5a6")


#Estatísticas descritivas
summary (base2018)

# Amostra segmentada segundo voto

G1 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente) %>%
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(porc = n/sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100*porc, accuracy = 0.01,
                                                    decimal.mark = ",",
                                                    suffix = "%"))) %>% 
  ggplot(aes(x = voto_presidente, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = rotulo), size = 3, vjust = -0.5) +
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G1.png", width = 6, height = 4.5, dpi = 400)#esse código é para salvar com qualidade melhor

G1

# Distribuição da amostra por localidade e escolha do eleitor (opção a)

G2a <- base2018 %>%
  filter(!is.na(localidade)) %>% 
  group_by(localidade, voto_presidente) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(localidade) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = voto_presidente, y = n, fill = localidade)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = rotulo), size = 3, hjust = -0.1, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a localidade e a escolha do eleitor",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G2a.png", width = 14, height = 4.5, dpi = 400)

G2a

# Distribuição da amostra por localidade e escolha do eleitor (opção b - gostei mais)
G2b <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, localidade) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = localidade, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = -1.5, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a localidade",
       y = "Ocorrências",
       x = "Escolha do eleitor por localidade",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G2b.png", width = 14, height = 4.5, dpi = 400)

G2b

# Distribuição da amostra por região e escolha do eleitor

G3 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, regiao) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = regiao, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a região",
       y = "Ocorrências",
       x = "Escolha do eleitor por região",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G3.png", width = 14, height = 4.5, dpi = 400)

G3

# Distribuição da amostra por sexo e escolha do eleitor
G4 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, sexo) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = sexo, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = -1.5, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e o sexo",
       y = "Ocorrências",
       x = "Escolha do eleitor por sexo",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G4.png", width = 14, height = 4.5, dpi = 400)
G4

# Distribuição da amostra por cor/raça e escolha do eleitor
G5 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, cor_raca) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = cor_raca, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = -1.5, position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a cor/raça",
       y = "Ocorrências",
       x = "Escolha do eleitor por cor/raça",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G5.png", width = 14, height = 4.5, dpi = 400)

G5

# Distribuição da amostra por faixa etária e escolha do eleitor
G6 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, faixa_etaria) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = faixa_etaria, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a faixa etária",
       y = "Ocorrências",
       x = "Escolha do eleitor por faixa etária",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G6.png", width = 14, height = 4.5, dpi = 400)

G6

# Distribuição da amostra por escolaridade e escolha do eleitor
G7 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, escolaridade) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = escolaridade, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a escolaridade",
       y = "Ocorrências",
       x = "Escolha do eleitor por escolaridade",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G7.png", width = 20, height = 4.5, dpi = 400)

G7

# Distribuição da amostra por faixa de renda e escolha do eleitor
G8 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, faixa_renda) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = faixa_renda, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a faixa de renda",
       y = "Ocorrências",
       x = "Escolha do eleitor por faixa de renda",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G8.png", width = 20, height = 4.5, dpi = 400)

G8

# Distribuição da amostra por situação profissional e escolha do eleitor
G9 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, situacao_profissional) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = situacao_profissional, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a situação profissional",
       y = "Ocorrências",
       x = "Escolha do eleitor por situação profissional",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G9.png", width = 20, height = 4.5, dpi = 400)

G9

# Distribuição da amostra por situação profissional e avaliação da atuacao da Policia Federal
G10 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, avaliacao_atuacao_PoliciaFederal) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = avaliacao_atuacao_PoliciaFederal, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a avaliação da atuacao da Policia Federal",
       y = "Ocorrências",
       x = "Escolha do eleitor por avaliação da atuacao da Policia Federal",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G10.png", width = 20, height = 4.5, dpi = 400)

G10

# Distribuição da amostra por situação profissional e avaliação da atuacao do Governo Federal
G11 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, avaliacao_atuacao_GovernoFederal) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = avaliacao_atuacao_GovernoFederal, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a avaliação da atuacao do Governo Federal",
       y = "Ocorrências",
       x = "Escolha do eleitor por avaliação da atuacao do Governo Federal",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G11.png", width = 20, height = 4.5, dpi = 400)

G11

# Distribuição da amostra por situação profissional e avaliação da atuacao do Poder Judiciário
G12 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, avaliacao_atuacao_PoderJudiciario) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = avaliacao_atuacao_PoderJudiciario, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a avaliação da atuacao do Poder Judiciario",
       y = "Ocorrências",
       x = "Escolha do eleitor por avaliação da atuacao do Poder Judiciario",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G12.png", width = 20, height = 4.5, dpi = 400)

G12

# Distribuição da amostra por situação profissional e avaliação da atuacao dos Partidos Políticos
G13 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, avaliacao_atuacao_PartidosPoliticos) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = avaliacao_atuacao_PartidosPoliticos, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a avaliação da atuacao dos Partidos políticos",
       y = "Ocorrências",
       x = "Escolha do eleitor por avaliação da atuacao dos Partidos políticos",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G13.png", width = 20, height = 4.5, dpi = 400)

G13

# Distribuição da amostra por situação profissional e avaliação da atuacao do Congresso Nacional e Camara dos Deputados
G14 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, avaliacao_atuacao_CongressoNacionalCamaraDeputados) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = avaliacao_atuacao_CongressoNacionalCamaraDeputados, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a avaliação da atuacao do Congresso Nacional e da Camara dos Deputados",
       y = "Ocorrências",
       x = "Escolha do eleitor por avaliação da atuacao do Congresso Nacional e da Camara dos Deputados",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G14.png", width = 20, height = 4.5, dpi = 400)

G14

# Distribuição da amostra por situação profissional e avaliação da atuacao das Forças Armadas
G15 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, avaliacao_atuacao_ForcasArmadas) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = avaliacao_atuacao_ForcasArmadas, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a avaliação da atuacao das Forças Armadas",
       y = "Ocorrências",
       x = "Escolha do eleitor por avaliação da atuacao das Forças Armadas",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G15.png", width = 20, height = 4.5, dpi = 400)

G15

# Distribuição da amostra por situação profissional e avaliação da atuacao do Ministerio Publico
G16 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, avaliacao_atuacao_MinisterioPublico) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = avaliacao_atuacao_MinisterioPublico, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a avaliação da atuacao do Ministério Público",
       y = "Ocorrências",
       x = "Escolha do eleitor por avaliação da atuacao do Ministério Público",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G16.png", width = 20, height = 4.5, dpi = 400)

G16

# Distribuição da amostra por situação profissional e avaliação da atuacao da rede globo
G17 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, avaliacao_atuacao_Redeglobo) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = avaliacao_atuacao_Redeglobo, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a avaliação da atuacao da Rede Globo",
       y = "Ocorrências",
       x = "Escolha do eleitor por avaliação da atuacao da Rede Globo",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G17.png", width = 20, height = 4.5, dpi = 400)

G17

# Distribuição da amostra por situação profissional e avaliação da atuacao de Outras Emissoras de TV
G18 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, avaliacao_atuacao_OutrasEmissorasTV) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = avaliacao_atuacao_OutrasEmissorasTV, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e a avaliação da atuacao de Outras Emissoras de TV",
       y = "Ocorrências",
       x = "Escolha do eleitor por avaliação da atuacao de Outras Emissoras de TV",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G18.png", width = 20, height = 4.5, dpi = 400)

G18

# Distribuição da amostra por situação profissional e satisfação com o Controle de Criminalidade
controle_criminalidade <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_ControleCriminalidade >= 0 & 
           nota_satisfacao_PoliticaPublica_ControleCriminalidade <= 10)

# Plotando boxplot no mesmo padrão
G19 <- controle_criminalidade %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_ControleCriminalidade, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de controle da criminalidade por escolha do eleitor",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G19.png", width = 20, height = 4.5, dpi = 400)

G19

# Distribuição da amostra por situação profissional e satisfação com o Acesso à Tratamento de Saude
tratamento_saude <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_AcessoTratamentoSaude >= 0 & 
           nota_satisfacao_PoliticaPublica_AcessoTratamentoSaude <= 10)

# Plotando boxplot no mesmo padrão
G20 <- tratamento_saude %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_AcessoTratamentoSaude, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de acesso à tratamento de saúde por escolha do eleitor",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G20.png", width = 20, height = 4.5, dpi = 400)

G20

# Distribuição da amostra por situação profissional e satisfação com a Diminuicao da Desigualdade Social
desigualdade_social <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_DiminuicaoDesigualdadeSocial >= 0 & 
           nota_satisfacao_PoliticaPublica_DiminuicaoDesigualdadeSocial <= 10)

# Plotando boxplot no mesmo padrão
G21 <- desigualdade_social %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_DiminuicaoDesigualdadeSocial, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de diminuição da desigualdade social",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G21.png", width = 20, height = 4.5, dpi = 400)

G21

# Distribuição da amostra por situação profissional e satisfação com o ensino publico
ensino_publico <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_QualidadeEnsinoPublico >= 0 & 
           nota_satisfacao_PoliticaPublica_QualidadeEnsinoPublico <= 10)

# Plotando boxplot no mesmo padrão
G22 <- ensino_publico %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_QualidadeEnsinoPublico, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de ensino público",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G22.png", width = 20, height = 4.5, dpi = 400)

G22

# Distribuição da amostra por situação profissional e satisfação com o ensino profissionalizante
ensino_profissionalizante <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_AcessoEnsinoProfissionalizante >= 0 & 
           nota_satisfacao_PoliticaPublica_AcessoEnsinoProfissionalizante <= 10)

# Plotando boxplot no mesmo padrão
G23 <- ensino_profissionalizante %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_AcessoEnsinoProfissionalizante, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de ensino profissionalizante",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G23.png", width = 20, height = 4.5, dpi = 400)

G23

# Distribuição da amostra por situação profissional e satisfação com o controle da violencia policial
violencia_policial <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_ControleViolenciaPolicial >= 0 & 
           nota_satisfacao_PoliticaPublica_ControleViolenciaPolicial <= 10)

# Plotando boxplot no mesmo padrão
G24 <- violencia_policial %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_ControleViolenciaPolicial, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de controle da violência policial",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G24.png", width = 20, height = 4.5, dpi = 400)

G24

# Distribuição da amostra por situação profissional e satisfação com o Acesso ao ensino médio
ensino_medio <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio >= 0 & 
           nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio <= 10)

# Plotando boxplot no mesmo padrão
G25 <- ensino_medio %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de acesso ao ensino médio",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G25.png", width = 20, height = 4.5, dpi = 400)

G25

# Distribuição da amostra por situação profissional e satisfação com o Acesso ao ensino superior
ensino_superior <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior >= 0 & 
           nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior <= 10)

# Plotando boxplot no mesmo padrão
G26 <- ensino_superior %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de acesso ao ensino superior",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G26.png", width = 20, height = 4.5, dpi = 400)

G26

# Distribuição da amostra por situação profissional e satisfação com o Acesso à cultura
acesso_cultura <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_AcessoCultura >= 0 & 
           nota_satisfacao_PoliticaPublica_AcessoCultura <= 10)

# Plotando boxplot no mesmo padrão
G27 <- acesso_cultura %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_AcessoCultura, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de acesso à cultura",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G27.png", width = 20, height = 4.5, dpi = 400)

G27

# Distribuição da amostra por situação profissional e satisfação com a diminuição do desemprego
diminuicao_desemprego <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_DiminuicaoDesemprego >= 0 & 
           nota_satisfacao_PoliticaPublica_DiminuicaoDesemprego <= 10)

# Plotando boxplot no mesmo padrão
G28 <- diminuicao_desemprego %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_DiminuicaoDesemprego, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de diminuição do desemprego",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G28.png", width = 20, height = 4.5, dpi = 400)

G28

# Distribuição da amostra por situação profissional e satisfação com o acesso a justiça
acesso_justica <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_AcessoJustica >= 0 & 
           nota_satisfacao_PoliticaPublica_AcessoJustica <= 10)

# Plotando boxplot no mesmo padrão
G29 <- acesso_justica %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_AcessoJustica, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de acesso à justiça",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

# Salvando o gráfico
ggsave("G29.png", width = 20, height = 4.5, dpi = 400)

G29

# Distribuição da amostra por situação profissional e satisfação com o acesso de direitos das minorias
direitos_minorias <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias >= 0 & 
           nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias <= 10)

# Plotando boxplot no mesmo padrão
G30 <- direitos_minorias %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de acesso à direitos das minorias",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G30.png", width = 20, height = 4.5, dpi = 400)

G30

# Distribuição da amostra por situação profissional e satisfação com o respeito as leis
respeito_leis <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_RespeitoLeis >= 0 & 
           nota_satisfacao_PoliticaPublica_RespeitoLeis <= 10)

# Plotando boxplot no mesmo padrão
G31 <- respeito_leis %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_RespeitoLeis, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de respeito as leis",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G31.png", width = 20, height = 4.5, dpi = 400)

G31

# Distribuição da amostra por situação profissional e satisfação com o acesso a remedios
acesso_remedios <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_AcessoRemedios >= 0 & 
           nota_satisfacao_PoliticaPublica_AcessoRemedios <= 10)

# Plotando boxplot no mesmo padrão
G32 <- acesso_remedios %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_AcessoRemedios, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de acesso à remedios",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G32.png", width = 20, height = 4.5, dpi = 400)

G32

# Distribuição da amostra por situação profissional e satisfação com o acesso a tratamento médico especializado
medico_especializado <- base2018 %>%
  filter(nota_satisfacao_PoliticaPublica_AcessoTratamentoMedicoEspecializado >= 0 & 
           nota_satisfacao_PoliticaPublica_AcessoTratamentoMedicoEspecializado <= 10)

# Plotando boxplot no mesmo padrão
G33 <- medico_especializado %>%
  ggplot(aes(x = voto_presidente, y = nota_satisfacao_PoliticaPublica_AcessoTratamentoMedicoEspecializado, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de satisfação com políticas públicas de acesso à tratamento médico especializado",
       y = "Nota de satisfação",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G33.png", width = 20, height = 4.5, dpi = 400)

G33

# Distribuição da amostra por escolha do eleitor e nota da eficiência da PF
eficiencia_pf <- base2018 %>%
  filter(nota_corrupcao_eficiencia_PoliciaFederal >= 0 & 
           nota_corrupcao_eficiencia_PoliciaFederal <= 10)

# Plotando boxplot no mesmo padrão
G34 <- eficiencia_pf %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_PoliciaFederal, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Polícia Federal",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G34.png", width = 20, height = 4.5, dpi = 400)

G34

# Distribuição da amostra por escolha do eleitor e nota da eficiência do MP
eficiencia_mp <- base2018 %>%
  filter(nota_corrupcao_eficiencia_MinisterioPublico >= 0 & 
           nota_corrupcao_eficiencia_MinisterioPublico <= 10)

# Plotando boxplot no mesmo padrão
G35 <- eficiencia_mp %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_MinisterioPublico, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Ministério Público",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G35.png", width = 20, height = 4.5, dpi = 400)

G35

# Distribuição da amostra por escolha do eleitor e nota da eficiência do judiciário
eficiencia_judiciario <- base2018 %>%
  filter(nota_corrupcao_eficiencia_Judiciario >= 0 & 
           nota_corrupcao_eficiencia_Judiciario <= 10)

# Plotando boxplot no mesmo padrão
G36 <- eficiencia_judiciario %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_Judiciario, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Judiciário",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G36.png", width = 20, height = 4.5, dpi = 400)

G36

# Distribuição da amostra por escolha do eleitor e nota da eficiência da defensoria
eficiencia_defensoria <- base2018 %>%
  filter(nota_corrupcao_eficiencia_DefensoriaPublica >= 0 & 
           nota_corrupcao_eficiencia_DefensoriaPublica <= 10)

# Plotando boxplot no mesmo padrão
G37 <- eficiencia_defensoria %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_DefensoriaPublica, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Defensoria Pública",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G37.png", width = 20, height = 4.5, dpi = 400)

G37

# Distribuição da amostra por escolha do eleitor e nota da eficiência da TCU
eficiencia_tcu <- base2018 %>%
  filter(nota_corrupcao_eficiencia_TCU >= 0 & 
           nota_corrupcao_eficiencia_TCU <= 10)

# Plotando boxplot no mesmo padrão
G38 <- eficiencia_tcu %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_TCU, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Tribunal de Contas da União",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G38.png", width = 20, height = 4.5, dpi = 400)

G38

# Distribuição da amostra por escolha do eleitor e nota da eficiência da CGU
eficiencia_cgu <- base2018 %>%
  filter(nota_corrupcao_eficiencia_CGU >= 0 & 
           nota_corrupcao_eficiencia_CGU <= 10)

# Plotando boxplot no mesmo padrão
G39 <- eficiencia_cgu %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_CGU, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Controladoria Geral da União",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G39.png", width = 20, height = 4.5, dpi = 400)

G39

# Distribuição da amostra por escolha do eleitor e nota da eficiência da CNJ
eficiencia_cnj <- base2018 %>%
  filter(nota_corrupcao_eficiencia_CNJ >= 0 & 
           nota_corrupcao_eficiencia_CNJ <= 10)

# Plotando boxplot no mesmo padrão
G40 <- eficiencia_cnj %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_CNJ, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Conselho Nacional de Justiça",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G40.png", width = 20, height = 4.5, dpi = 400)

G40

# Distribuição da amostra por escolha do eleitor e nota da eficiência da ouvidoria
eficiencia_ogu <- base2018 %>%
  filter(nota_corrupcao_eficiencia_OGU >= 0 & 
           nota_corrupcao_eficiencia_OGU <= 10)

# Plotando boxplot no mesmo padrão
G41 <- eficiencia_ogu %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_OGU, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Ouvidoria Geral da União",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G41.png", width = 20, height = 4.5, dpi = 400)

G41

# Distribuição da amostra por escolha do eleitor e nota das Comissões Parlamentares de Inquérito (CPIs) 
eficiencia_cpis <- base2018 %>%
  filter(nota_corrupcao_eficiencia_CPIs >= 0 & 
           nota_corrupcao_eficiencia_CPIs <= 10)

# Plotando boxplot no mesmo padrão
G42 <- eficiencia_cpis %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_CPIs, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Comissões Parlamentares de Inquérito (CPIs)",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G42.png", width = 20, height = 4.5, dpi = 400)

G42

# Distribuição da amostra por escolha do eleitor e nota do TSE
eficiencia_tse <- base2018 %>%
  filter(nota_corrupcao_eficiencia_TSE >= 0 & 
           nota_corrupcao_eficiencia_TSE <= 10)

# Plotando boxplot no mesmo padrão
G43 <- eficiencia_tse %>%
  ggplot(aes(x = voto_presidente, y = nota_corrupcao_eficiencia_TSE, fill = voto_presidente)) +
  geom_boxplot() +
  scale_fill_manual(values = paleta) +
  labs(title = "Distribuição das notas de eficiência no combate à corrupção: Tribunal Superior Eleitoral",
       y = "Nota",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10))

ggsave("G43.png", width = 20, height = 4.5, dpi = 400)

G43

# Distribuição da amostra por Opinião sobre segunda instância combaer corrupção
G44 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, opiniao_PrisaoSegundaInstanciaCombateCorrupcao) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = opiniao_PrisaoSegundaInstanciaCombateCorrupcao, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e opinião: A prisão em segunda instância combate a corrupção",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G44.png", width = 20, height = 4.5, dpi = 400)

G44

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G45 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, opiniao_DelacaoPremiadaAjudaCorruptos) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = opiniao_DelacaoPremiadaAjudaCorruptos, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e opinião: A delação premiada ajuda os corruptos",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G45.png", width = 20, height = 4.5, dpi = 400)

G45

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G46 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, opiniao_ConducaoCoercitivaFereDireitos) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = opiniao_ConducaoCoercitivaFereDireitos, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e opinião: A condução coercitiva fere direitos",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G46.png", width = 20, height = 4.5, dpi = 400)

G46

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G47 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, opiniao_LavaJato) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = opiniao_LavaJato, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e opinião: Sobre a operação Lava Jato, você acredita que",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G47.png", width = 20, height = 4.5, dpi = 400)

G47

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G48 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, opiniao_LavaJato_CombateCorrupcao) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = opiniao_LavaJato_CombateCorrupcao, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e opinião: A operação Lava Jato combate a corrupção",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G48.png", width = 20, height = 4.5, dpi = 400)

G48

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G49 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, interesse_politica) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = interesse_politica, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e interesse em política",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G49.png", width = 20, height = 4.5, dpi = 400)

G49

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G50 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, intensidade_acompanha_politica) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = intensidade_acompanha_politica, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e intensidade com que acompanha política",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G50.png", width = 20, height = 4.5, dpi = 400)

G50

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G51 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, entende_problemas_policos_importantes) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = entende_problemas_policos_importantes, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e opinião: Entende os problemas políticos mais importantes",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G51.png", width = 20, height = 4.5, dpi = 400)

G51

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G52 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, representacao_partidaria) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = representacao_partidaria, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e representação partidária",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G52.png", width = 20, height = 4.5, dpi = 400)

G52

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G53 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, filiacao_partidaria) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = filiacao_partidaria, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e filiação partidária",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G53.png", width = 20, height = 4.5, dpi = 400)

G53

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G54 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, partido_proximo) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = partido_proximo, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e partido próximo",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G54.png", width = 20, height = 50, dpi = 400, limitsize = FALSE)

G54

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G55 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, meio_informacao_politica) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = meio_informacao_politica, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e meio de informação política",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G55.png", width = 20, height = 8, dpi = 400)

G55

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G56 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, rede_social_informacao_politica) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = rede_social_informacao_politica, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e rede social mais utilizada para obter informação política",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G56.png", width = 20, height = 8, dpi = 400)

G56

# Distribuição da amostra por Opinião sobre delação premiada ajudar corruptos
G57 <- base2018 %>%
  filter(!is.na(voto_presidente)) %>% 
  group_by(voto_presidente, acao_decisao_voto) %>%
  summarise(n = n(), .groups = 'drop') %>% 
  ungroup() %>% 
  group_by(voto_presidente) %>%
  mutate(porc = n / sum(n),
         rotulo = paste0(n, "  |  ", scales::number(100 * porc, accuracy = 0.01, decimal.mark = ",", suffix = "%"))) %>% 
  ggplot(aes(x = acao_decisao_voto, y = n, fill = voto_presidente)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = rotulo), size = 3, vjust = 0.5, hjust = -0.3, position = position_dodge(width = 0.9)) + # Ajuste de hjust para fora da barra
  scale_fill_manual(values = paleta) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribuição da amostra segundo a escolha do eleitor e ações para decisão do voto",
       y = "Ocorrências",
       x = "Escolha do eleitor",
       fill = "Escolha do eleitor") +
  theme_classic() +
  theme(legend.position = "bottom",
        title = element_text(size = 10)) +
  coord_flip()

ggsave("G57.png", width = 20, height = 20, dpi = 400)

G57

#Estatísticas descritivas
#Descrição do perfil socioeconômico por atitude de voto
#Tabela com estatísticas descritivas
#carrega pacotes
if(!require(pacman)) install.packages("pacman")
library(pacman)
library(gtsummary)

pacman::p_load(dplyr, car, psych, nnet, AER, lmtest,
               gtsummary, reshape2, ggplot2, DescTools,
               ggrastr, sjPlot)
#Crio tabela
tbl_summary
Tabela_socio <- base2018%>%
  tbl_summary(
    by = voto_presidente,
    statistic = list(all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 2,
    label = list(localidade ~ "Localidade",
                 regiao ~ "Região",
                 sexo ~ "Sexo",
                 faixa_etaria ~ "Faixa etária",
                 escolaridade ~ "Escolaridade",
                 faixa_renda ~ "Faixa de renda",
                 cor_raca ~ "Cor/Raça",
                 situacao_profissional ~ "Situação profissional"),
    type = list(c(base2018) ~ "categorical"),
    percent = "row")%>%
  bold_labels()%>%
  add_overall(statistic = list(all_categorical() ~ "{p}% ({n})"),
              last = T)%>%
  modify_spanning_header(c("stat_1", "stat_2", "stat_3", "stat_4") ~"**Escolha no segundo turno da eleição**") %>%
  modify_caption("**Tabela 1: Distribuição percentual dos entrevistados,
  por atitude no segundo turno da eleição de 2018, segundo variáveis socioeconômicas**")

#Verifico nomes das colunas da tabela
show_header_names(Tabela_socio)

#Altero nomes das colunas da tabela
Tabela_socio%>%
  modify_header(update = list(label ~ "**Variáveis**"))

#Converto tabela para formato word
install.packages("flextable") #instalo pacote
library(flextable) #chamo biblioteca

Tabela_socio <- as_flex_table(Tabela_socio)#converto em word

save_as_docx(Tabela_socio, path = "C:\Users\arsee\Desktop\TCC2024\Tabela_Socio.docx")
write.table(Tabela_socio, file = “Tabela_Socio.docx”, sep = “,”)#salvo na pasta

