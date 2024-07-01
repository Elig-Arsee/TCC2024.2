### DATA WRANGLING
# Instalar pacotes necessários
#install.packages("tidyverse")

# Carregar pacotes
library(tidyverse)
library(dplyr)

# Carregar os dados
base2018 <- read.csv2("2018_turno2_decod.csv", sep = ",", fileEncoding = "UTF-8", stringsAsFactors = TRUE)

# Visualizando primeiras linhas do dataset
head(base2018)

# Renomeando a variável dependente
base2018 <- base2018 %>%
  rename(voto_presidente = `Q12P2.B..Em.quem.o.a..sr.a..votou.para.presidente.no.segundo.turno.`)

glimpse(base2018)

# Renomeando todas as outras variáveis pra ficar um pouco mais simples
base2018 <- base2018 %>%
  rename(regiao = `região`,
         sexo = `D2_sexo`,
         faixa_etaria = `D1A_faixa_etaria`,
         escolaridade = `D3_escolaridade`,
         faixa_renda = `D9B_faixa_renda`,
         cor_raca = `D12a_cor_raca`,
         situacao_profissional = `D.07_situacao_profissional`,
         avaliacao_atuacao_PoliciaFederal = `P3.3..De.maneira.geral..como.o.a..sr.a..avalia.a.atuação..Da.Polícia.Federal`,
         avaliacao_atuacao_GovernoFederal = `P3.4..De.maneira.geral..como.o.a..sr.a..avalia.a.atuação..Do.Governo.Federal`,
         avaliacao_atuacao_PoderJudiciario = `P3.5..De.maneira.geral..como.o.a..sr.a..avalia.a.atuação..Da.Justiça..Poder.Judiciário`,
         avaliacao_atuacao_PartidosPoliticos = `P3.7..De.maneira.geral..como.o.a..sr.a..avalia.a.atuação..Dos.Partidos.Políticos`,
         avaliacao_atuacao_CongressoNacionalCamaraDeputados = `P3.8..De.maneira.geral..como.o.a..sr.a..avalia.a.atuação..Do.Congresso.Nacional..Senado.e.Câmara.dos.Deputados.`,
         avaliacao_atuacao_ForcasArmadas = `P3.9..De.maneira.geral..como.o.a..sr.a..avalia.a.atuação..Dos.Militares..Forças.Armadas`,
         avaliacao_atuacao_MinisterioPublico = `P3.10..De.maneira.geral..como.o.a..sr.a..avalia.a.atuação..Do.Ministério.Público`,
         avaliacao_atuacao_Redeglobo = `P3.11..De.maneira.geral..como.o.a..sr.a..avalia.a.atuação..Da.Rede.Globo`,
         avaliacao_atuacao_OutrasEmissorasTV = `P3.12..De.maneira.geral..como.o.a..sr.a..avalia.a.atuação..Das.outras.emissoras.de.televisão..como.a.Record..SBT..etc.`,
         nota_satisfacao_PoliticaPublica_ControleCriminalidade = `P6.1..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Controle.da.criminalidade`,
         nota_satisfacao_PoliticaPublica_AcessoTratamentoSaude = `P6.2..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Acesso.a.tratamento.de.saúde`,
         nota_satisfacao_PoliticaPublica_DiminuicaoDesigualdadeSocial = `P6.3..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Diminuição.das.desigualdades.sociais`,
         nota_satisfacao_PoliticaPublica_QualidadeEnsinoPublico = `P6.4..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Qualidade.do.ensino.público`,
         nota_satisfacao_PoliticaPublica_AcessoEnsinoProfissionalizante = `P6.5..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Acesso.ao.ensino.profissionalizante`,
         nota_satisfacao_PoliticaPublica_ControleViolenciaPolicial = `P6.6..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Controle.da.violência.policial`,
         nota_satisfacao_PoliticaPublica_AcessoEnsinoMedio = `P6.7..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Acesso.ao.Ensino.Médio`,
         nota_satisfacao_PoliticaPublica_AcessoEnsinoSuperior = `P6.8..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Acesso.ao.ensino.superior`,
         nota_satisfacao_PoliticaPublica_AcessoCultura = `P6.9..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Acesso.à.cultura..museus..cinemas..teatros..shows..etc.`,
         nota_satisfacao_PoliticaPublica_DiminuicaoDesemprego = `P6.10..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Diminuição.do.desemprego`,
         nota_satisfacao_PoliticaPublica_AcessoJustica = `P6.11..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Acesso.do.cidadão.comum.à.Justiça`,
         nota_satisfacao_PoliticaPublica_AcessoDireitosMinorias = `P6.12..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Acesso.a.políticas.de.direitos.de.mulheres.e.de.minorias.como..Negros..indígenas..homossexuais..LGBTs..entre.outros.`,
         nota_satisfacao_PoliticaPublica_RespeitoLeis = `P6.13..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Respeito.às.leis`,
         nota_satisfacao_PoliticaPublica_AcessoRemedios = `P6.14..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Acesso.a.remédios`,
         nota_satisfacao_PoliticaPublica_AcessoTratamentoMedicoEspecializado = `P6.15..Vou.citar.algumas.políticas.e.gostaria.que.o.a..sr.a..dizesse.o.quanto.está.satisfeito.com.cada.uma.delas..Acesso.a.tratamento.médico.especializado`,
         nota_corrupcao_eficiencia_PoliciaFederal = `P16.1..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..Policia.Federal`,
         nota_corrupcao_eficiencia_MinisterioPublico = `P16.2..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..Ministério.Público`,
         nota_corrupcao_eficiencia_Judiciario = `P16.3..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..Judiciário`,
         nota_corrupcao_eficiencia_DefensoriaPublica = `P16.4..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..Defensoria.Pública`,
         nota_corrupcao_eficiencia_TCU = `P16.5..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..Tribunais.de.Contas.da.União..TCU.`,
         nota_corrupcao_eficiencia_CGU = `P16.6..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..Controladoria.Geral.da.União..CGU.`,
         nota_corrupcao_eficiencia_OGU = `P16.7..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..Ouvidoria.Geral.da.União`,
         nota_corrupcao_eficiencia_CPIs = `P16.8..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..CPIs`,
         nota_corrupcao_eficiencia_TSE = `P16.9..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..Tribunal.Superior.Eleitoral..TSE.`,
         nota_corrupcao_eficiencia_CNJ = `P16.10..Com.relação.à.corrupção.no.Brasil.gostaria.que.o.a..sr.a..desse.uma.nota.de.1.a.10.sobre.a.eficiência.das.instituições..Que.nota.o.a..sr.a..dá.para..Conselho.Nacional.de.Justiça`,
         opiniao_PrisaoSegundaInstanciaCombateCorrupcao = `P17.5..Em.relação.às.afirmações.lidas..gostaria.que.o.a..sr.a..dissesse.se.concorda.ou.discorda.de.cada.uma.delas..A.prisão.em.segunda.instância.combate.a.corrupção`,
         opiniao_DelacaoPremiadaAjudaCorruptos = `P17.6..Em.relação.às.afirmações.lidas..gostaria.que.o.a..sr.a..dissesse.se.concorda.ou.discorda.de.cada.uma.delas..A.delação.premiada.ajuda.os.corruptos`,
         opiniao_ConducaoCoercitivaFereDireitos = `P17.7..Em.relação.às.afirmações.lidas..gostaria.que.o.a..sr.a..dissesse.se.concorda.ou.discorda.de.cada.uma.delas..A.condução.coercitiva.fere.direitos`,
         opiniao_LavaJato = `P19..A.operação.Lava.Jato.é.uma.operação.para.combater.a.corrupção.na.política..Sobre.a.operação.Lava.Jato..o.a..sr.a..acredita.que.`,
         opiniao_LavaJato_CombateCorrupcao = `P20..Na.sua.opinião..a.Operação.Lava.Jato.combate.ou.não.combate.a.corrução.`,
         interesse_politica = `Q1..Quanto.o.a..sr.a..se.interessa.por.política..O.a..sr.a..diria.que.é.`,
         intensidade_acompanha_politica = `Q2..E.com.qual.intensidade.o.a..sr.a..acompanha.política.na.TV..no.rádio..nos.jornais.ou.na.internet..Muita.intensidade..alguma.intensidade..pouca.intensidade..ou.não.acompanha.política.`,
         entende_problemas_policos_importantes = `Q3..O.quanto.o.a..sr.a..concorda.com.a.seguinte.afirmação...Você.entende.sobre.os.problemas.políticos.mais.importantes.do.país..`,
         representacao_partidaria = `Q10A..Existe.algum.partido.político.que.representa.a.maneira.como.o.a..sr.a..pensa...Espontânea.`,
         partido_proximo = `Q22C..Qual.partido.o.a..sr.a..mais.se.sente.próximo...Espontânea.`,
         filiacao_partidaria = `D5B..Agora.gostaria.de.saber.se.o.a..sr.a..é.filiado.a.algum.partido.político.`,
         meio_informacao_politica = `P25...CARTELA.6..Qual.destes.meios.o.a..sr.a..mais.utiliza.para.se.informar.sobre.política.`,
         rede_social_informacao_politica = `P26...CARTELA.7..Qual.destas.redes.sociais.o..a..sr.a..mais.utiliza.para.se.informar.sobre.política.`,
         acao_decisao_voto = `P27...CARTELA.8..Aqui.estão.algumas.ações.que.as.pessoas.consideram.importantes.para.decidir.o.voto.para.presidente..Nesta.eleição..qual.delas.foi.a.mais.importante.para.o.a..sr.a..decidir.o.seu.voto.para.presidente.`)

# Visualização
glimpse(base2018)

# Selecionando categorias mais recorrente como referência
#Estabeleço a ordem das categorias para a variável dependente Voto presidente
summary(base2018$voto_presidente)

base2018$voto_presidente <- factor(base2018$voto_presidente,
                                           levels=c("Jair Bolsonaro (PSL)", "Fernando Haddad (PT)", "Anulou o voto","Votou em branco"))
summary(base2018$voto_presidente)

## Faixa etária
summary(base2018$faixa_etaria)

levels(base2018$faixa_etaria) #Verifico níveis
table(base2018$faixa_etaria) # Verifico ocorrências de cada nível
base2018$base2018$faixa_etaria <- relevel(base2018$faixa_etaria,
                                    ref = "25 a 34 anos")#seleciono categoria mais recorrente como referencia

summary(base2018$faixa_etaria)

# Sexo
summary(base2018$sexo) #Não precisa estabelecer referência porque feminino já é mais recorrente

# Escolaridade
summary(base2018$escolaridade)

base2018$escolaridade <- relevel(base2018$escolaridade,
                                    ref = "Fundamental incompleto")

summary(base2018$escolaridade)

# Faixa de renda
summary(base2018$faixa_renda)

base2018$faixa_renda <- relevel(base2018$faixa_renda,
                                 ref = "Mais de  2 até 5 salários mínimos (R$ 1.908,00 até R$ 4.770,00)")

summary(base2018$faixa_renda)

# Cor/Raça
summary(base2018$cor_raca)

base2018$cor_raca <- relevel(base2018$cor_raca,
                                ref = "Não branco")

summary(base2018$cor_raca)

# Situação Profissional
summary(base2018$situacao_profissional)

base2018$situacao_profissional <- relevel(base2018$situacao_profissional,
                                          ref = "Empregado")

summary(base2018$situacao_profissional)
