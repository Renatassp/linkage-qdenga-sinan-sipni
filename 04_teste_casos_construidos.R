# Teste com casos construídos à mão: cada pessoa exercita uma regra
# (dois CNS com grafias diferentes do nome, registro de dose a menos de 90
# dias do anterior, nascida em 29/02, saída aos 15 anos, entrada aos 10 anos,
# notificação duplicada). As duas últimas linhas conferem que a tabela
# diária soma as mesmas pessoas-dia que os delta_dia_* individuais.
# COMO RODAR: abra este arquivo no RStudio e clique em "Source".
pasta_scripts <- "C:/Renata/Doutorado_e_EpiSUS/Saúde_Pública_FMRP/Belíssimo/Projeto_vacina_dengue/Doutorado/Script"
source(file.path(pasta_scripts, "01_funcoes_linkage.R"))
source(file.path(pasta_scripts, "02_funcoes_derivadas.R"))
set.seed(1)
cns_valido <- function() repeat {
  d <- c(7L, sample(0:9, 13, TRUE)); s <- sum(d * 15:2); last <- (11L - s %% 11L) %% 11L
  if (last < 10L) return(paste0(c(d, last), collapse = ""))
}
cx <- replicate(10, cns_valido())
vac <- data.table(
  nome_paciente = c("JOAO SILVA","João da Silva","MARIA SOUZA","MARIA SOUZA","ANA LIMA",
                    "LUIZA ROCHA","LUIZA ROCHA","LUIZA ROCHA","CARLOS DIAS","CARLOS DIAS","BRUNA MELO"),
  nome_mae = c("TEREZA SILVA","TEREZA SILVA","JOANA SOUZA","JOANA SOUZA","RITA LIMA",
               "CLARA ROCHA","CLARA ROCHA","CLARA ROCHA","EVA DIAS","EVA DIAS","LIA MELO"),
  data_nasc = c("10/05/2012","2012-05-10","20/02/2013","20/02/2013","15/01/2014",
                "29/02/2012","29/02/2012","29/02/2012","01/06/2009","01/06/2009","20/11/2015"),
  numero_sus = c(cx[1], cx[2], cx[3], cx[3], cx[4], cx[5], cx[5], cx[5], cx[6], cx[6], cx[7]),
  sexo = c("M","M","F","F","F","F","F","F","M","M","F"), mun = c(rep("3543402", 11)),
  dt_vacina = c("2024-03-01","2024-06-01","2024-04-01","2024-06-20","2024-05-01",
                "2024-03-10","2024-03-15","2024-06-15","2024-02-15","2024-05-20","2025-11-25"),
  dose = c("1ª Dose","2ª Dose","1ª Dose","2ª Dose","2ª Dose","1ª Dose","1ª Dose","2ª Dose","1ª Dose","2ª Dose","1ª Dose"))
sin <- data.table(
  nome_paciente = c("JOAO SILVA","PEDRO ALVES","PEDRO ALVES","LUIZA ROCHA"),
  nome_mae = c("TEREZA SILVA","MARTA ALVES","MARTA ALVES","CLARA ROCHA"),
  data_nasc = c("10/05/2012","01/07/2011","01/07/2011","29/02/2012"),
  numero_sus = c(NA, cx[8], cx[8], NA), sexo = c("M","M","M","F"), mun = "354340",
  DT_SIN_PRI = c("2024-08-10","2025-02-10","2025-02-20","2025-01-05"),
  DT_NOTIFIC = c("2024-08-12","2025-02-11","2025-02-21","2025-01-07"),
  DT_INTERNA = c("2024-08-13", NA, NA, NA), HOSPITALIZ = c("1","2","2","2"),
  CLASSI_FIN = c("12","11","11","10"), EVOLUCAO = "1", RESUL_PCR_ = c("1", NA, NA, NA),
  RESUL_NS1 = NA, RESUL_VI_N = NA)
cols <- list(nome="nome_paciente", mae="nome_mae", data="data_nasc", cns="numero_sus", sexo="sexo", mun="mun")
res <- ln_pipeline(sin, vac, limiar = 18, verbose = FALSE, col_a = cols, col_b = cols,
                   col_data_dedup_a = "DT_SIN_PRI", col_data_dedup_b = "dt_vacina")
cat("pares:\n"); print(res$pares[, .(ida, idb, escore = round(escore,1), n_campos)])
pes <- dv_unificar(res); print(pes)
doses <- dv_consolidar_doses(res$registros_b, pes, "dt_vacina"); print(doses)
jan <- dv_janela(pes); print(jan)
cols_s <- list(sin_pri="DT_SIN_PRI", notific="DT_NOTIFIC", interna="DT_INTERNA", classi="CLASSI_FIN",
               hospitaliz="HOSPITALIZ", evolucao="EVOLUCAO", pcr="RESUL_PCR_", ns1="RESUL_NS1", isolamento="RESUL_VI_N")
ev <- dv_eventos(res$registros_a, pes, cols_s)
bp <- dv_base_pessoas(pes, doses, jan, ev); print(bp[, .(pid, grupo_linkage, dt_d1, dt_d2, entrada, saida_adm,
   evento_principal, evento_confirmado, dias_seguimento, delta_dia_nao_vacinado, delta_dia_parcialmente_vacinado, delta_dia_vacinado)])
bn <- dv_base_notificacoes(ev, pes, doses, res$registros_a, cols_clinicas = c("CLASSI_FIN")); print(bn)
pe <- dv_primeiro_evento(ev, jan, "principal")
iv <- dv_intervalos(jan, doses, pe, 14L); print(iv)
cz <- dv_casos(jan, doses, pe, 14L); print(cz)
tab <- dv_tabela_diaria(iv, cz, c("sexo","idade")); print(tab[, .(pd_parc=sum(n_parcial), pd_vac=sum(n_vacinado),
   cnv=sum(casos_nao_vacinado), cp=sum(casos_parcial), cv=sum(casos_vacinado))])
tabi <- dv_tabela_diaria(iv, cz, c("sexo","idade"), "intervalos"); print(tabi$pessoas_dia)
# checagem de consistência: pessoas-dia da tabela = soma dos deltas individuais
cat("check parcial:", sum(tab$n_parcial), "vs", sum(bp$delta_dia_parcialmente_vacinado, na.rm=TRUE), "\n")
cat("check vacinado:", sum(tab$n_vacinado), "vs", sum(bp$delta_dia_vacinado, na.rm=TRUE), "\n")
