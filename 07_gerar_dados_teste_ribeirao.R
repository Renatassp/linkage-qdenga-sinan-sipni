#=======================================================================
# GERA DADOS FICTÍCIOS NO FORMATO ESPERADO DO HYGIA E DO SINAN MUNICIPAL
# (opcional) — para testar o 06_execucao_ribeirao.R antes de enviá-lo à SMS.
# 3.000 pessoas no cadastro (100 com cadastro duplicado e outro CNS), 1.200
# vacinadas (30 com a 2ª dose no cadastro duplicado), 510 notificadas
# (60 fora do Hygia), com erros de digitação. Grava em 'pasta_dados'.
# Depois, no 06, aponte 'pasta' para 'pasta_dados' e use
# arq_cep_setor = "cep_setor.csv".
#=======================================================================
pasta_scripts <- "C:/CAMINHO/DOS/SCRIPTS"              # ajuste
pasta_dados   <- file.path(pasta_scripts, "dados_teste_ribeirao")
dir.create(pasta_dados, showWarnings = FALSE)
source(file.path(pasta_scripts, "01_funcoes_linkage.R")); set.seed(7)
cnsv <- function() repeat { d <- c(7L, sample(0:9, 13, TRUE)); l <- (11L - sum(d*15:2) %% 11L) %% 11L; if (l < 10L) return(paste0(c(d,l), collapse="")) }
n <- 3000
P <- data.table(verd = sprintf("V%05d", 1:n),
  nome = paste(sample(c("ANA","JOAO","MARIA","PEDRO","LUIZA","CARLOS","BRUNA","JOSE","RAFAEL","JULIA"), n, TRUE),
               sample(c("SILVA","SOUZA","LIMA","ROCHA","DIAS","MELO","COSTA"), n, TRUE),
               sample(c("ALVES","NUNES","PIRES","GOMES","RAMOS"), n, TRUE)),
  mae = paste(sample(c("TEREZA","JOANA","RITA","CLARA","EVA","LIA","SONIA"), n, TRUE), sample(c("SILVA","SOUZA","LIMA","ROCHA"), n, TRUE)),
  dn = as.IDate("2008-01-01") + sample(0:3650, n, TRUE), sexo = sample(c("M","F"), n, TRUE))
P[, cns := replicate(.N, cnsv())]
P[, `:=`(cep = sprintf("140%05d", sample(1:40, .N, TRUE)), distrito = sample(c("Norte","Sul","Leste","Oeste","Central"), .N, TRUE))]
# cadastro (100 pessoas com cadastro duplicado, CNS diferente)
cad <- P[, .(ID_PACIENTE = sprintf("H%06d", .I), NOME = nome, NOME_MAE = mae, DATA_NASCIMENTO = format(dn, "%d/%m/%Y"),
             CNS = cns, SEXO = sexo, CEP = cep, DISTRITO_SAUDE = distrito,
             DATA_ULTIMO_REGISTRO = format(as.IDate("2021-01-01") + sample(0:1700, .N, TRUE)),
             DATA_OBITO = NA_character_, verd)]
dup <- cad[1:100][, `:=`(ID_PACIENTE = sprintf("D%06d", .I), CNS = replicate(.N, cnsv()))]
cad <- rbind(cad, dup)
cad[3001:3002, DATA_OBITO := "2024-08-01"]
# vacinas: Qdenga para 1:1200 (D1 e D2; para 1:30 a D2 no cadastro duplicado), HPV/ACWY
q <- cad[verd %in% P$verd[1:1200] & substr(ID_PACIENTE,1,1) == "H"]
d1 <- as.IDate("2024-02-15") + sample(0:300, nrow(q), TRUE)
vac <- rbind(q[, .(ID_PACIENTE, COD_VACINA = "QDENGA", DATA_APLICACAO = format(d1), LOTE = "L1")],
             q[, .(ID_PACIENTE, COD_VACINA = "QDENGA", DATA_APLICACAO = format(d1 + 100L), LOTE = "L2")])
vac[COD_VACINA == "QDENGA" & ID_PACIENTE %in% sprintf("H%06d", 1:30) & DATA_APLICACAO > "2024-05-25",
    ID_PACIENTE := sub("^H", "D", ID_PACIENTE)]     # D2 registrada no cadastro duplicado
h <- cad[substr(ID_PACIENTE,1,1) == "H"][sample(.N, 1500)]
vac <- rbind(vac, h[, .(ID_PACIENTE, COD_VACINA = "HPV", DATA_APLICACAO = format(as.IDate("2022-01-01") + sample(0:900, .N, TRUE)), LOTE = "X")])
# SINAN: 200 vacinados, 250 não vacinados do Hygia, 60 fora do Hygia; 30 com notificação prévia
fora <- data.table(verd = sprintf("F%04d", 1:60), nome = paste("FORA", sample(c("SILVA","LIMA"), 60, TRUE), 1:60),
                   mae = "MAE FORA", dn = as.IDate("2010-01-01") + 1:60, sexo = "M", cns = NA_character_)
alvo <- rbind(P[c(1:200, 1301:1550), .(verd, nome, mae, dn, sexo, cns)], fora)
s <- alvo[, .(NM_PACIENT = nome, NM_MAE_PAC = mae, DT_NASC = format(dn), ID_CNS_SUS = ifelse(runif(.N) < .5, cns, NA),
              CS_SEXO = sexo, ID_MN_RESI = "354340", verd,
              DT_SIN_PRI = format(as.IDate("2024-02-01") + sample(0:650, .N, TRUE)))]
prev <- s[sample(.N, 30)][, DT_SIN_PRI := format(as.IDate("2019-03-01") + sample(0:1000, .N, TRUE))]
s <- rbind(s, prev)
s[, DT_NOTIFIC := format(as.IDate(DT_SIN_PRI) + 2L)]
s[, `:=`(CLASSI_FIN = sample(c("10","11","12","5"), .N, TRUE, prob = c(.5,.2,.05,.25)), CRITERIO = "1",
         HOSPITALIZ = sample(c("1","2"), .N, TRUE, prob = c(.2,.8)), DT_INTERNA = NA, EVOLUCAO = "1",
         RESUL_PCR_ = sample(c("1", NA), .N, TRUE), RESUL_NS1 = NA, RESUL_VI_N = NA, ALRM_HIPOT = "2", ID_UNIDADE = "99")]
s[1:25, NM_PACIENT := sub("A", "E", NM_PACIENT)]
fwrite(cad[, !"verd"], file.path(pasta_dados, "hygia_cadastro.csv")); fwrite(vac, file.path(pasta_dados, "hygia_vacinas.csv"))
fwrite(s[, !"verd"], file.path(pasta_dados, "sinan_dengue_rp_2015_2025.csv"))
fwrite(data.table(cd_setor = sprintf("S%02d", 1:40), ivs_cat = sample(1:5, 40, TRUE)), file.path(pasta_dados, "ivs_setor.csv"))
fwrite(data.table(cep = sprintf("140%05d", 1:40), cd_setor = sprintf("S%02d", 1:40)), file.path(pasta_dados, "cep_setor.csv"))
fwrite(cad[, .(ID_PACIENTE, verd)], file.path(pasta_dados, "verdade_cad.csv")); fwrite(s[, .(DT_SIN_PRI, NM_PACIENT, verd)], file.path(pasta_dados, "verdade_sinan.csv"))
