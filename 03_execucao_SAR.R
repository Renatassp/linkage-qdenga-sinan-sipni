#=======================================================================
# LINKAGE — SINAN (dengue) x SI-PNI (vacina Qdenga)
# VERSÃO DE TESTE com as bases fictícias (8 a 16 anos)

# Este é o script principal do processamento: integra as funções de linkage
# e de derivação, realiza o linkage SINAN x SI-PNI, constrói a população
# de estudo, define vacinação e eventos durante o seguimento, gera as tabelas
# de pessoa-tempo e produz as bases finais para análise e exportação.

#=======================================================================
# Não é preciso rodar o 01 e o 02 antes, nem usar setwd(): este script
# carrega os outros dois pelo caminho completo da pasta abaixo.
# Os resultados são gravados na subpasta "resultados_teste_linkage_v2",
# criada automaticamente dentro da pasta dos dados, para não sobrescrever
# arquivos de rodadas anteriores.
#
# Para rodar na SAR com os dados do Ministério, troque, na CONFIGURAÇÃO,
# a pasta, os nomes dos arquivos e os nomes das colunas de identificação,
# e apague o bloco "AVALIAÇÃO" da seção 2 (não existe id_pessoa_verdadeiro
# nos dados reais).
#=======================================================================

pasta_scripts <- "C:/Renata/Doutorado_e_EpiSUS/Saúde_Pública_FMRP/Belíssimo/Projeto_vacina_dengue/Doutorado/Script"
source(file.path(pasta_scripts, "01_funcoes_linkage.R"))
source(file.path(pasta_scripts, "02_funcoes_derivadas.R"))

#--------------------------------------------------------------------------# 0. CONFIGURAÇÃO
#-------------------------------------------------------------------------------
CFG <- list(
  pasta        = "C:/Renata/Doutorado_e_EpiSUS/Saúde_Pública_FMRP/Belíssimo/Projeto_vacina_dengue/Doutorado/Script",
  pasta_saida  = "resultados_teste_linkage_v2",   # subpasta criada dentro de 'pasta'
  arq_vacina   = "base_vacina_ficticia_para_teste_8a16anos.csv",
  arq_sinan    = "base_sinan_ficticia_para_teste_8a16anos.csv",
  # recorte pela data de nascimento, igual nas duas bases (na v1, a
  # vacina era filtrada pela idade na aplicação e o SINAN por ANO_NASC)
  nasc_min     = as.IDate("2008-01-01"),   # 8-16 anos em 2024 ou 2025
  nasc_max     = as.IDate("2017-12-31"),
  ini_seg      = as.IDate("2024-01-01"),
  fim_seg      = as.IDate("2025-12-31"),

  # nomes das colunas de identificação em cada base (bases fictícias)
  col_sinan  = list(nome = "nome_paciente", mae = "nome_mae", data = "data_nasc",
                    cns = "numero_sus", sexo = "CS_SEXO", mun = "ID_MN_RESI"),
  col_vacina = list(nome = "nome_paciente", mae = "nome_mae", data = "data_nasc",
                    cns = "numero_sus", sexo = "tp_sexo_paciente", mun = "co_municipio_paciente"),

  # Limiar calibrado na base de teste em escala real (Material Suplementar
  # S04.1). Não recalcular aqui: não há id_pessoa_verdadeiro nos dados reais.
  limiar       = 17,

  # Deduplicação temporal (mesmo episódio/dose dentro da janela) — Seção 2.
  # [CONFIRMAR] "DT_SIN_PRI" é o nome de campo padrão do SINAN para data dos
  # primeiros sintomas nos extratos do DATASUS; confirme contra o dicionário
  # de variáveis do extrato que vocês vão receber do Ministério.
  col_data_dedup_sinan  = "DT_SIN_PRI",
  col_data_dedup_vacina = "dt_vacina",
  janela_dedup_dias     = 90L,

  # campos usados na derivação (S06)
  col_vac_data  = "dt_vacina",
  col_vac_delet = "dt_deletado_rnds",
  col_sinan_ev  = list(sin_pri = "DT_SIN_PRI", notific = "DT_NOTIFIC", interna = "DT_INTERNA",
                       classi = "CLASSI_FIN", hospitaliz = "HOSPITALIZ", evolucao = "EVOLUCAO",
                       pcr = "RESUL_PCR_", ns1 = "RESUL_NS1", isolamento = "RESUL_VI_N"),
  # cols_clinicas e cols_vacina_export são definidas automaticamente depois
  # da leitura (fim da seção 1): todas as colunas, exceto identificadores,
  # datas exatas e códigos de registro.
  janelas         = c(10L, 14L, 21L),
  desfechos       = c("principal", "direto", "confirmado"),
  usar_marcadores = TRUE,

  # Não há filtro por fabricante: em 2024-2025 a Qdenga foi a única
  # vacina contra dengue no SUS, o código da vacina não distingue fabricantes
  # e o campo de fabricante é texto livre mal preenchido (seção 4.2).

  # Marco municipal (seção 4.2): municípios que ofertaram a vacina.
  marco = list(
    aplicar          = TRUE,                           # FALSE = sem restrição municipal
    col_mun          = "co_municipio_estabelecimento", # [CONFIRMAR] município da aplicação
    min_doses        = 1L,     # [DECIDIR] 1 = primeira dose (texto atual da seção 4.2)
    entrada_no_marco = FALSE   # FALSE = todos entram em 01/01/2024 (seguimento 2024-2025)
  ),

  tam_amostra_revisao       = 300L,   # pares aceitos sorteados para revisão manual
  tam_amostra_proximo_corte = 50L,    # pares logo abaixo do limiar, para checar o corte
  semente      = 2026L
)
p  <- function(...) file.path(CFG$pasta, ...)                    # leitura dos dados
dir.create(file.path(CFG$pasta, CFG$pasta_saida), showWarnings = FALSE)
ps <- function(...) file.path(CFG$pasta, CFG$pasta_saida, ...)   # gravação dos resultados
set.seed(CFG$semente)
relatorio <- data.table(etapa = character(), n = numeric())   # [V2-7]
reg <- function(etapa, n) {
  relatorio <<- rbind(relatorio, data.table(etapa = etapa, n = as.numeric(n)))
  cat(sprintf("  %-70s %s\n", etapa, format(n, big.mark = ",")))
}

#-------------------------------------------------------------------------------
# 1. LEITURA E RECORTE POR DATA DE NASCIMENTO NOS BANCOS REAIS   [V2-7]
#-------------------------------------------------------------------------------
cat("=== 1. LEITURA E RECORTE ===\n")

vacina_filtrada <- fread(p(CFG$arq_vacina), colClasses = "character")
reg("SI-PNI: registros lidos", nrow(vacina_filtrada))
if (CFG$col_vac_delet %in% names(vacina_filtrada)) {
  vacina_filtrada <- vacina_filtrada[is.na(get(CFG$col_vac_delet)) | get(CFG$col_vac_delet) == ""]
  reg("SI-PNI: após remover registros deletados na RNDS", nrow(vacina_filtrada))
}
vacina_filtrada[, .dn := ln_parse_data(get(CFG$col_vacina$data))]
vacina_filtrada <- vacina_filtrada[!is.na(.dn) & .dn >= CFG$nasc_min & .dn <= CFG$nasc_max][, .dn := NULL]
reg("SI-PNI: registros no recorte de nascimento", nrow(vacina_filtrada))

sinan_filtrado <- fread(p(CFG$arq_sinan), colClasses = "character")
reg("SINAN: registros lidos", nrow(sinan_filtrado))
sinan_filtrado[, .dn := ln_parse_data(get(CFG$col_sinan$data))]
sinan_filtrado <- sinan_filtrado[!is.na(.dn) & .dn >= CFG$nasc_min & .dn <= CFG$nasc_max][, .dn := NULL]
reg("SINAN: registros no recorte de nascimento", nrow(sinan_filtrado))
if (!nrow(vacina_filtrada) || !nrow(sinan_filtrado))
  stop("Recorte zerou uma das bases — confira a coluna de data de nascimento.")

gc()

# Variáveis que acompanham os bancos exportados: todas as colunas de cada
# base, exceto identificadores, datas exatas (saem só como mês ou
# intervalos), a unidade notificadora e códigos de registro/documento.
CFG$cols_clinicas <- setdiff(names(sinan_filtrado),
  c(unlist(CFG$col_sinan), "id_pessoa_verdadeiro", "ID_UNIDADE", "arquivo_origem",
    grep("^DT_", names(sinan_filtrado), value = TRUE)))
CFG$cols_vacina_export <- setdiff(names(vacina_filtrada),
  c(unlist(CFG$col_vacina), "id_pessoa_verdadeiro", "co_documento", "co_paciente", "arquivo_origem",
    grep("^dt_", names(vacina_filtrada), value = TRUE)))

#--------------------------------------------------------------------------
# 2. LINKAGE
#--------------------------------------------------------------------------
cat("\n=== 2. LINKAGE ===\n")
res <- ln_pipeline(sinan_filtrado, vacina_filtrada, limiar = CFG$limiar, verbose = TRUE,
                    col_a = CFG$col_sinan, col_b = CFG$col_vacina,          
                    col_data_dedup_a = CFG$col_data_dedup_sinan,            # SINAN: data 1os sintomas
                    col_data_dedup_b = CFG$col_data_dedup_vacina,           # SI-PNI: dt_vacina
                    janela_dedup_dias = CFG$janela_dedup_dias)
reg("SINAN: pessoas", nrow(res$pessoas_a))
reg("SI-PNI: pessoas", nrow(res$pessoas_b))
reg("Pares aceitos", nrow(res$pares))
reg("SINAN: % de registros com CNS válido", round(100 * mean(!is.na(res$registros_a$cns)), 1))
reg("SI-PNI: % de registros com CNS válido", round(100 * mean(!is.na(res$registros_b$cns)), 1))

#--------------------------------------------------------------------------
# AVALIAÇÃO — SÓ NO TESTE COM DADOS FICTÍCIOS (apagar para os dados reais)
#--------------------------------------------------------------------------
# esperados = pessoas cujo id_pessoa_verdadeiro aparece nas duas bases
cat("\n=== AVALIAÇÃO DO LINKAGE (id_pessoa_verdadeiro) ===\n")
avaliacao <- ln_avaliar(res$pares, res$pessoas_a, res$pessoas_b, prefixo_comum = "")
perdas    <- ln_diagnosticar_perdas(res$pares, res$comparacoes, res$pessoas_a, res$pessoas_b,
                                    prefixo_comum = "")
print(avaliacao); print(perdas)
fwrite(cbind(avaliacao, perdas), ps("avaliacao_linkage_teste.csv"), sep = ";")

#--------------------------------------------------------------------------
# 3. VALIDAÇÃO (sem verdade conhecida — dados reais não têm id_pessoa_verdadeiro)
#--------------------------------------------------------------------------
cat("\n=== 3. VALIDAÇÃO ===\n")

# 3.1 Distribuição do escore e do número de campos comparados entre os
# pares aceitos. Não substitui a revisão manual, mas mostra o quão perto
# do limiar a decisão ficou e se algum campo está sistematicamente ausente.
cat("Distribuição do escore entre os pares aceitos:\n")
print(summary(res$pares$escore))
cat("\nPares aceitos por número de campos efetivamente comparados:\n")
print(res$pares[, .N, by = n_campos])

# m e u empíricos x usados no escore
mu <- ln_estimar_mu(res$pessoas_a, res$pessoas_b)
print(mu)
fwrite(mu, ps("diagnostico_m_u.csv"), sep = ";")

# 3.2 Amostras para revisão manual: pares aceitos, e pares logo abaixo do
# limiar (para checar se o corte não está descartando pares verdadeiros).
# Isso é o substituto de sensibilidade/VPP quando não há id_pessoa_verdadeiro
# para comparar. [V2-7] A amostra de aceitos é estratificada por quartil de
# escore, para não concentrar a revisão nos pares óbvios.
amostra_aceitos <- copy(res$pares)
amostra_aceitos[, faixa := ceiling(4 * frank(escore, ties.method = "first") / .N)]
amostra_aceitos <- amostra_aceitos[, .SD[sample(.N, min(.N, ceiling(CFG$tam_amostra_revisao / 4)))],
                                   by = faixa]

proximos_ao_corte <- res$comparacoes[
  escore >= CFG$limiar - 2 & escore < CFG$limiar & n_campos >= 2
][sample(.N, min(CFG$tam_amostra_proximo_corte, .N))]

fwrite(amostra_aceitos,   ps("revisao_manual_pares_aceitos.csv"),     sep = ";")
fwrite(proximos_ao_corte, ps("revisao_manual_proximos_ao_corte.csv"), sep = ";")
cat(sprintf("Amostras gravadas para revisão manual: %d pares aceitos, %d próximos ao corte.\n",
            nrow(amostra_aceitos), nrow(proximos_ao_corte)))

#--------------------------------------------------------------------------
# 4. VARIÁVEIS DERIVADAS (Material Suplementar S06)
#--------------------------------------------------------------------------
cat("\n=== 4. VARIÁVEIS DERIVADAS ===\n")
pessoas <- dv_unificar(res)
reg("Pessoas unificadas", nrow(pessoas))

reg_b_qdenga <- res$registros_b   # todos os registros de vacina contra dengue (Qdenga)

# Marco municipal e restrição aos municípios ofertantes (seção 4.2)
marco <- NULL
if (isTRUE(CFG$marco$aplicar) && !CFG$marco$col_mun %in% names(reg_b_qdenga)) {
  warning("Coluna ", CFG$marco$col_mun, " ausente: marco municipal NÃO aplicado.")
} else if (isTRUE(CFG$marco$aplicar)) {
  marco <- dv_marco_municipal(reg_b_qdenga, CFG$col_vac_data, CFG$marco$col_mun,
                              ini = CFG$ini_seg, fim = CFG$fim_seg, min_doses = CFG$marco$min_doses)
  fwrite(marco, ps("tabela_marco_municipal.csv"), sep = ";")   # agregada: sai da SAR
  reg("Municípios ofertantes (com marco)", nrow(marco))
}
for (g in 1:3) reg(sprintf("  grupo_linkage = %d", g), pessoas[grupo_linkage == g, .N])

doses <- dv_consolidar_doses(reg_b_qdenga, pessoas, CFG$col_vac_data)
for (f in grep("^flag_", names(doses), value = TRUE)) reg(paste("Doses:", f), sum(doses[[f]]))

janela <- dv_janela(pessoas, CFG$ini_seg, CFG$fim_seg, marco = marco,
                    entrada_no_marco = isTRUE(CFG$marco$entrada_no_marco))
reg("Pessoas com tempo elegível (10-14 anos no período)", nrow(janela))
if (!is.null(marco)) {
  j0 <- dv_janela(pessoas, CFG$ini_seg, CFG$fim_seg)
  reg("  fora de município ofertante ou sem município de residência (excluídas)", nrow(j0) - nrow(janela))
}

eventos <- dv_eventos(res$registros_a, pessoas, CFG$col_sinan_ev, CFG$usar_marcadores)
base_pessoas <- dv_base_pessoas(pessoas, doses, janela, eventos, w = 14L)
base_notif   <- dv_base_notificacoes(eventos, pessoas, doses, res$registros_a, w = 14L,
                                     cols_clinicas = CFG$cols_clinicas)

# Tabelas agregadas (Quadro S06.4): nacional diária (modelo principal) e
# municipal em períodos constantes (modelos estratificados)
for (w in CFG$janelas) for (d in CFG$desfechos) {
  pe  <- dv_primeiro_evento(eventos, janela, d)
  iv  <- dv_intervalos(janela, doses, pe, w)
  cz  <- dv_casos(janela, doses, pe, w)
  nac <- dv_tabela_diaria(iv, cz, c("sexo", "idade"), "diario", CFG$fim_seg)
  mun <- dv_tabela_diaria(iv, cz, c("mun", "sexo", "idade"), "intervalos", CFG$fim_seg)
  fwrite(nac,             ps(sprintf("tabela_diaria_nacional_j%02d_%s.csv", w, d)), sep = ";")
  fwrite(mun$pessoas_dia, ps(sprintf("tabela_municipal_pessoasdia_j%02d_%s.csv", w, d)), sep = ";")
  fwrite(mun$casos,       ps(sprintf("tabela_municipal_casos_j%02d_%s.csv", w, d)), sep = ";")
  stopifnot(sum(nac$n_parcial) + sum(nac$n_vacinado) == sum(iv$dias))   # checagem interna
  reg(sprintf("Casos (janela %d, %s)", w, d), nrow(cz))
}

#--------------------------------------------------------------------------
# 5. SAÍDAS
#--------------------------------------------------------------------------
cat("\n=== 5. GRAVANDO RESULTADOS ===\n")

fwrite(res$pares, ps("linkage_pares_reais_8a16anos.csv"), sep = ";")

sinan_reg <- copy(res$registros_a)
sinan_reg[, pareado := id_paciente %in% res$pares$ida]
fwrite(sinan_reg[pareado == FALSE], ps("sinan_nao_pareados_reais_8a16anos.csv"), sep = ";")

vacina_reg <- copy(res$registros_b)
vacina_reg[, pareado := id_paciente %in% res$pares$idb]
fwrite(vacina_reg[pareado == FALSE], ps("vacina_nao_pareados_reais_8a16anos.csv"), sep = ";")

# Bases do S06: versões internas (datas exatas, ficam na SAR) e
# versões para exportação (código aleatório, datas no nível de mês, sem
# data de nascimento). A tabela de correspondência fica na SAR.
fwrite(base_pessoas, ps("interno_base_pessoas.csv"), sep = ";")
fwrite(base_notif,   ps("interno_base_notificacoes.csv"), sep = ";")
correspondencia <- data.table(pid = pessoas$pid, id_estudo = sample(nrow(pessoas)))
fwrite(merge(correspondencia, pessoas[, .(pid, ida, idb)], by = "pid"),
       ps("interno_correspondencia_ids.csv"), sep = ";")

# Os três bancos para exportação: sem identificadores, código aleatório no
# lugar do ID, datas no nível de mês. Cada pessoa aparece em um só banco.
attr_doses <- dv_atributos_doses(reg_b_qdenga, pessoas, doses, CFG$col_vac_data, CFG$cols_vacina_export)
bancos <- dv_bancos_exportacao(base_pessoas, base_notif, attr_doses, CFG$desfechos)
fwrite(dv_preparar_exportacao(bancos$pareados, correspondencia),
       ps("export_banco1_pareados.csv"), sep = ";")
fwrite(dv_preparar_exportacao(bancos$vacinados_sem_notificacao, correspondencia),
       ps("export_banco2_vacinados_sem_notificacao.csv"), sep = ";")
fwrite(dv_preparar_exportacao(bancos$notificados_sem_vacina, correspondencia),
       ps("export_banco3_notificados_sem_vacina.csv"), sep = ";")
reg("Banco 1 (pareados): pessoas",        uniqueN(bancos$pareados$pid))
reg("Banco 1 (pareados): notificações",   nrow(bancos$pareados))
reg("Banco 2 (vacinados sem notificação): pessoas", nrow(bancos$vacinados_sem_notificacao))
reg("Banco 3 (notificados sem vacina): pessoas",      uniqueN(bancos$notificados_sem_vacina$pid))
reg("Banco 3 (notificados sem vacina): notificações", nrow(bancos$notificados_sem_vacina))
# checagem: cada pessoa em um e só um banco
stopifnot(uniqueN(bancos$pareados$pid) + nrow(bancos$vacinados_sem_notificacao) +
          uniqueN(bancos$notificados_sem_vacina$pid) == nrow(pessoas))
fwrite(relatorio, ps("relatorio_processamento.csv"), sep = ";")

cat("\nPROCESSAMENTO CONCLUÍDO. Resultados em:", ps(), "\n")
cat(sprintf("Pares aceitos: %s | escore mediano: %.1f\n",
            format(nrow(res$pares), big.mark = ","), median(res$pares$escore)))

#--------------------------------------------------------------------------
# LEMBRETES
#-------------------------------------------------------------------------
# - NÃO chame ln_avaliar(), ln_diagnosticar_perdas() nem ln_calibrar_limiar()
#   neste script: as três dependem de id_pessoa_verdadeiro, que só existe na
#   simulação. Recalibrar o limiar exige rodar de novo a simulação com
#   bancos fictícios na escala equivalente, não uma varredura aqui.
# - Depois da revisão manual e da decisão final dos pares, remova nome, nome
#   da mãe e CNS das bases antes de qualquer exportação para fora da SAR —
#   só id_paciente, as variáveis epidemiológicas/administrativas e o
#   indicador de pareamento (`pareado`) devem sair.
# - Confirme os nomes reais das colunas de identificação (nome do paciente,
#   nome da mãe, data de nascimento, CNS) nos extratos do Ministério antes
#   de rodar — os defaults de ln_preparar() foram herdados do gerador
#   fictício e podem não bater com o dicionário de variáveis real.
# - Arquivos "interno_*", "linkage_pares*", "*_nao_pareados*" e
#   "revisao_*" contêm dados identificáveis ou datas exatas e NÃO saem da
#   SAR. As tabelas agregadas saem conforme as regras de divulgação da SAR
#   (verificar exigência de tamanho mínimo de célula).
