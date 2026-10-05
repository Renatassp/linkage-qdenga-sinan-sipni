#=======================================================================
# EXECUÇÃO — ANÁLISE DE SENSIBILIDADE LOCAL, RIBEIRÃO PRETO/SP
# Executado pela Secretaria Municipal da Saúde (SMS-RP), no ambiente da
# Prefeitura. Seção 4.7.5 e Material Suplementar S07 do projeto.
#=======================================================================
# COMO RODAR (no RStudio):
#   1. Deixe na mesma pasta: 01_funcoes_linkage.R, 02_funcoes_derivadas.R,
#      05_funcoes_ribeirao.R e este arquivo.
#   2. Ajuste 'pasta_scripts' e a CONFIGURAÇÃO abaixo (itens [CONFIRMAR]).
#   3. Abra este arquivo e clique em "Source".
#
# O linkage e as variáveis derivadas usam, SEM ALTERAÇÃO, o mesmo código da
# análise nacional (01 e 02), para que a comparação entre o denominador
# nominal e o denominador estimado reflita só a diferença de denominador:
#   ETAPA A (espelho da análise nacional): SINAN x doses de Qdenga do Hygia
#           -> grupos 1 (notificados e vacinados), 2 (vacinados sem
#              notificação) e 3 (notificados sem vacina)
#   ETAPA B (exclusiva desta análise): grupo 3 x cadastro dos não vacinados
#           -> 3a (no cadastro), 3b (fora do cadastro) e 4 (não vacinados
#              sem notificação)
#
# SAÍDAS: subpasta 'pasta_saida'.
#   export_*  e  tabela_*  -> podem ser enviados à equipe de pesquisa
#   interno_* e  revisao_* -> NÃO saem da Prefeitura (identificáveis)
#=======================================================================

pasta_scripts <- "C:/CAMINHO/DOS/SCRIPTS"              # [CONFIRMAR]
source(file.path(pasta_scripts, "01_funcoes_linkage.R"))
source(file.path(pasta_scripts, "02_funcoes_derivadas.R"))
source(file.path(pasta_scripts, "05_funcoes_ribeirao.R"))

#-------------------------------------------------------------------------------
# 0. CONFIGURAÇÃO
#-------------------------------------------------------------------------------
CFG <- list(
  pasta        = "C:/CAMINHO/DOS/DADOS",               # [CONFIRMAR]
  pasta_saida  = "resultados_ribeirao",
  arq_cadastro = "hygia_cadastro.csv",                 # [CONFIRMAR] 1 linha por cadastro
  arq_vacinas  = "hygia_vacinas.csv",                  # [CONFIRMAR] 1 linha por dose aplicada
  arq_sinan    = "sinan_dengue_rp_2015_2025.csv",      # [CONFIRMAR] notificações de residentes
  arq_ivs      = "ivs_setor.csv",                      # fornecido pela equipe: cd_setor; ivs_cat
  arq_cep_setor = NULL,   # opcional: "cep_setor_cnefe2022.csv" (cep; cd_setor), se só houver CEP
  cod_mun      = "354340",                             # Ribeirão Preto (IBGE, 6 dígitos)

  # [CONFIRMAR] nomes das colunas no Hygia (dicionário de dados da SMS)
  col_cad = list(id = "ID_PACIENTE", nome = "NOME", mae = "NOME_MAE", data = "DATA_NASCIMENTO",
                 cns = "CNS", sexo = "SEXO", cep = "CEP", setor = NULL,
                 distrito = "DISTRITO_SAUDE", dt_ultimo = "DATA_ULTIMO_REGISTRO",
                 dt_obito = "DATA_OBITO"),
  col_vac = list(id = "ID_PACIENTE", vacina = "COD_VACINA", data = "DATA_APLICACAO"),
  cod_qdenga = c("QDENGA"),                            # [CONFIRMAR] códigos das vacinas no Hygia
  cod_hpv    = c("HPV"),
  cod_acwy   = c("MENACWY"),

  # SINAN: nomes padrão do sistema
  col_sinan = list(nome = "NM_PACIENT", mae = "NM_MAE_PAC", data = "DT_NASC",
                   cns = "ID_CNS_SUS", sexo = "CS_SEXO", mun = "ID_MN_RESI"),
  col_sinan_ev = list(sin_pri = "DT_SIN_PRI", notific = "DT_NOTIFIC", interna = "DT_INTERNA",
                      classi = "CLASSI_FIN", hospitaliz = "HOSPITALIZ", evolucao = "EVOLUCAO",
                      pcr = "RESUL_PCR_", ns1 = "RESUL_NS1", isolamento = "RESUL_VI_N"),

  # Parâmetros iguais aos da análise nacional
  nasc_min = as.IDate("2008-01-01"), nasc_max = as.IDate("2017-12-31"),
  ini_seg  = as.IDate("2024-01-01"), fim_seg  = as.IDate("2025-12-31"),
  limiar   = 17,
  janela_dedup_dias = 90L,
  janelas   = c(10L, 14L, 21L),
  desfechos = c("principal", "direto", "confirmado"),
  usar_marcadores = TRUE,

  # Estratos da tabela com denominador nominal (covariáveis desta análise)
  # [V3] registro_hygia_recente permite a análise de sensibilidade restrita a
  # quem teve registro no Hygia a partir de 2022 (seção 4.7.5)
  estratos_nominal = c("sexo", "idade", "ivs_cat", "distrito", "dengue_previa_conf", "hpv_ou_acwy_antes",
                       "registro_hygia_recente"),

  tam_amostra_revisao = 200L, tam_amostra_proximo_corte = 50L,
  semente = 2026L
)
p  <- function(...) file.path(CFG$pasta, ...)
dir.create(file.path(CFG$pasta, CFG$pasta_saida), showWarnings = FALSE)
ps <- function(...) file.path(CFG$pasta, CFG$pasta_saida, ...)
set.seed(CFG$semente)
relatorio <- data.table(etapa = character(), n = numeric())
reg <- function(etapa, n) {
  relatorio <<- rbind(relatorio, data.table(etapa = etapa, n = as.numeric(n)))
  cat(sprintf("  %-70s %s\n", etapa, format(n, big.mark = ",")))
}

#-------------------------------------------------------------------------------
# 1. LEITURA E RECORTE
#-------------------------------------------------------------------------------
cat("=== 1. LEITURA E RECORTE ===\n")
cad <- fread(p(CFG$arq_cadastro), colClasses = "character")
reg("Hygia: cadastros lidos", nrow(cad))
cad[, .dn := ln_parse_data(get(CFG$col_cad$data))]
cad <- cad[!is.na(.dn) & .dn >= CFG$nasc_min & .dn <= CFG$nasc_max][, .dn := NULL]
cad[, id_hygia := as.character(get(CFG$col_cad$id))]
reg("Hygia: cadastros no recorte de nascimento", nrow(cad))

vac <- fread(p(CFG$arq_vacinas), colClasses = "character")
vac <- vac[as.character(get(CFG$col_vac$id)) %in% cad$id_hygia]
reg("Hygia: doses (qualquer vacina) dos cadastros no recorte", nrow(vac))
reg("Hygia: doses de Qdenga", vac[get(CFG$col_vac$vacina) %in% CFG$cod_qdenga, .N])

sinan <- fread(p(CFG$arq_sinan), colClasses = "character")
reg("SINAN: notificações lidas", nrow(sinan))
sinan <- sinan[substr(gsub("[^0-9]", "", get(CFG$col_sinan$mun)), 1, 6) == CFG$cod_mun]
sinan[, .dn := ln_parse_data(get(CFG$col_sinan$data))]
sinan <- sinan[!is.na(.dn) & .dn >= CFG$nasc_min & .dn <= CFG$nasc_max][, .dn := NULL]
sinan[, .rid := .I]                         # liga as notificações entre as etapas A e B
reg("SINAN: notificações de residentes no recorte de nascimento", nrow(sinan))

ivs <- fread(p(CFG$arq_ivs), colClasses = "character")
cep_setor <- if (!is.null(CFG$arq_cep_setor)) fread(p(CFG$arq_cep_setor), colClasses = "character") else NULL

# Variáveis que acompanham os bancos exportados (sem identificadores e datas)
CFG$cols_clinicas <- setdiff(names(sinan),
  c(unlist(CFG$col_sinan), ".rid", "ID_UNIDADE", "arquivo_origem", grep("^DT_", names(sinan), value = TRUE)))
CFG$cols_vacina_export <- setdiff(names(vac),
  c(unlist(CFG$col_vac), grep("^(DT_|dt_|DATA)", names(vac), value = TRUE)))

#-------------------------------------------------------------------------------
# 2. ADAPTADOR DO HYGIA
#-------------------------------------------------------------------------------
cat("\n=== 2. ADAPTADOR DO HYGIA ===\n")
cad_prep  <- rp_preparar_cadastro(cad, CFG$col_cad)
reg("Hygia: pessoas (cadastros duplicados unidos)", uniqueN(cad_prep$pessoa_hygia))
vac_sipni <- rp_vacina_como_sipni(cad, vac, CFG$col_cad, CFG$col_vac, CFG$cod_qdenga, CFG$cod_mun)
col_hygia <- list(nome = CFG$col_cad$nome, mae = CFG$col_cad$mae, data = CFG$col_cad$data,
                  cns = CFG$col_cad$cns, sexo = CFG$col_cad$sexo, mun = "mun_residencia")

#-------------------------------------------------------------------------------
# 3. LINKAGE — ETAPA A (espelho da análise nacional)
#-------------------------------------------------------------------------------
cat("\n=== 3. LINKAGE, ETAPA A: SINAN x doses de Qdenga do Hygia ===\n")
resA <- ln_pipeline(sinan, vac_sipni, limiar = CFG$limiar, verbose = TRUE,
                    col_a = CFG$col_sinan, col_b = col_hygia,
                    col_data_dedup_a = CFG$col_sinan_ev$sin_pri, col_data_dedup_b = "dt_vacina",
                    janela_dedup_dias = CFG$janela_dedup_dias)
reg("Etapa A: pessoas SINAN", nrow(resA$pessoas_a))
reg("Etapa A: pessoas vacinadas (Hygia)", nrow(resA$pessoas_b))
reg("Etapa A: pares aceitos", nrow(resA$pares))

#-------------------------------------------------------------------------------
# 4. LINKAGE — ETAPA B (exclusiva desta análise)
#-------------------------------------------------------------------------------
cat("\n=== 4. LINKAGE, ETAPA B: notificados sem vacina x cadastro dos não vacinados ===\n")
rid_g3   <- resA$registros_a[!id_paciente %in% resA$pares$ida, .rid]
sinan_g3 <- sinan[.rid %in% rid_g3]
cad_nv   <- rp_cadastro_nao_vacinados(cad, cad_prep, vac, CFG$col_cad, CFG$col_vac, CFG$cod_qdenga)
cad_nv[, mun_residencia := CFG$cod_mun]
reg("Etapa B: cadastros de não vacinados", nrow(cad_nv))
resB <- ln_pipeline(sinan_g3, cad_nv, limiar = CFG$limiar, verbose = TRUE,
                    col_a = CFG$col_sinan, col_b = col_hygia,
                    col_data_dedup_a = CFG$col_sinan_ev$sin_pri, col_data_dedup_b = NULL,
                    janela_dedup_dias = CFG$janela_dedup_dias)
reg("Etapa B: pares aceitos", nrow(resB$pares))

#-------------------------------------------------------------------------------
# 5. VALIDAÇÃO (revisão manual pela SMS; diagnóstico de m e u)
#-------------------------------------------------------------------------------
cat("\n=== 5. VALIDAÇÃO ===\n")
amostra_revisao <- function(res, etapa) {
  if (!nrow(res$pares)) return(invisible())
  a <- copy(res$pares)
  a[, faixa := ceiling(4 * frank(escore, ties.method = "first") / .N)]
  a <- a[, .SD[sample(.N, min(.N, ceiling(CFG$tam_amostra_revisao / 4)))], by = faixa]
  pr <- res$comparacoes[escore >= CFG$limiar - 2 & escore < CFG$limiar & n_campos >= 2]
  pr <- pr[sample(.N, min(CFG$tam_amostra_proximo_corte, .N))]
  fwrite(a,  ps(sprintf("revisao_manual_etapa%s_pares_aceitos.csv", etapa)), sep = ";")
  fwrite(pr, ps(sprintf("revisao_manual_etapa%s_proximos_ao_corte.csv", etapa)), sep = ";")
}
amostra_revisao(resA, "A"); amostra_revisao(resB, "B")
for (e in list(list(resA, "A"), list(resB, "B"))) {
  cat(sprintf("\nEtapa %s — pares por número de campos comparados:\n", e[[2]]))
  print(e[[1]]$pares[, .N, by = n_campos])
}
mu <- ln_estimar_mu(resA$pessoas_a, resA$pessoas_b, n_aleatorios = 5e4)
fwrite(mu, ps("diagnostico_m_u_etapaA.csv"), sep = ";")

#-------------------------------------------------------------------------------
# 6. GRUPOS E VARIÁVEIS DERIVADAS
#-------------------------------------------------------------------------------
cat("\n=== 6. GRUPOS E VARIÁVEIS DERIVADAS ===\n")
pessoas <- rp_unificar(resA, resB, cad_prep, CFG$cod_mun)
reg("Pessoas unificadas", nrow(pessoas))
for (g in c("1", "2", "3a", "3b", "4")) reg(sprintf("  grupo %s", g), pessoas[grupo_rp == g, .N])

doses   <- dv_consolidar_doses(resA$registros_b, pessoas, "dt_vacina")
eventos <- dv_eventos(resA$registros_a, pessoas, CFG$col_sinan_ev, CFG$usar_marcadores)
janela  <- dv_janela(pessoas, CFG$ini_seg, CFG$fim_seg)
cov     <- rp_covariaveis(pessoas, janela, cad, cad_prep, vac, eventos, CFG$col_cad, CFG$col_vac,
                          CFG$cod_hpv, CFG$cod_acwy, ivs, cep_setor, CFG$ini_seg)
# coorte nominal: pessoas do Hygia (grupos 1, 2, 3a e 4), com óbito como fim do seguimento
janela_nom <- rp_janela_com_obito(janela[pid %in% pessoas[grupo_rp != "3b", pid]], cov)
reg("Coorte nominal: pessoas com tempo elegível", nrow(janela_nom))
reg("Coorte nominal: sem categoria de IVS", cov[pid %in% janela_nom$pid & is.na(ivs_cat), .N])

#-------------------------------------------------------------------------------
# 7. TABELAS AGREGADAS
#-------------------------------------------------------------------------------
# (a) denominador ESTIMADO: reproduz a análise nacional (pessoas-dia dos
#     vacinados; casos dos grupos 1, 2, 3a e 3b). A pessoa-tempo não
#     vacinada é obtida fora, pela população do IBGE, como no nacional.
# (b) denominador NOMINAL: pessoas-dia de todos os status a partir dos
#     indivíduos do Hygia (grupos 1, 2, 3a e 4), por covariáveis.
# (c) casos do grupo 3b, para a análise de sensibilidade que os inclui
#     como casos não vacinados.
cat("\n=== 7. TABELAS AGREGADAS ===\n")
for (w in CFG$janelas) for (d in CFG$desfechos) {
  sufixo <- sprintf("j%02d_%s", w, d)
  pe  <- dv_primeiro_evento(eventos, janela, d)
  iv  <- dv_intervalos(janela, doses, pe, w)
  cz  <- dv_casos(janela, doses, pe, w)
  est <- dv_tabela_diaria(iv, cz, c("sexo", "idade"), "diario", CFG$fim_seg)
  stopifnot(sum(est$n_parcial) + sum(est$n_vacinado) == sum(iv$dias))
  fwrite(est, ps(sprintf("tabela_denominador_estimado_%s.csv", sufixo)), sep = ";")

  pe_n <- dv_primeiro_evento(eventos, janela_nom, d)
  iv_n <- dv_intervalos(janela_nom, doses, pe_n, w, incluir_nao_vacinado = TRUE)
  cz_n <- dv_casos(janela_nom, doses, pe_n, w)
  nom  <- rp_tabela(iv_n, cz_n, cov, CFG$estratos_nominal, CFG$fim_seg)
  stopifnot(sum(nom$n_nao_vacinado) + sum(nom$n_parcial) + sum(nom$n_vacinado) == sum(iv_n$dias))
  jn <- pe_n[copy(janela_nom), on = "pid"][, s := pmin(saida_adm, dt_evento, na.rm = TRUE)]
  stopifnot(sum(iv_n$dias) == sum(as.integer(jn$s - jn$entrada) + 1L))   # pessoa-tempo completa
  fwrite(nom, ps(sprintf("tabela_denominador_nominal_%s.csv", sufixo)), sep = ";")

  c3b <- cz[pid %in% pessoas[grupo_rp == "3b", pid], .N, by = .(data, sexo, idade)]
  fwrite(c3b, ps(sprintf("tabela_casos_grupo3b_%s.csv", sufixo)), sep = ";")
  reg(sprintf("Casos %s — denominador estimado (grupos 1, 2, 3a, 3b)", sufixo), nrow(cz))
  reg(sprintf("Casos %s — denominador nominal (grupos 1, 2, 3a)", sufixo), nrow(cz_n))
  reg(sprintf("Casos %s — grupo 3b", sufixo), sum(c3b$N))
}

#-------------------------------------------------------------------------------
# 8. BANCOS INDIVIDUAIS E SAÍDAS
#-------------------------------------------------------------------------------
cat("\n=== 8. GRAVANDO RESULTADOS ===\n")
cov_exp <- copy(cov)[, obito_no_periodo := as.integer(!is.na(dt_obito) & dt_obito <= CFG$fim_seg)][, dt_obito := NULL]
base_pessoas <- dv_base_pessoas(pessoas, doses, janela_nom, eventos, w = 14L)
base_pessoas <- pessoas[, .(pid, grupo_rp)][base_pessoas, on = "pid"]
base_pessoas <- cov_exp[base_pessoas, on = "pid"]
base_notif   <- dv_base_notificacoes(eventos, pessoas, doses, resA$registros_a, w = 14L,
                                     cols_clinicas = CFG$cols_clinicas)
attr_doses <- dv_atributos_doses(resA$registros_b, pessoas, doses, "dt_vacina", CFG$cols_vacina_export)
bancos <- dv_bancos_exportacao(base_pessoas, base_notif, attr_doses, CFG$desfechos)
banco4 <- base_pessoas[grupo_rp == "4"]
stopifnot(uniqueN(bancos$pareados$pid) + nrow(bancos$vacinados_sem_notificacao) +
          uniqueN(bancos$notificados_sem_vacina$pid) + nrow(banco4) == nrow(pessoas))

correspondencia <- data.table(pid = pessoas$pid, id_estudo = sample(nrow(pessoas)))
fwrite(pessoas[correspondencia, on = "pid", .(pid, id_estudo, grupo_rp, id_hygia, ida, idb)],
       ps("interno_correspondencia_ids.csv"), sep = ";")
fwrite(resA$pares, ps("interno_linkage_pares_etapaA.csv"), sep = ";")
fwrite(resB$pares, ps("interno_linkage_pares_etapaB.csv"), sep = ";")

fwrite(dv_preparar_exportacao(bancos$pareados, correspondencia),
       ps("export_banco1_pareados.csv"), sep = ";")
fwrite(dv_preparar_exportacao(bancos$vacinados_sem_notificacao, correspondencia),
       ps("export_banco2_vacinados_sem_notificacao.csv"), sep = ";")
fwrite(dv_preparar_exportacao(bancos$notificados_sem_vacina, correspondencia),
       ps("export_banco3_notificados_sem_vacina.csv"), sep = ";")     # grupos 3a e 3b (coluna grupo_rp)
fwrite(dv_preparar_exportacao(banco4, correspondencia),
       ps("export_banco4_nao_vacinados_sem_notificacao.csv"), sep = ";")
fwrite(relatorio, ps("relatorio_processamento.csv"), sep = ";")

# Checagem final: nenhum identificador nos arquivos que saem da Prefeitura
proib <- c(unlist(CFG$col_cad[c("id", "nome", "mae", "data", "cns", "cep", "setor")]),
           unlist(CFG$col_sinan[c("nome", "mae", "data", "cns")]),
           "id_hygia", "pessoa_hygia", "pid", "ida", "idb", "dn", "nome_n", "mae_n", "cns", ".rid")
for (f in list.files(ps(), pattern = "^(export|tabela)_", full.names = TRUE)) {
  cols <- names(fread(f, nrows = 0))
  if (any(cols %in% proib)) stop("Coluna identificadora em ", basename(f), " — não enviar.")
}
cat("\nPROCESSAMENTO CONCLUÍDO. Resultados em:", ps(), "\n")
