#=======================================================================
# FUNÇÕES DA ANÁLISE DE SENSIBILIDADE LOCAL — RIBEIRÃO PRETO/SP
# (seção 4.7.5 e Material Suplementar S07 do projeto)
#
# Usam, sem alteração, as funções da análise nacional:
#   01_funcoes_linkage.R  (linkage)
#   02_funcoes_derivadas.R (variáveis derivadas do S06)
# e acrescentam apenas:
#   - o ADAPTADOR, que transforma o Hygia em uma base no formato do SI-PNI
#     (registros de doses de Qdenga com os campos de identificação);
#   - a ETAPA B do linkage (casos sem vacina x cadastro dos não vacinados);
#   - a unificação dos grupos 1, 2, 3a, 3b e 4;
#   - as covariáveis exclusivas desta análise (índice de vulnerabilidade
#     social, distrito de saúde, dengue prévia, HPV e meningocócica ACWY,
#     permanência no município e óbito).
#
# Grupos:
#   1  = notificado e vacinado (pareado na etapa A)
#   2  = vacinado sem notificação
#   3a = notificado sem vacina, presente no cadastro do Hygia (etapa B)
#   3b = notificado sem vacina, ausente do cadastro do Hygia
#   4  = não vacinado e sem notificação (cadastro do Hygia)
#=======================================================================

#-----------------------------------------------------------------------
# 1. Adaptador do Hygia
#-----------------------------------------------------------------------
# Padroniza o cadastro e identifica a PESSOA do Hygia: cadastros diferentes
# da mesma pessoa (mesmo CNS ou mesma chave nome + mãe + nascimento) são
# unidos por componentes conexos, com a mesma função da análise nacional.
# Isso impede que alguém vacinado em um cadastro apareça como "não
# vacinado" em um cadastro duplicado.
rp_preparar_cadastro <- function(cad, col) {
  cp <- ln_preparar(cad, col$nome, col$mae, col$data, col$cns, col$sexo, NULL)
  cp <- ln_identificar_pessoas(cp)
  setnames(cp, "id_paciente", "pessoa_hygia")
  cp[, id_hygia := as.character(get(col$id))]
  cp[]
}

# Registros de doses de Qdenga com os campos de identificação do cadastro:
# é a base que faz o papel do SI-PNI na etapa A.
rp_vacina_como_sipni <- function(cad, vac, col_cad, col_vac, cod_qdenga, cod_mun) {
  v <- copy(vac[as.character(get(col_vac$vacina)) %in% cod_qdenga])   # demais colunas seguem junto
  setnames(v, c(col_vac$id, col_vac$data), c("id_hygia", "dt_vacina"))
  v[, id_hygia := as.character(id_hygia)]
  ident <- cad[, c(col_cad$id, col_cad$nome, col_cad$mae, col_cad$data, col_cad$cns, col_cad$sexo), with = FALSE]
  setnames(ident, col_cad$id, "id_hygia")
  ident[, id_hygia := as.character(id_hygia)]
  out <- merge(v, ident, by = "id_hygia")
  out[, mun_residencia := cod_mun]
  out[]
}

# Cadastro das pessoas do Hygia sem nenhuma dose de Qdenga em nenhum dos
# seus cadastros (pool da etapa B).
rp_cadastro_nao_vacinados <- function(cad, cad_prep, vac, col_cad, col_vac, cod_qdenga) {
  ids_vac <- unique(as.character(vac[as.character(get(col_vac$vacina)) %in% cod_qdenga, get(col_vac$id)]))
  pessoas_vac <- unique(cad_prep[id_hygia %in% ids_vac, pessoa_hygia])
  ids_nv <- cad_prep[!pessoa_hygia %in% pessoas_vac, id_hygia]
  cad[as.character(get(col_cad$id)) %in% ids_nv]
}

#-----------------------------------------------------------------------
# 2. Unificação dos grupos 1, 2, 3a, 3b e 4
#-----------------------------------------------------------------------
# resA: ln_pipeline(SINAN, vacina_como_sipni)   — etapa A
# resB: ln_pipeline(SINAN do grupo 3, cadastro dos não vacinados) — etapa B
# O SINAN precisa ter a coluna .rid (número da linha original), que liga
# as notificações entre as duas etapas.
rp_unificar <- function(resA, resB, cad_prep, cod_mun) {
  p <- dv_unificar(resA)                                  # grupos 1, 2, 3 da etapa A
  p[, grupo_rp := as.character(grupo_linkage)]

  # pessoa do Hygia dos vacinados (grupos 1 e 2)
  hb <- resA$registros_b[, .(id_hygia = id_hygia[1L]), by = .(idb = id_paciente)]
  p <- hb[p, on = "idb"]

  # grupo 3 -> 3a/3b, pela ligação das notificações (.rid) entre as etapas
  mapa <- unique(merge(resA$registros_a[, .(ida = id_paciente, .rid)],
                       resB$registros_a[, .(idaB = id_paciente, .rid)], by = ".rid")[, .(ida, idaB)])
  mapa <- mapa[!duplicated(ida)]
  hB <- resB$registros_b[, .(id_hygia_B = id_hygia[1L]), by = .(idbB = id_paciente)]
  parB <- hB[resB$pares[, .(idaB = ida, idbB = idb)], on = "idbB"]
  mapa <- parB[mapa, on = "idaB"]
  p <- mapa[, .(ida, id_hygia_B)][p, on = "ida"]
  p[grupo_rp == "3", grupo_rp := fifelse(!is.na(id_hygia_B), "3a", "3b")]
  p[grupo_rp == "3a", id_hygia := id_hygia_B]
  p[, id_hygia_B := NULL]

  # grupo 4: não vacinados do cadastro que não parearam na etapa B
  b4 <- resB$registros_b[!id_paciente %in% resB$pares$idb, .(id_hygia = id_hygia[1L]), by = id_paciente]
  g4 <- data.table(ida = NA_integer_, idb = NA_integer_, grupo_linkage = 4L, grupo_rp = "4",
                   id_hygia = b4$id_hygia, dn = as.IDate(NA), sexo = NA_character_, mun = cod_mun)
  g4[, pid := max(p$pid) + seq_len(.N)]
  p <- rbind(p, g4, use.names = TRUE, fill = TRUE)

  # atributos do cadastro do Hygia têm prioridade (é a base populacional)
  ca <- cad_prep[, .(id_hygia, pessoa_hygia, dn_h = dn, sexo_h = sexo)]
  p <- ca[p, on = "id_hygia"]
  p[!is.na(dn_h), dn := dn_h]
  p[!is.na(sexo_h), sexo := sexo_h]
  p[grupo_rp != "3b", mun := cod_mun]
  p[, c("dn_h", "sexo_h") := NULL]
  p[grupo_linkage == 3L & grupo_rp %in% c("3a", "3b"), grupo_linkage := 3L]
  setkey(p, pid)
  p[]
}

#-----------------------------------------------------------------------
# 3. Covariáveis exclusivas desta análise
#-----------------------------------------------------------------------
# cad_prep: cadastro padronizado (com id_hygia e pessoa_hygia)
# ivs: data.table(cd_setor, ivs_cat); cep_setor: data.table(cep, cd_setor),
#      opcional, construída a partir do CNEFE 2022 (setor majoritário do CEP)
rp_covariaveis <- function(pessoas, janela, cad, cad_prep, vac, eventos, col_cad, col_vac,
                           cod_hpv, cod_acwy, ivs, cep_setor = NULL,
                           ini_seg = as.IDate("2024-01-01"), data_registro_recente = as.IDate("2022-01-01")) {
  g <- function(nm) if (!is.null(col_cad[[nm]]) && col_cad[[nm]] %in% names(cad))
                      as.character(cad[[col_cad[[nm]]]]) else rep(NA_character_, nrow(cad))
  cc <- data.table(id_hygia = as.character(cad[[col_cad$id]]),
                   distrito = g("distrito"), setor = g("setor"),
                   cep = gsub("[^0-9]", "", g("cep")),
                   dt_ultimo = ln_parse_data(g("dt_ultimo")), dt_obito = ln_parse_data(g("dt_obito")))
  # setor censitário: coluna do cadastro (geocodificação da SMS) ou CEP -> setor (CNEFE)
  if (!is.null(cep_setor)) {
    cs <- copy(cep_setor)[, cep := gsub("[^0-9]", "", cep)]
    cc <- cs[, .(cep, setor_cep = as.character(cd_setor))][cc, on = "cep"]
    cc[is.na(setor) | setor == "", setor := setor_cep]
    cc[, setor_cep := NULL]
  }
  cc <- ivs[, .(setor = as.character(cd_setor), ivs_cat = as.character(ivs_cat))][cc, on = "setor"]

  # consolidação por pessoa do Hygia (cadastros duplicados)
  cc <- cad_prep[, .(id_hygia, pessoa_hygia)][cc, on = "id_hygia"]
  pc <- cc[, .(distrito = distrito[!is.na(distrito)][1L], ivs_cat = ivs_cat[!is.na(ivs_cat)][1L],
               dt_ultimo = if (all(is.na(dt_ultimo))) as.IDate(NA) else max(dt_ultimo, na.rm = TRUE),
               dt_obito  = if (all(is.na(dt_obito)))  as.IDate(NA) else min(dt_obito,  na.rm = TRUE)),
           by = pessoa_hygia]

  # HPV e meningocócica ACWY (histórico vacinal completo)
  vv <- vac[, .(id_hygia = as.character(get(col_vac$id)), cod = as.character(get(col_vac$vacina)),
                dt = ln_parse_data(get(col_vac$data)))]
  vv <- cad_prep[, .(id_hygia, pessoa_hygia)][vv, on = "id_hygia", nomatch = NULL]
  primeira <- function(codigos, nome) {          # data da 1ª dose por pessoa (vazio se não houver)
    x <- vv[cod %in% codigos & !is.na(dt)]
    if (!nrow(x)) return(setnames(data.table(pessoa_hygia = integer(), d = as.IDate(character())), "d", nome))
    setnames(x[, .(d = min(dt)), by = pessoa_hygia], "d", nome)
  }
  hpv  <- primeira(cod_hpv,  "dt_hpv1")
  acwy <- primeira(cod_acwy, "dt_acwy1")

  out <- pessoas[, .(pid, pessoa_hygia, grupo_rp)]
  out <- pc[out, on = "pessoa_hygia"]
  out <- hpv[out, on = "pessoa_hygia"]; out <- acwy[out, on = "pessoa_hygia"]
  out <- janela[, .(pid, entrada)][out, on = "pid"]
  out[, `:=`(hpv_antes_entrada  = as.integer(!is.na(dt_hpv1)  & !is.na(entrada) & dt_hpv1  < entrada),
             acwy_antes_entrada = as.integer(!is.na(dt_acwy1) & !is.na(entrada) & dt_acwy1 < entrada),
             registro_hygia_recente = as.integer(!is.na(dt_ultimo) & dt_ultimo >= data_registro_recente))]
  out[, hpv_ou_acwy_antes := as.integer(hpv_antes_entrada == 1L | acwy_antes_entrada == 1L)]

  # dengue notificada prévia (antes do início do seguimento): confirmada
  conf <- c("1", "2", "3", "4", "10", "11", "12")   # classificações antigas e atuais
  pr <- eventos[!is.na(dt_sin_pri) & dt_sin_pri < ini_seg,
                .(n_notif_previas = .N, dengue_previa_conf = as.integer(any(classi %in% conf))), by = pid]
  out <- pr[out, on = "pid"]
  out[is.na(n_notif_previas), `:=`(n_notif_previas = 0L, dengue_previa_conf = 0L)]
  # grupo 3b não está no Hygia: covariáveis do Hygia ficam ausentes
  out[grupo_rp == "3b", c("hpv_antes_entrada", "acwy_antes_entrada", "hpv_ou_acwy_antes",
                          "registro_hygia_recente") := NA_integer_]
  out[, c("entrada", "pessoa_hygia", "grupo_rp", "dt_ultimo", "dt_hpv1", "dt_acwy1") := NULL]
  out[]
}

# Óbito como fim do seguimento (análise nominal)
rp_janela_com_obito <- function(janela, cov) {
  j <- cov[, .(pid, dt_obito)][copy(janela), on = "pid"]
  j[!is.na(dt_obito), saida_adm := pmin(saida_adm, dt_obito)]
  j[, dt_obito := NULL]
  j[entrada <= saida_adm]
}

#-----------------------------------------------------------------------
# 4. Tabelas agregadas com covariáveis
#-----------------------------------------------------------------------
# Acrescenta as covariáveis aos intervalos e aos casos e agrega pelos
# estratos pedidos, com as mesmas funções da análise nacional.
rp_tabela <- function(iv, cz, cov, estratos, fim_periodo) {
  extra <- setdiff(estratos, c("sexo", "idade", "mun"))
  if (length(extra)) {
    iv <- cov[, c("pid", extra), with = FALSE][iv, on = "pid"]
    cz <- cov[, c("pid", extra), with = FALSE][cz, on = "pid"]
    for (e in extra) {                                 # ausente vira categoria própria
      set(iv, which(is.na(iv[[e]])), e, "ignorado"); set(cz, which(is.na(cz[[e]])), e, "ignorado")
    }
    for (e in extra) { iv[, (e) := as.character(get(e))]; cz[, (e) := as.character(get(e))] }
  }
  dv_tabela_diaria(iv, cz, estratos, "diario", fim_periodo)
}
