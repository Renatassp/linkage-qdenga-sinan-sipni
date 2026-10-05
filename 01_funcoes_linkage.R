#=======================================================================
# FUNÇÕES DE RECORD LINKAGE — SINAN (dengue) x SI-PNI/VACINA (Qdenga) DADOS REAIS

#Primeiro padronizando e deduplicando os registros, depois identificando pessoas dentro de cada banco, gerando pares candidatos por blocking, comparando os candidatos por similaridade e escore Fellegi-Sunter e, por fim, selecionando os pares mais prováveis em uma relação 1:1.

#O fluxo geral é:

#SINAN + SI-PNI
#↓
#Padronização
#↓
#Identificação de pessoas dentro de cada banco
#↓
#Deduplicação temporal
#↓
#Perfil único por pessoa
#↓
#Blocking
#↓
#Pares candidatos
#↓
#Comparação dos campos
#↓
#Escore Fellegi-Sunter
#↓
#Limiar
#↓
#Resolução 1:1
#↓

#Pares vinculados SINAN × SI-PNI


#linkage sobre os dados REAIS na Sala de Acesso Restrito (SAR).
#O código está organizado em funções puras (sem depender de objetos globais)
#para que possa ser movido, sem alteração, para R/ de um pacote.
#  A deduplicação temporal de 90 dias continua valendo para os dois bancos
#  (SINAN e SI-PNI), como na v1.
#=======================================================================
suppressPackageStartupMessages({ #evita que mensagens dos pacotes apareçam na tela
  library(data.table) #manipulação rápida de grandes bases de dados
  library(stringi) #tratamento e padronização de textos
  library(stringdist)#cálculo de similaridade/distância entre textos.
})
setDTthreads(0) #permite que o data.table use todos os núcleos disponíveis do computador


#=======================================================================
#1. UTILITÁRIOS DE NORMALIZAÇÃO
#=======================================================================

#Normaliza texto para comparação (minúsculas, sem acento, sem pontuação)
##Transliteração é feita direto pelo stringi, que já lida com o encoding declarado.
ln_normalizar_texto <- function(x) {
  x <- stri_trans_general(as.character(x), "Latin-ASCII")
  x <- stri_trans_tolower(x)
  x <- stri_replace_all_regex(x, "[^a-z0-9 ]", " ")
  x <- stri_trim_both(stri_replace_all_regex(x, "\\s+", " "))
  x[x == ""] <- NA_character_
  x
}



#######################################################################
#Converte datas em múltiplos formatos para IDate
#Identifica o layout por expressão regular ancorada, monta a string ISSO
# e só então converte, sem ambiguidade. Datas fora da janela plausível viram NA em vez de entrarem no linkage com ano absurdo.
ln_parse_data <- function(x, min_ano = 1900L,
                          max_ano = as.integer(format(Sys.Date(), "%Y"))) {
  x <- trimws(as.character(x))
  x <- sub("[T ][0-9:.]+$", "", x)              # descarta hora, se houver
  x[x %in% c("", "NA", "NULL", "null", "-", "//", "00/00/0000", "0000-00-00")] <- NA_character_

  iso <- rep(NA_character_, length(x))
  pega <- function(rx) !is.na(x) & is.na(iso) & grepl(rx, x)

  i <- pega("^[0-9]{2}/[0-9]{2}/[0-9]{4}$")
  iso[i] <- paste0(substr(x[i], 7, 10), "-", substr(x[i], 4, 5), "-", substr(x[i], 1, 2))
  i <- pega("^[0-9]{2}-[0-9]{2}-[0-9]{4}$")
  iso[i] <- paste0(substr(x[i], 7, 10), "-", substr(x[i], 4, 5), "-", substr(x[i], 1, 2))
  i <- pega("^[0-9]{2}\\.[0-9]{2}\\.[0-9]{4}$")
  iso[i] <- paste0(substr(x[i], 7, 10), "-", substr(x[i], 4, 5), "-", substr(x[i], 1, 2))
  i <- pega("^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  iso[i] <- x[i]
  i <- pega("^[0-9]{4}/[0-9]{2}/[0-9]{2}$")
  iso[i] <- chartr("/", "-", x[i])

  # 8 dígitos: ddmmaaaa ou aaaammdd — desempate por plausibilidade
  i <- pega("^[0-9]{8}$")
  if (any(i)) {
    v  <- x[i]
    a1 <- as.integer(substr(v, 1, 4)); m1 <- as.integer(substr(v, 5, 6)); d1 <- as.integer(substr(v, 7, 8))
    d2 <- as.integer(substr(v, 1, 2)); m2 <- as.integer(substr(v, 3, 4)); a2 <- as.integer(substr(v, 5, 8))
    ok_iso <- a1 >= min_ano & a1 <= max_ano & m1 %in% 1:12 & d1 %in% 1:31
    ok_dmy <- a2 >= min_ano & a2 <= max_ano & m2 %in% 1:12 & d2 %in% 1:31
    iso[i] <- fifelse(ok_dmy & !ok_iso,
                      paste0(substr(v, 5, 8), "-", substr(v, 3, 4), "-", substr(v, 1, 2)),
               fifelse(ok_iso,
                      paste0(substr(v, 1, 4), "-", substr(v, 5, 6), "-", substr(v, 7, 8)),
                      NA_character_))
  }

  out <- as.IDate(iso, format = "%Y-%m-%d") #transforma tudo em uma variável de data padronizada.
  a <- year(out)
  out[!is.na(out) & (a < min_ano | a > max_ano)] <- NA
  out
}



#########################################################################
#Idade em anos completos de nu_idade_paciente codificado no padrão DATASUS
#Em vários extratos do OpenDataSUS o campo é codificado (1º dígito = unidade: 1 hora, 2 #dia, 3 mês, 4 ano).Esta função detecta e converte; use `ln_idade_anos(x, codificado = 
#NA) para deixar a detecção automática.

ln_idade_anos <- function(x, codificado = NA) {
  x <- suppressWarnings(as.integer(x))
  if (is.na(codificado)) {
    codificado <- mean(x >= 4000 & x <= 4130, na.rm = TRUE) > 0.5
  }
  if (!codificado) return(x)
  unidade <- x %/% 1000L
  valor   <- x %%  1000L
  fifelse(unidade == 4L, valor,
   fifelse(unidade %in% c(1L, 2L, 3L), 0L, NA_integer_))
}



###########################################################################
#' Mantém apenas dígitos e valida o CNS (15 dígitos, módulo 11)
ln_limpar_cns <- function(x, validar = TRUE) {
  y <- gsub("[^0-9]", "", as.character(x)) #mantém somente números
  y[nchar(y) != 15L] <- NA_character_ #descarta CNS que não possuem 15 dígitos.
  if (validar) {
    ok <- which(!is.na(y))
    if (length(ok)) {
      z <- y[ok]
      soma <- integer(length(z))
      # [PERF] soma posicao a posicao: 15 vetores inteiros em vez de um
      # strsplit/unlist de 15 x N elementos (que estourava a memoria em 3 milhoes)
      for (i in 1:15) soma <- soma + as.integer(substr(z, i, i)) * (16L - i)
      y[ok[soma %% 11L != 0L]] <- NA_character_
    }
  }
  y
}



###########################################################################
#Primeiro e último token de um nome já normalizado
#Extrai o primeiro e o último nome para serem usados nas etapas de blocking

ln_primeiro_nome <- function(x) sub(" .*$", "", x)
ln_ultimo_nome   <- function(x) sub("^.* ", "", x)



###########################################################################
#Padroniza o sexo para M ou F, aceitando códigos numéricos ou texto.
ln_sexo <- function(x) {
  x <- toupper(substr(trimws(as.character(x)), 1, 1))
  fifelse(x %in% c("M", "1"), "M", fifelse(x %in% c("F", "2"), "F", NA_character_))
}


##########################################################################
#Padroniza o código do município para os 6 primeiros dígitos do código IBGE (o SINAN usa 6; o SI-PNI pode trazer 7)
ln_municipio6 <- function(x) {
  y <- gsub("[^0-9]", "", as.character(x))
  fifelse(nchar(y) >= 6L, substr(y, 1, 6), NA_character_)
}



#======================================================================
# 2. PREPARO, DEDUPLICAÇÃO DE IDENTIDADE E DEDUPLICAÇÃO TEMPORAL
#=======================================================================


#Padroniza as variáveis de identificação dos dois bancos e cria variáveis auxiliares para o linkage. Ela cria nome padronizado, nome da mãe padronizado, data de nascimento padronizado, CNS limpo/validado, sexo padronizado, municipio padronizado, primeiro nome, último nome, primeiro nome da mãe, último nome da mae.
#A chamada chave forte é nome + nome da mãe + data de nascimento

ln_preparar <- function(dt, col_nome = "nome_paciente", col_mae = "nome_mae",
                        col_data = "data_nasc", col_cns = "numero_sus",
                        col_sexo = NULL, col_mun = NULL,
                        validar_cns = TRUE) {
  dt <- copy(dt)
  dt[, nome_n := ln_normalizar_texto(get(col_nome))]
  dt[, mae_n  := ln_normalizar_texto(get(col_mae))]
  dt[, dn     := ln_parse_data(get(col_data))]
  dt[, cns    := ln_limpar_cns(get(col_cns), validar = validar_cns)]
  dt[, sexo   := if (!is.null(col_sexo)) ln_sexo(get(col_sexo)) else NA_character_] 
  dt[, mun    := if (!is.null(col_mun))  ln_municipio6(get(col_mun)) else NA_character_] 
  dt[, `:=`(nome_1 = ln_primeiro_nome(nome_n), nome_u = ln_ultimo_nome(nome_n),
            mae_1  = ln_primeiro_nome(mae_n),  mae_u  = ln_ultimo_nome(mae_n))]
  dt[]
}



########################################################################
#Objetivo: descobrir quais registros dentro do mesmo banco pertencem à mesma pessoa.
#Agrupa registros que pertencem à mesma pessoa dentro de cada banco, usando CNS OU nome + mãe + data de nascimento
#A~B pelo CNS 1 e B~C pela chave forte => A, B e C são
#a mesma pessoa, mesmo com CNS diferentes.


ln_identificar_pessoas <- function(dt, max_iter = 50L) {
  dt <- copy(dt)
  # [V2-1] na chave forte, partículas (de, da, do, dos, das, e) são ignoradas:
  # "joao da silva" e "joao silva" formam a mesma chave
  sem_part <- function(x) stri_trim_both(stri_replace_all_regex(
    stri_replace_all_regex(x, "\\b(de|da|do|das|dos|e)\\b", " "), "\\s+", " "))
  dt[, chave_forte := fifelse(!is.na(nome_n) & !is.na(mae_n) & !is.na(dn),
                              paste(sem_part(nome_n), sem_part(mae_n), dn), NA_character_)]
  dt[, .g := .I]
  for (it in seq_len(max_iter)) {
    antes <- dt$.g
    dt[!is.na(cns),         .g := min(.g), by = cns]
    dt[!is.na(chave_forte), .g := min(.g), by = chave_forte]
    if (identical(antes, dt$.g)) break
    if (it == max_iter) warning("ln_identificar_pessoas: sem convergência em max_iter iterações")
  }
  # registros sem nenhuma chave mantêm o próprio rótulo (identificador próprio)
  dt[, id_paciente := .GRP, by = .g]#identificador da pessoa dentro daquele banco
  dt[, c(".g", "chave_forte") := NULL]
  dt[]
}



##########################################################################

#Remove duplicatas temporais por pessoa (mesmo episódio/dose dentro de uma janela)
#Remove registros da mesma pessoa que ocorrem com menos de 90 dias de intervalo, considerando-os duplicatas temporais.
#Registros sem data não são excluídos por essa regra
#Regra: dentro de cada pessoa (id_paciente, já atribuído por
#ln_identificar_pessoas())
#data dos primeiros sintomas (SINAN); data da dose (SI-PNI)

ln_remover_duplicatas_temporais <- function(dt, col_data, janela_dias = 90L) {   #uma janela de 90 dias
  stopifnot(col_data %in% names(dt), "id_paciente" %in% names(dt))
  dt <- copy(dt)
  dt[, .rowid      := .I]
  dt[, .data_dedup := ln_parse_data(get(col_data))]

  com_data <- dt[!is.na(.data_dedup)]
  sem_data <- dt[is.na(.data_dedup)]        # nunca descartados por esta regra

  setorder(com_data, id_paciente, .data_dedup, .rowid)   # ordem cronologica, desempate estavel
  com_data[, .manter := {
    d <- .data_dedup
    manter <- rep(TRUE, length(d))
    ultimo_mantido <- d[1]
    if (length(d) > 1L) {
      for (i in 2:length(d)) {
        if ((d[i] - ultimo_mantido) < janela_dias) {
          manter[i] <- FALSE
        } else {
          ultimo_mantido <- d[i]
        }
      }
    }
    manter
  }, by = id_paciente]

  saida <- rbindlist(list(com_data[.manter == TRUE], sem_data), fill = TRUE)
  setorder(saida, .rowid)
  saida[, c(".data_dedup", ".rowid", ".manter") := NULL]
  saida[]
}



#########################################################################
#Depois da deduplicação, a função cria um perfil por pessoa, completando campos faltantes com as réplicas.
#Assim, o linkage passa a ser pessoa X pessoa, e não registro X registro
#Isso evita que duplicatas e doses inflem artificialmente o número de
#' pares, permite que a resolução 1:1 faça sentido e recupera campos
vazios de um registro usando a réplica que estiver preenchida.

ln_perfil_pessoa <- function(dt) {
  cols <- c("nome_n", "mae_n", "dn", "cns")
  tem_verdade <- "id_pessoa_verdadeiro" %in% names(dt)
  if (tem_verdade) {
    perfil <- dt[, .(n_registros = .N, id_pessoa_verdadeiro = id_pessoa_verdadeiro[1L]),
                 keyby = id_paciente]
  } else {
    perfil <- dt[, .(n_registros = .N), keyby = id_paciente]
  }
  for (cl in cols) {
    dt[, .falta := is.na(get(cl))]
    setorder(dt, id_paciente, .falta)                 # nao-NA primeiro dentro da pessoa
    v <- dt[!duplicated(id_paciente), c("id_paciente", cl), with = FALSE]
    setkey(v, id_paciente)
    perfil[v, (cl) := get(paste0("i.", cl))]
  }
  dt[, .falta := NULL]
  perfil[, `:=`(nome_1 = ln_primeiro_nome(nome_n), nome_u = ln_ultimo_nome(nome_n),
                mae_1  = ln_primeiro_nome(mae_n),  mae_u  = ln_ultimo_nome(mae_n))]
  perfil[]
}


#=======================================================================
# 3. BLOCKING MULTI-PASSO
#=======================================================================
#São vários passos independentes, cada um exigindo apenas 2 campos.
#O par entra na comparação se sobreviver a QUALQUER UM dos passos, então é
#preciso estragar dois campos ao mesmo tempo para perder a pessoa.


#########################################################################
#Define diferentes combinações de variáveis usadas para selecionar possíveis pares antes da comparação detalhada
#O blocking reduz drasticamente o número de pares que precisam ser comparados. 
#Um par pode ser selecionado por qualquer um dos passos. Ou seja, ele não decide ainda que duas pessoas são iguais.
#Ele apenas diz: "Essas duas pessoas são candidatas a serem comparadas."

ln_passos_padrao <- function() list(
  P1_cns          = "k_cns",                #Busca pessoas com o mesmo CNS
  P2_data_nome    = c("k_dn", "k_nome"), #Mesma dt de nasc + chave do nome
  P3_data_mae     = c("k_dn", "k_mae"), #Mesma dt de nasc + chave da mãe.
  P4_nome_mae     = c("k_nome", "k_mae"), #Mesmo nome + mesma chave da mãe
  P5_anomes_nome  = c("k_anomes", "k_nome", "k_mae1") #Mesmo ano/mês de nascimento + nome + início do nome da mãe
)




#########################################################################
#Cria chaves simplificadas a partir dos dados de cada pessoa para serem usadas no blocking, reduzindo o número de pares que precisarão ser comparados.

ln_criar_chaves <- function(p) {
  p[, k_cns    := cns]          #Se duas pessoas tiverem o mesmo CNS, elas podem entrar no mesmo bloco
  p[, k_dn     := as.character(dn)] #chave de blocking baseada na data de nascimento completa
  p[, k_anomes := substr(as.character(dn), 1, 7)] #ano e mês de nascimento, usado como chave de blocking mais flexível que a data completa.
  p[, k_nome   := fifelse(!is.na(nome_n), paste0(substr(nome_1, 1, 4), "|", substr(nome_u, 1, 4)), NA_character_)] #primeiros 4 caracteres do primeiro nome + primeiros 4 caracteres do último nome.
  p[, k_mae    := fifelse(!is.na(mae_n),  paste0(substr(mae_1, 1, 4), "|", substr(mae_u, 1, 4)),  NA_character_)] #primeiros 4 caracteres do primeiro e do último nome da mãe
  p[, k_mae1   := substr(mae_1, 1, 3)] #primeiros 3 caracteres do primeiro nome da mãe.
  p[]
}


###########################################################################
#Gera os pares candidatos entre os dois bancos usando os diferentes passos de blocking

ln_gerar_candidatos <- function(A, B, passos = ln_passos_padrao(),
                                max_pares_bloco = 5e5, verbose = TRUE) { #max_pares_bloco: se um bloco produzir pares demais, ele é ignorado. Isso evita que um nome muito comum, por exemplo, gere milhões de combinações.
  saida <- vector("list", length(passos))
  for (j in seq_along(passos)) {
    ks <- passos[[j]]
    a <- A[stats::complete.cases(A[, ..ks]), c("id_paciente", ks), with = FALSE]
    b <- B[stats::complete.cases(B[, ..ks]), c("id_paciente", ks), with = FALSE]
    setnames(a, "id_paciente", "ida"); setnames(b, "id_paciente", "idb")

    # teto de segurança: blocos absurdos (nomes muito frequentes) são separados
    tam <- merge(a[, .N, by = ks], b[, .N, by = ks], by = ks, suffixes = c(".a", ".b"))
    tam[, pares := as.numeric(N.a) * as.numeric(N.b)]
    grandes <- tam[pares > max_pares_bloco]
    if (nrow(grandes) && verbose)   #informa também quantas pessoas de A
      cat(sprintf("    %s: %d bloco(s) acima do teto ignorado(s) (%s pessoas de A)\n",
                  names(passos)[j], nrow(grandes), format(sum(grandes$N.a), big.mark = ",")))
    if (nrow(grandes)) {
      a <- a[!grandes, on = ks]; b <- b[!grandes, on = ks]
    }
    r <- merge(a, b, by = ks, allow.cartesian = TRUE)[, .(ida, idb)]
    if (verbose) cat(sprintf("    %s: %s pares candidatos\n",
                             names(passos)[j], format(nrow(r), big.mark = ",")))
    saida[[j]] <- r
  }
  unique(rbindlist(saida)) #junta todos os candidatos dos diferentes passos e elimina duplicatas.
}



#=======================================================================
# 4. COMPARAÇÃO E ESCORE FELLEGI-SUNTER
#=======================================================================

#Calcula o peso de concordância e discordância de cada variável no método Fellegi-Sunter: cada campo contribui com log2(m/u) quando concorda e log2((1-m)/(1-u)) quando discorda, e contribui ZERO quando está ausente em um dos lados (ausência não é discordância).
# Um CNS diferente, por exemplo, passa a ser um indício contrário moderado e não um veto

ln_pesos <- function(m, u) c(mais = log2(m / u), menos = log2((1 - m) / (1 - u)))


#########################################################################
#Define os valores usados para: nome; nome da mãe; data de nascimento; CNS
#m = probabilidade de concordância quando os registros são da mesma pessoa.
#u = probabilidade de concordância quando os registros são de pessoas diferent
#Também são definidos os pontos de corte de similaridade.

ln_par_escore <- function(
    m_nome = 0.92, u_nome = 0.004,
    m_mae  = 0.88, u_mae  = 0.008,
    m_data = 0.93, u_data = 1 / 3650,
    m_cns  = 0.85, u_cns  = 1e-6,
    corte_nome = 0.85, corte_mae = 0.85, corte_data = 0.80,
    corte_cns  = 0.50   
) as.list(environment())


##########################################################################
#Compara cada par candidato campo a campo e calcula um escore de similaridade Fellegi-Sunter.
#Primeiro ela recupera, para cada candidato: nome; nome da mãe; data de nascimento; CNS, depois calcula e usa Jaro-Winkler para medir similaridade.

ln_comparar <- function(cand, A, B, par = ln_par_escore()) {
  pa <- A[, .(id_paciente, nome_a = nome_n, mae_a = mae_n, dn_a = dn, cns_a = cns)]
  pb <- B[, .(id_paciente, nome_b = nome_n, mae_b = mae_n, dn_b = dn, cns_b = cns)]
  x <- pa[cand, on = c(id_paciente = "ida")]
  setnames(x, "id_paciente", "ida")
  x <- pb[x, on = c(id_paciente = "idb")]
  setnames(x, "id_paciente", "idb")
  setcolorder(x, c("ida", "idb"))

  if ("id_pessoa_verdadeiro" %in% names(A) && "id_pessoa_verdadeiro" %in% names(B)) {
    x[A, id_verd_a := i.id_pessoa_verdadeiro, on = c(ida = "id_paciente")]   
    x[B, id_verd_b := i.id_pessoa_verdadeiro, on = c(idb = "id_paciente")]  
  }

  x[, s_nome := fifelse(!is.na(nome_a) & !is.na(nome_b),
                        1 - stringdist(nome_a, nome_b, method = "jw", p = 0.1), NA_real_)]
  x[, s_mae  := fifelse(!is.na(mae_a) & !is.na(mae_b),
                        1 - stringdist(mae_a, mae_b, method = "jw", p = 0.1), NA_real_)]
  x[, s_data := {
      igual <- fifelse(!is.na(dn_a) & !is.na(dn_b), as.numeric(dn_a == dn_b), NA_real_)
      inv <- !is.na(igual) & igual == 0 &
             mday(dn_a) == month(dn_b) & month(dn_a) == mday(dn_b) & year(dn_a) == year(dn_b)
      fifelse(inv, 0.85, igual)                 # dia/mês trocados: concordância parcial
  }]
  x[, s_cns := fifelse(!is.na(cns_a) & !is.na(cns_b),
                fifelse(cns_a == cns_b, 1,
                 fifelse(stringdist(cns_a, cns_b, method = "dl") <= 1, 0.6, 0)), NA_real_)]


#Transforma a similaridade de cada campo em um peso que será somado ao escore final.
  contrib <- function(s, w, corte) {
    g <- pmin(pmax((s - corte) / (1 - corte), 0), 1)
    fifelse(is.na(s), 0, w["menos"] + g * (w["mais"] - w["menos"]))
  } # Se a variável estiver ausente, ela contrbui com zero, ou seja, Dado ausente não é tratado como discordância.

  w_nome <- ln_pesos(par$m_nome, par$u_nome); w_mae <- ln_pesos(par$m_mae, par$u_mae)
  w_data <- ln_pesos(par$m_data, par$u_data); w_cns <- ln_pesos(par$m_cns, par$u_cns)

#Escore final
#Soma as evidências de nome, mãe, data de nascimento e CNS para produzir o escore final de cada par.

  x[, escore := contrib(s_nome, w_nome, par$corte_nome) +
                contrib(s_mae,  w_mae,  par$corte_mae)  +
                contrib(s_data, w_data, par$corte_data) +
                contrib(s_cns,  w_cns,  par$corte_cns)]
  x[, n_campos := rowSums(cbind(!is.na(s_nome), !is.na(s_mae), ! is.na(s_data), !is.na(s_cns)))]
  x[]
} #n_campos: informa quantos campos puderam ser comparados.



#Seleciona os pares com escore suficiente e garante que cada pessoa seja vinculada a no máximo uma pessoa do outro banco. O par precisa ter pelo menos 2 campos comparáveis e escore ≥ 14.
#O algoritmo é iterativo: escolhe os melhores pares; retira as pessoas já pareadas; procura novamente entre as pessoas restantes; repete até não haver mais pares elegíveis.

ln_decidir <- function(cmp, limiar = 14, min_campos = 2L, max_iter = 100L) {
  ok <- cmp[n_campos >= min_campos & escore >= limiar]
  setorder(ok, -escore, ida, idb)
  aceitos <- ok[0]
  for (it in seq_len(max_iter)) {
    if (!nrow(ok)) break
    rodada <- ok[!duplicated(ida)][!duplicated(idb)]
    aceitos <- rbind(aceitos, rodada)
    ok <- ok[!ida %in% rodada$ida & !idb %in% rodada$idb]
  }
  setorder(aceitos, -escore)
  aceitos[]
}

#=======================================================================
# 5. AVALIAÇÃO
#=======================================================================
#Essa parte serve para avaliar o desempenho do linkage. Depende de existir uma variável id_pessoa_verdadeiro, ou seja, uma identificação verdadeira conhecida. É principalmente útil para dados simulados ou dados em que o pareamento verdadeiro é conhecido. O código alerta que essas variáveis não existem nos dados reais.

#Ela calcula: sensibilidade; VPP/PPV; F1; verdadeiros positivos; falsos positivos.

ln_avaliar <- function(pares, A, B, prefixo_comum = "^COMUM_") {
  esperados <- intersect(A[grepl(prefixo_comum, id_pessoa_verdadeiro), unique(id_pessoa_verdadeiro)],
                         B[grepl(prefixo_comum, id_pessoa_verdadeiro), unique(id_pessoa_verdadeiro)])
  certos <- pares[id_verd_a == id_verd_b]
  vp <- uniqueN(certos$id_verd_a); fp <- nrow(pares) - nrow(certos)
  sens <- vp / length(esperados); ppv <- nrow(certos) / max(nrow(pares), 1)
  data.table(esperados = length(esperados), pares = nrow(pares),
             verdadeiros = vp, falsos_positivos = fp,
             sensibilidade = round(100 * sens, 2), ppv = round(100 * ppv, 2),
             f1 = round(100 * 2 * sens * ppv / (sens + ppv), 2))
}


##########################################################################

#' Explica campo a campo por que os pares verdadeiros não foram encontrados

ln_diagnosticar_perdas <- function(pares, cmp, A, B, prefixo_comum = "^COMUM_") {
  esperados <- intersect(A[grepl(prefixo_comum, id_pessoa_verdadeiro), unique(id_pessoa_verdadeiro)],
                         B[grepl(prefixo_comum, id_pessoa_verdadeiro), unique(id_pessoa_verdadeiro)])
  achados <- pares[id_verd_a == id_verd_b, unique(id_verd_a)]
  perdidos <- setdiff(esperados, achados)
  em_cand <- cmp[id_verd_a == id_verd_b & id_verd_a %in% perdidos]
  data.table(
    perdidos_total = length(perdidos),
    perdidos_no_blocking = length(perdidos) - uniqueN(em_cand$id_verd_a),
    perdidos_no_escore   = uniqueN(em_cand$id_verd_a),
    escore_mediano_perdido = round(median(em_cand$escore), 2)
  )
}


######################################################################
#Testa diferentes pontos de corte do escore para identificar um limiar adequado ao desempenho desejado.
#Pode escolher o limiar considerando: F1; VPP; sensibilidade.
#Quanto maior o número de possíveis comparações, maior pode ser a necessidade de aumentar o limiar para evitar falsos pareamentos.

ln_calibrar_limiar <- function(cmp, A, B, limiares = seq(4, 30, by = 1),
                               criterio = c("f1", "vpp", "sensibilidade"),
                               alvo = 99, min_campos = 2L) {
  criterio <- match.arg(criterio)
  grade <- rbindlist(lapply(limiares, function(L)
    cbind(limiar = L, ln_avaliar(ln_decidir(cmp, L, min_campos), A, B))))
  escolhido <- switch(criterio,
    f1            = grade[which.max(f1), limiar],
    vpp           = { ok <- grade[ppv >= alvo]
                      if (!nrow(ok)) NA_real_ else ok[which.max(sensibilidade), limiar] },
    sensibilidade = { ok <- grade[sensibilidade >= alvo]
                      if (!nrow(ok)) NA_real_ else ok[which.max(ppv), limiar] })
  list(limiar = escolhido, grade = grade[],
       ajuste_escala = function(na_novo, nb_novo, na_atual = nrow(A), nb_atual = nrow(B))
         escolhido + log2((as.numeric(na_novo) * nb_novo) / (as.numeric(na_atual) * nb_atual)))
}


######################################################################
#Estima empiricamente os parâmetros m e u nos dados reais para verificar se os valores usados no escore são compatíveis com as bases reais.
#Ela faz isso usando: pares aleatórios para estimar u; pares com CNS idêntico para aproximar m.
#Essa função apenas diagnostica; ela não altera automaticamente os pesos usados no linkage
#u: taxa de concordância em pares aleatórios A x B (quase todos não pares).
#m: taxa de concordância nos pares com CNS válido idêntico (quase todos verdadeiros). Compare com ln_par_escore(): diferenças grandes indicam que os pesos da simulação não representam os dados reais e que o limiar deve ser revisto. 

ln_estimar_mu <- function(pA, pB, n_aleatorios = 2e5, par = ln_par_escore(), semente = 1L) {
  set.seed(semente)
  concord <- function(x) x[, .(
    nome = mean(s_nome >= par$corte_nome, na.rm = TRUE),
    mae  = mean(s_mae  >= par$corte_mae,  na.rm = TRUE),
    data = mean(s_data == 1,              na.rm = TRUE))]
  alea <- data.table(ida = sample(pA$id_paciente, n_aleatorios, replace = TRUE),
                     idb = sample(pB$id_paciente, n_aleatorios, replace = TRUE))
  u <- concord(ln_comparar(alea, pA, pB, par))
  cns_iguais <- merge(pA[!is.na(cns), .(ida = id_paciente, cns)],
                      pB[!is.na(cns), .(idb = id_paciente, cns)], by = "cns")[, .(ida, idb)]
  m <- concord(ln_comparar(cns_iguais, pA, pB, par))
  rbind(cbind(parametro = "u_empirico", u), cbind(parametro = "m_empirico", m),
        data.table(parametro = "usado_no_escore_m", nome = par$m_nome, mae = par$m_mae, data = par$m_data),
        data.table(parametro = "usado_no_escore_u", nome = par$u_nome, mae = par$u_mae, data = par$u_data))
}



#=======================================================================
# 6. PIPELINE
#=======================================================================

#Função que junta tudo. Executa todo o linkage do início ao fim.

ln_pipeline <- function(banco_a, banco_b, limiar = 14, verbose = TRUE,
                        passos = ln_passos_padrao(), par_escore = ln_par_escore(),
                        col_a = list(nome = "nome_paciente", mae = "nome_mae", data = "data_nasc",
                                     cns = "numero_sus", sexo = NULL, mun = NULL),
                        col_b = col_a,
                        col_data_dedup_a = NULL, col_data_dedup_b = NULL,
                        janela_dedup_dias = 90L) {
  
  if (verbose) cat("[1/6] Padronizando campos...\n")
  A <- ln_preparar(banco_a, col_a$nome, col_a$mae, col_a$data, col_a$cns, col_a$sexo, col_a$mun)
  B <- ln_preparar(banco_b, col_b$nome, col_b$mae, col_b$data, col_b$cns, col_b$sexo, col_b$mun)


  if (verbose) cat("[2/6] Identificando pessoas dentro de cada banco (componentes conexos)...\n")
  A <- ln_identificar_pessoas(A); B <- ln_identificar_pessoas(B)

  if (verbose) cat("[3/6] Removendo duplicatas temporais (mesmo episódio/dose)...\n")
  if (!is.null(col_data_dedup_a)) {
    n_antes <- nrow(A)
    A <- ln_remover_duplicatas_temporais(A, col_data_dedup_a, janela_dedup_dias)
    if (verbose) cat(sprintf("      A: %s -> %s registros (%s duplicatas removidas)\n",
                             format(n_antes, big.mark = ","), format(nrow(A), big.mark = ","),
                             format(n_antes - nrow(A), big.mark = ",")))
  } else if (verbose) cat("      A: col_data_dedup_a não informado — etapa pulada\n")
  if (!is.null(col_data_dedup_b)) {
    n_antes <- nrow(B)
    B <- ln_remover_duplicatas_temporais(B, col_data_dedup_b, janela_dedup_dias)
    if (verbose) cat(sprintf("      B: %s -> %s registros (%s duplicatas removidas)\n",
                             format(n_antes, big.mark = ","), format(nrow(B), big.mark = ","),
                             format(n_antes - nrow(B), big.mark = ",")))
  } else if (verbose) cat("      B: col_data_dedup_b não informado — etapa pulada\n")

  pA <- ln_criar_chaves(ln_perfil_pessoa(A))
  pB <- ln_criar_chaves(ln_perfil_pessoa(B))
  if (verbose) cat(sprintf("      pessoas: A = %s | B = %s\n",
                           format(nrow(pA), big.mark = ","), format(nrow(pB), big.mark = ",")))

  if (verbose) cat("[4/6] Blocking multi-passo...\n")
  cand <- ln_gerar_candidatos(pA, pB, passos, verbose = verbose)
  if (verbose) cat(sprintf("      candidatos únicos: %s\n", format(nrow(cand), big.mark = ",")))

  if (verbose) cat("[5/6] Comparando e pontuando (Fellegi-Sunter)...\n")
  cmp <- ln_comparar(cand, pA, pB, par_escore)

  if (verbose) cat("[6/6] Decidindo pares (resolução 1:1 iterativa)...\n")
  pares <- ln_decidir(cmp, limiar)

  list(pares = pares, comparacoes = cmp, pessoas_a = pA, pessoas_b = pB,
       registros_a = A, registros_b = B)
}
