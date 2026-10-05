#=======================================================================
# FUNÇÕES DE DERIVAÇÃO — variáveis do Material Suplementar S06
# Executadas na SAR, depois do linkage (01_funcoes_linkage.R).


#Resumo
#Este script recebe o resultado do record linkage entre SINAN e SI-PNI e deriva as variáveis necessárias para a análise da efetividade da vacina. Primeiro unifica as pessoas, consolida as doses, define a janela de seguimento, classifica os eventos de dengue e o status vacinal, calcula o tempo de seguimento e pessoa-tempo e, por fim, gera as bases individuais e a tabela agregada para o modelo de Poisson.

#E a sequência lógica é:
#RESULTADO DO LINKAGE
#        ↓
#dv_unificar()
#        ↓
#Identificação única da pessoa (pid)
#        ↓
#dv_consolidar_doses()
#        ↓
#D1 / D2
#        ↓
#dv_janela()
#        ↓
#Entrada e saída do seguimento
#        ↓
#dv_eventos()
#        ↓
#Definição dos desfechos
#        ↓
#dv_primeiro_evento()
#        ↓
#Primeiro evento de cada desfecho
#        ↓
#dv_intervalos()
#        ↓
#Pessoa-tempo por status vacinal e idade
#        ↓
#dv_tabela_diaria()
#        ↓
#Tabela para análise de Poisson

# Entradas: a lista devolvida por ln_pipeline() (registros_a = SINAN,
# registros_b = SI-PNI, pares) e os nomes das colunas de interesse.
# Saídas:
#   - dv_base_pessoas()      -> Quadro S06.3, nível pessoa
#   - dv_base_notificacoes() -> Quadro S06.3, nível notificação
#   - dv_tabela_diaria()     -> Quadro S06.4 (entrada do modelo de Poisson)
# As mesmas funções servem à análise local de Ribeirão Preto (seção 4.7.5).
#
#Resultado do linkage
#↓
#identifica as pessoas
#↓
organiza as doses
#↓
#define período de acompanhamento
#↓
#identifica os casos
#↓
#calcula tempo de exposição à vacina
#↓
#monta as bases para análise.

#Produz principalmente:
#base de pessoas — Quadro S06.3;
#base de notificações — Quadro S06.3;
#tabela diária de pessoas-tempo e casos — Quadro S06.4.

#=======================================================================

#-----------------------------------------------------------------------
# 1. Utilitários de tempo
#-----------------------------------------------------------------------

# Data do k-ésimo aniversário; nascidos em 29/02 fazem aniversário em 01/03
# nos anos não bissextos (Quadro S06.2).

dv_aniversario <- function(dn, k) {
  a  <- year(dn) + as.integer(k)
  md <- format(dn, "%m-%d")
  d  <- suppressWarnings(as.IDate(sprintf("%d-%s", a, md), format = "%Y-%m-%d"))
  i  <- which(is.na(d) & !is.na(dn) & md == "02-29")
  if (length(i)) d[i] <- as.IDate(sprintf("%d-03-01", a[i]))
  d
}


# Idade em anos completos na data d, pelo aniversário exato. Considerando se o aniversário já ocorre
dv_idade <- function(dn, d) {
  md_d  <- month(d) * 100L + mday(d)
  md_dn <- month(dn) * 100L + mday(dn)
  md_dn[md_dn == 229L] <- 301L                 # 29/02 -> 01/03
  year(d) - year(dn) - as.integer(md_d < md_dn)
}



# Status vacinal no dia t para a janela w (Quadro S06.2)
#Classifica o status vacinal da pessoa na data t, considerando a janela de 14 dias após cada dose.
dv_status <- function(t, d1, d2, w = 14L) { #janela de 14 dias
  w <- as.integer(w)
  fcase(!is.na(d2) & t >= d2 + w + 1L, "vacinado",
        !is.na(d1) & t >= d1 + w + 1L, "parcialmente_vacinado",
        default = "nao_vacinado")
}



#-----------------------------------------------------------------------
# 2. Unificação das pessoas entre as bases
#-----------------------------------------------------------------------
# Cria um identificador único de pessoa (pid) e o grupo do linkage:
# 1 = pareado (notificado e vacinado); 2 = vacinado sem notificação;
# 3 = notificado sem registro de vacinação.
#Essa função transforma as duas bases: SINAN e SI-PNI em uma única população de pessoas. O resultado terá três grupos:
#Grupo 1: pessoa pareada: SINAN + SI-PNI
#Grupo 2:pessoa vacinada sem notificação
#Grupo 3:pessoa notificada sem registro de vacinação
#Classifica todas as pessoas em três grupos: pareadas, vacinadas sem notificação e notificadas sem vacinação.

dv_unificar <- function(res) {
  pares <- res$pares[, .(ida, idb)] #Pega os pares encontrados pelo linkage
  ids_a <- unique(res$registros_a$id_paciente)
  ids_b <- unique(res$registros_b$id_paciente) #Obtém todas as pessoas existentes em A = SINAN; em B = SI-PNI
  so_a  <- setdiff(ids_a, pares$ida)#Identifica quem ficou sem pareamento: pessoas do SINAN sem vacinação encontrada.
  so_b  <- setdiff(ids_b, pares$idb)##Identifica quem ficou sem pareamento: pessoas do SI-PNI sem notificação no SINAN.
  mapa <- rbindlist(list(   #Criando o mapa
    pares[, .(ida, idb, grupo_linkage = 1L)], #pareados
    data.table(ida = NA_integer_, idb = so_b, grupo_linkage = 2L), #vacinados sem notificação
    data.table(ida = so_a, idb = NA_integer_, grupo_linkage = 3L))) #notificados sem vacinação
  mapa[, pid := .I] #Cria o identificador único da pessoa (pid) usado nas análises seguintes.


  # Atributos da pessoa: primeiro valor não ausente, com prioridade ao SI-PNI
  atrib <- function(reg, col_id) {   #pega de cada pessoa
    reg <- copy(reg)[, .(id = id_paciente, dn, sexo, mun)] #procura o primeiro valor disponível para cada variável.
    reg[, .(dn = dn[!is.na(dn)][1L], sexo = sexo[!is.na(sexo)][1L],
            mun = mun[!is.na(mun)][1L]), by = id]
  }
  ab <- atrib(res$registros_b); aa <- atrib(res$registros_a) #ab (SI-PNI) e aa (Sinan)
  p <- merge(mapa, ab, by.x = "idb", by.y = "id", all.x = TRUE)
  p <- merge(p, aa, by.x = "ida", by.y = "id", all.x = TRUE, suffixes = c("", "_a")) #Junta as informações das duas bases.
  p[is.na(dn),   dn   := dn_a]
  p[is.na(sexo), sexo := sexo_a]
  p[is.na(mun),  mun  := mun_a]
  p[, c("dn_a", "sexo_a", "mun_a") := NULL]Se o SI-PNI não tiver a informação, utiliza a informação do SINAN.
  setkey(p, pid)
  p[]
}#Unifica os dados pessoais das duas bases, dando prioridade ao SI-PNI e usando o SINAN quando o dado estiver ausente.

#-----------------------------------------------------------------------
# 3. Consolidação das doses (Quadro S06.1)
#-----------------------------------------------------------------------
#Transforma os registros de vacinação em D1 e D2 por pessoa

#reg_b: registros do SI-PNI já deduplicados no linkage (janela de 90 dias
# por pessoa: registros a menos de 90 dias do último mantido são duplicidade).
# Depois disso, as doses restantes de cada pessoa distam 90 dias ou mais
# entre si, e a ordem é dada pela data de aplicação: D1 = mais antiga,
# D2 = a seguinte. O campo de número da dose não é usado, por ser de
# preenchimento não obrigatório e pouco preenchido.

dv_consolidar_doses <- function(reg_b, pessoas, col_data_vac) {
  v <- reg_b[, .(idb = id_paciente, dt = ln_parse_data(get(col_data_vac)))] #Pega:identificador da pessoa e data da vacinação.
  v <- pessoas[!is.na(idb), .(idb, pid)][v, on = "idb", nomatch = NULL] #Relaciona cada vacinação ao pid.
  n_sem_data <- v[is.na(dt), .(n_registros_sem_data = .N), by = pid]
  v <- unique(v[!is.na(dt)], by = c("pid", "dt")) #Se houver dois registros da mesma pessoa na mesma data, considera apenas um
  setorder(v, pid, dt) #ordena por pessoa e data
  out <- v[, .(dt_d1 = dt[1L], dt_d2 = dt[2L], n_doses = .N), by = pid] #define: primeira dose = D1; segunda dose = D2; número total de doses. Importante: a ordem é determinada pela data, e não pelo campo de número da dose do SI-PNI.
  out <- n_sem_data[out, on = "pid"]
  out[is.na(n_registros_sem_data), n_registros_sem_data := 0L]
  out[, `:=`(intervalo_d1_d2 = as.integer(dt_d2 - dt_d1), #Calcula quantos dias existem entre as duas doses.
             flag_doses_extras = n_doses > 2L)] #marca pessoas com mais de duas doses
  setcolorder(out, c("pid", "dt_d1", "dt_d2", "intervalo_d1_d2", "n_doses",
                     "flag_doses_extras", "n_registros_sem_data"))
  out[]
} 


#-----------------------------------------------------------------------
# 4. Janela de seguimento e eventos
#-----------------------------------------------------------------------
#Define a janela individual de acompanhamento: entrada aos 10 anos e saída antes dos 15 anos ou em 31/12/2025, o que ocorrer primeiro. 
#entrada = max(ini, 10º aniversário[, marco municipal]);
# saida_adm = min(fim, véspera do 15º aniversário).
# restringir_marco = TRUE: só entram pessoas residentes em municípios com
#     marco (municípios que ofertaram a vacina), como no denominador do IBGE;
#     pessoas sem município de residência também saem.
#   entrada_no_marco = TRUE: a entrada passa a ser o marco do município;
#     FALSE (padrão): todos entram em 'ini' (01/01/2024), e o marco serve
#     só para definir os municípios.


dv_janela <- function(pessoas, ini = as.IDate("2024-01-01"), fim = as.IDate("2025-12-31"),
                      idade_min = 10L, idade_max = 14L, marco = NULL,
                      restringir_marco = TRUE, entrada_no_marco = FALSE) {
  p <- pessoas[!is.na(dn), .(pid, dn, sexo, mun)]
  p[, entrada   := pmax(ini, dv_aniversario(dn, idade_min))] #A pessoa entra no estudo na data mais tardia entre: 01/01/2024; seu 10º aniversário.
  p[, saida_adm := pmin(fim, dv_aniversario(dn, idade_max + 1L) - 1L)] #Sai na data mais cedo entre 31/12/2025 e véspera do 15º aniversário.
  if (!is.null(marco)) {
    p <- marco[, .(mun, dt_marco)][p, on = "mun"]
    if (restringir_marco) p <- p[!is.na(dt_marco)]
    if (entrada_no_marco) p[!is.na(dt_marco), entrada := pmax(entrada, dt_marco)]
    p[, dt_marco := NULL]
  }
  p[entrada <= saida_adm]
}

# Marco municipal (seção 4.2): data a partir da qual o município passa
# a ser considerado ofertante da vacina. Usa as doses de Qdenga já
# deduplicadas (registros_b do linkage), aplicadas a pessoas de idade_min a
# idade_max anos na data da dose, dentro do período do estudo.
# col_mun: coluna do município que define a oferta (padrão: município do
# estabelecimento que aplicou a dose, "administrada localmente").
# min_doses: o município só é marcado quando acumula esse número de doses
# nessa faixa etária; dt_marco é a data da min_doses-ésima dose. Com 1,
# é a primeira dose (texto atual da seção 4.2). Um valor maior evita que doses esporádicas (remanejamento de doses próximas do vencimento, 2025)
# marquem o município.
# Saída agregada (sem identificação): mun, dt_marco, n_doses_faixa.

dv_marco_municipal <- function(reg_b, col_data_vac, col_mun, idade_min = 10L, idade_max = 14L,
                               ini = as.IDate("2024-01-01"), fim = as.IDate("2025-12-31"),
                               min_doses = 1L) {
  stopifnot(col_mun %in% names(reg_b))
  v <- reg_b[, .(mun = ln_municipio6(get(col_mun)), dn, dt = ln_parse_data(get(col_data_vac)))]
  v <- v[!is.na(mun) & !is.na(dn) & !is.na(dt) & dt >= ini & dt <= fim]
  v <- v[dv_idade(dn, dt) >= idade_min & dv_idade(dn, dt) <= idade_max]
  setorder(v, mun, dt)
  v[, .(n_doses_faixa = .N,
        dt_marco = if (.N >= min_doses) dt[min_doses] else as.IDate(NA)), by = mun][!is.na(dt_marco)]
}

#dt_marco = ... define a data em que o município alcançou o número mínimo de doses estabelecido por min_doses, se min_doses = 1.
#Identifica a data a partir da qual cada município passa a ser considerado ofertante da vacina, com base nos registros de doses.



# Classifica cada notificação nas três definições de desfecho (Quadro S06.2).
#Identificação dos eventos de dengue
#dv_eventos() é uma função central, classifica cada notificação do SINAN em três definições de desfecho: evento_principal; evento_direto; evento_confirmado.

# cols: lista com os nomes das colunas do SINAN.

#Primeiro ela pega as informações clínicas do SINAN: classi, hospitaliz, evolucao, pcr, ns1, isolamento

dv_eventos <- function(reg_a, pessoas, cols, usar_marcadores = TRUE) {
  g <- function(nm) if (!is.null(cols[[nm]]) && cols[[nm]] %in% names(reg_a))
                      as.character(reg_a[[cols[[nm]]]]) else rep(NA_character_, nrow(reg_a))
  e <- data.table(ida = reg_a$id_paciente,
                  dt_sin_pri = ln_parse_data(g("sin_pri")),
                  dt_notific = ln_parse_data(g("notific")),
                  dt_interna = ln_parse_data(g("interna")),
                  classi = g("classi"), hospitaliz = g("hospitaliz"), evolucao = g("evolucao"),
                  pcr = g("pcr"), ns1 = g("ns1"), iso = g("isolamento"))
  e[, n_linha := .I]                 #posição no reg_a, para recuperar outras colunas
  e <- pessoas[!is.na(ida), .(ida, pid)][e, on = "ida", nomatch = NULL]   #só pessoas mantidas
  e[, evento_principal := classi %in% c("11", "12")]
  if (usar_marcadores) #também considera situações de hospitalização /evolução compatíveis quando a classificação está ausente ou é específica
    e[(is.na(classi) | classi %in% "8") & (hospitaliz %in% "1" | evolucao %in% "2"),
      evento_principal := TRUE]
  e[, evento_direto     := evento_principal & (pcr %in% "1" | ns1 %in% "1" | iso %in% "1")] #evento principal + confirmação laboratorial direta
  e[, evento_confirmado := classi %in% c("10", "11", "12")] #Usa a classificação clínica final correspondente.
  e[]
}

#Classifica cada notificação em três definições de desfecho: principal, diretamente confirmado por marcador laboratorial e confirmado pela classificação do SINAN.



########################################################################
#Procura o primeiro evento daquele desfecho dentro da janela de acompanhamento

dv_primeiro_evento <- function(eventos, janela, desfecho) {
  col <- paste0("evento_", desfecho)
  e <- janela[, .(pid, entrada, saida_adm)][eventos[get(col) == TRUE & !is.na(dt_sin_pri),##primeiro seleciona como data do início dos sintomas. 
                                                    .(pid, dt_sin_pri)], on = "pid", nomatch = NULL]
  e <- e[dt_sin_pri >= entrada & dt_sin_pri <= saida_adm] #Depois restringe, ou seja, o evento precisa ocorrer no perído que a pessoa está send acompanhada 
  if (!nrow(e)) return(data.table(pid = janela$pid[0], dt_evento = as.IDate(character())))  
  e[, .(dt_evento = min(dt_sin_pri)), by = pid] #pega o primeiro evento.
}

#Identifica o primeiro evento de cada desfecho ocorrido dentro da janela individual de seguimento.


#-----------------------------------------------------------------------
# 5. Intervalos de pessoa-tempo (vacinados) por status e idade
#-----------------------------------------------------------------------

#Transforma a história vacinal de cada pessoa em períodos de: parcialmente vacinado; vacinado; opcionalmente não vacinado.

# Para uma janela w e um desfecho, devolve um intervalo por pessoa x status
# (parcial/vacinado) x idade, com [ini, fim] já cortados pela entrada, pela
# saída e pelo primeiro evento (fim do seguimento).
# incluir_nao_vacinado = TRUE acrescenta também o período não vacinado de
# cada pessoa (usado na análise de Ribeirão Preto, com denominador nominal;
# na análise nacional esse período vem da população do IBGE).


dv_intervalos <- function(janela, doses, prim_evento, w = 14L,
                          idade_min = 10L, idade_max = 14L, incluir_nao_vacinado = FALSE) {
  w <- as.integer(w)
  p <- doses[, .(pid, dt_d1, dt_d2)][janela, on = "pid", nomatch = if (incluir_nao_vacinado) NA else NULL]
  p <- prim_evento[p, on = "pid"]
  p[, saida := pmin(saida_adm, dt_evento, na.rm = TRUE)]
  st <- rbindlist(list(
    p[, .(pid, status = "parcialmente_vacinado", ini = dt_d1 + w + 1L, #Começa após a janela de 14 dias da D1.
          fim = fifelse(is.na(dt_d2), saida, dt_d2 + w))], #termina a janela correspondete à D2
    p[!is.na(dt_d2), .(pid, status = "vacinado", ini = dt_d2 + w + 1L, fim = saida)], #Começa após os 14 dias da D2 e vai até o final do acompanhamento ou primeiro evento
    if (incluir_nao_vacinado)
      p[, .(pid, status = "nao_vacinado", ini = entrada,
            fim = fifelse(is.na(dt_d1), saida, pmin(saida, dt_d1 + w)))]))
  st <- st[!is.na(ini)]
  st <- p[, .(pid, entrada, saida, dn, sexo, mun)][st, on = "pid"]
  st[, `:=`(ini = pmax(ini, entrada), fim = pmin(fim, saida))]
  st <- st[ini <= fim]
  # corte pelos aniversários, divide esses períodos conforme idade da pessoa (10, 11, 12, 13 e 14 anos)
  idades <- idade_min:idade_max
  ag <- st[, .(pid, status, ini, fim, dn, sexo, mun)][
    , .(idade = idades), by = .(pid, status, ini, fim, dn, sexo, mun)]
  ag[, `:=`(a_ini = dv_aniversario(dn, idade), a_fim = dv_aniversario(dn, idade + 1L) - 1L)] #calcula para saber a idade
  ag[, `:=`(ini = pmax(ini, a_ini), fim = pmin(fim, a_fim))]
  ag <- ag[ini <= fim, .(pid, status, idade, ini, fim, sexo, mun)]
  ag[, dias := as.integer(fim - ini) + 1L]
  ag[]
}
# Divide o seguimento em períodos por status vacinal e idade, calculando os dias de pessoa-tempo em cada combinação.



#-----------------------------------------------------------------------
# 6. Quadro S06.4 — tabela agregada diária
#-----------------------------------------------------------------------
#Transforma os intervalos anteriores em uma tabela para o modelo de Poisson
# estratos: colunas de agregação (ex.: c("sexo", "idade") para o modelo
# nacional; c("mun", "sexo", "idade") para os modelos estratificados).
# formato = "diario": uma linha por dia x estrato com contagem > 0;
# formato = "intervalos": uma linha por período de contagem constante
# (data_ini, data_fim), equivalente e muito mais compacto.
#algo do tipo

# Data        Sexo       Idade    Status       Pessoas    Casos
# 01/01/24     F           10     Vacinado        X         X
# 01/01/24     F           10     Parcial         X         X
# 01/01/24     F           10     Não vacinado    X         X

#Em vez de criar uma linha para cada pessoa em cada dia, calcula quantas pessoas estão naquele status em cada período. Isso é uma forma mais eficiente de calcular pessoa-tempo.


dv_tabela_diaria <- function(interv, casos, estratos = c("sexo", "idade"),
                             formato = c("diario", "intervalos"),
                             fim_periodo = as.IDate("2025-12-31")) {
  formato <- match.arg(formato)
  by_s <- c(estratos, "status")
 

 # Diferenças: +1 no início do intervalo, -1 no dia seguinte ao fim.
  # A soma acumulada dá o número de pessoas em cada status a partir de
  # cada data de mudança, sem expandir pessoa a pessoa.

  d <- rbindlist(list(
    interv[, c(by_s, "ini"), with = FALSE][, .(data = ini, delta = 1L), by = by_s],
    interv[, c(by_s, "fim"), with = FALSE][, .(data = fim + 1L, delta = -1L), by = by_s]))
  d <- d[, .(delta = sum(delta)), by = c(by_s, "data")]
  setorderv(d, c(by_s, "data"))
  d[, n := cumsum(delta), by = by_s]
  d[, data_fim := shift(data, -1L) - 1L, by = by_s]
  d <- d[n > 0L & !is.na(data_fim) & data <= fim_periodo]
  d[, data_fim := pmin(data_fim, fim_periodo)]
  d[, delta := NULL]

  cs <- casos[, .N, by = c(estratos, "data", "status")]
  if (formato == "intervalos") {
    setnames(d, c("data", "n"), c("data_ini", "n_pessoas"))
    return(list(pessoas_dia = d[], casos = cs[]))
  }

  # formato diário: expande cada período constante em dias

  d[, rid := .I]
  ex <- d[rep(rid, as.integer(data_fim - data) + 1L)]
  ex[, data := data + (seq_len(.N) - 1L), by = rid]
  f <- as.formula(paste(paste(c(estratos, "data"), collapse = " + "), "~ status"))
  tab <- dcast(ex, f, value.var = "n", fill = 0L)
  for (s in c("parcialmente_vacinado", "vacinado")) if (!s %in% names(tab)) tab[, (s) := 0L]
  setnames(tab, c("parcialmente_vacinado", "vacinado"), c("n_parcial", "n_vacinado"))
  if ("nao_vacinado" %in% names(tab)) setnames(tab, "nao_vacinado", "n_nao_vacinado")
  if (nrow(cs)) {
    cs <- dcast(cs, f, value.var = "N", fill = 0L)
  } else cs <- tab[0, c(estratos, "data"), with = FALSE]
  for (s in c("nao_vacinado", "parcialmente_vacinado", "vacinado")) if (!s %in% names(cs)) cs[, (s) := 0L]
  setnames(cs, c("nao_vacinado", "parcialmente_vacinado", "vacinado"),
           c("casos_nao_vacinado", "casos_parcial", "casos_vacinado"))
  tab <- merge(tab, cs, by = c(estratos, "data"), all = TRUE)
  for (cl in c("n_parcial", "n_vacinado", "casos_nao_vacinado", "casos_parcial", "casos_vacinado"))
    set(tab, which(is.na(tab[[cl]])), cl, 0L)
  setorderv(tab, c(estratos, "data"))
  tab[]
}


# Casos do desfecho (primeiro evento), com status e idade na data do evento
#Cria a base de casos, atribuindo a cada evento a data, idade e status vacinal da pessoa no momento do evento.

dv_casos <- function(janela, doses, prim_evento, w = 14L) {
  cz <- janela[, .(pid, dn, sexo, mun)][prim_evento, on = "pid", nomatch = NULL]
  cz <- doses[, .(pid, dt_d1, dt_d2)][cz, on = "pid"]
  cz[, `:=`(data = dt_evento, idade = dv_idade(dn, dt_evento),
            status = dv_status(dt_evento, dt_d1, dt_d2, w))]
  cz[]
}

#-----------------------------------------------------------------------
# 7. Quadro S06.3 — bases individuais
#-----------------------------------------------------------------------
Essa função cria o Quadro S06.3 no nível pessoa. Ela junta: identificação;
grupo do linkage; nascimento; sexo; município; doses; idade na vacinação;
janela de seguimento; ocorrência dos três desfechos; tempo de seguimento.


dv_base_pessoas <- function(pessoas, doses, janela, eventos, w = 14L) {
  b <- doses[pessoas[, .(pid, grupo_linkage, dn, sexo, mun)], on = "pid"]
  b[, idade_anos_vacina := dv_idade(dn, dt_d1)]
  b <- janela[, .(pid, entrada, saida_adm)][b, on = "pid"]
  for (d in c("principal", "direto", "confirmado")) {
    pe <- dv_primeiro_evento(eventos, janela, d)
    b[pe, (paste0("evento_", d)) := 1L, on = "pid"]
    b[is.na(get(paste0("evento_", d))) & !is.na(entrada), (paste0("evento_", d)) := 0L]
    if (d == "principal") b[pe, dt_evento_principal := i.dt_evento, on = "pid"]
  }
  # delta_dia_* (resumo individual; janela w, fim no desfecho principal)
  b[, saida := pmin(saida_adm, dt_evento_principal, na.rm = TRUE)]
  b[, dias_seguimento := fifelse(!is.na(entrada), as.integer(saida - entrada) + 1L, NA_integer_)] #Calcula quantos dias a pessoa ficou em acompanhamento
  w <- as.integer(w)
  sobrepor <- function(a1, a2, b1, b2) pmax(0L, as.integer(pmin(a2, b2) - pmax(a1, b1)) + 1L)
  b[, delta_dia_parcialmente_vacinado := fifelse(is.na(entrada) | is.na(dt_d1), 0L,
        sobrepor(entrada, saida, dt_d1 + w + 1L, fifelse(is.na(dt_d2), saida, dt_d2 + w)))]
  b[, delta_dia_vacinado := fifelse(is.na(entrada) | is.na(dt_d2), 0L,
        sobrepor(entrada, saida, dt_d2 + w + 1L, saida))]
  b[is.na(entrada), c("delta_dia_parcialmente_vacinado", "delta_dia_vacinado") := NA_integer_]
  b[, delta_dia_nao_vacinado := dias_seguimento - delta_dia_parcialmente_vacinado - delta_dia_vacinado]
  b[, c("saida", "dt_evento_principal") := NULL]
  b[]
}

#Monta a base individual com características, vacinação, eventos e tempo de seguimento, incluindo dias em cada status vacinal.



#Aqui a unidade deixa de ser a pessoa e passa a ser a notificação do SINAN.

dv_base_notificacoes <- function(eventos, pessoas, doses, reg_a, w = 14L, cols_clinicas = NULL) {
  n <- doses[, .(pid, dt_d1, dt_d2)][eventos, on = "pid"]
  n <- pessoas[, .(pid, dn)][n, on = "pid"]
  n[, `:=`(idade_anos_dengue = dv_idade(dn, dt_sin_pri),
           status_vacinal = fifelse(is.na(dt_sin_pri), NA_character_,
                                    dv_status(dt_sin_pri, dt_d1, dt_d2, w)),
           tempo_notif_sintomas = as.integer(dt_notific - dt_sin_pri),
           tempo_sintomas_internacao = fifelse(hospitaliz %in% "1",
                                               as.integer(dt_interna - dt_sin_pri), NA_integer_))]
  cols_clinicas <- intersect(cols_clinicas, names(reg_a))
  if (length(cols_clinicas)) n <- cbind(n, reg_a[n$n_linha, ..cols_clinicas])
  n[, c("dn", "dt_d1", "dt_d2", "n_linha", "ida") := NULL]
  n[]
}
#Cria a base no nível de notificação, acrescentando idade, status vacinal e intervalos entre sintomas, notificação e internação.


#-----------------------------------------------------------------------
# 8. Exportação (fora da SAR)
#-----------------------------------------------------------------------
# Substitui o pid por um código aleatório, reduz datas ao nível de mês e
# remove a data de nascimento. A tabela de correspondência fica na SAR.


dv_preparar_exportacao <- function(tab, correspondencia) {
  x <- correspondencia[, .(pid, id_estudo)][copy(tab), on = "pid"]
  x[, pid := NULL]
  datas <- names(x)[vapply(x, inherits, logical(1), what = "Date")]
  for (cl in datas) set(x, j = cl, value = format(x[[cl]], "%Y-%m"))
  if ("dn" %in% names(x)) x[, dn := NULL]
  setcolorder(x, "id_estudo")
  x[]
}


#-----------------------------------------------------------------------
# 9. Os três bancos para exportação (uma pessoa aparece em um só banco)
#-----------------------------------------------------------------------
# Atributos do SI-PNI no registro de cada dose (estabelecimento, fabricante,
# lote...), em colunas com sufixo _d1 e _d2: uma linha por pessoa, sem
# perder a segunda dose.

dv_atributos_doses <- function(reg_b, pessoas, doses, col_data_vac, cols_vacina) {
  cols_vacina <- intersect(cols_vacina, names(reg_b))
  v <- reg_b[, c("id_paciente", col_data_vac, cols_vacina), with = FALSE]
  setnames(v, c("id_paciente", col_data_vac), c("idb", ".dt"))
  v[, .dt := ln_parse_data(.dt)]
  v <- pessoas[!is.na(idb), .(idb, pid)][v, on = "idb", nomatch = NULL]
  v[, idb := NULL]
  v <- unique(v[!is.na(.dt)], by = c("pid", ".dt"))
  d1 <- doses[, .(pid, .dt = dt_d1)][v, on = c("pid", ".dt"), nomatch = NULL]
  d2 <- doses[!is.na(dt_d2), .(pid, .dt = dt_d2)][v, on = c("pid", ".dt"), nomatch = NULL]
  d1[, .dt := NULL]; d2[, .dt := NULL]
  if (length(cols_vacina)) {
    setnames(d1, cols_vacina, paste0(cols_vacina, "_d1"))
    setnames(d2, cols_vacina, paste0(cols_vacina, "_d2"))
  }
  merge(d1, d2, by = "pid", all = TRUE)
}

# Banco 1 (pareados) e banco 3 (notificados sem vacina): uma linha por
# notificação (episódio, após a deduplicação de 90 dias), com as variáveis
# da pessoa repetidas em cada linha. Quem teve duas notificações tem duas
# linhas com o mesmo código:
#   - ordem_notificacao / n_notificacoes_pessoa: posição e total por pessoa
#   - caso_<desfecho>_seguimento = 1 só na notificação que conta como caso
#     daquele desfecho (o primeiro evento dentro do seguimento), a mesma
#     usada nas tabelas agregadas.
# Para contar PESSOAS, use os códigos distintos; para contar CASOS, some
# caso_<desfecho>_seguimento.
# Banco 2 (vacinados sem notificação): uma linha por pessoa, com as duas
# doses em colunas (dt_d1, dt_d2 e atributos _d1/_d2).
dv_bancos_exportacao <- function(base_pessoas, base_notif, attr_doses,
                                 desfechos = c("principal", "direto", "confirmado")) {
  bn <- copy(base_notif)
  crus <- intersect(c("classi", "hospitaliz", "evolucao", "pcr", "ns1", "iso"), names(bn))
  if (length(crus)) bn[, (crus) := NULL]          # já presentes como colunas originais do SINAN
  setorder(bn, pid, dt_sin_pri, na.last = TRUE)
  bn[, `:=`(ordem_notificacao = seq_len(.N), n_notificacoes_pessoa = .N), by = pid]
  bn <- base_pessoas[, .(pid, .ent = entrada, .sai = saida_adm)][bn, on = "pid"]
  for (d in desfechos) {
    ev <- paste0("evento_", d); fl <- paste0("caso_", d, "_seguimento")
    bn[, (fl) := 0L]
    bn[get(ev) == TRUE & !is.na(.ent) & !is.na(dt_sin_pri) & dt_sin_pri >= .ent & dt_sin_pri <= .sai,
       .k := seq_len(.N), by = pid]
    bn[.k == 1L, (fl) := 1L]
    bn[, .k := NULL]
    bn[, (ev) := as.integer(get(ev))]
  }
  bn[, c(".ent", ".sai") := NULL]

  pes <- base_pessoas[, setdiff(names(base_pessoas), paste0("evento_", desfechos)), with = FALSE]
  bnp <- merge(bn, pes, by = "pid", all.x = TRUE)
  bnp <- merge(bnp, attr_doses, by = "pid", all.x = TRUE)

  cols_vac <- c("dt_d1", "dt_d2", "intervalo_d1_d2", "n_doses", "flag_doses_extras",
                "n_registros_sem_data", "idade_anos_vacina",
                grep("_d[12]$", names(attr_doses), value = TRUE))
  banco1 <- bnp[grupo_linkage == 1L]
  banco3 <- bnp[grupo_linkage == 3L][, intersect(names(bnp), setdiff(names(bnp), cols_vac)), with = FALSE]
  banco2 <- merge(base_pessoas[grupo_linkage == 2L], attr_doses, by = "pid", all.x = TRUE)

  primeiras <- c("pid", "grupo_linkage", "ordem_notificacao", "n_notificacoes_pessoa")
  for (b in list(banco1, banco2, banco3)) setcolorder(b, intersect(primeiras, names(b)))
  list(pareados = banco1[], vacinados_sem_notificacao = banco2[], notificados_sem_vacina = banco3[])
}
