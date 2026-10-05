# Linkage entre SINAN e SI-PNI --- Vacinação contra dengue (Qdenga)
# e análise de sensibilidade local (Ribeirão Preto/SP)

## 1. Descrição do projeto

Este projeto contém scripts em R para realizar o **record linkage entre
registros de dengue do Sistema de Informação de Agravos de Notificação
(SINAN) e registros de vacinação contra dengue do Sistema de Informação
do Programa Nacional de Imunizações (SI-PNI)**.

O processamento tem como finalidade:

1.  Padronizar os campos de identificação das pessoas nas duas bases;
2.  Identificar e agrupar registros pertencentes à mesma pessoa dentro
    de cada sistema;
3.  Remover duplicidades temporais de notificações e registros de
    vacinação;
4.  Realizar o pareamento entre pessoas do SINAN e do SI-PNI;
5.  Derivar variáveis relacionadas à idade, vacinação, seguimento e
    ocorrência de dengue;
6.  Construir bases individuais e tabelas agregadas de pessoa-tempo;
7.  Produzir arquivos para análises epidemiológicas e revisão manual.

O código foi estruturado para permitir a execução com bases fictícias de
teste e posterior adaptação para dados reais, que devem ser processados
em ambiente seguro, como a Sala de Acesso Restrito (SAR).

O projeto inclui também a **análise de sensibilidade local em Ribeirão
Preto/SP** (seção 10), executada pela Secretaria Municipal da Saúde
(SMS-RP) com o sistema municipal Hygia, que permite identificar
nominalmente os não vacinados. Ela usa exatamente as mesmas funções de
linkage e de derivação da análise nacional, para que a comparação entre
o denominador nominal (Hygia) e o denominador estimado (IBGE) reflita
apenas a diferença de denominador.

------------------------------------------------------------------------

## 2. Organização dos arquivos

O projeto é composto pelos seguintes scripts:

  -----------------------------------------------------------------------
  Arquivo                             Finalidade
  ----------------------------------- -----------------------------------
  `01_funcoes_linkage.R`              Funções de preparação,
                                      deduplicação, geração de
                                      candidatos, comparação e pareamento
                                      entre SINAN e SI-PNI

  `02_funcoes_derivadas.R`            Funções para criação das variáveis
                                      derivadas, definição do seguimento,
                                      classificação de eventos e
                                      construção das bases analíticas

  `03_execucao_SAR.R`                 Script principal de execução do
                                      processamento, leitura das bases,
                                      aplicação do linkage, geração das
                                      tabelas e exportação dos resultados

  `04_teste_casos_construidos.R`   Script com casos construídos
                                      manualmente para verificar regras
                                      específicas do processamento

  `05_funcoes_ribeirao.R`             Funções exclusivas da análise de
                                      Ribeirão Preto: adaptador do Hygia,
                                      etapa B do linkage, grupos e
                                      covariáveis locais

  `06_execucao_ribeirao.R`            Script de execução da análise de
                                      Ribeirão Preto (rodado pela SMS-RP)

  `07_gerar_dados_teste_ribeirao.R`   Opcional: gera dados fictícios no
                                      formato do Hygia e do SINAN
                                      municipal para testar o 06
  -----------------------------------------------------------------------

Os arquivos `01_funcoes_linkage.R` e `02_funcoes_derivadas.R` são
**comuns às duas análises**: não existe versão exclusiva de nenhum deles.
Recomenda-se manter os sete arquivos em uma única pasta, com uma só cópia
do 01 e do 02, para que qualquer correção valha automaticamente para as
duas análises.

Relação entre os arquivos:

``` text
01_funcoes_linkage.R ----+--> carregados por --> 03_execucao_SAR.R
                         |                       (análise nacional)
02_funcoes_derivadas.R --+
                         +--> carregados por --> 06_execucao_ribeirao.R
05_funcoes_ribeirao.R ---+                       (Ribeirão Preto)

04_teste_casos_construidos.R  (opcional e independente; também carrega
                               o 01 e o 02; recomenda-se rodá-lo antes
                               do 03 para conferir o ambiente)
```

Os arquivos `01_funcoes_linkage.R` e `02_funcoes_derivadas.R` só definem
funções e são carregados pelo script principal a partir do caminho
completo informado em `pasta_scripts`. Portanto, não é necessário
executá-los manualmente nem usar `setwd()`. Os quatro arquivos devem
estar na mesma pasta e com exatamente esses nomes (atenção ao
`02_funcoes_derivadas.R`, com "s" no final) (o navegador pode
acrescentar sufixos como "(1)" ao baixar um arquivo repetido; nesse caso,
renomeie-o).

Existe uma única versão do script 03, configurada para as bases fictícias
de teste (8 a 16 anos): ela grava os resultados na subpasta
`resultados_teste_linkage_v2` e inclui o bloco "AVALIAÇÃO", que usa
`id_pessoa_verdadeiro`. Para os dados reais do Ministério da Saúde, use
este mesmo arquivo e:

1.  troque `pasta_scripts`, `CFG$pasta` e os nomes dos arquivos;
2.  confira os nomes das colunas (`col_sinan`, `col_vacina` e os demais
    itens `[CONFIRMAR]`);
3.  apague o bloco "AVALIAÇÃO" (não existe `id_pessoa_verdadeiro` nos
    dados reais).

A antiga versão SAR (pasta `linkage_v2`) está desatualizada e não deve ser
usada.

O que enviar a cada instituição:

  Destino                         Arquivos
  ------------------------------- ------------------------------------------
  Ministério da Saúde (SAR)       01, 02 e 03
  Prefeitura de Ribeirão Preto    01, 02, 05, 06 e a tabela `ivs_setor.csv`
                                  (e, se necessário, `cep_setor.csv`)

------------------------------------------------------------------------

## 3. Requisitos

### 3.1. Software

-   R;
-   RStudio, preferencialmente para execução e inspeção dos resultados.

### 3.2. Pacotes utilizados

O script de linkage utiliza:

-   `data.table`;
-   `stringi`;
-   `stringdist`.

O script de derivação utiliza os mesmos pacotes (as datas são tratadas
com o tipo `IDate` do `data.table`). Os pacotes são carregados pelo
script 01.

Para instalar os pacotes:

``` r
install.packages(c("data.table", "stringi", "stringdist"))
```

------------------------------------------------------------------------

## 4. Bases de entrada

O processamento utiliza duas bases principais:

### 4.1. Base do SINAN

Contém os registros de notificações de dengue e campos de identificação
e informação clínica.

Entre os campos utilizados estão:

-   Nome do paciente;
-   Nome da mãe;
-   Data de nascimento;
-   Cartão Nacional de Saúde (CNS);
-   Sexo;
-   Município de residência;
-   Data dos primeiros sintomas;
-   Data da notificação;
-   Data da internação;
-   Classificação final;
-   Hospitalização;
-   Evolução;
-   Resultado de PCR;
-   Resultado de NS1;
-   Resultado de isolamento viral;
-   Variáveis clínicas adicionais.

### 4.2. Base do SI-PNI

Contém os registros de vacinação contra dengue, incluindo os campos de
identificação e as datas de aplicação das doses.

Entre os campos utilizados estão:

-   Nome do paciente;
-   Nome da mãe;
-   Data de nascimento;
-   CNS;
-   Sexo;
-   Município de residência;
-   Município do estabelecimento que aplicou a dose (marco municipal);
-   Data de vacinação;
-   Data de exclusão/deleção na RNDS, quando disponível.

Os nomes das colunas podem variar entre os bancos. Por isso, devem ser
configurados explicitamente no objeto `CFG` do script
`03_execucao_SAR.R`.

------------------------------------------------------------------------

## 5. Fluxo geral do processamento

O processamento principal segue as seguintes etapas:

``` text
Leitura das bases
        |
        v
Remoção de registros vacinais deletados na RNDS
        |
        v
Recorte por data de nascimento
        |
        v
Padronização dos campos de identificação
        |
        v
Identificação de pessoas dentro de cada base
        |
        v
Deduplicação temporal
        |
        v
Criação dos perfis individuais
        |
        v
Blocking em múltiplas etapas
        |
        v
Comparação dos pares candidatos
        |
        v
Cálculo do escore Fellegi-Sunter
        |
        v
Resolução 1:1 dos pares
        |
        v
Unificação das pessoas
        |
        v
Marco municipal e restrição aos municípios ofertantes
        |
        v
Consolidação das doses
        |
        v
Definição da janela de seguimento
        |
        v
Classificação dos eventos de dengue
        |
        v
Construção das bases e tabelas analíticas
        |
        v
Validação, revisão manual e exportação
```

------------------------------------------------------------------------

## 6. Script `01_funcoes_linkage.R`

### 6.1. Objetivo

Este script reúne as funções utilizadas para realizar o **record linkage
entre o SINAN e o SI-PNI**.

As funções foram organizadas de maneira modular, sem depender de objetos
globais, permitindo sua reutilização e eventual incorporação em um
pacote R.

### 6.2. Padronização dos campos

A função `ln_preparar()` realiza a padronização dos campos de
identificação, incluindo:

-   Normalização dos nomes;
-   Normalização dos nomes das mães;
-   Conversão das datas para um formato padronizado;
-   Limpeza e validação do CNS;
-   Padronização do sexo;
-   Padronização do código do município;
-   Criação do primeiro e do último token dos nomes.

A normalização textual:

-   Converte os caracteres para minúsculas;
-   Remove acentos;
-   Remove pontuação;
-   Padroniza espaços;
-   Converte strings vazias em valores ausentes.

### 6.3. Identificação de pessoas dentro de cada base

A função `ln_identificar_pessoas()` agrupa registros que provavelmente
pertencem à mesma pessoa.

A identificação pode ocorrer por:

1.  CNS válido compartilhado;
2.  Chave forte formada por:
    -   Nome;
    -   Nome da mãe;
    -   Data de nascimento.

A chave forte só é formada quando os três componentes estão presentes, e
nela as partículas de ligação (de, da, do, dos, das, e) são ignoradas:
"joao da silva" e "joao silva" geram a mesma chave. A comparação entre as
bases (seção 6.7) continua usando o nome completo normalizado.

A regra é aplicada de forma transitiva (componentes conexos). Assim, se a
primeira dose foi registrada com um CNS e a segunda com outro, mas os dois
registros compartilham a chave forte, eles pertencem à mesma pessoa.
Registros sem nenhuma chave recebem identificador próprio.

### 6.4. Deduplicação temporal

A função `ln_remover_duplicatas_temporais()` remove registros
considerados duplicados dentro de uma janela temporal.

A configuração padrão utilizada no pipeline é de **90 dias**.

A regra considera:

-   A pessoa identificada pelo `id_paciente`;
-   A data relevante para a deduplicação;
-   A comparação com o último registro mantido;
-   A manutenção de registros com diferença igual ou superior à janela
    definida.

Registros sem data não são removidos por essa regra, pois não é possível
avaliar o intervalo temporal.

No SINAN, a data utilizada é a data dos primeiros sintomas. No SI-PNI, a
data utilizada é a data da vacinação.

> **Atenção:** a adequação das colunas de data deve ser confirmada no
> dicionário de variáveis dos extratos reais.

### 6.5. Formação dos perfis individuais

A função `ln_perfil_pessoa()` cria um perfil por pessoa, consolidando
informações disponíveis em diferentes registros.

Quando existem várias linhas para a mesma pessoa, os valores não
ausentes são priorizados para preencher campos faltantes.

### 6.6. Blocking

O blocking reduz o número de comparações necessárias entre as bases.

O código utiliza múltiplas estratégias de blocking, incluindo:

-   CNS;
-   Data de nascimento e nome;
-   Data de nascimento e nome da mãe;
-   Nome e nome da mãe;
-   Ano e mês de nascimento, nome e primeiro token do nome da mãe.

Um par candidato é mantido quando sobrevive a pelo menos uma das
estratégias de blocking.

Existe também um limite de segurança para evitar blocos excessivamente
grandes, que poderiam gerar um número elevado de comparações.

### 6.7. Comparação dos pares

A função `ln_comparar()` compara os pares candidatos utilizando:

-   Similaridade do nome;
-   Similaridade do nome da mãe;
-   Concordância da data de nascimento;
-   Concordância do CNS.

A similaridade textual é calculada pelo método Jaro-Winkler.

A data de nascimento recebe tratamento específico para situações em que
o dia e o mês podem estar invertidos. Nesses casos, a concordância é
considerada parcial.

Para o CNS, o código considera:

-   Concordância exata;
-   Pequena diferença entre os números;
-   Discordância.

A ausência de informação em um campo não é automaticamente tratada como
discordância.

### 6.8. Escore Fellegi-Sunter

O pareamento utiliza um escore baseado no método de Fellegi-Sunter.

Cada campo contribui para o escore conforme a concordância ou
discordância observada. O escore final é calculado a partir dos pesos
`m` e `u` definidos para os campos.

Os parâmetros podem ser configurados pela função `ln_par_escore()`.

O código também contém a função `ln_estimar_mu()`, que fornece uma
estimativa empírica dos parâmetros `m` e `u` nos dados reais. Essa
função tem finalidade diagnóstica e não altera automaticamente os pesos
utilizados no escore.

### 6.9. Decisão dos pares

A função `ln_decidir()`:

1.  Mantém pares com número mínimo de campos comparáveis;
2.  Aplica o limiar de escore;
3.  Realiza a resolução 1:1;
4.  Seleciona iterativamente os melhores candidatos disponíveis.

No script principal, o limiar configurado é:

``` r
limiar = 17
```

O valor foi escolhido no teste com as bases fictícias na escala real
(1,0 milhão de pessoas no SINAN e 3,1 milhões no SI-PNI, 95.312 pares
verdadeiros), em que maximizou o F1 (Material Suplementar S04.1):

  Limiar   Sensibilidade (%)   VPP (%)   F1 (%)
  -------- ------------------- --------- --------
  16       98,62               99,19     98,90
  17       98,45               99,86     99,15
  18       98,21               99,89     99,04

Quase todos os falsos positivos ocorreram em pares com apenas dois
campos comparados, cujo escore não os distingue dos pares verdadeiros; um
limiar mais alto só para esses pares foi testado e descartado, por
eliminar cerca de 2.200 pares verdadeiros. Na escolha, pesou também a
direção do viés: perdas de pareamento classificam casos vacinados como
não vacinados e tendem a superestimar a efetividade.

### 6.10. Função principal do linkage

A função `ln_pipeline()` executa sequencialmente:

1.  Preparação das bases;
2.  Identificação de pessoas;
3.  Deduplicação temporal;
4.  Criação dos perfis individuais;
5.  Geração dos candidatos;
6.  Comparação dos pares;
7.  Decisão dos pares.

O resultado é uma lista contendo:

-   `pares`;
-   `comparacoes`;
-   `pessoas_a`;
-   `pessoas_b`;
-   `registros_a`;
-   `registros_b`.

------------------------------------------------------------------------

## 7. Script `02_funcoes_derivadas.R`

### 7.1. Objetivo

Este script contém as funções utilizadas após o linkage para construir
as variáveis analíticas e as bases do Material Suplementar S06.

As funções permitem gerar:

-   Base no nível da pessoa;
-   Base no nível da notificação;
-   Tabelas de pessoa-tempo;
-   Tabelas diárias ou em intervalos constantes.

### 7.2. Cálculo da idade e dos aniversários

A função `dv_idade()` calcula a idade em anos completos na data de
interesse.

A função `dv_aniversario()` calcula a data do aniversário correspondente
a determinada idade.

Para pessoas nascidas em 29 de fevereiro, o código considera 1º de março
nos anos não bissextos.

### 7.3. Status vacinal

A função `dv_status()` classifica o status vacinal em determinada data e
para uma janela de proteção definida.

As categorias são:

-   `nao_vacinado`;
-   `parcialmente_vacinado`;
-   `vacinado`.

A janela padrão utilizada no processamento é de 14 dias, embora o script
principal também produza resultados para janelas de 10, 14 e 21 dias.

### 7.4. Unificação das pessoas

A função `dv_unificar()` cria um identificador único de pessoa,
denominado `pid`.

Também classifica as pessoas em três grupos:

  Grupo   Descrição
  ------- ----------------------------------------------------------------
  `1`     Pessoa pareada entre SINAN e SI-PNI
  `2`     Pessoa vacinada sem notificação no SINAN
  `3`     Pessoa notificada no SINAN sem registro de vacinação no SI-PNI

Os atributos da pessoa são consolidados, com prioridade inicial para os
dados do SI-PNI e complementação com os dados do SINAN quando
necessário.

### 7.5. Consolidação das doses

A função `dv_consolidar_doses()`:

-   Organiza as datas de vacinação por pessoa;
-   Identifica a primeira e a segunda dose;
-   Conta o número de doses registradas;
-   Calcula o intervalo entre a primeira e a segunda dose;
-   Identifica pessoas com mais de duas doses;
-   Conta registros sem data de vacinação.

O campo de número da dose não é utilizado para ordenar as doses. A
ordenação é realizada pela data de aplicação.

### 7.6. Janela de seguimento

A função `dv_janela()` define o período elegível de seguimento de cada
pessoa.

Por padrão, considera:

-   Início do período: 1º de janeiro de 2024;
-   Final do período: 31 de dezembro de 2025;
-   Idade mínima: 10 anos;
-   Idade máxima: 14 anos.

A entrada no seguimento é definida pelo maior valor entre:

-   Início do período do estudo;
-   Data do décimo aniversário;
-   Marco municipal, apenas se `entrada_no_marco = TRUE`.

Quando a tabela de marcos é informada e `restringir_marco = TRUE` (padrão),
só entram no seguimento as pessoas residentes em municípios com marco, ou
seja, nos municípios que ofertaram a vacina (seção 4.2 do projeto), o que
mantém o numerador coerente com o denominador do IBGE. Pessoas sem
município de residência também ficam de fora.

### 7.6.1. Marco municipal [V3]

A função `dv_marco_municipal()` calcula, para cada município, a data a
partir da qual ele é considerado ofertante da vacina, a partir das doses
de Qdenga já deduplicadas, aplicadas a pessoas de 10 a 14 anos no período
do estudo. O município usado é, por padrão, o do estabelecimento que
aplicou a dose. Com `min_doses = 1`, o marco é a primeira dose (texto atual
da seção 4.2); um valor maior evita que doses esporádicas, como as
remanejadas em 2025 por proximidade do vencimento, marquem o município. A
saída é agregada (município, data do marco e número de doses na faixa
etária) e sai da SAR como `tabela_marco_municipal.csv`; ela também serve
aos modelos estratificados por data de início da oferta (seção 4.7.3).

A saída administrativa é definida pelo menor valor entre:

-   Final do período do estudo;
-   Véspera do décimo quinto aniversário.

### 7.7. Classificação dos eventos

A função `dv_eventos()` classifica as notificações segundo três
definições de desfecho:

-   Evento principal;
-   Evento direto;
-   Evento confirmado.

O evento principal é definido a partir da classificação final e, quando
habilitado, de marcadores relacionados à hospitalização e à evolução.

O evento direto considera, adicionalmente, resultados positivos de PCR,
NS1 ou isolamento viral.

O evento confirmado é definido a partir das categorias especificadas no
campo de classificação final.

> **Atenção:** os códigos utilizados nas classificações devem ser
> conferidos com o dicionário de variáveis e com a definição operacional
> do protocolo do estudo.

### 7.8. Primeiro evento

A função `dv_primeiro_evento()` identifica o primeiro evento de cada
definição de desfecho dentro da janela de seguimento.

A data utilizada para o evento é a data dos primeiros sintomas.

### 7.9. Intervalos de pessoa-tempo

A função `dv_intervalos()` constrói os intervalos de seguimento
classificados por:

-   Pessoa;
-   Status vacinal;
-   Idade;
-   Data de início;
-   Data de fim;
-   Sexo;
-   Município.

Os intervalos são limitados por:

-   Entrada no seguimento;
-   Saída administrativa;
-   Primeiro evento;
-   Datas relacionadas à vacinação;
-   Aniversários que delimitam as faixas etárias.

A função calcula o número de dias de cada intervalo.

Por padrão, são gerados apenas os intervalos dos status
`parcialmente_vacinado` e `vacinado`: na análise nacional, a pessoa-tempo
não vacinada vem da população do IBGE. Com a opção
`incluir_nao_vacinado = TRUE`, usada só na análise de Ribeirão Preto
(denominador nominal), o período não vacinado de cada pessoa também é
gerado. A opção não altera o resultado da análise nacional.

### 7.10. Tabela agregada diária

A função `dv_tabela_diaria()` produz tabelas agregadas de pessoa-tempo e
casos.

Pode gerar dois formatos:

-   `diario`: uma linha por dia e estrato;
-   `intervalos`: períodos em que a contagem permanece constante.

Os estratos podem incluir, por exemplo:

-   Sexo;
-   Idade;
-   Município.

O método utilizado calcula mudanças no número de pessoas em cada status
sem precisar expandir inicialmente cada intervalo pessoa a pessoa.

As colunas de pessoas-dia são `n_parcial` e `n_vacinado` e, quando o
período não vacinado é incluído, também `n_nao_vacinado`.

### 7.11. Bases individuais

A função `dv_base_pessoas()` produz uma base no nível da pessoa,
incluindo informações como:

-   Grupo de linkage;
-   Datas das doses;
-   Idade na vacinação;
-   Janela de seguimento;
-   Ocorrência dos desfechos;
-   Dias de seguimento;
-   Dias em cada status vacinal.

A função `dv_base_notificacoes()` produz uma base no nível da
notificação, incluindo:

-   Idade na data dos sintomas;
-   Status vacinal na data dos sintomas;
-   Tempo entre sintomas e notificação;
-   Tempo entre sintomas e internação;
-   Variáveis clínicas selecionadas.

### 7.12. Os três bancos para exportação

As funções `dv_atributos_doses()` e `dv_bancos_exportacao()` produzem
três bancos. Cada pessoa aparece em **um único banco**:

  Banco   Conteúdo                                   Unidade da linha
  ------- ------------------------------------------ ------------------
  1       Pareados (notificados e vacinados)         Notificação
  2       Vacinados sem notificação                  Pessoa
  3       Notificados sem registro de vacinação      Notificação

Regras para não contar a mesma pessoa duas vezes e não perder informação:

-   Nos bancos 1 e 3, cada linha é uma notificação (episódio que
    permaneceu após a deduplicação de 90 dias). Quem teve duas
    notificações tem duas linhas com o mesmo código de pessoa, e as
    variáveis da pessoa (inclusive as da vacina, no banco 1) se repetem;
-   `ordem_notificacao` e `n_notificacoes_pessoa` indicam a posição da
    notificação e o total por pessoa;
-   `caso_principal_seguimento`, `caso_direto_seguimento` e
    `caso_confirmado_seguimento` valem 1 apenas na notificação que conta
    como caso daquele desfecho (o primeiro evento dentro do seguimento),
    a mesma usada nas tabelas agregadas;
-   As duas doses ficam em colunas: `dt_d1`, `dt_d2` e as variáveis do
    registro de cada dose com sufixo `_d1` e `_d2` (por exemplo,
    `co_municipio_estabelecimento_d1` e `co_municipio_estabelecimento_d2`).

Para contar **pessoas**, contar os códigos de pessoa distintos; para
contar **casos**, somar a coluna `caso_<desfecho>_seguimento`.

### 7.13. Preparação para exportação

A função `dv_preparar_exportacao()` prepara uma versão da base para
exportação.

O procedimento:

-   Substitui o `pid` por um identificador de estudo;
-   Converte datas para o nível de mês;
-   Remove a data de nascimento;
-   Mantém uma tabela de correspondência para uso interno.

A tabela de correspondência deve permanecer no ambiente seguro e não
deve ser exportada junto com os dados analíticos.

------------------------------------------------------------------------

## 8. Script `03_execucao_SAR.R`

### 8.1. Objetivo

Este é o script principal de execução do pipeline.

Ele realiza:

1.  Configuração dos caminhos e parâmetros;
2.  Leitura das bases;
3.  Aplicação dos filtros;
4.  Execução do linkage;
5.  Avaliação e diagnóstico;
6.  Construção das variáveis derivadas;
7.  Geração das tabelas;
8.  Exportação dos resultados.

O script está configurado para as bases fictícias de pessoas de 8 a 16
anos. Para utilizar dados reais, altere os caminhos, os nomes dos
arquivos e os nomes das colunas de identificação, e apague o bloco
"AVALIAÇÃO" da seção 2 do script (não existe `id_pessoa_verdadeiro` nos
dados reais); ver seção 2.

### 8.2. Configuração

Os principais parâmetros estão no objeto `CFG`.

Entre eles:

-   `pasta`: diretório dos scripts e das bases;
-   `pasta_saida`: subpasta de resultados;
-   `arq_vacina`: arquivo da base de vacinação;
-   `arq_sinan`: arquivo da base do SINAN;
-   `nasc_min` e `nasc_max`: limites da data de nascimento;
-   `ini_seg` e `fim_seg`: período de seguimento;
-   `col_sinan`: mapeamento das colunas do SINAN;
-   `col_vacina`: mapeamento das colunas do SI-PNI;
-   `limiar`: ponto de corte do linkage (17);
-   `janela_dedup_dias`: janela de deduplicação;
-   `janelas`: janelas de proteção vacinal;
-   `desfechos`: definições de desfecho;
-   `tam_amostra_revisao`: tamanho da amostra de pares aceitos para
    revisão;
-   `tam_amostra_proximo_corte`: tamanho da amostra de pares próximos ao
    limiar;
-   `semente`: semente utilizada para a reprodutibilidade das amostras.

Parâmetro acrescentado na versão 3 (não há filtro por fabricante: em
2024–2025 a Qdenga foi a única vacina contra dengue no SUS, o código da
vacina não distingue fabricantes e o campo de fabricante é texto livre com
milhares de grafias):

-   `marco`: lista com `aplicar` (TRUE/FALSE), `col_mun` (coluna do
    município da aplicação), `min_doses` e `entrada_no_marco` (FALSE = todos
    entram em 01/01/2024, como decidido para a análise nacional).

As listas `cols_clinicas` (SINAN) e `cols_vacina_export` (SI-PNI), que
definem as variáveis que acompanham os bancos exportados, são definidas
automaticamente depois da leitura: todas as colunas de cada base, exceto
os identificadores, as colunas de data originais (`DT_*` e `dt_*`, que
sairiam com a data completa), a unidade notificadora (`ID_UNIDADE`) e os
códigos de registro (`co_documento`, `co_paciente`, `arquivo_origem`).

### 8.3. Leitura e filtragem

Na etapa de leitura:

1.  As bases são importadas como texto;
2.  Registros vacinais com indicação de exclusão na RNDS são removidos,
    quando a coluna está disponível;
3.  As datas de nascimento são convertidas;
4.  As bases são filtradas pelo intervalo de nascimento;
5.  O processamento é interrompido se uma das bases ficar vazia após o
    filtro;
6.  As listas de variáveis dos bancos exportados são definidas (seção
    8.2).

### 8.4. Linkage

O pipeline é executado por meio da função:

``` r
res <- ln_pipeline(...)
```

O processamento registra:

-   Número de pessoas no SINAN;
-   Número de pessoas no SI-PNI;
-   Número de pares aceitos;
-   Percentual de registros com CNS válido em cada base.

### 8.5. Avaliação e validação

A avaliação com `id_pessoa_verdadeiro` é destinada aos dados fictícios
ou às simulações em que a identidade verdadeira é conhecida.

Essa avaliação não deve ser aplicada diretamente aos dados reais caso
essa variável não exista.

Para os dados reais, o script produz diagnósticos como:

-   Distribuição dos escores dos pares aceitos;
-   Número de campos comparados;
-   Estimativas empíricas de `m` e `u`;
-   Amostra de pares aceitos para revisão manual;
-   Amostra de pares próximos, mas abaixo do limiar.

A revisão dos pares próximos ao limiar pode ajudar a identificar
possíveis pares verdadeiros que foram rejeitados pelo ponto de corte.

### 8.6. Variáveis derivadas e tabelas

O script constrói:

-   Pessoas unificadas;
-   Doses consolidadas;
-   Janelas de seguimento;
-   Eventos de dengue;
-   Base de pessoas;
-   Base de notificações;
-   Tabela de marcos municipais e restrição aos municípios ofertantes;
-   Tabelas nacionais diárias;
-   Tabelas municipais em intervalos constantes;
-   Os três bancos para exportação (seção 7.12).

Os bancos incluem todas as pessoas do linkage; quem mora fora de município ofertante aparece com `entrada`
vazia e não contribui para as tabelas agregadas.

São processadas as seguintes janelas:

``` r
janelas = c(10L, 14L, 21L)
```

E os seguintes desfechos:

``` r
desfechos = c("principal", "direto", "confirmado")
```

### 8.7. Checagem interna

O script inclui uma verificação de consistência:

``` r
stopifnot(sum(nac$n_parcial) + sum(nac$n_vacinado) == sum(iv$dias))
```

Essa checagem compara a soma das pessoas-dia nas tabelas agregadas com a
soma dos dias nos intervalos individuais.

Uma segunda checagem confirma que cada pessoa está em um e só um dos
três bancos exportados (a soma das pessoas dos três bancos deve ser igual
ao total de pessoas unificadas).

A execução do teste deve ser acompanhada da inspeção dos resultados e de
outras verificações de consistência.

------------------------------------------------------------------------

## 9. Script `04_teste_casos_construidos.R`

### 9.1. Objetivo

Este script utiliza casos construídos manualmente para exercitar regras
específicas do linkage e das variáveis derivadas.

Entre as situações representadas estão:

-   Diferenças na grafia dos nomes;
-   Uso de CNS diferentes;
-   Registros de vacinação com menos de 90 dias entre si;
-   Pessoas nascidas em 29 de fevereiro;
-   Entrada no seguimento aos 10 anos;
-   Saída do seguimento aos 15 anos;
-   Notificações duplicadas;
-   Comparação entre pessoas-dia agregadas e os dias calculados
    individualmente.

### 9.2. Execução

No RStudio:

1.  Abra o arquivo `04_teste_casos_construidos.R`;
2.  Confirme o caminho configurado em `pasta_scripts`;
3.  Clique em **Source**;
4.  Inspecione os objetos e as mensagens impressas no console.

O script imprime resultados intermediários, incluindo:

-   Pares aceitos;
-   Pessoas unificadas;
-   Doses consolidadas;
-   Janelas de seguimento;
-   Eventos;
-   Base de pessoas;
-   Base de notificações;
-   Intervalos;
-   Casos;
-   Tabelas agregadas;
-   Verificações de consistência.

------------------------------------------------------------------------

## 10. Análise de sensibilidade local — Ribeirão Preto/SP (scripts 05 a 07)

### 10.1. Objetivo

Estimar a efetividade com denominador nominal, usando o Hygia, que reúne
o cadastro de todas as pessoas com ao menos uma dose de qualquer vacina
na rede municipal e o registro das doses aplicadas, inclusive as de
Qdenga. O processamento é feito pela SMS-RP, no ambiente da Prefeitura;
a equipe de pesquisa recebe apenas arquivos sem identificadores (seção
4.7.5 e Material Suplementar S07 do projeto).

### 10.2. Bases de entrada

  Arquivo (padrão no `CFG`)             Conteúdo
  ------------------------------------- ------------------------------------
  `hygia_cadastro.csv`                  Cadastro: identificação, CEP ou
                                        setor, distrito de saúde, data do
                                        último registro, data de óbito
  `hygia_vacinas.csv`                   Uma linha por dose aplicada, de
                                        qualquer vacina (histórico completo)
  `sinan_dengue_rp_2015_2025.csv`       Notificações de dengue de
                                        residentes, 2015 a 2025
  `ivs_setor.csv`                       Fornecida pela equipe: setor
                                        censitário e categoria do IVS
  `cep_setor.csv` (opcional)            CEP e setor majoritário, a partir
                                        do CNEFE 2022, se o Hygia só tiver CEP

### 10.3. Grupos

  Grupo   Descrição                                               Etapa
  ------- ------------------------------------------------------- -------
  1       Notificado e vacinado                                   A
  2       Vacinado sem notificação                                A
  3a      Notificado sem vacina, presente no cadastro do Hygia    B
  3b      Notificado sem vacina, ausente do cadastro do Hygia     B
  4       Não vacinado e sem notificação (cadastro do Hygia)      B

### 10.4. Fluxo

1.  **Adaptador** (`rp_preparar_cadastro()`, `rp_vacina_como_sipni()`):
    padroniza o cadastro, une cadastros duplicados da mesma pessoa (mesma
    função de componentes conexos da análise nacional) e transforma as
    doses de Qdenga em uma base no formato do SI-PNI;
2.  **Etapa A** (espelho da análise nacional): SINAN x doses de Qdenga,
    com `ln_pipeline()` sem alteração, limiar 17 e deduplicação de 90
    dias. Gera os grupos 1, 2 e 3;
3.  **Etapa B** (exclusiva desta análise): notificações do grupo 3 x
    cadastro das pessoas sem nenhuma dose de Qdenga em nenhum dos seus
    cadastros (`rp_cadastro_nao_vacinados()`). Isso evita que alguém
    vacinado em um cadastro apareça como não vacinado em um cadastro
    duplicado. Gera os grupos 3a, 3b e 4;
4.  **Unificação** (`rp_unificar()`): um código por pessoa e o grupo;
    para quem está no Hygia, idade e sexo vêm do cadastro;
5.  **Covariáveis** (`rp_covariaveis()`): categoria do IVS, distrito de
    saúde, dengue notificada prévia (antes de 2024), HPV e meningocócica
    ACWY antes da entrada, registro recente no Hygia e óbito;
6.  **Variáveis derivadas**: as mesmas funções do 02;
7.  **Tabelas e bancos** (seções 10.5 e 10.6).

### 10.5. Tabelas agregadas (para cada janela e desfecho)

  Arquivo                                  Conteúdo
  ---------------------------------------- ---------------------------------
  `tabela_denominador_estimado_*`          Reproduz a análise nacional:
                                           pessoas-dia dos vacinados e casos
                                           dos grupos 1, 2, 3a e 3b, por sexo
                                           e idade; a pessoa-tempo não
                                           vacinada vem do IBGE, fora da
                                           Prefeitura
  `tabela_denominador_nominal_*`           Pessoas-dia de todos os status a
                                           partir dos indivíduos do Hygia
                                           (grupos 1, 2, 3a e 4), por sexo,
                                           idade, IVS, distrito, dengue
                                           prévia, HPV/ACWY e registro no
                                           Hygia a partir de 2022; o óbito
                                           encerra o seguimento
  `tabela_casos_grupo3b_*`                 Casos do grupo 3b, para a análise
                                           de sensibilidade que os inclui
                                           como casos não vacinados

Covariável ausente vira a categoria `ignorado` nas tabelas nominais.

### 10.6. Bancos para exportação

Os três bancos da análise nacional (seção 7.12), com a coluna `grupo_rp`
separando 3a e 3b no banco 3, mais
`export_banco4_nao_vacinados_sem_notificacao.csv` (grupo 4, uma linha por
pessoa). Cada pessoa aparece em um só banco.

Os arquivos `interno_*` e `revisao_manual_etapaA_*` / `revisao_manual_etapaB_*`
contêm dados identificáveis e **não saem da Prefeitura**.

### 10.7. Checagens internas

-   Pessoas-dia das tabelas iguais à soma dos intervalos individuais;
-   Na tabela nominal, pessoa-tempo total igual à soma dos períodos de
    seguimento de todos os indivíduos da coorte;
-   Cada pessoa em um e só um dos quatro bancos;
-   Nenhum identificador nos arquivos `export_*` e `tabela_*`.

### 10.8. Como executar

1.  Deixe 01, 02, 05 e 06 na mesma pasta;
2.  No 06, ajuste `pasta_scripts`, `CFG$pasta` e os itens `[CONFIRMAR]`;
3.  Abra o 06 no RStudio e clique em **Source**.

Para testar antes de enviar à SMS, rode o 07 (ajustando
`pasta_scripts`), aponte `CFG$pasta` do 06 para a pasta
`dados_teste_ribeirao` criada por ele e use
`arq_cep_setor = "cep_setor.csv"`.

### 10.9. A confirmar com a SMS-RP antes do envio

-   Nomes das colunas do cadastro e das vacinas no Hygia (dicionário de
    dados);
-   Códigos da Qdenga, da HPV e da meningocócica ACWY no Hygia;
-   Se o Hygia tem o setor censitário geocodificado ou apenas o CEP (neste
    caso, montar `cep_setor.csv` a partir do CNEFE 2022);
-   Se o Hygia registra óbito (se não registrar, a coluna fica vazia e o
    seguimento termina pelos demais critérios).

### 10.10. Observação sobre o limiar

O limiar 17 é mantido nas duas etapas para que a etapa A seja idêntica à
análise nacional. Em bases do tamanho de Ribeirão Preto, a chance de
coincidência ao acaso é menor e um limiar mais baixo seria admissível;
17 tende a gerar poucos falsos positivos e algumas perdas a mais. A
revisão manual das duas etapas mostrará se isso é um problema.

------------------------------------------------------------------------

## 11. Como executar

### 11.1. Execução do script principal

1.  Abra o RStudio;
2.  Abra o arquivo `03_execucao_SAR.R`;
3.  Atualize `pasta_scripts` e o objeto `CFG` com os caminhos e nomes
    reais dos arquivos;
4.  Confirme os nomes das colunas em ambas as bases;
5.  Confirme as definições operacionais e os códigos dos desfechos;
6.  Confirme as colunas utilizadas para a deduplicação temporal;
7.  Clique em **Source**.

O script carrega automaticamente:

``` r
source(file.path(pasta_scripts, "01_funcoes_linkage.R"))
source(file.path(pasta_scripts, "02_funcoes_derivadas.R"))
```

Não é necessário executar os dois arquivos de funções separadamente,
desde que estejam no caminho informado.

### 11.2. Execução dos testes

Antes da execução com dados reais, recomenda-se executar:

``` text
04_teste_casos_construidos.R
```

Também é recomendável verificar os resultados da simulação ou dos dados
fictícios utilizados para calibrar o limiar do linkage.

------------------------------------------------------------------------

## 12. Arquivos de saída

Os resultados são gravados na subpasta definida em:

``` r
CFG$pasta_saida
```

Entre os arquivos produzidos estão:

  ------------------------------------------------------------------------------
  Arquivo                                    Conteúdo
  ------------------------------------------ -----------------------------------
  `linkage_pares_reais_8a16anos.csv`         Pares aceitos pelo linkage

  `sinan_nao_pareados_reais_8a16anos.csv`    Registros do SINAN não pareados

  `vacina_nao_pareados_reais_8a16anos.csv`   Registros do SI-PNI não pareados

  `interno_base_pessoas.csv`                 Base individual interna

  `interno_base_notificacoes.csv`            Base de notificações interna

  `interno_correspondencia_ids.csv`          Correspondência entre
                                             identificadores

  `export_banco1_pareados.csv`              Banco 1: pareados, uma linha por
                                             notificação (seção 7.12)

  `export_banco2_vacinados_sem_notificacao.csv` Banco 2: vacinados sem
                                             notificação, uma linha por pessoa

  `export_banco3_notificados_sem_vacina.csv` Banco 3: notificados sem
                                             vacina, uma linha por notificação

  `relatorio_processamento.csv`              Registro das quantidades observadas
                                             em cada etapa

  `tabela_marco_municipal.csv`               Marco de oferta por município
                                             (agregada; sai da SAR)

  `avaliacao_linkage_teste.csv`              Avaliação do linkage com dados que
                                             possuem identidade verdadeira

  `diagnostico_m_u.csv`                      Diagnóstico dos parâmetros
                                             empíricos de concordância

  `revisao_manual_pares_aceitos.csv`         Amostra de pares aceitos para
                                             revisão manual

  `revisao_manual_proximos_ao_corte.csv`     Amostra de pares abaixo do limiar
  ------------------------------------------------------------------------------

Os três arquivos `export_banco*` são os preparados para sair da SAR: sem
identificadores, com código aleatório de pessoa e datas no nível de mês.

As tabelas agregadas são geradas para cada combinação de janela e
desfecho: 3 janelas x 3 desfechos x 3 arquivos = 27 arquivos. Para a
análise principal, o arquivo é `tabela_diaria_nacional_j14_principal.csv`;
os demais servem às análises de sensibilidade e aos modelos
estratificados. Em testes, para uma pasta mais enxuta e uma execução mais
rápida, pode-se usar `janelas = 14L` e `desfechos = "principal"` no
`CFG`.

Exemplos:

``` text
tabela_diaria_nacional_j10_principal.csv
tabela_diaria_nacional_j14_principal.csv
tabela_diaria_nacional_j21_principal.csv

tabela_municipal_pessoasdia_j14_principal.csv
tabela_municipal_casos_j14_principal.csv
```

------------------------------------------------------------------------

## 13. Segurança e confidencialidade

Os arquivos de saída podem conter dados identificáveis ou informações
sensíveis, como:

-   Nome do paciente;
-   Nome da mãe;
-   CNS;
-   Datas exatas;
-   Identificadores internos;
-   Correspondências entre bases.

Os arquivos abaixo devem permanecer no ambiente seguro, salvo
autorização específica:

-   Arquivos `interno_*`;
-   Arquivos `linkage_pares*`;
-   Arquivos `*_nao_pareados*`;
-   Arquivos `revisao_*`;
-   Arquivo de correspondência entre identificadores.

Antes de qualquer exportação para fora da SAR:

1.  Remover os identificadores diretos;
2.  Avaliar a necessidade de remover ou reduzir a precisão das datas;
3.  Utilizar identificadores de estudo;
4.  Verificar as regras de divulgação aplicáveis;
5.  Avaliar a existência de células com tamanho reduzido nas tabelas
    agregadas.

Os arquivos preparados para sair da SAR são os três `export_banco*` e as
tabelas agregadas (`tabela_*`), estas conforme as regras de divulgação.

A função `dv_preparar_exportacao()` reduz a precisão das datas criadas
pelo script para o nível de mês e remove a data de nascimento. As colunas
de data originais das bases não entram nos bancos exportados (seção 8.2).
Ainda assim, a exportação deve ser revisada antes do compartilhamento.

------------------------------------------------------------------------

## 14. Pontos que devem ser confirmados antes da execução com dados reais

Antes de executar o pipeline com os extratos reais, verificar:

1.  Os caminhos dos arquivos;
2.  Os nomes das colunas de identificação;
3.  O formato das datas;
4.  O nome correto da coluna de data dos primeiros sintomas;
5.  O nome correto da coluna de data de vacinação;
6.  O nome e o significado da coluna de registros deletados na RNDS;
7.  Os códigos de classificação final;
8.  Os códigos de hospitalização e evolução;
9.  Os códigos dos resultados de PCR, NS1 e isolamento;
10. A definição operacional de cada desfecho;
11. A janela de deduplicação temporal;
12. O limiar utilizado no linkage (17, calibrado no teste em escala
    real; reavaliar à luz da revisão manual nos dados reais);
13. O marco municipal: coluna do município da aplicação e número mínimo
    de doses (`min_doses`), a decidir;
14. As regras de divulgação das tabelas agregadas;
15. Os procedimentos de revisão manual;
16. A remoção de identificadores antes da exportação.

Os comentários no código marcados como `[CONFIRMAR]` indicam pontos que
precisam de verificação adicional.

------------------------------------------------------------------------

## 15. Limitações e cuidados metodológicos

-   A ausência de um identificador em uma das bases pode reduzir a
    capacidade de pareamento.
-   A similaridade dos nomes não garante, isoladamente, que dois
    registros pertençam à mesma pessoa.
-   O CNS pode apresentar erros, duplicidades ou variações entre
    sistemas.
-   A deduplicação temporal depende da qualidade e da completude das
    datas.
-   O limiar de pareamento deve ser justificado para a escala e as
    características das bases utilizadas.
-   A revisão manual é importante para avaliar pares aceitos e pares
    próximos ao ponto de corte.
-   As funções de avaliação que dependem de `id_pessoa_verdadeiro` não
    devem ser utilizadas diretamente nos dados reais quando essa
    informação não estiver disponível.
-   Os códigos dos campos clínicos e dos desfechos devem ser confirmados
    com os dicionários oficiais e com o protocolo do estudo.
-   Os testes construídos manualmente verificam regras específicas, mas
    não substituem a validação com dados reais ou com uma base de
    referência adequada.
-   Nas bases fictícias de teste, sexo e município de residência vêm de
    registros reais sorteados independentemente em cada base e não são
    coerentes para a mesma pessoa (concordância de 50% no sexo e de 1%
    no município entre pares verdadeiros). Por isso, esses campos não
    entram no escore, e os valores por sexo, município e idade nas
    tabelas geradas com dados fictícios não têm significado
    substantivo. A inclusão de sexo e município no escore fica como
    refinamento, a ser considerado se a revisão manual nos dados reais
    mostrar falsos positivos concentrados em pares com dois campos
    comparados.

------------------------------------------------------------------------

## 16. Reprodutibilidade

Para favorecer a reprodutibilidade:

-   Manter os scripts versionados;
-   Registrar alterações nos parâmetros do objeto `CFG`;
-   Manter a semente definida para as amostras de revisão;
-   Guardar o relatório de processamento;
-   Registrar a versão dos bancos utilizados;
-   Documentar as decisões sobre os códigos dos desfechos;
-   Registrar a justificativa para o limiar do linkage;
-   Preservar os resultados das revisões manuais;
-   Não alterar os arquivos de entrada originais.

------------------------------------------------------------------------

## 17. Resumo do fluxo de uso

``` text
1. Conferir os dicionários das bases
2. Ajustar os caminhos e nomes das colunas
3. Executar os casos construídos
4. Verificar as regras de deduplicação e derivação
5. Confirmar o limiar do linkage
6. Executar o pipeline principal
7. Avaliar os diagnósticos do linkage
8. Realizar a revisão manual
9. Inspecionar as checagens de consistência
10. Gerar as bases e tabelas analíticas
11. Aplicar as regras de segurança e exportação
12. Documentar a versão final do processamento
```

------------------------------------------------------------------------

## 18. Status do script

Este README descreve a estrutura e o funcionamento dos scripts na
versão atual (versão 3, setembro de 2026). A versão 3 acrescentou o
marco municipal com restrição aos
municípios ofertantes e, em Ribeirão Preto, o estrato de registro recente
no Hygia; foi testada com casos construídos e com dados simulados
pequenos.

O pipeline completo foi executado com as bases fictícias na escala real
(3,4 milhões de registros de vacinação e 1,1 milhão de notificações), sem
erros, com sensibilidade de 98,4% e valor preditivo positivo de 99,9% no
linkage (limiar 17) e com as checagens internas aprovadas.

A análise de Ribeirão Preto foi testada com dados simulados (3.000
pessoas, 100 com cadastro duplicado, 510 notificações, 60 de pessoas fora
do Hygia): nenhum vacinado foi classificado nos grupos 3a ou 4, nenhum não
vacinado nos grupos 1 ou 2, as pessoas com a 2ª dose registrada em
cadastro duplicado ficaram com as duas doses e todas as checagens
internas foram aprovadas. Os nomes das colunas e os códigos do Hygia ainda
dependem do dicionário de dados da SMS-RP.

Antes da utilização definitiva com dados reais, devem ser confirmados:

-   Os nomes e significados das colunas;
-   As regras operacionais dos desfechos;
-   O limiar final do linkage;
-   A adequação das janelas temporais;
-   Os procedimentos de revisão manual;
-   As regras de segurança e exportação;
-   A execução bem-sucedida dos testes e das checagens internas.
