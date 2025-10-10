
#______________________________  Carrega funções ____________________________________________________________ 

configs <- function() {

    library(readxl)
    library(dplyr)
    diretorio <- getwd()
    
   #localização das Bases históricas caso deseja-se juntar com a atual
   dir_desp_ate2024 <- paste0(diretorio,"/Orçamento_Publico/Despesas-LN-2008-atual.xlsx")
   dir_rec_ate2024  <- paste0(diretorio,"/Orçamento_Publico/Receitas-LN-2008-atual.xlsx")
   
  
   ### Define municipios de interesse            
   dir_rmvale <- paste0(diretorio,"/Tabelas/Municipios_RMVale.xlsx")
   lista_mun_rmvale <- read_excel(
                       path =  dir_rmvale,
                       sheet = "municipios_rmvale" ) # 39 municipios da RM_Vale
   
   lista_mun_rmvale <- lista_mun_rmvale |> 
                       filter(CD_SUB_REG == "SR5")
   
   lista_municipios <- lista_mun_rmvale$nm_mun       # separa apenas o nome do Municipio minusculo

  } #  FUNÇÃO: configs (configurações)
#______________________________________________________________________________________________________________
load_packages <- function() {
  
  install.packages(c('readxl', 'dplyr'))  # pacotes minimos para executar a função
  library('readxl')
  library('dplyr')
  
  pacotes_df <- read_xlsx("Tabelas/pacotes.xlsx")    #le planilha de pacotes e grava data frame
  pacotes_df <- filter(pacotes_df, carregar == "S")
  pacotes    <- as.vector( pacotes_df$pacote)
  
  pacotes_df$carregar
  
  
  print (pacotes)
  
  if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){ # retorna matriz pacotes ja instalados
    instalador <- pacotes[!pacotes %in% installed.packages()]   # isola os pacotes não instalados 
    for(i in 1:length(instalador)) {
      install.packages(instalador, dependencies = T)
      break()}
    sapply(pacotes, require, character = T) 
  } else {
    sapply(pacotes, require, character = T)  # opção: lapply(carregar, require, character.only = TRUE)   
  }
  
  # Observação:
  # O pacote rayshader que está no CRAN, no momento, possui alguns bugs. 
  # A versão que está no GitHub do autor do pacote já é mais funcional. Para instalá-la: (responder 3 na console)
  # devtools::install_github("tylermorganwall/rayshader")  #(só a primeira vez)
  # Para carregar o rayshader  (faz graficos tridimensionais - barras sobre o mapa)
  # library(rayshader)
  
} # FUNÇÃO: carrega pacotes
#______________________________________________________________________________________________________________
rm_accent <- function(str,pattern="all") {
  # FUNÇÃO: rm_accent - Remove acentuações
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}  # FUNÇÃO: rm_accents - Remove acentuações
#______________________________________________________________________________________________________________
std_str <- function(str2) {
  # FUNÇÃO std_str - Padronização dos caracteres
  str2 <- tolower(str2)                             # coloca tudo em lowercase - para Uppercase seria toupper(str_origem) 
  str2 <- rm_accent(str2)                           # Remove todas acentuações 
  str2 <- str_replace_all(str2, "[^[:alnum:]]", "") # remove non alphanumeric characters
  return(str2)
} # FUNÇÃO: std_str - Padronização dos caracteres especiais, uppercase, etc.
#______________________________________________________________________________________________________________
download_receitas <- function(anos_f, lista_municipios_f) {

    library(readxl)
    library(writexl)
    #anos_f             <- "2025"
    #lista_municipios_f <- "ilhabela"
    tipo                <- "receitas"
    diretorio           <- getwd()
  
    # cria um arquivo vazio para acumular os arquivos baixados
    receitas_acum <- read.csv(file = "Orçamento_Publico/receitas_vazia.csv", 
                              sep = ";", 
                              header = TRUE,
                              encoding = "latin1" )
    
    # caso deseje juntar com o arquivo historico, descomentariar as 2 linhas abaixo  
    # dir_rec_ate2024  <- paste0(diretorio,"/Orçamento_Publico/Receitas-LN-2014-atual.xlsx")
    # receitas_acum    <- read_xlsx(dir_rec_ate2024) # opção de juntar antigo 
  
  
  for(ano in anos_f) { #Looping para baixar todos os anos e municipios recebidos no argumento da função
     
    for(mun in lista_municipios_f) {
      
      print (paste("baixando", tipo, "de:", mun, "ano:", ano))
      
      url_baixar <- paste("https://transparencia.tce.sp.gov.br/sites/default/files/csv/", tipo, "-", mun, "-", ano, ".zip",sep = "")
      
      df_name_zip   <- paste(tipo, "-", mun, "-", ano, ".zip",sep = "")
      df_name_csv   <- paste(tipo, "-", mun, "-", ano, ".csv",sep = "")
      df_name_pasta <- paste(tipo, "-", mun, "-", ano,        sep = "")
      
      download.file(url_baixar, df_name_zip)               #traz para meu diretorio (vem zipado)
      unzip(df_name_zip, files = df_name_csv)              #Unzipa           
      
      #remove arquivos temporários
      file.remove(df_name_zip)
      # file.remove(df_name_pasta)
      
      receitas <- read.csv(file = df_name_csv, sep = ";", header = T, 
                           encoding = "latin1",dec = ",")
      receitas$categoria  <- ''   #Cria nova coluna Categoria
      
      receitas_acum <- rbind.data.frame(receitas_acum, receitas)
      
      rm (receitas)
      file.remove (df_name_csv)
      
    }  # Fim do loop de município  
  }  # Fim do loop de ano
  
  colnames(receitas_acum) <-    #Rename nas colunas para terem nomes mais compreensíveis
    c('Identificação da Receita',	
      'Ano',	
      'Municipio',	
      'Orgão',	
      'Mês',	
      'Mês extenso',	
      'Poder',	
      'Fonte de Recurso',	
      'Código aplicacao fixo',	
      'Código aplicação variavel',	
      'Categoria Econômica',	
      'Sub Categoria',	
      'Fonte',	
      'Rubrica',	
      'Alínea',	
      'Sub Alínea',
      'tipo',
      'Valor arrecadacao',
      'Categoria'
    )
  	
  dsname_Rdata <- paste(tipo, "-", "municipios", "-", ano, ".Rdata",sep = "")
  save(despesas_acum, file = dsname_Rdata) # grava resultado em formato RData
  
  dsname_xlsx <- paste(tipo, "-", "municipios", "-", ano, ".xlsx",sep = "")
  write_xlsx(receitas_acum, dsname_xlsx) #grava data frame em formato *.xlsx
  
} # FUNÇÃO download_receitas do TCE  - municípios da lista_mun_minusculas
#______________________________________________________________________________________________________________
download_despesas <- function(anos_f, lista_municipios_f) {
 
  library(readxl)
  library(writexl)
  library(dplyr)
  library(stringr)
  
  #anos_f             <- "2025"
  #lista_municipios_f <- "ilhabela"
  tipo                <- "despesas"
  diretorio           <- getwd()
  
  #cria um arquivo vazio para acumular os arquivos baixados
   despesas_acum <- read.csv(file = "Orçamento_Publico/despesas_vazia.csv", 
                            sep = ";", 
                            header = T, 
                            encoding = "latin1")
   
   {
     # caso deseje juntar com o arquivo historico, descomentariar as linhas abaixo  
     # dir_desp_ate2024 <- paste0(diretorio,"/Orçamento_Publico/Despesas-LN-2014-atual.xlsx")
     # despesas_hist <- read_excel(
     #                  path =  dir_desp_ate2024,
     #                  sheet = "Despesas_Ilhabela_2014-2024"  )  # Substitua pelo nome da planilha
   } # Opção A: Ler pelo NOME da planilha - histórico das despesas
   
   
  for(ano in anos_f) {
    
    for(mun in lista_municipios_f) {
      
      print (paste("baixando", tipo, "de:", mun, "ano:", ano))
      
      df_name_zip   <- paste(tipo, "-", mun, "-", ano, ".zip",sep = "")
      df_name_csv   <- paste(tipo, "-", mun, "-", ano, ".csv",sep = "")
      df_name_pasta <- paste(tipo, "-", mun, "-", ano,        sep = "")
      
      url_baixar <- paste("https://transparencia.tce.sp.gov.br/sites/default/files/csv/", tipo, "-", mun, "-", ano, ".zip",sep = "")
      
      download.file(url_baixar, 
                    destfile = df_name_zip,
                    mode = "wb")               #traz para meu diretorio (vem zipado)
      
            unzip(zipfile = df_name_zip,     #Unzipa   
            # files = df_name_csv, 
            exdir = ".")
      
      file.remove(df_name_zip)
      
      # file.remove(df_name_pasta)
      
      despesas <- read.csv(file = df_name_csv, 
                           sep = ";", 
                           header = T, 
                           encoding = "latin1", 
                           dec = ",")
      
      # Cria Novas Colunas
        despesas$historico_std     <- ''
        despesas$categoria         <- ''
        despesas$subcategoria      <- ''
        despesas$eventos           <- ''
        despesas$selecao           <- ''
        despesas$otmu              <- ''
      
      despesas_vl <- dplyr::filter(despesas, tp_despesa == "Valor Liquidado")
      
      despesas_acum <- rbind.data.frame(despesas_acum, despesas_vl)
      
      rm (despesas, despesas_vl)
      
      file.remove (df_name_csv)
      
    }  # Fim do loop de município  
  }  # Fim do loop de ano
  
  despesas_acum$historico_std <- std_str(despesas_acum$historico_despesa) # cria campo histórico padronizado (sem acentuação)
   
  colnames(despesas_acum) <- c('identificação da despesa detalhe',
                               'ano exercicio',
                               'municipio',
                               'Orgão',
                               'mês',
                               'mês extenso',
                               'tipo despesa',	
                               'Num. Empenho (interno)',	
                               'identificador despesa',
                               'destino da despesa (Fornecedor)',
                               'data emissao despesa',
                               'Valor',
                               'funcao de governo',
                               'subfuncao governo',
                               'codigo do programa',
                               'descrição do programa',	
                               'cód Ação',	
                               'descrição da acao',	
                               'Fonte de Recurso',	
                               'código da aplicacao fixo',	
                               'modalidade de licitação',
                               'Categoria Econômica e Descrição  da Despesa',	
                               'Histórico da despesa',
                               'historico_std',
                               'categoria',
                               'sub_categoria',
                               'eventos',
                               'selecão',
                               'otmu')
  
  dsname_Rdata <- paste(tipo, "-", "municipios", "-", ano, ".Rdata",sep = "")
  save(despesas_acum, file = dsname_Rdata) # grava resultado em formato RData
 
  dsname_xlsx <- paste(tipo, "-", "municipios", "-", ano, ".xlsx",sep = "")
 
  write_xlsx(despesas_acum, dsname_xlsx) #grava data frame em formato *.xlsx
  
} # FUNÇÃO: Download Receitas do TCE  - municípios da lista_mun_minusculas
