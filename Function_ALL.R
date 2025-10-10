
#______________________________  Carrega funções auxiliares ____________________________________________________________ 

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

