############ Função Download Despesas do TCE  - municípios da lista_mun_minusculas ###########

download_despesas <- function(anos_f, lista_municipios_f) {
 
  library(dplyr)
  library(readxl)
  library(writexl)
  library(stringr)
 
  getwd()
  
 # anos_f             <- "2025"
 # lista_municipios_f <- "ilhabela"
  
  tipo = "despesas"
  
  despesas_acum <- read.csv(file = "Orçamento_Publico/despesas_vazia.csv", 
                            sep = ";", 
                            header = TRUE,
                            encoding = "latin1" )
  
   # Opção para acumular as despesas baixadas em arquivo histórico
   # despesas_acum <- read_xlsx("Despesas-Ilhabela_2008-2022.xlsx") # opção para juntar base até 2022dados 
  
  for(ano in anos_f) {
    
    for(mun in lista_municipios_f) {
      
      print (paste("baixando", tipo, "de:", mun, "ano:", ano))
      
      url_baixar <- paste("https://transparencia.tce.sp.gov.br/sites/default/files/csv/", tipo, "-", mun, "-", ano, ".zip",sep = "")
      
      df_name_zip   <- paste(tipo, "-", mun, "-", ano, ".zip",sep = "")
      df_name_csv   <- paste(tipo, "-", mun, "-", ano, ".csv",sep = "")
      df_name_pasta <- paste(tipo, "-", mun, "-", ano,        sep = "")
      
      download.file(url_baixar, df_name_zip)               #traz para meu diretorio (vem zipado)
      unzip(df_name_zip, files = df_name_csv)              #Unzipa           
      
      file.remove(df_name_zip)
      
      # file.remove(df_name_pasta)
      
      despesas <- read.csv(file = df_name_csv, sep = ";", header = T, 
                           encoding = "latin1", dec = ",")
      
      # Cria Novas Colunas
      despesas$historico_std     <- ''
      despesas$categoria         <- ''
      despesas$subcategoria      <- ''
      despesas$eventos           <- ''
      despesas$selecao           <- ''
      despesas$otmu              <- ''
     
      despesas_vl <- filter(despesas, tp_despesa == "Valor Liquidado")
      
      despesas_acum <- rbind.data.frame(despesas_acum, despesas_vl)
      
      rm (despesas, despesas_vl)
      
      file.remove (df_name_csv)
      
    }  # Fim do loop de município  
  }  # Fim do loop de ano
  
  despesas_acum$historico_std <- std_str(despesas_acum$historico_despesa)  # cria campo histórico padronizado (sem acentuação)
  
  # colnames(despesas_acum) <- c('identificação da despesa detalhe',
  #                               'ano exercicio',
  #                               'municipio',
  #                               'Orgão',
  #                               'mês',
  #                               'mês extenso',
  #                               'tipo despesa',	
  #                               'Num. Empenho (interno)',	
  #                               'identificador despesa',
  #                               'destino da despesa (Fornecedor)',
  #                               'data emissao despesa',
  #                               'Valor',
  #                               'funcao de governo',
  #                               'subfuncao governo',
  #                               'codigo do programa',
  #                               'descrição do programa',	
  #                               'cód Ação',	
  #                               'descrição da acao',	
  #                               'Fonte de Recurso',	
  #                               'código da aplicacao fixo',	
  #                               'modalidade de licitação',
  #                               'Categoria Econômica e Descrição  da Despesa',	
  #                               'Histórico da despesa',
  #                               'historico_std',
  #                               'categoria',
  #                               'subcategoria',
  #                               'eventos',
  #                               'selecão',
  #                               'OTMU')

  
  dsname_Rdata <- paste(tipo, "-", "municipios", "-", ano, ".Rdata",sep = "")
  save(despesas_acum, file = dsname_Rdata) # grava resultado em formato RData
  
  dsname_xlsx <- paste(tipo, "-", "municipios", "-", ano, ".xlsx",sep = "")
  write_xlsx(despesas_acum, dsname_xlsx) #grava data frame em formato *.xlsx
  
  } # FUNÇÃO: download_despesas TCE
  
