# Função para realização de download de receitas do TCE-SP

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