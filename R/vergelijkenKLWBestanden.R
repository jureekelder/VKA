#Jur Eekelder; 23-08-2021
#jur.eekelder@demarke.eu
#Vergelijken van mappen VKA / VKO voor namen


vergelijkenKLWBestanden = function(input_folder){
  
  getNumbersFromString <- function(x) {
    elements = unlist(strsplit(x, ""))
    numberString = NULL
    
    for (e in elements) {
      suppressWarnings({
        number = as.numeric(e)
      })
      if (is.na(number)) {
        
      } else {
        numberString = paste(numberString, number, sep = "")
      }
      
    }
    
    if (is.null(numberString)) {
      return(NA)
    } else {
      return(numberString)
    }
    
  }
  
  main_dir = (input_folder)
  setwd(main_dir)
  file_list_main_directory = list.files()
  
  file_list_all = NULL
  for(folder in file_list_main_directory){
    
    setwd(paste(main_dir, "/", folder, sep = ""))
    file_list_dir = list.files()
    file_list_all = c(file_list_all, file_list_dir)
    
  }
  
  boer_jaar = data.frame()
  for(i in file_list_all){
    
    naam_plaats = unlist(str_split(i, " 2"))[1]
    jaartal = as.numeric(getNumbersFromString(i))
    
    data = cbind(naam_plaats, jaartal)
    boer_jaar = rbind(boer_jaar, data)
    
  }
  
  return(boer_jaar)
  
}



