#Jur Eekelder; 23-08-2021
#jur.eekelder@demarke.eu
#Script voor het controlerenr van kengetallen. Zijn sommige waardes reeël?
#BRON: https://edepot.wur.nl/544824

#INPUTS
#dataset --> data frame met KLW kengetallen

#OUTPUTS
#een lijst met KLW ID's die afgekeurd zijn.


controleDatasetKLW <- function(dataset){
  
  #Afkorten
  x = dataset
  
  checks = matrix(ncol = 3, byrow = T, data = c(
    
    "vet", 3.0, 7.0,
    "eiwit", 2.5, 5.5,
    "ureum", 9, 40,
    "opb_mais_ds", 0, 25000,
    "opb_graspr_ds", 0, 20000,
    "melkpkoe", 4000, 15000,
    "rants_geh_p", 2.5, 6.0,
    "rants_geh_re", 120, 200,
    "rants_geh_vem", 800, 1200
    
    
  ))
  
  KLW_id_list = NULL
  
  for(i in 1:nrow(checks)){
    
    variable = checks[i,1]
    lower = as.numeric(checks[i,2])
    upper = as.numeric(checks[i,3])
    
    print(variable)
    print(lower)
    print(upper)
    
    
    indexlower = NULL
    if(any(x[,variable] < lower, na.rm = T)){
      indexlower = which(x[,variable] < lower)
      
    } 
    
    indexupper = NULL
    if(any(x[,variable] > upper, na.rm = T)){
      indexupper = which(x[,variable] > upper)
      
    }
    
    
    index = NULL
    if(length(indexupper) > 0){
      index = c(index, indexupper)
    }
    
    if(length(indexlower) > 0){
      index = c(index, indexlower)
    }
    
    if(!is.null(index)){
      #print(index)
      KLW_id = x[index,"ID_KLW"]
      KLW_id_list = c(KLW_id_list, KLW_id)
    }
    
    
    
  }
  
  #print(KLW_id_list)
  
  return(unique(KLW_id_list))
  
  
}