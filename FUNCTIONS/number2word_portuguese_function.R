#https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
  if(x==0){
    print( "zero")
  } else{
    helper <- function(x){
      
      digits <- rev(strsplit(as.character(x), "")[[1]])
      nDigits <- length(digits)
      if (nDigits == 1) as.vector(ones[digits])
      else if (nDigits == 2)
        if (x <= 19) as.vector(teens[digits[1]])
      else trim(paste(tens[digits[2]], "e",
                      Recall(as.numeric(digits[1]))))
      else if (nDigits == 3) 
        if(digits[1] == 0 && digits[2] ==0 && digits[3] == 1) as.vector(hundreds[digits[1]])
      else trim(paste(hundreds[digits[3]], "e", 
                Recall(makeNumber(digits[2:1]))))
      else {
        nSuffix <- ((nDigits + 2) %/% 3) - 1
        if (nSuffix > length(suffixes)) stop(paste(x, "é muito grande!"))
        else if(digits[2] == 0 && digits[3] ==0)
        trim(paste(Recall(makeNumber(digits[
          nDigits:(3*nSuffix + 1)])),
          suffixes[nSuffix],"e" ,
          Recall(makeNumber(digits[(3*nSuffix):1]))))
        else trim(paste(Recall(makeNumber(digits[
          nDigits:(3*nSuffix + 1)])),
          suffixes[nSuffix],"," ,
          Recall(makeNumber(digits[(3*nSuffix):1]))))
      }
    }
    trim <- function(text){
      #Tidy leading/trailing whitespace, space before comma
      text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
      #Clear any trailing " and"
      text=gsub(" e$","",text)
      #Clear any trailing comma
      gsub("\ *,$","",text)
    }  
    makeNumber <- function(...) as.numeric(paste(..., collapse=""))     
    #Disable scientific notation
    opts <- options(scipen=100) 
    on.exit(options(opts)) 
    ones <- c("", "um", "dois", "três", "quatro", "cinco", "seis", "sete",
              "oito", "nove") 
    names(ones) <- 0:9 
    teens <- c("dez", "onze", "doze", "treze", "quatorze", "quinze",
               "dezesseis", "dezessete", "dezoito", "dezenove")
    names(teens) <- 0:9 
    tens <- c("vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta",
              "noventa") 
    names(tens) <- 2:9 
    hundreds <- c("cem", "cento", "duzentos", "trezentos", "quatrocentos", "quinhentos", "seiscentos", "setecentos",
              "oitocentos", "novecentos") 
    names(hundreds) <- 0:9 
    x <- round(x)
    suffixes <- c("mil", "milh?o", "bilh?o", "trilh?o")     
    if (length(x) > 1) return(trim(sapply(x, helper)))
    helper(x)    
  }
  
}


#essa função tem alguns problemas - não coloca o plural em milão/milhões
#bilão/bilhões - trilhão/trilhões
