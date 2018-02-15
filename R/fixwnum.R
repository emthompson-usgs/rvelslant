fixwnum <- function(num, l, dp){
  # num = Number, will be converted to a character
  # l   = length of character
  # dp  = decimal places
  options(scipen = 100) # to suppress sci notation
  chr <- as.character(round(num, dp))
  subs <- NULL
  L <- nchar(chr)
  for(j in 1:L){
    subs[j] <- substr(chr, j, j)
  }
  if(any(subs == ".")){
    per <- L - grep("\\.", subs)
  }else{
    per <- 0
  }
  if(per == 0){
    chr <- paste(sep = "", chr, ".")
  }
  zeros <- paste(sep = "", rep(0, (dp - per)), collapse = "")
  chr <- paste(sep = "", chr, zeros)
  L <- nchar(chr)
  spaces <- paste(sep = "", rep(" ", (l - L)), collapse = "")
  chr <- paste(sep = "", spaces, chr)
  return(chr)
}


