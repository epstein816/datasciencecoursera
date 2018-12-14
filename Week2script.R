setwd(paste("C:/Users/Epste/code/datasciencecoursera/", "specdata", sep=""))
specdata <- getwd()
getwd()
specdata

pollutantmean <- function(directory, pollutant, id = 1:332){
  
  setwd(paste("C:/Users/Epste/code/datasciencecoursera/", directory, sep=""))
  
  filelist <- list.files(getwd(), full.names = TRUE)
  dat <- data.frame()
  for(i in id){
      
      dat <- rbind(dat, read.csv(filelist[i]))
 
      
  }
  mean1 <- mean(dat[, pollutant], na.rm = TRUE)    
  print(mean1)
}



complete <- function(directory, id = 1:332){
  
  setwd(paste("C:/Users/Epste/code/datasciencecoursera/", directory, sep=""))
  filelist <- list.files(getwd(), full.names = TRUE)
  noblist <- integer()
  for (i in id) { #use this loop to create a vector of nobs
  
    noblist <- c(noblist, nrow(na.omit(read.csv(filelist[i]))))
    }
  
 print(data.frame(id = id, nob = noblist))
}
  
  

  
  
  
corr <- function(directory, threshold = 0){
  
  setwd(paste("C:/Users/Epste/code/datasciencecoursera/", directory, sep=""))
  filelist <- list.files(getwd(), full.names = TRUE)
  
  nobs <-complete("specdata") 
  obs <- as.numeric(nobs[nobs$nob>threshold, "id"])
  
  dat.c <- data.frame()
  for(i in obs){
    
    dat.c <- rbind(dat.c, read.csv(filelist[i]))
    dat.c <- na.omit(dat.c)
    
  }
  
  corrs <- numeric()
  for (i in obs) {
    c.obs <- dat.c[dat.c$ID == i,]
    corrs <- c(corrs, cor(c.obs$sulfate, c.obs$nitrate))
  }

   print(corrs) 
  
}


