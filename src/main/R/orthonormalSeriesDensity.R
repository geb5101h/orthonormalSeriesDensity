library(magrittr)


# Orthogonal series density estimator
# of Efromovich, 1999
# Currently supports cosine basis
orthonormalSeriesDensity <- function(data,
                                     highFreq = F,
                                     confBand = T,
                                     alpha = 0.95) {
  if (data %>% min < 0 | data %>% max > 1) {
    stop("Data needs to be scaled within [0,1]")
  }
  
  n = data %>% length
  Jn = floor(4 + 0.5 * log(n))
  Jmax = ifelse(highFreq == T,6 * Jn,Jn)
  # Get coefficients
  orthCoeffs <- (1:Jmax) %>% sapply(.,function(j)
    cosineBasis(data,j)) %>%
    apply(.,2,mean)
  
  Jrange = (1:Jn) %>% sapply(.,function(j) {
    2 / n - orthCoeffs[j] ^ 2
  })
  Jselect = Jrange %>% cumsum %>% which.min
  
  print(paste("Jn chosen to be",Jselect))
  # Get smoothing coefficients
  smooth <- (1 - 1 / (n * orthCoeffs ^ 2)) %>% sapply(.,function(x)
    max(0,x))
  
  plotDensityFn <- function(points) {
    (1:Jmax) %>% sapply(.,function(j) {
      if (j <= Jselect) {
        cosineBasis(points,j) * orthCoeffs[j] * smooth[j]
      }
      else
        if ((orthCoeffs[j] ^ 2 > 4 * log(n) / n)) {
          print(paste("Included basis element",j))
          
          cosineBasis(points,j) * orthCoeffs[j]
        }else
          points * 0
      
    }) %>%
      apply(.,1,sum) + 1
  }
  
  predictions <- plotDensityFn(data)
  maxY = max(predictions)
  if (confBand == T) {
    cc <- 2 * sqrt(Jselect * qchisq(alpha,Jselect) / n)
    f1 <- function(x){ plotDensityFn(x) - cc}
    f2 <- function(x){  plotDensityFn(x) + cc}
    maxY =  max(f2(data))
  }
  curve(
    plotDensityFn,0,1,ylim = c(0,maxY),
    xlab = paste("N=",n," Jn=",Jselect),
    main = "Orthonormal density estimate"
  )
  if (confBand == T) {
    curve(
      f1,0,1,
      xlab = paste("N=",n," Jn=",Jselect),
      add = T,
      lty = 2
    )
    curve(
      f2,0,1,
      add = T,
      lty = 2
    )
  }
  
  predictions
  
}

# Basis elements for cosine
# basis
cosineBasis <- function(x,j) {
  sqrt(2) * cos(pi * j * x)
}