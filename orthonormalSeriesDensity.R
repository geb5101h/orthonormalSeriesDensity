library(magrittr)


# Orthogonal series density estimator
# of Efromovich, 1999
# Currently supports cosine basis
orthonormalSeriesDensity <- function(data,J) {
  if (data %>% min < 0 | data %>% max > 1) {
    stop("Data needs to be scaled within [0,1]")
  }
  
  n = data%>%length
  
  # Get coefficients
  orthCoeffs <- (1:J) %>% sapply(.,function(j)
    cosineBasis(data,j)) %>%
    apply(.,2,mean)
  
  # Get smoothing coefficients
  smooth<- (1-1/(n*orthCoeffs^2))%>%sapply(.,function(x)max(0,x))
  
  plotDensityFn <- function(points) {
    (1:J) %>% sapply(.,function(j)
      cosineBasis(points,j) * orthCoeffs[j]*smooth[j]) %>%
      apply(.,1,sum) + 1
  }
  
  predictions <- plotDensityFn(data)
  
  curve(plotDensityFn,0,1,,ylim = c(min(predictions),max(predictions) * 1.001))
  
  predictions
  
}

# Basis elements for cosine
# basis
cosineBasis <- function(x,j) {
  sqrt(2) * cos(pi * j * x)
}