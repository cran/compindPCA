#' @importFrom factoextra get_eigenvalue
#' @importFrom stats prcomp
#' @export

compind <- function(data, var_p, var_n){
  data_p <- data[var_p]
  data_n <- data[var_n]
  norm_p <- function(x){return((x-min(x,na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}
  norm_n <- function(x){return((max(x,na.rm = TRUE)-x)/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))}
  norm_data_p <- as.data.frame(apply(data_p,2,norm_p))
  norm_data_n <- as.data.frame(apply(data_n,2,norm_n))
  data_final <- cbind(norm_data_p, norm_data_n)
  data_pca <- prcomp(data_final)
  pc <- data_pca$rotation
  pc1=abs(pc)
  eigen <- get_eigenvalue(data_pca)
  eigenvalue <- eigen$eigenvalue
  eigenpc <- rowSums(pc1%*%eigenvalue)
  weight<-eigenpc/sum(eigenpc)
  data_final <- as.matrix(data_final)
  Index <- rowSums(data_final%*%weight)
  final_results<- list(Index=Index, Weights=weight)
  return(final_results)
}
