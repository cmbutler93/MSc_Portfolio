library(FactoMineR)
library(factoextra)
dat = read.csv("~/Documents/GDA/culture_dat.csv", header = TRUE)
dat1 = dat[2:16]
dat2 = na.omit(dat1)

pca_res <- PCA(dat2, scale.unit = TRUE, ncp = 2, quali.sup = c(1:4,7,13:14))
pca_res1 <- PCA(dat2, scale.unit = TRUE, ncp = 2, quali.sup = c(1:4,7,13:14), quanti.sup = 15)

evalues <- get_eigenvalue(pca_res)

fviz_screeplot(res.pca, ncp=10)

# Change the gradient color
var_fiz <- fviz_pca_var(pca_res, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red")+theme_bw()

hcpc_res <- HCPC(pca_res1)
hcpc_plot <- plot.HCPC(hcpc_res, centers.plot = T)

