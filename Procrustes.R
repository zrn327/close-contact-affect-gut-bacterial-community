library(vegan)
ARG<-read.csv("ARGs.csv",row.names=1)
Metaphlan<-read.csv("Metaphlan.csv",row.names=1)
ARG_pca <- rda(ARG, scale = F)
Metaphlan_pca <- rda(Metaphlan, scale = F)
par(mfrow = c(1, 2))
biplot(ARG_pca, choices = c(1, 2), scaling = 1, 
       main = 'ARG', col = c('red', 'blue'))
biplot(species_pca, choices = c(1, 2), scaling = 1, 
       main = 'Mtaphlan', col = c('red', 'blue'))

site_arg <- summary(ARG_pca, scaling = 1)$site
site_metaphlan <- summary(Mtaphlan_pca, scaling = 1)$site
proc <- procrustes(X = ARG_pca, Y = Mtaphlan_pca, symmetric = TRUE)
summary(proc)
plot(proc, kind = 1, type = 'text')
names(proc)
head(proc$Yrot)
head(proc$X)
proc$ss 
proc$rotation 
set.seed(123)
prot <- protest(X = arg_pca, Y = species_pca, permutations = how(nperm = 999))
prot

library(ggplot2)
Y <- cbind(data.frame(proc$Yrot), data.frame(proc$X))
X <- data.frame(proc$rotation)
group<-read.csv("group.csv",row.names=1)
Y$samples <- rownames(Y)
Y <- merge(Y, group, by = 'samples')
p <- ggplot(Y) +
  geom_point(aes(X1, X2, color = group), size = 2, shape = 16) +
  geom_point(aes(PC1, PC2, color = group), size = 2, shape = 1) +
  scale_color_manual(values = c('red2', 'blue'), limits = c('dog', 'owner')) +
  geom_segment(aes(x = X1, y = X2, xend = PC1, yend = PC2), arrow = arrow(length = unit(0.1, 'cm')),
               color = 'lightblue', size = 0.5) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = 'transparent')) +
  labs(x = 'Dimension 1', y = 'Dimension 2', color = '') +
  geom_vline(xintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_abline(intercept = 0, slope = X[1,2]/X[1,1], size = 0.5) +
  geom_abline(intercept = 0, slope = X[2,2]/X[2,1], size = 0.5) +
  annotate('text', label = sprintf('M^2 == 0.7307682'),
           x = -0.41, y = 0.55, size = 5, parse = TRUE) +
  annotate('text', label = 'P < 0.01',
           x = -0.41, y = 0.52, size = 5, parse = TRUE)