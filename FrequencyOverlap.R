library(stringr)
library(ggplot2)
library(tidyr)
library(ggpubr)

hornbill.files <- list.files('/Users/denaclink/Desktop/RStudio Projects/Hornbill-Analysis/rehornbillannotations',
full.names = T)
hornbill.files.short <- list.files('/Users/denaclink/Desktop/RStudio Projects/Hornbill-Analysis/rehornbillannotations',
                             full.names = F)



myfiles = lapply(hornbill.files, read.delim2, stringsAsFactors = F )

myfiles.combined <- do.call(rbind.data.frame,myfiles)

myfiles.combined$species <- str_split_fixed(hornbill.files.short,
                                            pattern = '_',n=3)[,1]




myfiles.combined <- myfiles.combined[,c("species",  "Freq.5...Hz.",
                    "Freq.25...Hz.","Center.Freq..Hz.", "Freq.75...Hz.", 
                      "Freq.95...Hz.","Peak.Freq..Hz.")]

myfiles.combined.sorted<-gather(myfiles.combined, Feature, Measurement, Freq.5...Hz.:Peak.Freq..Hz., factor_key=TRUE)

myfiles.combined.sorted$Measurement <- as.numeric(myfiles.combined.sorted$Measurement)

my_comparisons <- list( c("Freq.5...Hz.","Freq.5...Hz.") )

ggpubr::ggboxplot(data=myfiles.combined.sorted,
                  x='Feature',y='Measurement',fill='species')+ylab('Frequency (Hz)')


my_comparisons <- list( c("Freq.5...Hz.","Freq.5...Hz.") )
ggboxplot(data=myfiles.combined.sorted, 
          x='Feature',y='Measurement',fill='species')+ylab('Frequency (Hz)')+ stat_compare_means()
  # Add pairwise comparisons p-value
  stat_compare_means(comparisons = my_comparisons)
  
  
  
  myfiles.combined.sorted$species <- as.factor(myfiles.combined.sorted$species)
  levels(myfiles.combined.sorted$species) <- c('Helmeted','Rhinoceros')
  
  levels(myfiles.combined.sorted$Feature) <- c('Frequency 5%','Frequency 25%',
                                               'Center Frequency','Frequency 75%',
                                               'Frequency 95%','Peak Frequency')
  
  # annotation table with adjusted pvals and y-position of the labels
  anno_df <- compare_means(Measurement ~ species, group.by = "Feature", 
                           data = myfiles.combined.sorted, p.adjust.method = "holm") %>%
    mutate(y_pos = 3000, p.adj = format.pval(p, digits = 1))
  
  anno_df$y_pos <- c(2000,2000,2000,2000,3900,2000)
  
  
  # Create a boxplot and add custom p-value
  ggboxplot(myfiles.combined.sorted, x = "species", y = "Measurement", fill='species',ggtheme = theme_bw())+
    facet_wrap(~Feature,scales = "fixed") + ylim(0,4000)+
    scale_fill_manual(values=c('Red','Grey'))+
    geom_signif(
      data=anno_df, 
      aes(xmin = group1, xmax = group2, annotations = p.adj, y_position = y_pos), 
      manual= TRUE
    )+ guides(fill='none') +xlab('Species') +ylab('Frequency (Hz)')


  
  myfiles.combined[-c(1)] <-  as.data.frame(apply(myfiles.combined[-c(1)], 2, as.numeric))
  
  
  hornbill.umap <- 
    umap::umap(myfiles.combined[-c(1)],
               controlscale=TRUE,scale=3)
  
  hornbill.umap <- cbind.data.frame(hornbill.umap$layout,  myfiles.combined$species)

colnames(hornbill.umap) <-
  c("Dim.1", "Dim.2", "Species")


ggscatter(data=hornbill.umap,x="Dim.1",y="Dim.2",color = "Species")
  
  
  
  