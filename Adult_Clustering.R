library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(reshape2)
library(cluster)
library(class) #for KNN
set.seed(99)
setwd("C:/Users/mrrob/Desktop/Machine Learning York University/Course 1/Project 2 US Census")
raw_df=read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header = FALSE, na.strings = c(" ?"))

#raw_df=read.csv("adult_training.csv", header = FALSE, na.strings = c(" ?"))

raw_df<-na.omit(raw_df)
colnames(raw_df)<-c('age','workclass','fnlwgt','education','education_num','marital_status','occupation','relationship','race','sex','capital_gain','capital_loss','hours_per_week','native_country','salary')
raw_df <- dplyr::select(raw_df, -education)
raw_df<-raw_df %>% mutate(across(where(is.character), str_trim))

any(is.na(raw_df))

df<-raw_df
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)], as.integer)
str(df)

dt<-setDT(df) #data table
scaled_dt<-scale(dt)

pop_cluster<-kmeans(scaled_dt,4,nstart=50)

pca<-prcomp(scaled_dt, scale = FALSE)
#fviz_eig(pca)

pca_scaled_dt <- predict(pca, newdata = scaled_dt)

cluster_pca_scaled_dt <- cbind(pca_scaled_dt, cluster = pop_cluster$cluster)
cluster_scaled_dt <- cbind(scaled_dt, cluster = pop_cluster$cluster)
cluster_raw_df <- cbind(raw_df, cluster = pop_cluster$cluster)

cluster_pca_scaled_df <- as.data.frame(cluster_pca_scaled_dt)

ggplot(cluster_pca_scaled_df, aes(PC1,PC2))+
  geom_point(aes(color = as.factor(cluster)),size=1) +
  labs(color="Cluster")

#note: clusplot internally does pca when it plots.

# Results for variables
pca_vars<-get_pca_var(pca)
pca_vars$coord          # Coordinates
pca_vars$contrib        # Contributions to the PCs
pca_vars$cos2           # Quality of representation 
# Results for individuals
pca_inds<- get_pca_ind(pca)
pca_inds$coord          # Coordinates
pca_inds$contrib        # Contributions to the PCs
pca_inds$cos2           # Quality of representation 

ggplot(cluster_raw_df, aes(education_num,relationship))+
  geom_point(aes(color = as.factor(cluster)),size=1) +
  labs(color="Cluster")

cluster_raw_df %>% 
  group_by(cluster) %>%
  summarise(mean(age), mean(education_num), mean(capital_gain), mean(capital_loss), mean(hours_per_week))

cluster_raw_chr_df<-select(cluster_raw_df,-c("hours_per_week","capital_gain","capital_loss","age","fnlwgt","education_num"))

##Cluster 1

cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==1,-9]

#ggplot(gather(cluster_raw_chr_plot_df, cols, value), aes(x = value)) + 
#  geom_histogram(stat = "count") + facet_grid(.~cols)

ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~key, scales = 'free_x') +
  #scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  theme(axis.text.x = element_text(angle = 45))

##Cluster 2

cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==2,-9]

#ggplot(gather(cluster_raw_chr_plot_df, cols, value), aes(x = value)) + 
#  geom_histogram(stat = "count") + facet_grid(.~cols)

ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~key, scales = 'free_x') +
  #scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  theme(axis.text.x = element_text(angle = 45))

##Cluster 3

cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==3,-9]

#ggplot(gather(cluster_raw_chr_plot_df, cols, value), aes(x = value)) + 
#  geom_histogram(stat = "count") + facet_grid(.~cols)

ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~key, scales = 'free_x') +
  #scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  theme(axis.text.x = element_text(angle = 45))

##Cluster 4

cluster_raw_chr_plot_df<-cluster_raw_chr_df[cluster_raw_chr_df$cluster==4,-9]

#ggplot(gather(cluster_raw_chr_plot_df, cols, value), aes(x = value)) + 
#  geom_histogram(stat = "count") + facet_grid(.~cols)

ggplot(gather(cluster_raw_chr_plot_df), aes(value)) + 
  geom_histogram(stat = "count") + 
  facet_wrap(~key, scales = 'free_x') +
  #scale_x_discrete(guide = guide_axis(n.dodge=3)) +
  theme(axis.text.x = element_text(angle = 45))


## New Customer

final_df <- cbind(raw_df,PC1=cluster_pca_scaled_df$PC1,PC2=cluster_pca_scaled_df$PC2,cluster=cluster_pca_scaled_df$cluster)
final_df<-select(final_df,-fnlwgt)

pc1_model<-lm(PC1 ~.,select(final_df,-PC2,-cluster))
pc2_model<-lm(PC2 ~.,select(final_df,-PC1,-cluster))

summary(pc1_model)
summary(pc2_model)

new_cust<-as.data.frame(t(c(50,"Private",16,"Married-civ-spouse","Prof-specialty","Husband","White","Male",99999,0,50,"United-States",">50K")))
#new_cust<-as.data.frame(t(c(44,"State-gov",13,"Never-married","Adm-clerical","Not-in-family","White","Female",0,0,40,"United-States",">50K")))

colnames(new_cust)<-colnames(final_df[,-c(14,15,16)])

new_cust$age<-as.integer(new_cust$age)
new_cust$education_num<-as.integer(new_cust$education_num)
new_cust$capital_gain<-as.integer(new_cust$capital_gain)
new_cust$capital_loss<-as.integer(new_cust$capital_loss)
new_cust$hours_per_week<-as.integer(new_cust$hours_per_week)

#for testing#######
pc1_model<-lm(PC1 ~.,select(final_df[-9119],-PC2,-cluster))
pc2_model<-lm(PC2 ~.,select(final_df[-9119],-PC1,-cluster))

#new_customer
select(final_df[9119,],everything())
new_cust<-select(final_df[9119,],-PC1,-PC2,-cluster)

###################
pred_PC1<-predict(pc1_model,new_cust)
pred_PC2<-predict(pc2_model,new_cust)
new_cust$PC1<-pred_PC1
new_cust$PC2<-pred_PC2

pred_cluster<-knn(final_df[,c("PC1","PC2")],new_cust[,c("PC1","PC2")],final_df[,"cluster"],k=10)
new_cust$cluster<-pred_cluster

new_cust

ggplot(cluster_pca_scaled_df, aes(PC1,PC2))+
  geom_point(aes(color = as.factor(cluster)),size=1) +
  labs(color="Cluster") +
  geom_point(aes(x=new_cust$PC1,y=new_cust$PC2),colour="black", shape=13, size=3)

######### trials ######

final_plot_df<-cluster_pca_scaled_df
final_plot_df$cluster[final_plot_df$cluster==1]<-'Gold'
final_plot_df$cluster[final_plot_df$cluster==2]<-'Silver'
final_plot_df$cluster[final_plot_df$cluster==3]<-'Bronze'
final_plot_df$cluster[final_plot_df$cluster==4]<-'Platinum'
final_plot_df$cluster <- factor(final_plot_df$cluster, levels = c('Platinum','Gold','Silver','Bronze'))

ggplot(final_plot_df, aes(PC1,PC2))+
  geom_point(aes(color = as.factor(cluster)),size=1) +
  labs(color="Customer Type") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  geom_point(aes(x=new_cust$PC1,y=new_cust$PC2),colour="black", shape=13, size=3)
