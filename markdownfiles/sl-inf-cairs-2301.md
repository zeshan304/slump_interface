---
title: "Developing a machine learning based web-interface for feild slump prediction"
author: "Muhammad Zeshan Akber"
date: "`r Sys.Date()`"
output:
  html_document: default
  pdf_document: default
---

# Loading necessary packages and setup computing cores

```{r pkgLoad, echo=TRUE, message=FALSE, warning=FALSE, results='hide', cache=TRUE}

library(ggplot2)
library(gridExtra)
library(repr)
library(dplyr)
library(tensorflow)
library(caret)
library(MLmetrics)
library(here)
library(skimr)
library(multiROC)
library(mltools)
library(irr)
library(ggpubr)
library(grid)
library(e1071)
library(corrplot)
library(readr)
library(ggpubr)
library(grid)
library(tidyverse)
library(stringr)
library(arm)
library(klaR)
library(doParallel)
library(reticulate)
library(rstatix)
library(yarrr)
# creating a folder for results
dir.create('results')
# Parallel computing to start working on 12 out of 16 cpus
cl <- makePSOCKcluster(12); registerDoParallel(cl)

```

# Data importing and preparing

```{r dataImp, echo=TRUE, message=FALSE, warning=FALSE, results='hide', cache=TRUE}

df <- read.csv(paste(getwd(),'/data/df_sl_int_230131.csv',sep="")
               , stringsAsFactors = FALSE)
# Setting the names of data variables
names(df) <- c("Date", "Ticket_No", "MIX_ID", "Batch_size", "CAG_10mm",
               "CAG_20mm", "FAG", "PFA", "Water", "WRA_KFDN100",
               "SP1_KFDNSP8G", "SP2_R1002", "Item_Description", "Mix_Code",
               "Slump", "Target_strength", "Cement", "CagT", "sp",
               "Slump_class")
```

# Getting data insights

```{r dataInsights, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE, fig.align='center'}
# Data preparation for statistical summary
input_colums <- c("Batch_size", "Cement", "Water", "FAG", "CAG_10mm",
                  "CAG_20mm", "PFA", "WRA_KFDN100", "SP1_KFDNSP8G", 
                  "SP2_R1002", "Slump_class")
df_Summary <- df[c("Slump_class",input_colums)]
df_Summary$Slump_class <- gsub(x=df_Summary$Slump_class, pattern = "sl_",
                               replacement = "")
df_Summary <- df_Summary %>% select(all_of(input_colums))
# Extracting statistical summary
Summary<- df_Summary %>% group_by(.,Slump_class) %>%
  get_summary_stats(.,show = c("min",	"max",	"mean",	"sd"))
Summary$variable <- factor(Summary$variable,levels = input_colums)
Slump_classes <- c("25mm","75mm","80mm","100mm", "125mm", "135mm", "150mm", 
                   "175mm", "200mm")
Summary$Slump_class <- factor(Summary$Slump_class, levels =Slump_classes)
knitr::kable(Summary %>% arrange(Slump_class,variable), escape = F,
             col.names = c("Slump","Concrete constituents",
                           "No. of observations","Min", "Max", "Mean",
                           "Standard deviation"), digits = 0, format = "simple")
# Extracting summary as csv file
Summary %>% arrange(Slump_class,variable) %>%
    write.csv(paste0("results/Summary_",Sys.Date(),".csv"))
# Making and drawing pirate plots
df_Summary$Slump_class <- factor(df_Summary$Slump_class, levels = Slump_classes)
# Custom function of pirate plot
CustPplot <- function(formula=Batch_size~Slump_class,
                      main = expression(Batch~size~(m^3)), xaxt=FALSE) {
  pirateplot(data = df_Summary, formula = formula,
             avg.line.lwd=.7, avg.line.col="blue",
             bean.lwd=0.1, bean.f.col = "lightpink",
             inf.method = "sd", inf.f.o=.2, inf.b.col="red",inf.lwd = 0.1,
             point.pch = 1, point.o = .4, point.cex = 0.2,
             xlab ="",  ylab = "", xaxt=xaxt, main = main)
 }
par(family = "serif", font=1, mai=c(.05,.7,.3,0.05))
# Batching plot
CustPplot()
# Cement
CustPplot(formula = Cement~Slump_class, main = expression(Cement~(kg)))
# Water
CustPplot(formula = Water~Slump_class, main = expression(Water~(kg)))
# Fine aggregated
CustPplot(formula = FAG~Slump_class, main = expression(Fine~aggregate~(kg)))
# Coarse aggregate (10mm)
CustPplot(formula = CAG_10mm~Slump_class,
          main = expression(Coarse~aggregate-paste("10mm")))
# Coarse aggregate (20mm)
CustPplot(formula = CAG_20mm~Slump_class,
          main = expression(Coarse~aggregate-paste("20mm")~(kg)))
# Fly ash
CustPplot(formula =PFA ~Slump_class, main = expression(Fly~ash~(kg)))
# Water reducer
CustPplot(formula = WRA_KFDN100~Slump_class,
          main = expression(Water~reducer~(g)))
par(family = "serif", font=1, mai=c(.4,.7,.3,0.05))
# Superplastisizer1
CustPplot(formula = SP1_KFDNSP8G~Slump_class, xaxt = NULL,
          main = expression(Superplasticizer-paste("type1")~(g)))
# Superplastisizer2
CustPplot(formula = SP2_R1002~Slump_class, xaxt = NULL,
          main = expression(Superplasticizer-paste("type2")~(g)))

```

# Data partitioning

```{r dataPart, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE}

set.seed(1226) # Setting the seeds to have consistency
# Making index for training data set based on slump class
df_index <- createDataPartition(df$Slump_class, p = 0.8, list=FALSE)
df_train <- df[df_index,] ; df_test <- df[-df_index,]

```

# ML model training and optimization

```{r MlLearn, echo=TRUE, message=FALSE, warning=FALSE, results='hide', cache=TRUE}
# Setting the seeds to have consistency
set.seed(1226); seeds <- vector(mode = "list", length = 51)
for (i in 1:50) seeds[[i]] <- sample.int(1000, 243)
set.seed(1226); seeds[[51]] <- sample.int(1000, 1)
# Setting arugments of training
train_Control<-trainControl("repeatedcv", repeats=10, num=5, returnData=TRUE,
                            classProbs = TRUE,  savePredictions = "all",
                            summaryFunction=multiClassSummary,
                            returnResamp="all", seeds = seeds)
# Setting the metric for optimization
metric <- "logLoss"
# Setting inputs and outputs
Form <- formula(`Slump_class`~ Batch_size+ Cement+ Water+ PFA+ FAG+ CAG_10mm+
                  CAG_20mm+ WRA_KFDN100+ SP1_KFDNSP8G+ SP2_R1002)
# Setting preprocess method
pre_process <- c("center", "scale")
# Regularized logistic regression
set.seed(1226)
fit_01regLogistic <- train(form=Form, data=df_train, metric=metric,
                           trControl=train_Control, preProcess=pre_process,
                           method="regLogistic")
# Generalized linear model
set.seed(1226)
fit_02glmnet <- train(form=Form, data=df_train, metric=metric,
                      trControl=train_Control, preProcess=pre_process,
                      method="glmnet")

# Linear discriminant analysis: "lda"
set.seed(1226)
fit_03lda <- train(form=Form, data=df_train, metric=metric,
                   trControl=train_Control, preProcess=pre_process,
                   method="lda")
# Shrinkage discriminant analysis: "sda"
set.seed(1226)
fit_04sda <- train(form=Form,data=df_train, metric=metric,
                   trControl=train_Control, preProcess=pre_process,
                   method="sda", verbose=F)
# Multi layer perceptron: "mlp"
set.seed(1226)
fit_05mlp <- train(form=Form,data=df_train, metric=metric,
                   trControl=train_Control, preProcess=pre_process,
                   method="mlp")
# Multi layer perceptron with weighted decay parameters: "mlpWeightDecay"
set.seed(1226)
fit_06mlpWD <- train(form=Form,data=df_train, metric=metric,
                     trControl=train_Control, preProcess=pre_process,
                     method="mlpWeightDecay")

# Multi layer perceptron network with dropout: "mlpKerasDropout"
set.seed(1226)
fit_07mlpKerasDropout <- train(form=Form,data=df_train,trControl=train_Control,
                               metric=metric,preProcess=pre_process,
                               method="mlpKerasDropout",verbose=F)
# Support vector machines with linear kernel: "svmLinear"
set.seed(1226)
fit_08svml <- train(form=Form,data=df_train, metric=metric,
                    trControl=train_Control, preProcess=pre_process,
                    method="svmLinear")
## Support vector machines with polynomial kernel: "svmPoly"
set.seed(1226)
fit_09svmp <- train(form=Form, data=df_train, metric=metric,
                    trControl=train_Control, preProcess=pre_process,
                    method="svmPoly")
# Support vector machines with radial basis function kernel: "svmRadial"
set.seed(1226)
fit_10svmr <- train(form=Form, data=df_train, metric=metric,
                    trControl=train_Control, preProcess=pre_process,
                    method="svmRadial")
# Classification and regression tree: "rpart2"
set.seed(1226)
fit_11cart <- train(form=Form, data=df_train, metric=metric,
                    trControl=train_Control, preProcess=pre_process,
                    method="rpart2")
# C5.0 decesion trees: "C5.0"
set.seed(1226)
fit_12C5 <- train(form=Form, data=df_train, metric=metric,
                  trControl=train_Control, preProcess=pre_process,
                  method="C5.0Tree")
# Random forest: "rf"
set.seed(1226)
fit_13rf <- train(form=Form, data=df_train, metric=metric,
                  trControl=train_Control, preProcess=pre_process,
                  method="rf")
# Extreme gradient boosting: "xgbTree"
set.seed(1226)
fit_14xgb <- train(form=Form, data=df_train, metric=metric,
                   trControl=train_Control, preProcess=pre_process,
                   method="xgbTree")

```

## Training performance

```{r trainResults, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE, fig.align='center'}

Model_list <-
          list ("RLR"=fit_01regLogistic,
                "GLM" =fit_02glmnet,
                "LDA"=fit_03lda,
                "SDA"=fit_04sda,
                "SVM-L"=fit_08svml,
                "SVM-P"=fit_09svmp,
                "SVM-R"=fit_10svmr,
                "MLP"=fit_05mlp,
                "MLP-WD"=fit_06mlpWD,
                "MLP-D"=fit_07mlpKerasDropout,
                "CART"=fit_11cart,
                "C5.0"=fit_12C5,
                "RF"=fit_13rf,
                "XGBoost"=fit_14xgb)
# Getting training results
resamp_list <- caret::resamples(x = Model_list)
# bwplot(resamp_list, metric=c("Accuracy","logLoss","Kappa"),
#        layout = c(1,3))
# bwplot(resamp_list, metric=c("logLoss"))
# bwplot(resamp_list, metric=c("Kappa"))
df_resamp_results<-resamp_list$values
df_resamp_results<-df_resamp_results%>%
  select(.,contains(match =c("~Accuracy","~Kappa","~logLoss") ))
dfmelt<-pivot_longer(data = df_resamp_results,cols = 1:42,
                     names_to = "Model",
                     values_to ="Value")


dfmelt[grepl(x =dfmelt$Model,pattern = "~Accuracy" ),"Metric"]<-"Accuracy"
dfmelt[grepl(x =dfmelt$Model,pattern = "~Kappa"),"Metric"]<-"Kappa"
dfmelt[grepl(x =dfmelt$Model,pattern = "~logLoss"),"Metric"]<-"logLoss"
dfmelt$Model<-gsub(x=dfmelt$Model,pattern = "~Accuracy",replacement = "")
dfmelt$Model<-gsub(x=dfmelt$Model,pattern = "~Kappa",replacement = "")
dfmelt$Model<-gsub(x=dfmelt$Model,pattern = "~logLoss",replacement = "")
dfmelt$Model<-factor(dfmelt$Model,
                     levels = dfmelt$Model%>%unique())
df_logLoss_summary<-dfmelt%>%
            filter(.,Metric=="logLoss")%>%
            group_by(.,Model)%>%
            get_summary_stats(.,
                              show = c("min","max","mean","median","sd","se"))


df_logLoss_summary%>%write.csv("results/df_logLoss_summary.csv")
dfmelt$Model<-factor(dfmelt$Model,
                     levels = dfmelt$Model%>%unique()%>%.[14:1])
knitr::kable(df_logLoss_summary%>%select(!c(variable,n))
             ,escape = F,digits = 4,format = "simple")
# Boxplots
bp_train_results<-ggplot(data=dfmelt%>%filter(.,Metric=="logLoss"),
                         aes(x=Value, y=Model
                             # color=Metric,fill=Metric
                             )) +
  geom_boxplot(varwidth = TRUE,color="blue",
               outlier.size = 0,
               outlier.colour = NA)+
  # facet_wrap(facets = "Metric",scales="free_x",nrow = 3)+
  theme_classic()+labs(x="logLoss variation",y="")+
  theme(axis.text.y = element_text(colour = "black",size=12,
                                   margin =margin(t = 5, r = 1, b = 5, l = 1,
                                                  unit = "pt")),
                axis.text = element_text(colour = "black",size=11),
        axis.title = element_text(colour = "black",size=12),
        legend.title = element_blank(),
        legend.position="none",
        # # strip.text = element_text("logLoss variation",size = 12),
        # # strip.background = element_rect(fill = "White"),
        # # panel.spacing = unit(1, "lines"),
        # axis.line.x.bottom = element_line(colour = "black"),
        # # plot.margin = margin(t = 10, r = 15, b = 10, l = 5, unit = "pt")
        )+
  # scale_colour_manual(values = rainbow(3))+
  scale_x_continuous(breaks = seq(0.5,2.5,0.5))
bp_train_results



# http://127.0.0.1:20991/graphics/plot_zoom_png?width=718&height=404
ggsave(plot = last_plot(),filename = "results/bp_train.jpeg",device = "jpeg",dpi=600,
       height = 404/96,width = 718/96)

```

> ### Optimization (Hyperparameter tunning) results

```{r optResults, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE}

df_hp_rlr<-fit_01regLogistic$results
df_hp_rlr$loss<-as.factor(df_hp_rlr$loss)
plt_rlr<-ggplot(data=df_hp_rlr, aes(y = logLoss, x = cost, color=loss)) +
  geom_point() + geom_line() + theme_classic()+
  facet_wrap(. ~epsilon, ncol = 3,labeller =label_both)+
  labs(color="loss",x=expression(cost),y="")+
  ggtitle("RLR")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(0.5,1.0,2.0))+
  scale_y_continuous(breaks = round(seq(0.76,0.872,length.out=5),3),
                     limits = c(0.76,.872))
plt_rlr

df_hp_glm<-fit_02glmnet$results
df_hp_glm$lambda<-signif(df_hp_glm$lambda,1)
df_hp_glm$lambda<-sprintf("%.4f",df_hp_glm$lambda)
plt_glm<-ggplot(data=df_hp_glm, aes(y = logLoss, x = alpha,color=lambda)) +
  geom_point() + geom_line() + theme_classic()+
  # facet_wrap(. ~epsilon, ncol = 3,labeller =label_both)+
  labs(color=expression(lambda),x=expression(alpha),y="") +
  ggtitle("GLM")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(0.1,.55,1.0))+
  scale_y_continuous(breaks = round(seq(0.75,1.25,length.out=5),3)
                     ,limits = c(0.75,1.24))
plt_glm

# df_hp_glm<-fit_02glmnet$results
# df_hp_glm$lambda<-signif(df_hp_glm$lambda,1)
# df_hp_glm$lambda<-as.factor(df_hp_glm$lambda)
plt_sda<-ggplot(data=fit_04sda$results,
                aes(y = logLoss, x = lambda)) +
  geom_point() + geom_line() + theme_classic()+
  # facet_wrap(. ~epsilon, ncol = 3,labeller =label_both)+
  labs(color="loss function",x=expression(lambda),y="") +
  ggtitle("SDA")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(0,.5,1.0))+
  scale_y_continuous(breaks = round(seq(1.111,2.270,length.out=5),3),
                     limits = c(1.111,2.27))
plt_sda

plt_mlp<-ggplot(data=fit_05mlp$results,
                aes(y = logLoss, x = size)) +
  geom_point() + geom_line() + theme_classic()+
  # facet_wrap(. ~epsilon, ncol = 3,labeller =label_both)+
  labs(color="loss function",x="size",y="") +
  ggtitle("MLP")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(1,3,5.0))+
  scale_y_continuous(breaks = round(seq(0.93,1.54,length.out=5),3)
                     ,limits = c(0.93,1.54))
plt_mlp

df_hp_mlpwd<-fit_06mlpWD$results
df_hp_mlpwd$decay<-sprintf("%.4f",df_hp_mlpwd$decay)
plt_mlpwd<-ggplot(data=df_hp_mlpwd,
                aes(y = logLoss, x = size,color=decay)) +
  geom_point() + geom_line() + theme_classic()+
  # facet_wrap(. ~epsilon, ncol = 3,labeller =label_both)+
  labs(color="decay",x="size",y="") +
  ggtitle("MLP-WD")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(1,3,5.0))+
  scale_y_continuous(breaks = round(seq(0.75,1.54,length.out=5),3)
                     ,limits = c(0.75,1.54))
plt_mlpwd

df_hp_mlpD<-fit_07mlpKerasDropout$results
df_hp_mlpD$dropout<-sprintf("%.2f",df_hp_mlpD$dropout)
plt_mlpD<-ggplot(data=df_hp_mlpD,
                aes(y = logLoss, x = size,color=dropout)) +
  geom_point() + geom_line() + theme_classic()+
  # facet_wrap(. ~epsilon, ncol = 3,labeller =label_both)+
  labs(color="dropout",x="size",y="") +
  ggtitle("MLP-D")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(1,3,5))+
  scale_y_continuous(breaks = round(seq(2.235,2.41,length.out=5),3),
                     limits = c(2.235,2.41))
plt_mlpD

df_hp_svmp<-fit_09svmp$results
df_hp_svmp$scale<-sprintf("%.3f",df_hp_svmp$scale)
plt_svmp<-ggplot(data=df_hp_svmp,
                aes(y = logLoss, x = degree,color=scale)) +
  geom_point() + geom_line() + theme_classic()+
  facet_wrap(. ~C, ncol = 3,labeller =label_both,scales = "fixed")+
  labs(color="scale",x="Polynomial degree",y="") +
  ggtitle("SVM-P")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(breaks = round(seq(1.166,1.379,length.out=5),3),
                     limits = c(1.166,1.379))
plt_svmp

# df_hp_svmr<-fit_10svmr$results
# df_hp_svmr$C<-factor(sprintf("%.2f",df_hp_svmr$C))
plt_svmr<-ggplot(data=fit_10svmr$results,
                aes(y = logLoss, x = C)) +
  geom_point() + geom_line() + theme_classic()+
  # facet_wrap(. ~degree, ncol = 3,labeller =label_both)+
  labs(color="",x="cost",y="") +
  ggtitle("SVM-R")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(.25,.5,1))+
  scale_y_continuous(breaks = round(seq(1.0930,1.1051,length.out=5),3),
                     limits = c(1.0930,1.1051))
plt_svmr

# df_hp_svmr<-fit_10svmr$results
# df_hp_svmr$C<-factor(sprintf("%.2f",df_hp_svmr$C))
plt_cart<-ggplot(data=fit_11cart$results,
                aes(y = logLoss, x = maxdepth)) +
  geom_point() + geom_line() + theme_classic()+
  # facet_wrap(. ~degree, ncol = 3,labeller =label_both)+
  labs(color="",x="maxdepth",y="") +
  ggtitle("CART")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(1,2,3))+
  scale_y_continuous(breaks = round(seq(.995,1.35,length.out=5),3)
                     ,limits = c(0.995,1.35))
plt_cart

# df_hp_svmr<-fit_10svmr$results
# df_hp_svmr$C<-factor(sprintf("%.2f",df_hp_svmr$C))
plt_rf<-ggplot(data=fit_13rf$results,
                aes(y = logLoss, x = mtry)) +
  geom_point() + geom_line() + theme_classic()+
  # facet_wrap(. ~degree, ncol = 3,labeller =label_both)+
  labs(color="",x="mtry",y="") +
  ggtitle("RF")+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing = unit(1, "lines"))+
  scale_x_continuous(breaks = c(2,6,10))+
  scale_y_continuous(breaks = round(seq(0.360,.387,length.out=5),3),
                     limits = c(0.360,.387))
plt_rf

plt_layout_matrix = rbind(c(3,2,2,1,1,1),c(4,5,5,9,9,9),
                          c(7,8,10,6,6,6))
plt_hp_all<-arrangeGrob(grobs=list(plt_rlr,plt_glm,plt_sda,plt_mlp,
                        plt_mlpD,plt_mlpwd,plt_cart,plt_rf,
                        plt_svmp,plt_svmr),nrow =3,ncol=6,
                        layout_matrix =plt_layout_matrix)
plt_hp_all<-grid.arrange(plt_hp_all)
plt_hp_all<-annotate_figure(p = plt_hp_all,
                            left = text_grob("logLoss", face = "plain",
                                             rot = 90, vjust = 0.85),
                            bottom = text_grob("Model hyperparameters",
                                               face = "plain", vjust = 0.45))
plt_hp_all<- plt_hp_all+
  geom_segment(aes(x = .025, y=0.03, xend=0.025, yend = .98), size =.6,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = .025, y=0.03, xend=0.98, yend = .03), size =.6,
               arrow = arrow(length = unit(0.3, "cm")))
# #http://127.0.0.1:21067/graphics/plot_zoom_png?width=1257&height=796
ggsave(filename = "results/plt_hp_all.jpeg",plot = last_plot(),
       device = "jpeg",width = 1257/96,height = 796/96,dpi = 600)

fit_14xgb$results$max_depth<-as.factor(fit_14xgb$results$max_depth)
plt_xgb<-ggplot(data=fit_14xgb$results,
                aes(y = logLoss, x = nrounds, color=max_depth),
                fill=max_depth) +
  geom_point(aes(colour = max_depth)) + geom_line() +
  labs(y="",x="")+
  ggtitle("XGBoost")+
  theme_classic() +
  facet_wrap(. ~colsample_bytree*eta*subsample,
             ncol = 4,labeller =label_both )+
  theme(legend.position = "right",
        axis.title = element_text(face = "plain",size = 11,color = "black"),
        axis.text = element_text(face = "plain",size = 11,color = "black"),
        strip.text = element_text(face = "plain",size = 11),
        legend.title = element_text(face = "plain",size = 11),
        legend.text = element_text(face = "plain",size = 11),
        panel.spacing.x = unit(1, "lines"))+
  scale_x_continuous(breaks = c(50,100,150))+
  scale_y_continuous(breaks =round(seq(0.360,0.756,length.out=5),3),
                     limits = c(0.360,0.756))
plt_xgb
plt_xgb<-annotate_figure(p = plt_xgb,
                            left = text_grob("logLoss", face = "plain",
                                             rot = 90, vjust = 0.85),
                            bottom = text_grob("nrounds",
                                               face = "plain", vjust = 0.45))
plt_xgb<- plt_xgb+
  geom_segment(aes(x = .025, y=0.03, xend=0.025, yend = .98), size =.6,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = .025, y=0.03, xend=0.98, yend = .03), size =.6,
               arrow = arrow(length = unit(0.3, "cm")))
# #http://127.0.0.1:21067/graphics/plot_zoom_png?width=1257&height=796
ggsave(filename = "results/plt_hp_xgb.jpeg",plot = last_plot(),
       device = "jpeg",width = 1257/96,height = 796/96,dpi = 600)

```

> ### 2nd arrangement of hp plots

```{r optResults2, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE}

plt_layout_matrix4 = rbind(c(1),c(2,2,2,3,3),c(4))
plt_hp_4<-arrangeGrob(
  grobs= list(plt_rlr,plt_glm,plt_sda,plt_svmp),
                         nrow =3,ncol=4,layout_matrix =plt_layout_matrix4)
plt_hp_4<-annotate_figure(p = plt_hp_4,
                            left = text_grob("logLoss", face = "plain",
                                             rot = 90, vjust = 0.25),
                            bottom = text_grob("Model hyperparameters",
                                               face = "plain", vjust = 0.25))
plt_hp_4<- plt_hp_4+
  geom_segment(aes(x = .04, y=0.045, xend=0.04, yend = .98), size =.6,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = .04, y=0.045, xend=0.98, yend = .045), size =.6,
               arrow = arrow(length = unit(0.3, "cm")))
plt_hp_4<-grid.arrange(plt_hp_4)

# http://127.0.0.1:21067/graphics/plot_zoom_png?width=786&height=605
ggsave(filename = "results/plt_hp_4.jpeg",plot = last_plot(),
       device = "jpeg",height = 605/96,width = 786/96,dpi = 600)

plt_layout_matrix5 = rbind(c(1,2),c(3),c(4),c(5,6))
plt_hp_5<-arrangeGrob(
  grobs= list(plt_svmr,plt_mlp,plt_mlpwd,plt_mlpD,plt_cart,plt_rf),
                         nrow =4,ncol=2,layout_matrix =plt_layout_matrix5)
plt_hp_5<-annotate_figure(p = plt_hp_5,
                            left = text_grob("logLoss", face = "plain",
                                             rot = 90, vjust = 0.25),
                            bottom = text_grob("Model hyperparameters",
                                               face = "plain", vjust = 0.25))
plt_hp_5<- plt_hp_5+
  geom_segment(aes(x = .045, y=0.040, xend=0.045, yend = .98), size =.6,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = .045, y=0.040, xend=0.98, yend = .040), size =.6,
               arrow = arrow(length = unit(0.3, "cm")))

plt_hp_5<-grid.arrange(plt_hp_5)

#http://127.0.0.1:21067/graphics/plot_zoom_png?width=674&height=678
ggsave(filename = "results/plt_hp_5.jpeg",plot = last_plot(),
       device = "jpeg",height = 678/96,width = 674/96,dpi = 600)


```

# Validation of ML models on test data

> ### Getting the results of Accuracy, Kappa and logLoss

```{r testResults, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE}

set.seed(1226); test_pred_data<-data.frame(c=1:1495)
set.seed(1226)
Test_result<-data.frame("Accuracy"=1:14, "logLoss"=1:14,
                        "kappa"=1:14,"MCC"=1:14,
                        row.names=names(Model_list))
df_test1<-as_tibble(df_test)
# Estimation of Accuracy of models  #
set.seed(1226)
Multi_logloss <- function(model, df=df_test1){
  set.seed(1226)
  pred <- predict(object=model, df, type = 'prob',verbose=F)
  logloss <- MLmetrics::MultiLogLoss(y_true = df$Slump_class,
                                     y_pred = as.matrix(pred))
  return(logloss)
}
set.seed(1226)
for (i in names(Model_list)) {
  set.seed(1226)
  df_test1[i] <- predict(Model_list[i],df_test,verbose=F)
  Test_result[i,"Accuracy"] <- Accuracy(y_pred = predict(Model_list[[i]],
                                                         newdata = df_test1),
                                        y_true = df_test$Slump_class)

  Test_result[i,"kappa"] <- kappam.fleiss(data.frame(df_test1[,"Slump_class"],
    predict(Model_list[[i]], newdata = df_test1)))$value
  Test_result[i,"MCC"] <- mcc(df_test1$Slump_class,
                              as.character(predict(Model_list[[i]],
                                                   newdata = df_test1)))
  Test_result[i,"logLoss"] <- Multi_logloss(model = Model_list[[i]],
                                            df = df_test1)
}

knitr::kable(Test_result,
             escape = F,
             col.names = c("Accuracy",
                           "logLoss","kappa", "MCC"),
             digits = 4,
             format = "simple")

Test_result%>%write.csv("results/test_Results.csv")

```

> ### Defining the function for AUC for ROC and PR curves

```{r AUCfunc, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE}

df_AUC_curv<-df_test1
for (i in names(df_AUC_curv[,20:length(df_test1)])) {
  df_AUC_curv[,i]<-gsub(pattern = "sl_",replacement = "",
                        x = unlist(df_AUC_curv[,i]))
  df_AUC_curv[,i]<-factor(x = unlist(df_AUC_curv[,i]),
                          levels = c("25mm","75mm", "80mm","100mm",
                                     "125mm","135mm","150mm",
                                     "175mm","200mm"))
  }

AUC<-function(model=fit_01regLogistic,df=df_AUC_curv, ggtitle=ggtitle("RLR"),
              method="_pred_fit_rlr", un=c(0.0,0.2,0,0),
              x.text=element_text()) {
    x.text=x.text
    un=un
    levels = c("25mm","75mm", "80mm","100mm","125mm","135mm","150mm",
               "175mm","200mm","Macro","Micro")
    colorLevels = c("#EEDFCC","#66CDAA","#C1CDCD","#0000CD","#000000",
                    "#CD3333","#FFFF00","#66CD00","#FF7F00","#008B00","#003B00")
    set.seed(1226)
    pred = data.frame(predict(model, df, type = 'prob'))
    colnames(pred)=paste(gsub("sl_", "", colnames(pred)))
    colnames(pred)=paste(colnames(pred), method,sep = "")
    true_label = data.frame(dummy::dummy(df_AUC_curv['Slump_class']))
    cnames=levels[1:9]
    colnames(true_label) = cnames
    colnames(true_label) = paste(colnames(true_label), "_true",sep = "")
    set.seed(1226)
    AUC_df = cbind(true_label, pred)
    set.seed(1226)
    # ROC Estimation
    AUC_SS = multi_roc(AUC_df)
    Plot_AUC_SS = plot_roc_data(AUC_SS)
    Plot_AUC_SS$Group%>%unique()
    Plot_AUC_SS$Group = factor(Plot_AUC_SS$Group,levels = levels)
    Plot_AUC_SS$gmean=sqrt(Plot_AUC_SS$Sensitivity*Plot_AUC_SS$Specificity)
    ss<- ggplot(Plot_AUC_SS, aes(x = 1-Specificity, y=Sensitivity)) +
      geom_path(aes(color = Group), size=0.50) +
      geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1),
                   colour='grey', linetype = 'dotdash') +
      ggtitle + theme_classic() + labs (x="",y="") +
      theme(
        # axis.title = element_text(face = "bold"),
        axis.text = element_text(size = 12,color = "black"),
        # axis.text.x = x.text,
        plot.title = element_text(size = 12,face = "plain",
                                      colour = "#04456b"),
        plot.margin = unit(un,"cm"),
        legend.title = element_blank(),
        legend.position="bottom",legend.direction = "horizontal",
        legend.background = element_rect(fill=NULL, size=0.5,
                                         linetype="dotted",colour ="black")) +
      scale_colour_manual(values = colorLevels)
    legend_ROC<- get_legend(ss)
    ss <- ss+theme(legend.position = "none")
  
    # PR estimation
    set.seed(1226)
    AUC_PR = multi_pr(AUC_df)
    Plot_AUC_PR = plot_pr_data(AUC_PR)
    Plot_AUC_PR$Group = factor(Plot_AUC_PR$Group,levels = levels)
    Plot_AUC_PR$fscore = (2*Plot_AUC_PR$Precision*Plot_AUC_PR$Recall)/
      (Plot_AUC_PR$Precision + Plot_AUC_PR$Recall)
    pr<- ggplot(Plot_AUC_PR, aes(x=Recall, y=Precision)) +
      geom_path(aes(color = Group), size=0.5) +
      geom_segment(aes(x = 0, y = 1, xend = 1, yend = 0),
                   colour='grey', linetype = 'dotdash') +
      ggtitle + theme_classic() + labs (x="",y="") +
      theme(
        # axis.text.x = x.text,
        axis.text = element_text(size = 12,color = "black"),
        plot.title = element_text(size = 12,face = "plain",colour = "#04456b"),
        plot.margin = unit(un,"cm"),
        legend.title=element_blank(),
        legend.position="bottom",legend.direction = "horizontal",
        legend.background
        = element_rect(fill=NULL, size=0.5,
                                         linetype="dotted", colour ="black"))+
    # scale_colour_manual(values = rainbow(10))+
    scale_colour_manual(values =  colorLevels)
    legend_pr<- get_legend(pr)
    pr <- pr+theme(legend.position = "none")
    return(list("ss"=ss,"pr"=pr,"ROC"=AUC_SS,"PR"=AUC_PR,"df_roc"=Plot_AUC_SS,
                "df_pr"=Plot_AUC_PR,"leg_ss"=legend_ROC,"leg_pr"=legend_pr))
}

```

> ### Getting and Drawing the results of AUC

```{r AUCfuncApply, echo=TRUE, message=FALSE,  cache=TRUE}

set.seed(1226)
AUC_rlr <- AUC(model= fit_01regLogistic, ggtitle= ggtitle("RLR"),
               method = "_pred_fit_01regLogistic",
               un= c(0.2,0.2,0.0,0))
AUC_glm <- AUC(model= fit_02glmnet,ggtitle= ggtitle("GLM"),
               method = "_pred_fit_02glmnet",
               un= c(0.2,0.2,0.0,0))
AUC_lda <- AUC(model= fit_03lda, ggtitle= ggtitle("LDA"),
               method = "_pred_fit_03lda",
               un= c(0.2,0.2,0.0,0))
AUC_sda <- AUC(model= fit_04sda, ggtitle= ggtitle("SDA"),
               method= "_pred_fit_04sda",
               un= c(0.2,0.2,0.0,0))
AUC_mlp <- AUC(model= fit_05mlp, ggtitle= ggtitle("MLP"),
                method= "_pred_fit_05mlp",
               un= c(0.0,0.2,0.0,0))
AUC_mlpD <- AUC(model= fit_06mlpWD, ggtitle= ggtitle("MLP-WD"),
              method = "_pred_fit_06mlpWD",
               un= c(0.0,0.2,0.0,0))
AUC_mlpWD <- AUC(model= fit_07mlpKerasDropout,ggtitle= ggtitle("MLP-D"),
               method = "_pred_fit_07mlpKerasDropout",
               un= c(0.0,0.2,0.0,0))
AUC_svml <- AUC(model= fit_08svml,ggtitle= ggtitle("SVM-L"),
                method= "_pred_fit_08svml",
               un= c(0.0,0.2,0.0,0))
AUC_svmp <- AUC(model= fit_09svmp,ggtitle= ggtitle("SVM-P"),
              method = "_pred_fit_09svmp",
               un= c(0.0,0.2,0.0,0))
AUC_svmR <- AUC(model= fit_10svmr,ggtitle= ggtitle("SVM-R"),
               method = "_pred_fit_10svmr",
               un= c(0.0,0.2,0.0,0),
               x.text = element_text())
AUC_CART <- AUC(model= fit_11cart,ggtitle= ggtitle("CART"),
               method = "_pred_fit_11cart",
               un= c(0.0,0.2,0.0,0),
               x.text = element_text())
AUC_C5 <- AUC(model= fit_12C5,ggtitle= ggtitle("C5.0"),
                method= "_pred_fit_12C5",
               un= c(0.0,0.2,0.0,0),
               x.text = element_text())
AUC_RF <- AUC(model= fit_13rf,ggtitle= ggtitle("RF"),
              method = "_pred_fit_13rf",
               un= c(0.0,0.2,0.0,0),
               x.text = element_text())
AUC_xgb <- AUC(model= fit_14xgb,ggtitle= ggtitle("XGBoost"),
               method = "_pred_fit_14xgb",
               un= c(0.0,0.2,0.0,0),
               x.text = element_text())

```

> ### ROC Plot

```{r AUCplots, echo=TRUE, message=FALSE, warning=FALSE,cache=TRUE}
plt_layout_matrix = rbind(c(1:4),c(5:8),c(9:12),c(13,14,15,15))
auc_roc_all<-arrangeGrob(
  grobs= list(AUC_rlr$ss,AUC_glm$ss,AUC_lda$ss,AUC_sda$ss,AUC_mlp$ss,
              AUC_mlpD$ss,AUC_mlpWD$ss,AUC_svml$ss,AUC_svmp$ss,AUC_svmR$ss,
              AUC_CART$ss,AUC_C5$ss,AUC_RF$ss,AUC_xgb$ss,AUC_rlr$leg_ss),
  nrow =4,ncol = 4,layout_matrix = plt_layout_matrix)

auc_roc_all<- annotate_figure(
  p = auc_roc_all,
  left = text_grob("Sensitivity",face = "plain",
                   rot = 90, size = 12,vjust = 1.0),
  bottom = text_grob("1-Specificity", face = "plain",
                     size = 12,vjust = 0.05))

auc_roc_all<- auc_roc_all +
  geom_segment(aes(x = .030, y=0.02, xend=0.030, yend = .98), size =.6,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = .030, y=0.02, xend=0.98, yend = .02), size =.6,
               arrow = arrow(length = unit(0.3, "cm")))

auc_roc_all
ggsave(filename = "results/plt_AUCROC_all4.jpeg",plot = last_plot(),
device = "jpeg",width = 1109/96,height = 1482/96,dpi = 600)
```

> ### PR Plot

```{r PRplots, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE}
PR_roc_all<-arrangeGrob(
  grobs= list(AUC_rlr$pr,AUC_glm$pr,AUC_lda$pr,AUC_sda$pr,AUC_mlp$pr,
              AUC_mlpD$pr,AUC_mlpWD$pr,AUC_svml$pr,AUC_svmp$pr,AUC_svmR$pr,
              AUC_CART$pr,AUC_C5$pr,AUC_RF$pr,AUC_xgb$pr,AUC_rlr$leg_ss),
  nrow =4,ncol = 4,layout_matrix = plt_layout_matrix)

PR_roc_all<- annotate_figure(
  p = PR_roc_all,
  left = text_grob("Precision",face = "plain",
                   rot = 90, size = 12,vjust = 1.0),
  bottom = text_grob("Recall", face = "plain",
                     size = 12,vjust = 0.05))

PR_roc_all<- PR_roc_all +
  geom_segment(aes(x = .030, y=0.02, xend=0.030, yend = .98), size =.6,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = .030, y=0.02, xend=0.98, yend = .02), size =.6,
               arrow = arrow(length = unit(0.3, "cm")))

PR_roc_all
ggsave(filename = "results/plt_AUC_PR.jpeg",plot = last_plot(),
       device = "jpeg",width = 1109/96,height = 1482/96,dpi = 600)
```

> ### AUC scores

```{r auc_scores, echo=TRUE, message=FALSE, warning=FALSE, cache=TRUE }

AUC_data_all<-t(
  data.frame("RLR_ROC"=unlist(AUC_rlr$ROC$AUC$fit_01regLogistic),
             "GLM_ROC"=unlist(AUC_glm$ROC$AUC$fit_02glmnet),
             "LDA_ROC"=unlist(AUC_lda$ROC$AUC$fit_03lda),
             "SDA_ROC"=unlist(AUC_sda$ROC$AUC$fit_04sda),
             "MLP_ROC"=unlist(AUC_mlp$ROC$AUC$fit_05mlp),
             "MLP-WD_ROC"=unlist(AUC_mlpWD$ROC$AUC$fit_07mlpKerasDropout),
             "MLP-D_ROC"=unlist(AUC_mlpD$ROC$AUC$fit_06mlpWD),
             "SVM-L_ROC"=unlist(AUC_svml$ROC$AUC$fit_08svml),
             "SVM-P_ROC"=unlist(AUC_svmp$ROC$AUC$fit_09svmp),
             "SVM-R_ROC"=unlist(AUC_svmR$ROC$AUC$fit_10svmr),
             "CART_ROC"=unlist(AUC_CART$ROC$AUC$fit_11cart),
             "C5_ROC"=unlist(AUC_C5$ROC$AUC$fit_12C5),
             "RF_ROC"=unlist(AUC_RF$ROC$AUC$fit_13rf),
             "XGB_ROC"=unlist(AUC_xgb$ROC$AUC$fit_14xgb),
             "RLR_PR"=unlist(AUC_rlr$PR$AUC$fit_01regLogistic),
             "GLM_PR"=unlist(AUC_glm$PR$AUC$fit_02glmnet),
             "LDA_PR"=unlist(AUC_lda$PR$AUC$fit_03lda),
             "SDA_PR"=unlist(AUC_sda$PR$AUC$fit_04sda),
             "MLP_PR"=unlist(AUC_mlp$PR$AUC$fit_05mlp),
             "MLP-WD_PR"=unlist(AUC_mlpWD$PR$AUC$fit_07mlpKerasDropout),
             "MLP-D_PR"=unlist(AUC_mlpD$PR$AUC$fit_06mlpWD),
             "SVM-L_PR"=unlist(AUC_svml$PR$AUC$fit_08svml),
             "SVM-P_PR"=unlist(AUC_svmp$PR$AUC$fit_09svmp),
             "SVM-R_PR"=unlist(AUC_svmR$PR$AUC$fit_10svmr),
             "CART_PR"=unlist(AUC_CART$PR$AUC$fit_11cart),
             "C5_PR"=unlist(AUC_C5$PR$AUC$fit_12C5),
             "RF_PR"=unlist(AUC_RF$PR$AUC$fit_13rf),
             "XGB_PR"=unlist(AUC_xgb$PR$AUC$fit_14xgb)))


knitr::kable(AUC_data_all,escape = F,digits = 4,format = "simple")
write.csv(AUC_data_all,"results/AUC_Data_all.csv")

```
