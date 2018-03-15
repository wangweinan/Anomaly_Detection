packages <- c("KernSmooth","ggplot2","hdrcde","np","ks","parallel","pbmcapply","reshape2","tidyverse","grid","gridExtra")
lapply(packages, require, character.only = TRUE)
###Middle point specifications
t <- seq(600,5000,400);
iter <- 500;
###Setting 1: fixed p=0.05, vary mu from 2 to 4. Investigate different time stamps FDR level.
m <- 5000;
p <- rep(0.05,m);
mu <- seq(2,4,length.out=5);
grid <- expand.grid(mu,1:iter);
###Setting 2: fixed p=0.01, vary mu from 2 to 4. Investigate different time stamps FDR level.
m <- 5000;
p <- rep(0.05,m);
mu <- seq(2,4,length.out=5);
grid <- expand.grid(mu,1:iter);
###Setting 3: varying p, linear ranging from 0 to 0.5. Investigate different time stamps FDR level.
m <- 5000;
p <- seq(0,0.5,length.out=m);
mu <- seq(2,4,length.out=5);
grid <- expand.grid(mu,1:iter);
###Setting 4: varying p, sin shape varying from 0 to 0.5. Investigate different time stamps FDR level.
m <- 5000;
p_pre <- seq(1,m,1);
p <- (sin(p_pre/m*2*pi)+1)/4;
mu <- seq(2,4,length.out=5);
grid <- expand.grid(mu,1:iter);
###Parallel computing results with progress bars
res_Oracle_Gamma <- pbmcmapply(Oracle_Gamma_Vary,mu=grid[,1],iter=grid[,2],MoreArgs=list(m=m,p=p,t=t),mc.cores=12);
res_LOND <- pbmcmapply(LOND,mu=grid[,1],iter=grid[,2],MoreArgs=list(m=m,p=p,t=t),mc.cores=12);
res_DD <- pbmcmapply(DD_Vary,mu=grid[,1],iter=grid[,2],MoreArgs=list(m=m,p=p,t=t),mc.cores=12);
#res_Oracle_SunCai <- pbmcmapply(Oracle_SunCai,mu=grid[,1],iter=grid[,2],MoreArgs=list(m=m,p=p),mc.cores=12);

###Transdorm results into tibble data frames
res_Oracle_Gamma <- as_tibble(t(res_Oracle_Gamma));
res_LOND <- as_tibble(t(res_LOND));
res_DD <- as_tibble(t(res_DD));
###Unnest all list columns
res_Oracle_Gamma <- res_Oracle_Gamma %>% unnest(mu,iter) %>% unnest(FDP,MDP,t);
res_LOND <- unnest(res_LOND,mu,iter)%>% unnest(mu,iter) %>% unnest(FDP,MDP,t);
res_DD <- unnest(res_DD,mu,iter)%>% unnest(mu,iter) %>% unnest(FDP,MDP,t);
###Group by mu and t
clean_OG <- res_Oracle_Gamma %>% group_by(mu,t) %>% summarise(OG_FDR=mean(FDP),OG_MDR=mean(MDP));
clean_LOND <- res_LOND %>% group_by(mu,t) %>% summarise(LOND_FDR=mean(FDP),LOND_MDR=mean(MDP));
clean_DD <- res_DD %>% group_by(mu,t) %>% summarise(DD_FDR=mean(FDP),DD_MDR=mean(MDP));
###Join tibbles and generate data frames for FDR and MDR respectively
final_res <- clean_OG %>% full_join(clean_LOND,by=c("mu","t")) %>% full_join(clean_DD,by=c("mu","t"));
final_res <- final_res %>% group_by(mu,t);
FDR_res <- select(final_res,contains("FDR"));
MDR_res <- select(final_res,contains("MDR"));
###Plotting
res_FDR_long <- melt(FDR_res,id=c("mu","t"));
p_FDR <- ggplot(data=res_FDR_long,aes(x=t,y=value,colour=variable,shape=variable))+geom_point(size=3)+geom_line()+facet_wrap(~mu,ncol=1,labeller=label_both)+ylim(0,0.1)+xlab(expression(t))+ylab("FDR")+scale_shape_discrete(name="Method",labels=c("Oracle_Gamma","LOND","DD"))+scale_colour_discrete(name="Method",labels=c("Oracle_Gamma","LOND","DD"))+theme(legend.position="bottom")+ggtitle("Intermediate FDR level Comparison")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"))+theme(plot.title=element_text(hjust=0.5))

res_MDR_long <- melt(MDR_res,id=c("mu","t"));
p_MDR <- ggplot(data=res_MDR_long,aes(x=t,y=value,colour=variable,shape=variable))+geom_point(size=3)+geom_line()+facet_wrap(~mu,ncol=1,labeller=label_both)+ylim(0,1)+xlab(expression(t))+ylab("MDR")+scale_shape_discrete(name="Method",labels=c("Oracle_Gamma","LOND","DD"))+scale_colour_discrete(name="Method",labels=c("Oracle_Gamma","LOND","DD"))+theme(legend.position="none")+ggtitle("Intermediate MDR level Comparison")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"))+theme(plot.title=element_text(hjust=0.5))

mylegend <- g_legend(p_FDR);
grid.arrange(arrangeGrob(p_FDR + theme(legend.position="none"),
                         p_MDR + theme(legend.position="none"),
                         ncol=2),
             mylegend, nrow=2,heights=c(10, 1), top = "Methods Comparison (vary p from 0 to 0.5 (sin-shape))")




