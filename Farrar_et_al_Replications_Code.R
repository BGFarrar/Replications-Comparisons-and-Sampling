####################################################################
####################################################################
## Paper:   Replications, Comparisons, Sampling and the Problem   ##
##          of Representativeness in Animal Cognition Research    ##
##                                                                ##
## Authors: Farrar, Voudouris and Clayton                         ##
##                                                                ##
## Code Author: BF                                                ##  
##                                                                ##
## Version: Revised Manuscript                                    ##
##                                                                ##
## Software: R 4.0.2                                              ##
##                                                                ##
## Purpose: Code to produce the replication/comparison            ##
##          simulation results and Figures 4-8                    ##
####################################################################
####################################################################

rm(list = ls())

##################################################
## Load packages, install packages if necessary ##
##################################################

package.list <- c("tidyverse", "extrafont", "scales")
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(package.list, library, character.only = TRUE)

## NB, if this is your first time running extraFont, the following  line is required to 
## load the fonts (run time ~ 3 mins). You might need to restart R following running the code, too. 

#font_import()
#loadfonts()

textsize <- 20 ## Set size of axis labels

######################
## Output Directory ##
######################

#figures <- "~/PhD/aaThesis/2b_Replications, Comparisons and Sampling/Revision/Figures"

################
## Simulation ##
################

################################
## Within-species replication ##
################################

## set seed for reproducibility

set.seed(2021)

##################################################
## Set popoulation paramters for each behaviour ##
##################################################

# set parameters for behavior 1
b01 <- 800 # intercept; i.e., the grand mean
S0s_sd1 <- 100 # by-subject random intercept sd
L0l_sd1 <- 100 # by-location random intercept sd
err_sd1 <- 50 # residual error

# set parameters for behaviour2
b02 <- 80 # intercept; i.e., the grand mean
S0s_sd2 <- 10 # by-subject random intercept sd
L0l_sd2 <- 10 # by-location random intercept sd
err_sd2 <- 5 # residual error

#################################
## simulate sites and subjects ##
#################################

nsites <- 100
nsubj <- 100

subjectID <- as.factor(1:(nsubj*nsites))
siteID <- as.factor(rep(seq(1:nsites), each=nsubj)) 
subjeff1 <- rnorm(nsites*nsubj, 0, S0s_sd1)
locationeff1 <- rep(rnorm(nsites, 0, L0l_sd1), each=nsubj)
error1 <- rnorm(nsites*nsubj, 0, err_sd1)
subjeff2 <- rnorm(nsites*nsubj, 0, S0s_sd2)
locationeff2 <- rep(rnorm(nsites, 0, L0l_sd2), each=nsubj)
error2 <- rnorm(nsites*nsubj, 0, err_sd2)

data <- data.frame(subjectID, siteID, subjeff1, locationeff1, b01, error1, subjeff2, locationeff2, b02, error2)
data$behavior1 <- data$subjeff1 + data$locationeff1 + data$b01 + data$error1
data$behavior2 <- data$subjeff2 + data$locationeff2 + data$b02 + data$error2

## Figure 3A: Highlight all animals at single site
s <- round(runif(1, 1, nsites))
singlesite <- subset(data, siteID == s)

(Fig3a <- ggplot(data, aes(x=behavior1, y=behavior2)) + geom_point(alpha=0.1, colour="grey") +
  geom_point(data = singlesite, aes(x=behavior1, y=behavior2), color="#481568FF") + 
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=textsize)
  ))

#ggsave("Fig3a.png", Fig3a, path = figures)

## Sample 10 individuals from the site

singlesitesample <- sample_n(singlesite, 10, FALSE)

## Figure 3B: highlight 10 individuals from this site

(Fig3b <- ggplot(data, aes(x=behavior1, y=behavior2, colour=4)) + geom_point(alpha=0.1, colour="grey") + theme_minimal() +
  geom_point(data = singlesitesample, aes(x=behavior1, y=behavior2), color="#481568FF", size=3) + 
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=textsize)
  ))

#ggsave("Fig3b.png", Fig3b, path = figures)

## Perform "replication study". Sample another 10 individuals from another site.

s2 <- round(runif(1, 1, nsites))
secondsite <- subset(data, siteID == s2)
secondsitesample <- sample_n(secondsite, 10, FALSE)

## Figure 4: Replication study

(Fig4 <- ggplot(data, aes(x=behavior1, y=behavior2, colour=4)) + geom_point(alpha=0.1, colour="grey") + theme_minimal() +
  geom_point(data = singlesitesample, aes(x=behavior1, y=behavior2), color="#481568FF", size= 3) +  
  geom_point(data = secondsitesample, aes(x=behavior1, y=behavior2), color="chocolate1", size = 3) + 
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=textsize)
  ))

#ggsave("Fig4.png", Fig4, path = figures)

## Compare behavior at both sites

t.test(singlesitesample$behavior1, secondsitesample$behavior1, alternative = "two.sided")
t.test(singlesitesample$behavior2, secondsitesample$behavior2, alternative = "two.sided")


## Compare observed effects relative to original paramters

## Population difference in behaivor 1
(secondsitesample$locationeff1 - singlesitesample$locationeff1) / b01

## Sample difference in behavior 1
(mean(secondsitesample$behavior1) - mean(singlesitesample$behavior1)) / b01

## Population difference in behaivor 2
(secondsitesample$locationeff2 - singlesitesample$locationeff2) / b02

## Sample difference in behavior 2
(mean(secondsitesample$behavior2) - mean(singlesitesample$behavior2)) / b02


## Compare oberserved effects to overall population mean

## site 1 neophobia
(mean(singlesitesample$behavior1) - b01) / b01

## site 1 self-control
(mean(singlesitesample$behavior2) - b02) / b02

## site 2 neophobia
(mean(secondsitesample$behavior1) - b01) / b01

## site 2 self-control
(mean(secondsitesample$behavior2) - b02) / b02

#################################
## Between-species replication ##
#################################

## Label previous data as chimpanzee data
data$Species <- "Chimpanzees"

## Simulate second dataset for bonobos

# behavior 1: 0.5 sd larger than the chimpanzees'
b012 <- b01 + 1.5*L0l_sd1 # intercept; i.e., the grand mean

# behavior 2: the same as the chimpanzees'
b022 <- 1*b02

## simulate new data
subjeff21 <- rnorm(nsites*nsubj, 0, S0s_sd1)
locationeff21 <- rep(rnorm(nsites, 0, L0l_sd1), each=nsubj)
error21 <- rnorm(nsites*nsubj, 0, err_sd1)
subjeff22 <- rnorm(nsites*nsubj, 0, S0s_sd2)
locationeff22 <- rep(rnorm(nsites, 0, L0l_sd2), each=nsubj)
error22 <- rnorm(nsites*nsubj, 0, err_sd2)

data2 <- data.frame(subjectID, siteID, subjeff21, locationeff21, b012, error21, subjeff22, locationeff22, b022, error22)
data2$behavior1 <- data2$subjeff21 + data2$locationeff21 + data2$b012 + data2$error21
data2$behavior2 <- data2$subjeff22 + data2$locationeff22 + data2$b022 + data2$error22
data2$Species <- "Bonobos"

## combine chimpanzee and bonobo dataframes

compdata <- rbind(data[,c(1, 2, 11, 12, 13)], data2[,c(1, 2, 11, 12, 13)])

## Generate data for 10 individuals from a single chimpanzee site 

sp1 <- round(runif(1, 1, nsites))
sp1singlesite <- subset(compdata, Species == "Chimpanzees" & siteID == sp1)
sp1singlesitesample <- sample_n(sp1singlesite, 10, FALSE)


## Generate data for 10 individuals from a single bonobo site

sp2 <- round(runif(1, 1, nsites))
sp2secondsite <- subset(compdata, Species == "Bonobos" & siteID == sp2)
sp2secondsitesample <- sample_n(sp2secondsite, 10, FALSE)

## Figure 5: Plot chimpanzee and bonobo comparison

(Fig5 <- ggplot(compdata, aes(x=behavior1, y=behavior2, group=Species, color=Species)) + geom_point(alpha=0.1) +
  scale_color_manual(values = c("grey", "lightblue")) +
  geom_point(data = sp1singlesitesample, aes(x=behavior1, y=behavior2), color="blue", size = 3, shape = 16) + 
  geom_point(data = sp2secondsitesample, aes(x=behavior1, y=behavior2), color="grey30", size = 3, shape = 16) + 
  theme_minimal()  + 
  xlab("Neophobia") + ylab("Self-control") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    text=element_text(family="Times New Roman", size=textsize)
  ))

#ggsave("Fig5.png", Fig5, path = figures)

## signifance test of neophobia difference
t.test(sp2secondsitesample$behavior1, sp1singlesitesample$behavior1, alternative = "two.sided")

## magnitude of neophobia difference (% over/under estimation of true difference)
((mean(sp2secondsitesample$behavior1) - mean(sp1singlesitesample$behavior1)) - (b012 - b01)) / (b012 - b01)

## signifance test of self-control difference
t.test(sp2secondsitesample$behavior2, sp1singlesitesample$behavior2, alternative = "two.sided")

## magnitude of self-control difference (% that bonbos we estimated as higher than chimps, 
## even though no overall difference was simulated)
(mean(sp2secondsitesample$behavior2) - mean(sp1singlesitesample$behavior2)) / mean(sp1singlesitesample$behavior2)



### Simulation Study

### set up new data with new params if required, larger sample for power calculations


set.seed(64)
# behavior 1
b01 <- 800 # intercept; i.e., the grand mean
S0s_sd1 <- 100 # by-subject random intercept sd
L0l_sd1 <- 100 # by-location random intercept sd
err_sd1 <- 50 # residual error

# behaviour2
b02 <- 80 # intercept; i.e., the grand mean
S0s_sd2 <- 10 # by-subject random intercept sd
L0l_sd2 <- 10 # by-location random intercept sd
err_sd2 <- 5 # residual error

## bonobo behavior 1
bb01 <- b01 + 1.5*L0l_sd1

## bonobo behavior 2

bb02 <- b02 + 0* L0l_sd2

## Generate sites and subjects, perform t-test comparing randomly sampled site of chimpanzees with randomly sampleed 
## site of bonobos

subj <- 10

pb1 <- NULL
pb2 <- NULL
overestimate <- NULL
difference <- NULL
stat1 <- NULL
stat2 <- NULL
nSims <- 100000

for(i in 1:nSims){
  chimpbehavior1 <- rnorm(subj, 0, S0s_sd1) + rnorm(1, 0, L0l_sd1) + b01 + rnorm(10, 0, err_sd1) 
  chimpbehavior2 <- rnorm(subj, 0, S0s_sd2) + rnorm(1, 0, L0l_sd2) + b02 + rnorm(10, 0, err_sd2) 
  bonbehavior1 <- rnorm(subj, 0, S0s_sd1) + rnorm(1, 0, L0l_sd1) + bb01 + rnorm(10, 0, err_sd1) 
  bonbehavior2 <- rnorm(subj, 0, S0s_sd2) + rnorm(1, 0, L0l_sd2) + bb02 + rnorm(10, 0, err_sd2)
  pb1[i] <- t.test(chimpbehavior1, bonbehavior1, alternative = "two.sided")$p.value
  overestimate[i] <- (((mean(bonbehavior1) - mean(chimpbehavior1)) - (b012 - b01)) / (b012 - b01))
  pb2[i] <- t.test(chimpbehavior2, bonbehavior2, alternative = "two.sided")$p.value
  difference[i] <- ((mean(bonbehavior2) - mean(chimpbehavior2)) / mean(sp1singlesitesample$behavior2))
}

hist(pb1, breaks = 20)
sum(pb1<0.05)/nSims

hist(pb2, breaks = 20)
sum(pb2<0.05)/nSims


stat11 <- data.frame("p" = pb1, "comp" = "Neophobia")
stat12 <- data.frame("p" = pb2, "comp" = "Self-Control")

stat <- rbind(stat11, stat12)

my_x_title <- expression(paste(italic("p"), "-value"))

## Figure 6a: p-value distributions and effect size

(Fig6a <-ggplot(stat, aes(x=p)) + geom_density(alpha=0.2) + facet_grid(.~comp) + xlim(0, 1) + 
  geom_vline(xintercept = 0.05, linetype="dotted", 
             color = "blue", size=1) +
  theme_classic()  + 
  xlab(my_x_title) + 
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      text=element_text(family="Times New Roman", size=textsize),
      panel.grid.major.x = element_line(), 
      strip.background = element_blank()
      ))

#ggsave("Fig6a.png", Fig6a, path = figures)

efsize <- data.frame(overestimate, difference)

(Fig6b <- ggplot(efsize, aes(x = overestimate)) + geom_density() + scale_x_continuous(limits = c(-3.5, 3.5), breaks = c(-3, -2, -1, 0, 1, 2, 3), labels = percent) +
    theme_classic() +
    xlab("\n Effect Size Overestimation  \n Neophobia") + 
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      text=element_text(family="Times New Roman", size=textsize),
      panel.grid.major.x = element_line()
    ))

#ggsave("Fig6b.png", Fig6b, path = figures)

## Freq ES over/under estimated by more than 100%
sum(abs(overestimate) > 1) / length(overestimate)
