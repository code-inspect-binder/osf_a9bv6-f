### regression analyses and correlations for study 3

load('regression_data.RData') # load the compiled data 

library(strengejacke) # for marginal plots and tab_model 

### correlations 
cor.test(dougie$r2t10, dougie$LDT.1200ms.Z)
cor.test(dougie$r4t10, dougie$LDT.1200ms.Z)
cor.test(dougie$r6t10, dougie$LDT.1200ms.Z)
cor.test(dougie$r8t10, dougie$LDT.1200ms.Z)

### regression models 
m1<-lm(LDT.1200ms.Z ~ Prime.Length + Prime.LogSubFreq + Prime.OrthoN +
         Target.Length + Target.LogSubFreq + Target.OrthoN +
         FAS + BAS + CueFanOut + TargetFanIn + LSA + 
         as.factor(condition), data = dougie)
m2<-lm(r2t10 ~ Prime.Length + Prime.LogSubFreq + Prime.OrthoN +
         Target.Length + Target.LogSubFreq + Target.OrthoN +
         FAS + BAS + CueFanOut + TargetFanIn + LSA + 
         as.factor(condition), data = dougie)
m3<-lm(r4t10 ~ Prime.Length + Prime.LogSubFreq + Prime.OrthoN +
         Target.Length + Target.LogSubFreq + Target.OrthoN +
         FAS + BAS + CueFanOut + TargetFanIn + LSA + 
         as.factor(condition), data = dougie)
m4<-lm(r6t10 ~ Prime.Length + Prime.LogSubFreq + Prime.OrthoN +
         Target.Length + Target.LogSubFreq + Target.OrthoN +
         FAS + BAS + CueFanOut + TargetFanIn + LSA + 
         as.factor(condition), data = dougie)
m5<-lm(r8t10 ~ Prime.Length + Prime.LogSubFreq + Prime.OrthoN +
         Target.Length + Target.LogSubFreq + Target.OrthoN +
         FAS + BAS + CueFanOut + TargetFanIn + LSA + 
         as.factor(condition), data = dougie)

tab_model(m1,m2,m3,m4,m5, show.est=F,show.std=T, show.se=T,show.ci = F,show.stat=T)

# marginal effect plots 
plot_model(m1, type = 'pred', terms = 'condition', title = '(a) RT model',
           axis.title = 'z-scored mean item RT')
plot_model(m2, type = 'pred', terms = 'condition', title = '(b) Retention = 0.2',
           axis.title = 'activation')
plot_model(m3, type = 'pred', terms = 'condition', title = '(c) Retention = 0.4',
           axis.title = 'activation')
plot_model(m4, type = 'pred', terms = 'condition', title = '(d) Retention = 0.6',
           axis.title = 'activation')
plot_model(m5, type = 'pred', terms = 'condition', title = '(e) Retention = 0.8',
           axis.title = 'activation')

