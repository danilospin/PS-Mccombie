library(ggplot2)
#y_n 
#y_b

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


#y_n = l + lambda + v*y
#y_b = (epsilon/pi)*z

edot=phi*(y_n/y_b)

l=0.01
lambda=0.02
v=0.3
epsilon=0.8
pi=0.8
z=0.02
y=0.01
#y_n 
  l + lambda + v*y
#y_b
  (epsilon/pi)*z
E=0:100
E=E/100
k<-c("l","lambda", "v", "epsilon", "pi", "z", "y")
d<-c(l,lambda,v,epsilon,pi,z,y )

parameter_table <- cbind(k,d)
parameter_table=as.table(parameter_table)

#Palley
#pi=alfa*E
alfa=0.8
y_b_p = (epsilon/(alfa*E))*z
#y=y_b
y_n_p = l + lambda + v*(epsilon/(alfa*E))*z
par(mfrow=c(1,2), mar=c(3.1, 2.9, 2.0, 1), mgp=c(2, 1, 0), las=0, cex.lab=0.7, cex.axis=0.7, cex.main=0.7, cex.sub=0.7)
plot( E, y_b_p, type="l", main="Palley demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_p, type="l", main="Palley supply rate", xlab="E", ylab="yn",col="green" )
require(reshape2)

#df <- data.frame(E, y_b_p,y_n_p)
# Basic line plot with points
#ggplot(data=df, aes(x=E, group=1)) +
#  geom_line(aes(y = y_b_p), color = "darkred") + 
#  geom_line(aes(y = y_n_p), color="steelblue", linetype="twodash") 




#Setterfield
#v=beta*E
beta=0.3
y_n_s = l + lambda + beta*E*(epsilon/pi)*z
y_b_s = (epsilon/pi)*z*E^0
par(mfrow=c(1,2), mar=c(3.1, 2.9, 2.0, 1), mgp=c(2, 1, 0), las=0, cex.lab=0.7, cex.axis=0.7, cex.main=0.7, cex.sub=0.7)
plot( E, y_b_s, type="l", main="Setterfield demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_s, type="l", main="Setterfield supply rate", xlab="E", ylab="yn",col="green" )


#Palley-Setterfield
y_b_ps = (epsilon/(alfa*E))*z
y_n_ps = l + lambda + beta*E*(epsilon/pi)*z
plot( E, y_b_ps, type="l", main="P-S demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_ps, type="l", main="P-S supply rate", xlab="E", ylab="yn",col="green" )

#McCombie Short-run
#l=phi*E
#lambda=psi*E
phi=0.01
psi=0.02
y_b_m_sr = (epsilon/pi)*z*(E/E)
y_n_m_sr = phi*E + psi*E + v*y
plot( E, y_b_m_sr, type="l", main="McCombie SR demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_m_sr, type="l", main="McCombie SR supply rate", xlab="E", ylab="yn",col="green" )



#COMPLETE MODEL
#Merging McCombie and Palley-Setterfield
#l=phi*E
#lambda=psi*E
y_b_c = (epsilon/(alfa*E))*z
y_n_c = phi*E + psi*E + beta*E*(epsilon/pi)*z
par(mfrow=c(1,2))
plot( E, y_b_c, type="l", main="Complete Model demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_c, type="l", main="Complete Model supply rate", xlab="E", ylab="yn",col="green" )


#COMPLETE MODEL Long-Run (Equilibrium)
#curva vertical na oferta - emprego de equilibrio 
par(mfrow=c(1,3), mar=c(3.8, 2, 2.8, 1), mgp=c(2, 1, 0), las=0, cex.lab=0.7, cex.axis=0.7, cex.main=0.7, cex.sub=0.7)

y_b_c_lr = (epsilon/(alfa*E))*z
K=1
Ebar=K
y_n_c_lr = Ebar*(E/E)
plot( E, y_b_c_lr, type="l", main="Demand Rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_c, type="l", main="Supply SR Rate", xlab="E", ylab="yn",col="green" )
plot( E, y_n_c_lr, type="l", main="Supply LR Rate", xlab="E", ylab="yn",col="black" )


#COMPLETE MODEL


#Setterfield Critique
#Labor Constrained
q=lambda+v*y
l=gamma+delta*y
gamma=0.01
delta=0.5
delta+v
y_pl=q+l
#Capital Constrained
y_pc=E*y




#Save in PDF File

pdf("Resultados_Palley_Setterfield_McCombie.pdf") 
# 2. Create a plot

p<-tableGrob(parameter_table)
grid.arrange(p)

par(mfrow=c(1,2), mar=c(3.1, 2.9, 2.5, 1), mgp=c(2, 1, 0), las=0, cex.lab=0.7, cex.axis=0.7, cex.main=0.7, cex.sub=0.7)
plot( E, y_b_p, type="l", main="Palley demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_p, type="l", main="Palley supply rate", xlab="E", ylab="yn",col="green" )

plot( E, y_b_s, type="l", main="Setterfield demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_s, type="l", main="Setterfield supply rate", xlab="E", ylab="yn",col="green" )

plot( E, y_b_ps, type="l", main="P-S demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_ps, type="l", main="P-S supply rate", xlab="E", ylab="yn",col="green" )

plot( E, y_b_m, type="l", main="McCombie demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_m, type="l", main="McCombie supply rate", xlab="E", ylab="yn",col="green" )

plot( E, y_b_c, type="l", main="Complete Model demand rate", xlab="E", ylab="yb",col="red" )
plot( E, y_n_c, type="l", main="Complete Model supply rate", xlab="E", ylab="yn",col="green" )
# Close the pdf file
dev.off() 


Ebar=0.8
omega=0.1
psmod <- function (t, y, parms) {
  with(as.list(y), {
    
    #Differential Equations
    dX=omega*(Ebar-X)

    list(c(dX)) })
}

#Initial Conditions
yini <- c(X = 0.6)

#Computing the dynamic equation (ODE)
times <- seq(from = 0, to = 100, by = 0.1)
out <- ode(y = yini, times = times, func = psmod,
           parms = NULL)
par(mar=c(2, 2, 2, 2))
par(mfrow=c(2, 2))

Evar=out[,2]

B=(epsilon/(alfa*Evar))*z
N=phi*Evar + psi*Evar + beta*Evar*(epsilon/pi)*z

plot(Evar, lwd = 2, type='l', main="Capacity Utilization", xlab="time", ylab="h",col="red")
plot(B, lwd = 2, type='l', main="Demand Rate", xlab="time", ylab="u",col="green")
plot(N, lwd = 2, type='l', main="Natural rate", xlab="time", ylab="e", ,col="blue")
