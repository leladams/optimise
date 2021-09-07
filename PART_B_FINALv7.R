########################################
#
#    Cost Functions
#
########################################

# returns LEVCOST
LEVEECOST <- function (DQ) {
  # C Estimate cost of levee cosntruction and return the annualized cost.
  # C This levee cost function assumes a 16 ft. levee crest width, 2:1 riverine 
  # C side-slopes, 4:1 landward side-slopes, and 2-ft. freeboards.
  # C DQ = levee design flood flow (cfs)
  # C SL = levee height, including freeboard (ft)
  # C LEVCOST = initial construction cost of levee
  # C L = total levee length in yards.
  # C R = real (inflation-adjusted) discount rate
  #REAL LEVCOST, LEVCOST0
  
  if (DQ <= 60000) {
    LEVCOST = 0
    return (LEVCOST)
  }
  
  L = 2000.0
  R = 0.05
  # C C0 = cost of construction per cubic yard of levee material.
  C0 = 10.0
  
  # C Stage-discharge relationship for above-bank flows (60,000 cfs).
  SL = (0.005*(DQ-60000.)**0.7)
  
  # C Convert levee height in feet to yards.
  # C Material volumes are in cubic yards.
  # C Levee freeboard is 2 ft.
  SL = (2 + SL)/3.0
  V = 16/3*SL + 3*((SL)**2.0)
  LEVCOST0 = C0*L*V
  # C Annualize levee cost, for a very long levee lifetime.
  LEVCOST = R*LEVCOST0
  return (LEVCOST)
}

#vectorize
LeveeCost <- Vectorize(LEVEECOST)

# return DAMAG, damages caused by a particular q
DAMAGE <- function (Q) {
#DAMAGE <- function (Q, DQ) {
  # C      need to include a stage-discharge relationship.
  # C Stage-discharge relationship above 60,000 cfs/3-ft stage is in line #10.
  # C DQ = levee design flood flow (cfs)
  # C SL = elevation of levee design height above bank (ft)
  # C Q = peak flood flow (cfs)
  # C DAMAG = damage caused by the flood
  # C SF = stage of flood above bank
  
  #if (Q > DQ) {
    SF = 0.005*(Q-mu_mu)**0.7
    #SL = 0.005*(DQ-mu)**0.7
    
    # C add damage function here!!!!!!
    # C Here's a hoakie damage function, $1 million per foot over bank, 
    # C provided the levee height is below the river stage.
    DAMAG = 1000000.0*SF
  #} else {
  #  DAMAG = 0.0
  #}
  
  return(DAMAG)
}

# Must if you want to call curve()
Damage <- Vectorize(DAMAGE, "Q")

########################################
#
#    Probability Distributions
#
########################################

#Cumulative distribution function
#F = function(Q) {exp(-exp(-a * (Q-b)))}

#Running ProbFQ as a function of functions

#writing a function for the Gaussian Distribution of the mean
#average of mean
mu_mu=60000
#std of mean
mu_sd=21000/sqrt(40)
#function that takes the integral of standard deviations
GausFxmu=function(mu){dnorm(mu, mu_mu, mu_sd)}

#seq of possible mean to see how GausFxmu is plotting
#means=seq(0, 10^6, 1000)
#plot(means, GausFxmu(means))

#writing a function for the sd
#average of standard deviation
sd_mu=21000
#std of st
sd_sd=7000
#function that takes the integral of standard deviations
GausFxsd=function(std){dnorm(std, sd_mu, sd_sd)}

#sds=seq(0, sd_mu+3*sd_mu, 1000)
#plot(sds, GausFxsd(sds))

#this makes the ftheta given parameters mu and sd
fTheta=function(mu, std){
  GausFxmu(mu)*GausFxsd(std)
}

#this takes the probability AT each Q for every mu and std
ProbFQ=function(Q, mu, std){
  #distribution parameters
  a = pi / (sqrt(6)*std)
  b = mu - 0.5772/a
  
  a*exp((-a*(Q-b))-exp(-a*(Q-b)))
}

#this multiplies the ProbFQ by the fTheta for every Q at every mu and standard deviation
Pf=function(Q, mu, std){ProbFQ(Q, mu, std)*fTheta(mu, std)}

#test to make fTheta integrate from -inf to info to 1
#adaptIntegrate(function(z){fTheta(z[1],z[2])},
#                           lower=c(-10^5,0),upper=c(10^5,10^5))

#test to make Pf integrates from -inf to info to 1
#adaptIntegrate(function(z){Pf(z[1],z[2], z[3])}, 
#                            lower=c(0,0,0),upper=c(10^6,10^5, 10^5), tol=1e-4)

#create damage function for each Q conditioned on current DQ/q
D=function(Q){DAMAGE(Q)}

#weight damage costs by probilities, this condenses Pf and D into one function at each Q
DamageFunction=function(Q, mu, std) {
  D(Q)*Pf(Q, mu, std)
}
#curve(DamageFunction(x,mu_mu,sd_mu), 60000, 200000)

#integrate the damage function over all the q and mu and sd
AllDamages0=function(q){
  adaptIntegrate(
    function(z){DamageFunction(z[1],z[2], z[3])},
    lower=c(q,0,0),upper=c(10^6,10^5,10^5), tol=1e-5)$integral
}
AllDamages=Vectorize(AllDamages0)
#curve(AllDamages(x), 60000, 400000)

#get the building and total costs
TC=function(q){LeveeCost(q)+AllDamages(q)}

#check TC over a discrezed Q range
range=seq(mu_mu, 3e5, 10000)
results = matrix(nrow=length(range), ncol=2)
for(i in seq(length(range))){
  q=range[i]
  print(q)
  results[i,1]=q
  results[i,2]=TC(q)
}
optimal = results[which.min(results[,2]),]

#get the optimal from optimziation function of TC based on different q
best1 = optimize(TC, lower=60000, upper=5e5 )

#calculates levee height
best_height = 2+((0.005*(best1$minimum-60000.)**0.7))

cat("Exact Method\n")
cat("DQ:", best1$minimum, "\n")
cat("Total Cost:", best1$objective, "\n")
best1_DQ = best1$minimum
cat("Damage Cost:", AllDamages(best1_DQ), "\n")
cat("Building Cost:", LeveeCost(best1_DQ), "\n")
cat("Best Levee Height:",best_height, "\n")

########################################
#
# Display Results
#
########################################

#Plotting Curves

#Plot Buildling Costs for all q
curve(LeveeCost(x),mu_mu,10^6, xlab="q", main="Building Costs for each q")

#Plot Damage Costs at each Q assuming q=mu, or in other words, with no levee
curve(DamageFunction(x,mu_mu,sd_mu), 60000, 200000, xlab="Q", ylab=paste("DamageFunction,", "q=mu"), main="Weighted Damage Function")

#Plot Damage Costs at each Q*P(Q) assuming q=mu, or in other words, with no levee is built
curve(AllDamages(x), 60000, 400000, xlab="Q", ylab="Damage Costs", main="Damage Costs at each Q")

#Plot Damage Costs at each q, in other words, area between q and Infinity (10^6)
curve(AllDamages(x), mu_mu, 10^6)

#Plot Total Costs at each q
curve(TC(x), mu_mu, 10^6, ylab=paste("TotalCost", toString(q)))

#Optimal Plot
plot.new()
curve(LeveeCost(x),mu_mu,10^6, xlab="q (cfs)", ylab="$", main="Annualized Costs at each Design Flow (q)")
par(new=T, xaxt='n', yaxt='n')
curve(AllDamages(x), mu_mu, 10^6, col="blue", ylab="", xlab="", add=T)
par(new=T, xaxt='n', yaxt='n')
curve(TC(x), mu_mu, 10^6, col="red", ylab="", xlab="", add=T) #ylab=toString(q))

#Optimal DQ point
optimalpoint=points(166958.5, 187121.8)

legend("bottomright", inset=.06, cex=.7,
       c("Expected Value Cost (EVC))","Damage Cost", "Building Cost", "Optimal Design Target:\n q=166959 cfs, EVC=$187,122"), # puts text in the legend 
       lty=c(1,1,1,NA), # gives the legend appropriate symbols (lines)
       #lwd=c(2.5,2.5),  # gives the legend lines the correct width
       pch=c(NA,NA,NA,1),
       col=c("red","blue","black", "black")) # gives the legend lines the correct color



#need to check if fTheta is vectorized so that it can take the vectors for mu and sd and run the function
#we assume because this function g works, fTheta is vectorized. also dnorm is vectorized. 
#if function g is fully vectorized:
#code from: http://r.789695.n4.nabble.com/two-dimensional-integration-td4658784.html
#y is the outer integral: in my case, there is no outer, just neeed correct bounds on all variables
#y here is the standard deviation because its the second in the fTheta function
#this first line says, integrate over a function of y. then later it will tell the program what y is
#CODE: integrate( function(y) { 
  #here more information is provided about wwhat y is. it says that no matter what y is, apply the function of y
  #to the entire vector of y at one time, so the output is a one scalar
#CODE:  sapply(y, function(y) { 
    #confusing because fTheta has 2 arguments and function(x) has only one argument, but this can be explained
    #integrate is a 1-D integration function, so need to give it an integrand in only 1-D
    #y is passed in earlier so it is fixed. it is fixed as a scalar because it already was integrated.
    #this integrates function(x). function(x) is a generic function reference that tells the computer to 
    #take fTheta(x,y) as only a function of x. it reads read fTheta(x,y) as a function of x
    #by this time the integrate function was applied to fThetay all at one time, so y is read as a scalar.
    #and this scalar is calculated before the integration of x, because it was called up first in the order
    #of operations for the integration function
#CODE: integrate(function(x) fTheta(x,y),0,100000)$value 
#CODE: }) 
  #this is the lower and upper bounds for hte integration of y. 
#CODE: },0,100000) 
#if y and x are vectorized this will work. if not then it wont.
#fTheta needs to be support vector arguments, or in other words be vectorizable so that it will work in the
#integrate function. y could maybe not be vectorizeable because it is being handled with sapply



