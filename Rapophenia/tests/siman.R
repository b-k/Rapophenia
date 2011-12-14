library(Rapophenia)

# model = (ll_function = ll,
#         name = "blah",
#         settings = c(apop_mle = c(tolerance=1e-5, verbose =1),
#                     apop_parts_wanted = c()
#             )
#         )


.C("init_registry")
mod <- get_C_model("banana")
est <- estimateRapopModel(NULL, mod)
params <- getModelElement(est, "parameters")
print("C side banana, no constraint")
print(params)

mod <- get_C_model("bananac")
est <- estimateRapopModel(NULL, mod)
params <- getModelElement(est, "parameters")
print("C side constrained banana")
print(params)


starthere <-as.vector(c(8,0.1))
scale <- 10

print("R optim says:")
    optim(starthere, 
 function(env){ return ((1-env[1])**2 + scale *(env[2]-env[1]**2)**2) }
 )



#Rosenbrock's banana function
#takes in an environment.
#   Assume elements named scaling and parameters
ll <- function(env){
    return (-((1-env$parameters$Vector[1])**2 + env$scaling*(env$parameters$Vector[2]-env$parameters$Vector[1]**2)**2))
}

#sets <- as.environment(list(
#apop_parts_wanted=as.environment(list(none=0)),
#apop_mle=as.environment(list(
#tolerance = 1e-6,
#starting_pt=starthere, verbose=1))))

#mod <- setupRapopModel(ll_function=ll, vbase=2, name="banana", settings=sets)

setobj <-
list(new("apop_mle_settings",tolerance=1e-6,starting_pt=starthere,
# method=APOP_SIMAN,
            verbose=TRUE),
	new("apop_parts_wanted"))
modobj <- new("apop_model",
	ll_function=ll,data=data.frame(scaling=scale),vbase=2L,name="banana",settings=setobj)
mod <- setupRapopModel(modobj)
data <- as.environment(list(scaling=scale))
est <- estimateRapopModel(data, mod)

params <- getModelElement(est, "parameters")
print("R-side via Apophenia")
print(params)
#stopifnot(sqrt(crossprod(params$Vector - as.vector(c(1,1)))) < 1e-3) ############################

#Some timing tests. Last time we tried it, Rapop was about 4x faster than optim.
#The reader may be able to find cases that reverse this.
#    print("Rapop timing")
#print(    system.time( for (i in 1:1000){
#    mod <- setupRapopModel(ll_function=ll, vbase=2, name="banana")
#    data <- as.environment(list(scaling=scale))
#    est <- estimateRapopModel(data, mod)
#}))
#
#    print("optim timing")
# print(   system.time( for (i in 1:1000){
#    optim(starthere, 
# function(env){ return ((1-env[1])^2 + scale *(env[2]-env[1]^2)^2) }
# )
#}))


cc <-function(env){
    #.Internal(inspect(env))
    penalty <- 2-sqrt(env$parameters$Vector[1]**2 + env$parameters$Vector[2]**2)
    if (penalty <= 0) return (0)
    #else
    env$parameters$Vector[1] <- env$parameters$Vector[1]*2/(2-penalty)
    env$parameters$Vector[2] <- env$parameters$Vector[2]*2/(2-penalty)
    return(penalty)
}

modobj <- new("apop_model",
	ll_function=ll, constraint_function=cc, data=data.frame(scaling=scale),vbase=2L,name="banana",settings=setobj)
mod <- setupRapopModel(modobj)
est <- estimateRapopModel(data, mod)

params <- getModelElement(est, "parameters")
print("R-side constrained via Apophenia")
print(params)


#, parameters=as.vector(c(1.2,2.3))
#.Call("Rapophenia_ll", m)
