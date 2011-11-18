library(Rapophenia)

# model = (ll_function = ll,
#         name = "blah",
#         settings = c(apop_mle = c(tolerance=1e-5, verbose =1),
#                     apop_parts_wanted = c()
#             )
#         )

starthere <-as.vector(c(8,0.1))
scale <- 100

    optim(starthere, 
 function(env){ return ((1-env[1])**2 + scale *(env[2]-env[1]**2)**2) }
 )



#Rosenbrock's banana function
#takes in an environment.
#   Assume elements named scaling and parameters
ll <- function(env){
    return (-((1-env$parameters[1,1])**2 + env$scaling*(env$parameters[2,1]-env$parameters[1,1]**2)**2))
}

sets <- as.environment(list(
            apop_parts_wanted=as.environment(list(none=0)),
            apop_mle=as.environment(list(
                tolerance = 1e-6,
                starting_pt=starthere, verbose=1))))

#mod <- setupRapopModel(ll_function=ll, vbase=2, name="banana", settings=sets)

setobj <- list(new("apop_mle_settings",tolerance=1e-5,starting_pt=starthere,verbose=TRUE),
	new("apop_parts_wanted"))
modobj <- new("apop_model",
	ll_function=ll,data=data.frame(scaling=scale),vbase=2L,name="banana",settings=setobj)
mod <- setupRapopModel(modobj)
data <- as.environment(list(scaling=scale))
est <- estimateRapopModel(mod, data)

params <- getModelElement(est, "parameters")
print(params)
stopifnot(sqrt(crossprod(params - as.vector(c(1,1)))) < 1e-3)

#Some timing tests. Last time we tried it, Rapop was about 4x faster than optim.
#The reader may be able to find cases that reverse this.
#    print("Rapop timing")
#print(    system.time( for (i in 1:1000){
#    mod <- setupRapopModel(ll_function=ll, vbase=2, name="banana")
#    data <- as.environment(list(scaling=scale))
#    est <- estimateRapopModel(mod, data)
#}))
#
#    print("optim timing")
# print(   system.time( for (i in 1:1000){
#    optim(starthere, 
# function(env){ return ((1-env[1])^2 + scale *(env[2]-env[1]^2)^2) }
# )
#}))


cc <-function(env){
    penalty <- 2-sqrt(env$parameters[1,1]**2 + env$parameters[2,1]**2)
    if (penalty > 0){
        env$parameters[1,1] <- env$parameters[1,1]*(2/(1-penalty))
        env$parameters[2,1] <- env$parameters[2,1]*(2/(1-penalty))
        return(penalty)
    } #else
    return(0)
}

modobj <- new("apop_model",
	ll_function=ll, constraint_function=cc, data=data.frame(scaling=scale),vbase=2L,name="banana",settings=setobj)
mod <- setupRapopModel(modobj)
est <- estimateRapopModel(mod, data)

params <- getModelElement(est, "parameters")
print(params)


#, parameters=as.vector(c(1.2,2.3))
#.Call("Rapophenia_ll", m)
