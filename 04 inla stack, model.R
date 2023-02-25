

## data stacking

pop_tox_2$rsei.score.noncancer_bin[pop_tox_2$rsei.score.noncancer.x == 1] <- 0
pop_tox_2$rsei.score.noncancer_bin[pop_tox_2$rsei.score.noncancer.x > 1] <- 1

pop_tox_2$rsei.score.cancer_bin[pop_tox_2$rsei.score.cancer == 1] <- 0
pop_tox_2$rsei.score.cancer_bin[pop_tox_2$rsei.score.cancer > 1] <- 1


sdat <- inla.stack(
  data = list(idarea = pop_tox_2$idarea), 
 # A = list(A, 1), 
  effects = list(iset, w = pop_tox_2$rsei.score.cancer_bin),
  tag = 'stdata') 



mesh = inla.mesh.create.helper(points.domain = loc,
                               max.edge = c(0.05, 0.2))
proj.obs = inla.mesh.projector(mesh, loc = loc)
proj.pred = inla.mesh.projector(mesh, loc = mesh$loc)
spde = inla.spde2.matern(mesh,
                         B.tau = cbind(log(1), 1, 0),
                         B.kappa = matrix(c(log(sqrt(8)/0.2), 0, 1), 1, 3))

covar = rnorm(n)
field = inla.qsample(n=1, Q=inla.spde2.precision(spde, theta=c(0,0)))[,1]
y = 2*covar + inla.mesh.project(proj.obs, field)

A.obs = inla.spde.make.A(mesh, loc=loc)
A.pred = inla.spde.make.A(mesh, loc=proj.pred$loc)
stack.obs =
  inla.stack(data=list(y=y),
             A=list(A.obs, 1),
             effects=list(c(list(intercept=rep(1, mesh$n)),
                            inla.spde.make.index("spatial", spde$n.spde)),
                          covar=covar),
             tag="obs")
stack.pred =
  inla.stack(data=list(y=NA),
             A=list(A.pred),
             effects=list(c(list(intercept=rep(1, mesh$n)),
                            inla.spde.make.index("spatial", mesh$n))),
             tag="pred")
stack = inla.stack(stack.obs, stack.pred)



## INLA preprocessing

## Create index
pop_tox_2$idarea <- 1:nrow(pop_tox_2)

## Queens case
dat_nb <- poly2nb(pop_tox_2)

## Write to file
nb2INLA("map.adj", dat_nb)
g <- inla.read.graph(filename = "map.adj")



xx <- inla(rsei.score_log ~ population_10k_c +
             black_c_log*pov_c_log + 
             aian_p_log*pov_c_log + 
             asian_p_log*pov_c_log + 
             nhpi_p_log*pov_c_log + 
             tom_p_log*pov_c_log + 
             other_p_log*pov_c_log + 
             f(idarea, model = "bym", graph = g), 
           #control.inla=list(cmin=0),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(compute = TRUE),
           data = pop_tox_2)
#aic intx: 13226.56
#aic detailed race*po: 13201.87

xx <- inla(rsei.score ~ population_10k_c +
             black_c*pov_c + 
             aian_c*pov_c + 
             asian_c*pov_c + 
             nhpi_c*pov_c + 
             tom_c*pov_c + 
             other_c*pov_c + 
             f(idarea, model = "bym", graph = g), 
           #control.inla=list(cmin=0),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(compute = TRUE),
           data = pop_tox_2, family = "gamma")
xx <- inla(rsei.score ~ population_10k_c +
             nonwhite_c*pov_c + 
             f(idarea, model = "bym", graph = g), 
           #control.inla=list(cmin=0),
           control.compute = list(dic = TRUE, waic = TRUE),
           control.predictor = list(compute = TRUE),
           data = pop_tox_2, family = "gamma")
summary(xx)

pop_tox_2$RW2D <- xx$summary.fitted.values[, "mean"]

tm_shape(pop_tox_2) +
  tm_polygons(col = "RW2D", lwd = 0, style = "cont")





