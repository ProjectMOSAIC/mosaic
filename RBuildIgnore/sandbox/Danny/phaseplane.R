
fhn <-  function(voltage,recovery){
	d.voltage <- -voltage*(voltage-0.3)*(voltage-1) - recovery;
	d.recovery <- 1*(voltage - 2.5*recovery);
	return( c(d.voltage, d.recovery) );
}

predator.prey <- function(pred=0,prey=0){
	dpred <- (1 - 0.001*prey)*pred;
	dprey <- (-1 + 0.001*pred)*prey;
	return( c(dpred=dpred, dprey=dprey))
}

competition <-  function(x,y){
	dx <- 2*(1-(x+y)/1000)*x;
	dy <- 2*(1-(x+y)/500)*y;
	return( c(dx, dy) );
}

newton.cooling <- function(obj,env){
	d.env <- 1*(env-obj);
	d.obj <- 1*(obj-env)/Inf;
	return( c(d.env, d.obj) );
}

SIR <- function(suscept,infective){
	dsuscept <- 1-2*suscept*infective;
	dinfective <- 2*suscept*infective - 1*infective;
	return( c(dsuscept, dinfective) );
}

RJ <- function(Romeo, Juliet){
	dRomeo <- 1*Romeo + 2*Juliet
	dJuliet <- 2*Romeo + 1*Juliet
	return( c(dRomeo, dJuliet) );
}

# need to add examples here using the functions above
