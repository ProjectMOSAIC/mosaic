fplot(sin, xlim=c(-2*pi, 2*pi))
fplot(list(sin,D(sin),D(D(sin)),D(D(D(sin)))), xlim=c(-2*pi, 2*pi))
