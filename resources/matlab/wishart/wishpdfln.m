
% From Wikipedia: http://en.wikipedia.org/wiki/Wishart_distribution

function lnpdf = wishpdfln(X, n, V)
p = size(X,1);
lnpdf = 0;
lnpdf = lnpdf + ((n - p - 1) / 2.0) * log(det(X));
lnpdf = lnpdf - (trace(inv(V) * X) / 2.0);
lnpdf = lnpdf - ((n * p) / 2.0) * log(2.0);
lnpdf = lnpdf - (n / 2.0) * log(det(V));
lnpdf = lnpdf - logmvgamma(n / 2.0, p);
