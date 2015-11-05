
# constructor

Sym <- function(..., retclass = c("Sym", "character")) {
	args <- list(...)
	retclass <- match.arg(retclass)
	value <- if (length(args) > 1) paste("(", ..., ")") else paste(args[[1]])
	if (retclass == "Sym") class(value) <- c("Sym", "character")
	value
}

# methods

as.character.Sym <- function(x, ...) as.character(unclass(x))

print.Sym <- function(x, ...) print(sympy(unclass(x), ...))

deriv.Sym <- function(expr, name = sympySymbols(x), n = 1, ...) 
	Sym("diff(", expr, ", ", name[1], ",", n, ")")

if (!isGenericS3("limit")) setGenericS3("limit")
limit.Sym <- function(expr, name = sympySymbols(x), value) 
	Sym("limit(", expr, ",", name[1], ",", value, ")")

solve.Sym <- function(a, b, method = c("'GE'", "'ADJ'", "'LU'"), ...) {
	stopifnot(missing(b))
	Sym(paste("(", a, ").inv(", match.arg(method), ")"))
}

if (!isGenericS3("integrate")) setGenericS3("integrate", dontWarn = "stats")
integrate.Sym <- function(x, lower = NULL, upper = NULL, name = sympySymbols(x), ..., subdivisions = Inf, rel.tol = 0, abs.tol = 0, stop.on.error = TRUE, keep.xy = FALSE, aux = NULL) {
	if (xor(is.numeric(lower), is.numeric(upper)))
		stop("lower or upper must both be specified or both be unspecified")
	if (!is.character(name) || length(name) == 0)
		stop("name must be a string")

	# TODO: use named arguments ... to plug into other variables as constants
	if (is.numeric(lower)) { # == is.numeric(upper)
		# definite integral
		Sym("integrate(", x, ",(", name[1], ",", lower[1], ",", upper[1], "))")
	} else {
		# indefinite integral
		Sym("integrate(", x, ",", name[1], ")")
	}
}

t.Sym <- function(x) Sym(paste("(", x, ").transpose()"))

# static factories

Var <- function(x, retclass = c("Sym", "character")) {
	x <- paste("var('", x, "')", sep = "")
	sympy(x, retclass = if (is.null(retclass)) NULL else match.arg(retclass))
}

List <- function(...) Sym("[", paste( ..., sep = ","), "]")
Matrix <- function(...) Sym("Matrix(", paste(..., sep = ","), ")")
Zero <- function(n) Sym(paste("zero(", n, ")"))
Eye <- function(n) Sym(paste("eye(", n, ")"))
Zeros <- function(m, n) Sym(paste("zero(", m, ",", n, ")"))

