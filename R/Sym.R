
# constructor

Sym <- function(..., retclass = c("Sym", "character", "expr")) {
	args <- list(...)
	retclass <- match.arg(retclass)
	value <- if (length(args) > 1) paste("(", ..., ")") else paste(args[[1]])
	if (retclass == "Sym") class(value) <- c("Sym", "character")
	value
}

# helper functions

coalesce <- function(x, def) if (mode(x) == mode(def) && length(x) > 0) x else def

# methods

as.character.Sym <- function(x, ...) as.character(unclass(x))

print.Sym <- function(x, ...) print(sympy(unclass(x), ...))

deriv.Sym <- function(expr, name = coalesce(sympySymbols(expr), "x"), n = 1, ...) 
	Sym("diff(", expr, ", ", name[1], ",", n, ")")

if (!isGenericS3("limit")) setGenericS3("limit")
limit.Sym <- function(expr, name = coalesce(sympySymbols(expr), "x"), value) 
	Sym("limit(", expr, ",", name[1], ",", value, ")")

solve.Sym <- function(a, b, method = c("'GE'", "'ADJ'", "'LU'"), ...) {
	stopifnot(missing(b))
	Sym(paste("(", a, ").inv(", match.arg(method), ")"))
}

if (!isGenericS3("integrate")) setGenericS3("integrate", dontWarn = "stats")
integrate.Sym <- function(x, lower = NULL, upper = NULL, name = coalesce(sympySymbols(x), "x"), ..., subdivisions = Inf, rel.tol = 0, abs.tol = 0, stop.on.error = TRUE, keep.xy = FALSE, aux = NULL) {
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

if (!isGenericS3("eval")) setGenericS3("eval", dontWarn = "base")
eval.Sym <- function(x, envir = parent.frame(), enclos = if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv(), retclass = c("character", "Sym", "expr")) {
	atoms <- sympySymbols(x)
	if (is.numeric(atoms)) atoms <- NULL
	if (length(atoms) == 0)
		return(sympy(x, retclass = if (is.null(retclass)) NULL else match.arg(retclass)))

	stopifnot(is.character(atoms))
	vals <- numeric(length(atoms))

	# data frames are lists too
	if (is.list(envir)) {
		if (is.null(enclos)) enclos = baseenv()

		from.envir <- unlist(lapply(atoms, function(x) is.numeric(envir[[x]])))
		vals[from.envir] <- as.numeric(envir[atoms[from.envir]])
		names(vals)[from.envir] <- atoms[from.envir]

		from.envir <- !from.envir
		envir <- enclos
	} else {
		from.envir <- rep(TRUE, length(atoms))
	}

	# search all symbols from: enclos if envir is a list, envir otherwise
	atoms <- atoms[from.envir]
	#stopifnot (all(unlist(lapply(atoms, exists, where = envir))))
	# without specifying `ifnotfound`, mget throws an exception if any symbol is not found
	vals[from.envir] <- as.numeric(mget(atoms, envir = envir, mode = "numeric", inherits = TRUE)[atoms])
	names(vals)[from.envir] <- atoms

	sympyEvalf(x, vals, retclass = if (is.null(retclass)) NULL else match.arg(retclass))
}

# AKA lambda(), turns an expression into an R function that can accept parameters
# TODO: if retclass is Sym, pass all numbers to sympy.core.numbers.Number __new__
as.function.Sym <- function(x, param = NULL, retclass = c("character", "Sym", "expr")) {
	atoms <- sympySymbols(x)

	if (is.null(param)) param <- atoms
	stopifnot(is.character(param))

	if (!isTRUE(all.equal(retclass, eval(formals()$retclass)))) {
		# retclass is not set to default value.
		retclass <- match.arg(retclass)
	} else {
		# retclass is set to the default value.
		if (!all(atoms %in% param)) {
			# not all free variables in x are bound.
			# by default, lambda should output a Sym expression.
			retclass <- "Sym"
		} else {
			# all variables are bound.
			# by default, lambda should output a character.
			retclass <- "character"
		}
	}

	f.param <- list()
	length(f.param) <- length(param)
	names(f.param) <- param

	lambda <- sympyLambdify(param, x)
	f <- function() executeLambda(lambda, match.call(), retclass)
	formals(f) <- as.pairlist(f.param)

	f
}

as.expression.Sym <- function(x) sympy(unclass(x), retclass = "expr")

t.Sym <- function(x) Sym(paste("(", x, ").transpose()"))

# static factories

Var <- function(x, retclass = c("Sym", "character", "expr")) {
	x <- paste("var('", x, "')", sep = "")
	sympy(x, retclass = if (is.null(retclass)) NULL else match.arg(retclass))
}

List <- function(...) Sym("[", paste( ..., sep = ","), "]")
Matrix <- function(...) Sym("Matrix(", paste(..., sep = ","), ")")
Zero <- function(n) Sym(paste("zero(", n, ")"))
Eye <- function(n) Sym(paste("eye(", n, ")"))
Zeros <- function(m, n) Sym(paste("zero(", m, ",", n, ")"))

