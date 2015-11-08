
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
		stop("lower and upper must both be specified or both be unspecified")
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
		return(sympyEvalf(x, retclass = if (is.null(retclass)) NULL else match.arg(retclass)))

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
	if (is.numeric(atoms)) atoms <- NULL

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

	# getting the empty symbol (for formal arguments with no default value) is
	# a bit hacky since as.name("") does not work
	f.param <- rep(c(quote(f(emptyname=))$emptyname), length(param))
	names(f.param) <- param

	lambda <- sympyLambdify(param, x)
	f <- function() executeLambda(lambda, match.call(), retclass)
	formals(f) <- as.pairlist(f.param)

	#reg.finalizer(environment(), function(obj) { str(f); str(obj); })

	f
}

as.expression.Sym <- function(x) sympy(unclass(x), retclass = "expr")

t.Sym <- function(x) Sym(paste("(", x, ").transpose()"))

# static factories

as.Sym <- function(x) {
	# basically equivalent to x <- quote(<this function's expression parameter>)
	# from the caller's context
	x <- substitute(x)

	# in case the expr x makes reference to variables
	# named e.g. x, env, unknown.symbols, or vars, we want
	# to prevent this function's local variables from
	# being improperly substituted into the passed expr x
	env <- parent.frame()

	if (!is.call(x))
		# no operations or function calls, just a single variable or constant
		x <- as.call(list(quote(`identity`), x))

	# call objects can be recursively descended to get constants and names.
	# if a variable exists in R, use the value in the R variable.
	# if a variable exists only in Python, use the value in the Python variable.
	# otherwise, call Var() to create a new Python variable.
	to.replace <- (f <- function(x) {
		if (is.name(x)) # symbols
			if (exists(as.character(x), where = env))
				# Substitute in R variable that exists in the caller's context
				NULL
			else if (pythonHasVariable(as.character(x)))
				# Sym(x): no R variable, but use the Python variable
				as.character(x)
			else
				# Var(x): create a Python symbol
				x
		else if (!is.call(x)) # constants
			if (is.nan(x))
				# Python uses nan for not-a-number
				"nan"
			else if (x == Inf)
				# SymPy represents infinity as two lowercase `o`s
				"oo"
			else
				# constants by definition are already known
				NULL
		else # nested function call/operator
			if (!exists(as.character(x), where = env))
				# x[-1] to skip the function name
				c(setNames(list(NA), 1), unlist(setNames(lapply(x[-1], f), 2:length(x))))
			else
				# x[-1] to skip the function name
				unlist(setNames(lapply(x[-1], f), 2:length(x)))
	})(x)

	if (!is.list(to.replace))
		# characters only
		to.replace <- as.list(to.replace)

	# in the case of passing through functions to SymPy, reversing the list
	# substitutes innermost function calls first, so addresses are not messed up
	# for outermost function calls
	to.replace <- rev(to.replace)

	# pass-through literal Python symbols not found in R
	unknown.symbols <- as.logical(unlist(lapply(to.replace, is.name)))
	# because of copy-on-write and scoping issues when trying to modify a value
	# in x inside another function, just keep carrying x-prime forward to the
	# next transform and finally return the final image (thus Reduce())
	x <- Reduce(function(x, name) {
		# unlist() set names of nested lists [[i]][[j]][[k]] to "i.j.k"
		address <- as.numeric(unlist(strsplit(name, ".", fixed = TRUE)))
		# first child of any parent must be a function name
		stopifnot(tail(address, 1) != 1 || length(x) == 1)
		# creates an expression to access e.g. x[[3]][[2]] when address==c(3,2)
		deref <- Reduce(function(acc, add) as.call(list(quote(`[[`), acc, add)), address, quote(x))
		# finally, assign our replacement string to e.g. x[[3]][[2]]
		eval(as.call(list(quote(`<-`), deref, quote(as.call(list(quote(Sym), to.replace[[name]]))))))
		# carry the transformed x to the next transformation function
		x
	}, names(to.replace)[!is.na(to.replace) & !unknown.symbols], x)

	# declare SymPy symbols for symbols not found in R or Python, and plug them in R
	unknown.symbols <- to.replace[unknown.symbols]
	vars <- unlist(lapply(unknown.symbols, as.character))
	vars <- setNames(lapply(vars, Var), vars)

	# pass through function names that aren't implemented in R to SymPy
	x <- Reduce(function(x, name) {
		# unlist() set names of nested lists [[i]][[j]][[k]] to "i.j.k"
		address <- as.numeric(unlist(strsplit(name, ".", fixed = TRUE)))
		# first child of any parent must be a function name
		stopifnot(tail(address, 1) == 1)
		# go to container for function call
		deref <- Reduce(function(acc, add) as.call(list(quote(`[[`), acc, add)), head(address, -1), quote(x))
		# get the function name
		fn.name <- eval(deref)[[1]]
		# get all right siblings (parameters to function)
		arguments <- lapply(eval(deref)[-1], eval, envir = vars, enclos = env)
		# replace e.g. x[[i]][[j]] with e.g. Sym(x[[i]][[j]][[1]], "(", x[[i]][[j]][[2]], x[[i]][[j]][[3]], ")")
		eval(as.call(list(quote(`<-`), deref, quote(as.call(c(list(quote(Sym), as.character(fn.name), "("), arguments, list(")")))))))
		# carry the transformed x to the next transformation function
		x
	}, names(to.replace)[is.na(to.replace)], x)

	Sym(eval.default(x, envir = vars, enclos = env))
}

Var <- function(x, retclass = c("Sym", "character", "expr")) {
	x <- paste("var('", x, "')", sep = "")
	sympy(x, retclass = if (is.null(retclass)) NULL else match.arg(retclass))
}

List <- function(...) Sym("[", paste( ..., sep = ","), "]")
Matrix <- function(...) Sym("Matrix(", paste(..., sep = ","), ")")
Zero <- function(n) Sym(paste("zero(", n, ")"))
Eye <- function(n) Sym(paste("eye(", n, ")"))
Zeros <- function(m, n) Sym(paste("zero(", m, ",", n, ")"))

