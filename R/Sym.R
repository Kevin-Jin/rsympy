
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
	Sym("Derivative(", expr, ", ", name[1], ",", n, ").doit()")

if (!isGenericS3("limit")) setGenericS3("limit")
limit.Sym <- function(expr, name = coalesce(sympySymbols(expr), "x"), value) 
	Sym("Limit(", expr, ",", name[1], ",", value, ").doit()")

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
		Sym("Integral(", x, ",(", name[1], ",", lower[1], ",", upper[1], ")).doit()")
	} else {
		# indefinite integral
		Sym("Integral(", x, ",", name[1], ").doit()")
	}

	# TODO: if analytic integration fails, do numeric integration with quad()
}

# CDF
pnorm.Sym <- function() {
	
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

# if (!isGenericS3("substitute")) setGenericS3("substitute", dontWarn = "base")
# substitute.Sym <- function(expr, env = parent.frame()) {
# 	# TODO: same as eval() but calls subs() function on expr instead of evalf()
# }

if (!isGenericS3("solve")) setGenericS3("solve", dontWarn = "base")
solve.Sym <- function(x, y) {
	# "try:solve(x,y) or None\nexcept(NotImplementedError):None\n"
	# "if x != None: return x\n"
	# "try:nsolve(x,y,0) or None\nexcept(ValueError):None\n"
	# "if x == None: return x"
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

# TODO: replace `==`(a,b) with Eq(a,b) when (isinstance(a, Basic) || isinstance(b, Basic))
# i.e. a or b is an expression that is some subclass of sympy.core.basic.Basic
as.Sym <- function(x) {
	# basically equivalent to x <- quote(<this function's expression parameter>)
	# from the caller's context
	x <- substitute(x)

	if (!is.call(x))
		# no operations or function calls, just a single variable or constant
		x <- as.call(list(quote(`identity`), x))

	assign.to <- NULL
	if (deparse(x[[1]]) == "<-") {
		assign.to <- deparse(x[[2]])
		x <- x[[3]]
	}

	# in case the expr x makes reference to variables
	# named e.g. x, env, unknown.symbols, or vars, we want
	# to prevent this function's local variables from
	# being improperly substituted into the passed expr x
	env <- new.env(parent = parent.frame())

	# Conditions are based on the observation that the call:
	# is.function(eval(as.call(list(quote(`function`), as.pairlist(alist(x=)), NULL))))
	# is TRUE for pretty much any value for the 3rd element in the call object.
	is.closure <- function(call.obj)
		# x[[2]] is formal parameters, x[[3]] is body, x[[4]] is debug info.
		deparse(call.obj[[1]]) == "function" && is.pairlist(call.obj[[2]]) && length(call.obj) >= 3

	to.jagged.array <- function(x) {
		# Base case: already 1 dimensional
		if (is.null(dim(x)) || length(dim(x)) == 1 || dim(x)[1] == 1)
			if (length(x) == 1)
				# Scalar
				return(unname(do.call(c, as.list(x))))
			else
				# List
				return(unname(as.list(x)))

		# Peel off the first dimension, so that we create a list of rows
		empty.indices <- rep(list(bquote()), length(dim(x)) - 1)
		lapply(1:dim(x)[1], function(i)
			to.jagged.array(do.call(`[`, c(list(quote(x), i), empty.indices)))
		)
	}

	jagged.array.to.Sym <- function(arg) {
		if (!is.list(arg))
			return(r.to.Sym(arg))

		Sym("[", paste(lapply(arg, function(x) {
			if (is.list(x))
				jagged.array.to.Sym(x)
			else
				r.to.Sym(x)
		}), collapse = ","), "]")
	}

	r.to.Sym <- function(arg) {
		if (!is.null(dim(arg)) || length(arg) != 1 || is.list(arg))
			# mutual recursion
			jagged.array.to.Sym(to.jagged.array(arg))
		else if (inherits(arg, "Date"))
			# create datetime.date instance
			Sym("date(", as.integer(format(arg, "%Y")), ",", as.integer(format(arg, "%m")), ",", as.integer(format(arg, "%d")), ")")
		else if (inherits(arg, "POSIXt"))
			# create datetime.datetime instance
			Sym("datetime(", (arg <- as.POSIXlt(arg))$year + 1900, ",", arg$mon + 1, ",", arg$mday, ",", arg$hour, ",", arg$min, ",", trunc(arg$sec), ",", round((arg$sec %% 1) * 1000000), if (!is.null(arg$gmtoff)) paste(",timezone(timedelta(seconds = ", arg$gmtoff, "))") else "", ")")
		else if (inherits(arg, "difftime"))
			# create datetime.timedelta instance
			Sym("timedelta(0,", as.double(arg, units = "secs"), ")")
		else if (is.complex(arg))
			# R formats with a+bi. Python formats with a+bj.
			Sym(paste(Re(arg), "+", Im(arg), "j", sep = ""))
		else if (is.character(arg) && !any(class(arg) == 'Sym'))
			# quote strings
			# R's escape sequences map directly to Python unicode strings,
			# except for "\`" because backtick are not quotes in Python,
			# but this doesn't matter since encodeString() quotes with "\"".
			Sym(paste("u", encodeString(arg, quote = "\""), sep = ""))
		else
			# pass-through
			arg
	}

	pass.through.function <- function(deref.evaled, uneval = FALSE) {
		# get the function name
		fn.name <- deref.evaled[[1]]
		if (is.call(fn.name))
			fn.name <- eval(fn.name)
		else
			fn.name <- deparse(fn.name)
		# get all right siblings (parameters to function)
		arguments <- setNames(lapply(deref.evaled[-1], eval, envir = vars, enclos = env), names(deref.evaled)[-1])
		# prepend argument values with argument names
		named.arguments <- names(arguments) != ""
		arguments[named.arguments] <- paste(names(arguments)[named.arguments], arguments[named.arguments], sep = "=")
		arguments <- lapply(arguments, r.to.Sym)
		if (uneval)
			bquote(as.call(.(c(list(quote(Sym), fn.name, "("), paste(arguments, collapse = ","), list(")")))))
		else
			Sym(fn.name, "(", paste(arguments, collapse = ","), ")")
	}

	unnest.parentheses <- function(x, address) {
		if (length(address) < 2)
			return(NULL)

		deref <- Reduce(function(acc, add) as.call(list(quote(`[[`), acc, add)), head(address, -2), quote(x))
		deref.evaled <- eval(deref)
		fn <- deparse(deref.evaled[[1]])
		if (length(fn) != 1 || fn != "(")
			return(NULL)

		eval(as.call(list(quote(`<-`), deref, quote(deref.evaled[[2]]))))
		return(x)
	}

	# tuple
	env$pl <- function(...)
		Sym(paste("(", paste(lapply(list(...), function(x) as.Sym(x)), collapse = ","), ")"))

	env$as.pl <- function(lst) do.call(env$pl, as.list(lst))

	# pyEval
	env$`.` <- function(...) {
		args <- list(...)
		args[Position(is.null, args)] <- "NULL"
		do.call(Sym, args)
	}

	# call objects can be recursively descended to get constants and names.
	# if a variable exists in R, use the value in the R variable.
	# if a variable exists only in Python, use the value in the Python variable.
	# otherwise, call Var() to create a new Python variable.
	to.replace <- as.list((f <- function(x, ignore = NULL, pyEval = FALSE) {
		if (is.name(x)) # symbols
			if (deparse(x) == "")
				# empty default value for a formal argument
				NULL
			else if (deparse(x) %in% ignore)
				# Sym(x): make sure that formal parameter usages are never evaled
				deparse(x)
			else if (exists(deparse(x), where = env) && !pyEval)
				if (is.function(fn <- eval(x)))
					# Replace the function handle with a closure
					fn
				else
					# Substitute in R variable that exists in caller's context
					NULL
			else if (pythonHasVariable(deparse(x)))
				# Sym(x): no R variable, but use the Python variable
				deparse(x)
			else
				# Var(x): create a Python symbol
				x
		else if (!is.call(x)) # constants
			if (pyEval)
				# in case Python has a variable named e.g. FALSE, do not touch
				NULL
			else if (is.null(x) || is.na(x))
				# Python uses None for NULL
				"None"
			else if (is.nan(x))
				# Python uses float("nan") for not-a-number
				#"float(\"NaN\")"
				"nan"
			else if (is.infinite(x))
				# Python uses float("inf") for positive infinity
				#"float(\"Inf\")"
				"oo"
			else if (identical(x, TRUE))
				# Python uses camel case booleans
				"True"
			else if (identical(x, FALSE))
				# Python uses camel case booleans
				"False"
			else
				# constants by definition are already known
				NULL
		else if (is.closure(x)) # function declaration
			# don't create any SymPy symbols for usages of the formal parameters
			unlist(c(`1` = NA, `2` = setNames(lapply(x[[2]], f, ignore), if (length(x[[2]]) > 0) 1:length(x[[2]]) else c()), `3` = f(x[[3]], c(ignore, names(x[[2]])))))
		else # nested function call/operator
			if (deparse(x[[1]]) == "<-")
				# Python assignments are statements, not expressions
				stop("Assignments are allowed only at the top level")
			else if (deparse(x[[1]]) == ".")
				# don't parse following symbol as R but as Python
				unlist(setNames(lapply(x[-1], f, ignore, pyEval = TRUE), if (length(x) > 1) 2:length(x) else c()))
			else if (is.call(x[[1]]))
				# the function we call is itself a function that returns a closure
				unlist(setNames(lapply(x, f, ignore), if (length(x) > 0) 1:length(x) else c()))
			else if (deparse(x[[1]]) %in% ignore || !exists(deparse(x[[1]]), where = env) || pyEval)
				# x[-1] to skip the function name. NA means use Python function
				c(list(`1` = NA), unlist(setNames(lapply(x[-1], f, ignore), if (length(x) > 1) 2:length(x) else c())))
			else
				# x[-1] to skip the function name
				unlist(setNames(lapply(x[-1], f, ignore), if (length(x) > 1) 2:length(x) else c()))
	})(x))

	# attempt to rewrite trivial R functions, referred by name, into Python
	function.handles <- sort(which(as.logical(unlist(lapply(to.replace, is.function)))), decreasing = TRUE)
	# replace the function handle with the closure itself
	x <- Reduce(function(x, name) {
		# unlist() set names of nested lists [[i]][[j]][[k]] to "i.j.k"
		address <- as.numeric(unlist(strsplit(name, ".", fixed = TRUE)))
		# first child of any parent must be a function name
		stopifnot(tail(address, 1) != 1 || length(x) == 1)
		# creates an expression to access e.g. x[[3]][[2]] when address==c(3,2)
		deref <- Reduce(function(acc, add) as.call(list(quote(`[[`), acc, add)), address, quote(x))
		eval(as.call(list(quote(`<-`), deref, enquote(as.call(list(quote(`function`), formals(to.replace[[name]]), body(to.replace[[name]])))))))
		x
	}, names(to.replace)[function.handles], x)
	# replace the function handle in to.replace with the replacements needed within its body
	to.replace <- Reduce(function(to.replace, loc) {
		# splice in replacements needed for closure
		fn <- to.replace[[loc]]
		inner.to.replace <- as.list(unlist(setNames(list(list(`1` = NA, `3` = f(body(fn), names(formals(fn))))), names(to.replace)[loc])))
		if (loc == 1 && loc == length(to.replace))
			NULL
		else if (loc == length(to.replace))
			c(to.replace[1:(loc - 1)], inner.to.replace)
		else if (loc == 1)
			c(inner.to.replace, to.replace[(loc + 1):length(to.replace)])
		else
			c(to.replace[1:(loc - 1)], inner.to.replace, to.replace[(loc + 1):length(to.replace)])
	}, function.handles, to.replace)

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
	vars <- unlist(lapply(unknown.symbols, deparse))
	vars <- setNames(lapply(vars, Var), vars)

	# process lambdas and pass through function names that aren't implemented in R to SymPy.
	# Both kinds of replacements replace entire R expression trees with strings
	# of Python code, so deepest replacements of EITHER kind must be done before
	# any replacements towards the root of the expression tree
	x <- Reduce(function(x, name) {
		# unlist() set names of nested lists [[i]][[j]][[k]] to "i.j.k"
		address <- as.numeric(unlist(strsplit(name, ".", fixed = TRUE)))
		# first child of any parent must be a function name
		stopifnot(tail(address, 1) == 1)
		# go to container for function call
		deref <- Reduce(function(acc, add) as.call(list(quote(`[[`), acc, add)), head(address, -1), quote(x))
		deref.evaled <- eval(deref)

		# branch for lambda instead of pass through function name processing
		if (is.closure(deref.evaled)) {
			fn.formals <- deref.evaled[[2]]
			fn.body <- deref.evaled[[3]]
			if (class(fn.body) == "{")
				# Python doesn't support multiline anonymous functions
				stop("Multi-statement lambdas not yet translatable")

			fn.body <- eval(fn.body, envir = c(vars, setNames(lapply(names(fn.formals), Sym), names(fn.formals))), enclos = env)
			fn.body <- r.to.Sym(fn.body)

			fn.formals[unlist(lapply(fn.formals, deparse)) == ""] <- list(NULL)
			# unlike in R, default values can't reference other formal parameters in Python, so no using fn.formals in envir
			fn.formals <- lapply(fn.formals, eval, envir = vars, enclos = env)
			fn.formals <- lapply(fn.formals, function(x) if (is.null(x)) NULL else r.to.Sym(x))
			empty.defaults <- unlist(lapply(fn.formals, is.null))
			fn.formals[empty.defaults] <- names(fn.formals)[empty.defaults]
			fn.formals[!empty.defaults] <- paste(names(fn.formals)[!empty.defaults], fn.formals[!empty.defaults], sep = "=")
			fn.formals <- paste(fn.formals, collapse = ",")

			eval(as.call(list(quote(`<-`), deref, quote(as.call(list(quote(Sym), "lambda", fn.formals, ":", fn.body))))))
		} else {
			# replace e.g. x[[i]][[j]] with e.g. Sym(x[[i]][[j]][[1]], "(", x[[i]][[j]][[2]], x[[i]][[j]][[3]], ")")
			eval(as.call(list(quote(`<-`), deref, pass.through.function(deref.evaled, uneval = TRUE))))
		}

		while (!is.null(new.x <- unnest.parentheses(x, address))) {
			x <- new.x
			address <- address[-(length(address) - 1)]
		}

		if (length(address) >= 2 && head(tail(address, 2), 1) == 1) {
			# e.g. we're processing f(), where f() is used in the call (f())().
			# f() is now substituted with a Sym, but Sym("f()")() can't be
			# called, so replace our parent call with Sym("f()()")
			deref <- Reduce(function(acc, add) as.call(list(quote(`[[`), acc, add)), head(address, -2), quote(x))
			deref.evaled <- eval(deref)
			eval(as.call(list(quote(`<-`), deref, pass.through.function(deref.evaled, uneval = TRUE))))
		}

		# carry the transformed x to the next transformation function
		x
	}, names(to.replace)[is.na(to.replace)], x)

	output <- eval.default(x, envir = vars, enclos = env)
	if (!is.null(assign.to))
		output <- Sym(paste(assign.to, "=", output))

	output
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

