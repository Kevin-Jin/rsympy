
# R is fundamentally single threaded, so we don't have to make anything thread
# local or surround assignments and retrievals of __Rsympy in atomic blocks

.onLoad <- function(libname, pkgname) {
	if (exists(".SympyConnected", .GlobalEnv)) return()

	# like system.file but on Windows uses \ in path rather than /
	system.file. <- function(...) {
		s <- system.file(...)
		if (.Platform$OS == "windows") gsub("/", "\\", s, fixed = TRUE) else s
	}

	if (!pyIsConnected()) pyConnect()

	pyExecp("import sys")
	pyExecp("from datetime import *")
	pyExecp( paste( "sys.path.append(", system.file( "Lib", package = "rSymPy" ), ")", sep = '"' ) )
	pyExecp("from sympy import *")
	pyExecp("from sympy.stats import *")
	pyExecp("from sympy.solvers import nsolve")
	pyExecp("from sympy.printing.mathml import mathml")
	pyExecp("from sympy.utilities.lambdify import lambdify")
	pyExecp("from sympy.functions.special.gamma_functions import *")

	invisible(assign('.SympyConnected', TRUE, pos = .GlobalEnv))
}

.onUnload <- function(libname, pkgname) {
	if (!exists(".SympyConnected", .GlobalEnv)) return()

	if (pyIsConnected()) pyExit()

	invisible(remove('.SympyConnected', pos = .GlobalEnv))
}

pyGetPoly <- PythonInR:::pyGetPoly
setMethod("pyGetPoly", signature(key = "character", autoTypecast = "logical", simplify = "logical", pyClass = "complex"), function(key, autoTypecast, simplify, pyClass)
	do.call(complex, as.list(pyExecg(sprintf("%s = [1, (%s).real, (%s).imag]", paste(key, "0", sep = ""), key, key))[[paste(key, "0", sep = "")]]))
)

setMethod("pyGetPoly", signature(key = "character", autoTypecast = "logical", simplify = "logical", pyClass = "list"), function(key, autoTypecast, simplify, pyClass) {
	is.scalar <- function(cond) return(function(x) length(x) == 1 && cond(x))
	homogenous <- function(x) all((type <- unlist(lapply(x, mode)))[1] == type)
	list.is <- function(x, cond) all(unlist(lapply(x, cond)))

	# Not sure why pyGetPoly() call doesn't cause infinite recursion, but it
	# doesn't, so it's a good way to call the super() method pyGetSimple()!
	base <- pyGetPoly(key, autoTypecast, simplify, "list")
	if (length(base) > 0) {
		non.numeric <- !unlist(lapply(base, is.scalar(is.numeric)))
		base[non.numeric] <- lapply(which(non.numeric) - 1, function(i) {
			pyExecp(sprintf("%s = (%s)[%d]", paste(key, "0", sep = ""), key, i))
			pyGet(paste(key, "0", sep = ""), autoTypecast, simplify)
		})
		if (simplify && list.is(base, is.scalar(is.vector)) && homogenous(base))
			base <- unlist(base)
	}
	base
})

timedelta.to.difftime <- function(pyObj)
	as.difftime(pyObj$days, units = "days") + as.difftime(pyObj$seconds + pyObj$microseconds / 1000000, units = "secs")

set.time <- function(pyObj) {
	tz.name <- pyObj$tzname()
	if (!is.null(tz.name))
		rObj <- as.POSIXlt(Sys.time(), tz = tz.name)
	else
		rObj <- as.POSIXlt(Sys.time())
	tz.offset <- pyObj$utcoffset()
	if (!is.null(tz.offset))
		rObj$gmtoff <- as.double(timedelta.to.difftime(tz.offset), units = "secs")

	rObj$hour <- pyObj$hour
	rObj$min <- pyObj$minute
	rObj$sec <- pyObj$second + pyObj$microsecond / 1000000
	rObj
}

setClass("datetime")
setMethod("pyGetPoly", signature(key = "character", autoTypecast = "logical", simplify = "logical", pyClass = "datetime"), function(key, autoTypecast, simplify, pyClass) {
	pyObj <- pyGetPoly(key, autoTypecast, simplify, "datetime")
	# See PythonInR::pyTransformReturn()
	pyObj <- pyObject(sprintf("__R__.namespace[%i]", pyObj$id))
	rObj <- set.time(pyObj)
	rObj$year <- pyObj$year - 1900
	rObj$mon <- pyObj$month - 1
	rObj$mday <- pyObj$day
	rObj
})

setClass("time")
setMethod("pyGetPoly", signature(key = "character", autoTypecast = "logical", simplify = "logical", pyClass = "time"), function(key, autoTypecast, simplify, pyClass) {
	pyObj <- pyGetPoly(key, autoTypecast, simplify, "time")
	# See PythonInR::pyTransformReturn()
	pyObj <- pyObject(sprintf("__R__.namespace[%i]", pyObj$id))
	rObj <- set.time(pyObj)
	rObj$year <- 0
	rObj$mon <- 0
	rObj$mday <- 1
	rObj
})

setClass("date")
setMethod("pyGetPoly", signature(key = "character", autoTypecast = "logical", simplify = "logical", pyClass = "date"), function(key, autoTypecast, simplify, pyClass) {
	pyObj <- pyGetPoly(key, autoTypecast, simplify, "date")
	# See PythonInR::pyTransformReturn()
	pyObj <- pyObject(sprintf("__R__.namespace[%i]", pyObj$id))
	as.Date(pyObj$isoformat())
})

setClass("timedelta")
setMethod("pyGetPoly", signature(key = "character", autoTypecast = "logical", simplify = "logical", pyClass = "timedelta"), function(key, autoTypecast, simplify, pyClass) {
	pyObj <- pyGetPoly(key, autoTypecast, simplify, "timedelta")
	# See PythonInR::pyTransformReturn()
	pyObj <- pyObject(sprintf("__R__.namespace[%i]", pyObj$id))
	timedelta.to.difftime(pyObj)
})

sympy <- function(..., retclass = c("character", "Sym", "expr"), debug = FALSE) {
	retclass <- if (is.null(retclass)) NULL else match.arg(retclass)
	if (!is.null(retclass)) {
		pyExec("__Rsympy=None")
		pyExecp(paste("__Rsympy=", ..., sep = ""))
		if (debug) pyExecp("print(__Rsympy)")
		if (retclass == "expr") {
			pyExec("if isinstance(__Rsympy, Expr): __Rsympy = mathml(__Rsympy)\n")
			out <- pyGet("__Rsympy")
			# TODO: parse MathML
			out
		} else {
			pyExec("__Rsympy = str(__Rsympy)")
			out <- pyGet("__Rsympy")
			if (!is.null(out) && retclass == "Sym") structure(out, class = "Sym")
			else out
		}
	} else pyExecp(paste(...))
}

# if the returned value is.numeric, then there are no free symbols in x
sympySymbols <- function(x, debug = FALSE) {
	pyExec("__Rsympy=None")
	pyExecp(paste("__Rsympy=", x, sep = ""))
	if (debug) pyExecp("print(__Rsympy)")
	pyExec("if isinstance(__Rsympy, Expr): __Rsympy = [str(x) for x in list(__Rsympy.free_symbols)]\n")
	# PythonInR bug "IndexError: list index out of range" when pyGet an empty list
	pyExec("if len(__Rsympy) == 0: __Rsympy = [-1]\n")
	pyGet("__Rsympy")
}

sympyEvalf <- function(x, subs, retclass = c("character", "Sym", "expr")) {
	if (!missing(subs) && is.numeric(subs)) {
		pyDict("__Rsympy", subs, regFinalizer = FALSE) # immediately overwrite the dict so no need to del(__Rsympy)
		pyExecp(paste("__Rsympy=(", x, ").evalf(subs = __Rsympy)", sep = ""))
	} else {
		pyExec("__Rsympy = None")
		pyExecp(paste("__Rsympy=(", x, ").evalf()", sep = ""))
	}
	retclass <- if (is.null(retclass)) NULL else match.arg(retclass)
	if (!is.null(retclass)) {
		if (retclass == "expr") {
			pyExec("if isinstance(__Rsympy, Expr): __Rsympy = mathml(__Rsympy)\n")
			out <- pyGet("__Rsympy")
			# TODO: parse MathML
			out
		} else {
			pyExec("__Rsympy = str(__Rsympy)")
			out <- pyGet("__Rsympy")
			if (!is.null(out) && retclass == "Sym") structure(out, class = "Sym")
			else out
		}
	} else pyExecp("print(__Rsympy)")
}

sympyLambdify <- function(args, expr) {
	pyTuple("__Rsympy", args, regFinalizer = FALSE) # immediately overwrite the tuple so no need to del(__Rsympy)
	pyExecp(paste("__Rsympy=lambdify(__Rsympy,", expr, ")", sep = ""))
	pyFunction("__Rsympy")
}

executeLambda <- function(fn, args, retclass) {
	# redirect the call to the Python function handle
	args[[1]] <- fn
	# order of parameters matters, but formal parameter names don't matter
	#args <- unname(args)
	# execute the function call
	result <- eval(args)

	if (!is.null(retclass)) {
		if (retclass == "expr") {
			out <- pyCall("mathml", result)
			# TODO: parse MathML
			out
		} else {
			out <- pyCall("str", result)
			if (!is.null(out) && retclass == "Sym") structure(out, class = "Sym")
			else out
		}
	} else pyCall("print", result)
}

pythonHasVariable <- function(x) {
	pySet("__Rsympy", x)
	pyExec("__Rsympy = __Rsympy in locals() or __Rsympy in globals() or __Rsympy in vars(__builtins__)")
	pyGet("__Rsympy")
}

pythonHasFunction <- function(x) {
	if (!pythonHasVariable(x))
		return(FALSE)

	pySet("__Rsympy", x)
	pyExec("__Rsympy = hasattr(eval(__Rsympy), '__call__')")
	pyGet("__Rsympy")
}



