
sympyStart <- function() {

	# like system.file but on Windows uses \ in path rather than /
	system.file. <- function(...) {
		s <- system.file(...)
		if (.Platform$OS == "windows") gsub("/", "\\", s, fixed = TRUE) else s
	}

	if (!pyIsConnected()) pyConnect()

	pyExecp("import sys")
	pyExecp( paste( "sys.path.append(", system.file( "Lib", package = "rSymPy" ), ")", sep = '"' ) )
	pyExecp("from sympy import *")
	pyExecp("from sympy.mpmath import *")
	pyExecp("from sympy.printing.mathml import mathml")
	pyExecp("from sympy.utilities.lambdify import lambdify")
	pyExecp("from sympy.functions.special.gamma_functions import *")

	assign('.SympyConnected', TRUE)
}

sympy <- function(..., retclass = c("character", "Sym", "expr"), debug = FALSE) {
	if (!exists(".SympyConnected", .GlobalEnv)) sympyStart()
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
	if (!exists(".SympyConnected", .GlobalEnv)) sympyStart()
	pyExec("__Rsympy=None")
	pyExecp(paste("__Rsympy=", x, sep = ""))
	if (debug) pyExecp("print(__Rsympy)")
	pyExec("if isinstance(__Rsympy, Expr): __Rsympy = [str(x) for x in list(__Rsympy.free_symbols)]\n")
	# PythonInR bug "IndexError: list index out of range" when pyGet an empty list
	pyExec("if len(__Rsympy) == 0: __Rsympy = [-1]\n")
	pyGet("__Rsympy")
}

sympyEvalf <- function(x, subs, retclass = c("character", "Sym", "expr")) {
	if (!exists(".SympyConnected", .GlobalEnv)) sympyStart()
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
	if (!exists(".SympyConnected", .GlobalEnv)) sympyStart()
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



