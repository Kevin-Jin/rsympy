
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
	pyExecp("from sympy.printing.mathml import mathml")
	pyExecp("from sympy.utilities.lambdify import lambdify")
	pyExecp("from sympy.functions.special.gamma_functions import *")

	assign('.SympyConnected', TRUE)
}

sympy <- function(..., retclass = c("character", "Sym"), debug = FALSE) {
	if (!exists(".SympyConnected", .GlobalEnv)) sympyStart()
	retclass <- if (is.null(retclass)) NULL else match.arg(retclass)
	if (!is.null(retclass)) {
		pyExec("__Rsympy=None")
		pyExecp(paste("__Rsympy=", ..., sep = ""))
		if (debug) pyExecp("print(__Rsympy)")
		pyExec("__Rsympy = str(__Rsympy)")
		out <- pyGet("__Rsympy")
		if (!is.null(out) && retclass == "Sym") structure(out, class = "Sym")
		else out
	} else pyExecp(paste(...))
}

sympySymbols <- function(x, debug = FALSE) {
	if (!exists(".SympyConnected", .GlobalEnv)) sympyStart()
	pyExec("__Rsympy=None")
	pyExecp(paste("__Rsympy=", x, sep = ""))
	if (debug) pyExecp("print(__Rsympy)")
	pyExec("if isinstance(__Rsympy, Expr): __Rsympy = [str(x) for x in list(__Rsympy.atoms(Symbol))]\n")
	pyGet("__Rsympy")
}


