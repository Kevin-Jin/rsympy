
rem This batch file only works on the author's system and is used
rem for copying jython and sympy to the rSymPy tree.
rem
rem To use edit the:
rem 1. 1st xcopy line to identify the sympy tree

:: The following xcopy arguments are used:
:: /e = recursively copy including empty directories
:: /i = target is directory to be created if not already present
:: /exclude = ignore compiled binaries
if not exist Lib md Lib
if not exist Lib\sympy md Lib\sympy
xcopy /e /i /exclude:kopy.exclude.txt C:\Python34\Lib\site-packages\sympy Lib\sympy
