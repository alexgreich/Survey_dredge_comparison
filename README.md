# Survey_dredge_comparison
Code to create a fishing power comparison between the new (Kodiak) dredge and the old (Homer) dredge.

Workign documents:
RMD_scallops_7_presentable is where I'm working on cleaning things to be... presentable.

RMD_scallops_7 is my last draft with experiemental code as well as the draft/outline write up.

A note on fishmethods::fmp(method=2). Method=2 commands the function to perform a randomized block ANOVA test.
I do not think it is working for me correctly. Just use lm() for this. More problematic is that after I run 
fmp(method=2), lm() NO LONGER WORKS CORRECTLY, even after unloading/reloading libraries and re-loading in data.
So do not run fmp(method=2) before lm(). The only way I've been able to fix it was by exiting Rstudio without saving the workspace and re-opening Rstudio.