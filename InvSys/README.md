# Inverse System
## Contents
* Overview
* Repo Contents
* System Requirements
* User Guide
* Demo
* Results
* License
# Overview
This resource is a demo code for an inversion system. It contains a module for processing a priori information, a module for flux inversion and a module for processing a posteriori fluxes. All these codes were used to generate the results based on the atmospheric inversion method in the paper "Toward Robust Estimates of Net Ecosystem Exchanges in Mega-Countries using GOSAT and OCO-2 Observations ".
# Repo Contents
* preInv: The preInv folder contains the code to process the a priori fluxes and observations to generate the matrices needed for the inversion.
  * preInv/Inversion_input: Contains example data for a priori fluxes, model simulations, and observations.
* Inv_code: Includes inverse process execution code.
* PostInvv: Contains code that processes the a posteriori flux.
* Result: Example of inversion results.
* run_inv: Example of an inverse system execution.
* test.rc: namelist file.
# System Requirements
## Hardware Requirements
When the number of observations and partitions is small and the inversion duration is short, a regular PC can be used to meet the processing requirements. However, in most cases, we may need a computer with higher hardware configuration for inversion to meet the computational needs. The server used in this study has the following specifications:
* RAM: 62G.
* CPU: Intel(R) Xeon(R) CPU E5-2667 v3 @ 3.20GHz.
* GPU: Matrox Electronics Systems Ltd. MGA G200EH.
## Software Requirements
* The code has been tested on the following OS systems:
  * Red Hat Enterprise Linux Server release 6.8 (Santiago).
  * Red Hat Enterprise Linux Server release 7.7 (Maipo).
* The code is implemented in Fortran (Intel Fortran Compiler 13.1.0).
# User Guide
We provide an example run_inv to run the code. It starts by compiling preInv to generate preInv.exe and postInv to generate PostInv.exe. Then you can run the model by referring to the run_inv configuration file.
# Demo
We provide example data and inversion results. The example data is stored in the preInv/Inversion_input directory. The directory contains:
* Observation information (mz4_gosatv9_5x5 directory).
* Partitions used for inversion (region directory).
* A priori flux data (15-19ocn_flux_test.inv and BEPSnee_monthregion2015-2019_0901.txt).
* A priori flux uncertainty files (npp_priori_uncert.inv and SD_priori_uncert.inv).
Using the above example data, execute the compiled program to complete the inversion.
# Result
The inversion results using the example data are stored in the Results folder.
# Usage License
This code is provided for academic research purposes only and may not be used for commercial purposes. Any commercial use, redistribution, or modification of the code requires prior written permission from the project maintainers.
If you would like to use the code for purposes other than academic research, please contact the project maintainers for further authorization.


