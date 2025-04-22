# WaterConservation_ClimateChange_ABM
This is the repository for the data and code associated with an agent-based model (ABM) that models water consumption under climate change in multiple Southwestern US cities.

Results from this analysis are currently under review. 

A permanent version of this repository is available on Zenodo: [![DOI](https://zenodo.org/badge/866216695.svg)](https://doi.org/10.5281/zenodo.15262284).

The ABM was developed in NetLogo (v6.2.2) and was last run on 31 March 2025. The analysis was conducted using two separate NetLogo files, one for the observation-based model (`observationanalysis.nlogo`) and one for the future analysis under climate change (`climatechangeanalysis.nlogo`). Running the NetLogo code requires several extensions: 

* array
* csv
* time

The results from running the ABM are located in the `abmresults` folder. Additionally, there are multiple csv files that are required as inputs, which can be found in the `observedData` and `GCMdata` folders. The NetLogo code references these folders, so users can run the code directly without needing to change directory paths.

The model runs best in the headless form of NetLogo, as it requires substantial computational resources. The code was developed on a Linux server using the SLURM workload manager. Users working in different systems or with different workload managers may need to adapt the code to meet their system requirements (see https://slurm.schedmd.com/rosetta.html for a guide for translating between SLURM and other workload managers). To run the code on a remote system, we recommend the following code. 

First, copy the data over to the remove machine:

```shell
scp -r WaterConservation_ClimateChange_ABM [remote machine address]:[remote machine path]
```

Or, use git commands directly on the remote machine: 

```shell
git clone https://github.com/reneeobringer/WaterConservation_ClimateChange_ABM
```

Once the repository is loaded on the remote machine, run `jobscript.sh`:

**will need to update to specify any changes needed in jobscript.sh**

```shell
sbatch --partition=[your partition here] --output=jobscript.out --time=14-00:00:00 jobscript.sh
```

Note that the climate change analysis code is set up to run 1200 experiments for different GCMs, climate change scenarios, and policy interventions. We ran the code with four standard memory nodes (80 total CPUs) and it took just under two weeks to complete all 1200 runs. 

Finally, there is are three supplemental R files associated with the NetLogo script that contain the calculations of the parameters for various distributions used in this analysis, as well as the post-processing and plotting of the modeled data. These files do not need to be run in order run the NetLogo script, but can be used as a reference for the inputs, as well as viewing code for plotting figures. To run this script, users will need to update the `path` variable to the downloaded or cloned respository. Additionally, the code can be run sequentially, or individual sections can be run separately, if users load the r data files prior to running. These files are located in the `rdatafiles` folder. The data used within this R script is in the `ObservedData` folder.
