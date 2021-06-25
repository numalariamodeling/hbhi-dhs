#!/bin/bash
#SBATCH -A b1139       # Allocation
#SBATCH -p b1139                 # Queue
#SBATCH -t 04:00:00             # Walltime/duration of the job
#SBATCH -N 1                    # Number of Nodes
#SBATCH --mem=64G               # Memory per node in GB needed for a job. Also see --mem-per-cpu
#SBATCH --ntasks-per-node=1
#SBATCH --mail-user=ifeoma.ozodiegwu@northwestern.edu
#SBATCH --mail-type=FAIL
#SBATCH --output=/home/ido0493/jobs/outputs/urban_malaria.out 
#SBATCH --error=/home/ido0493/jobs/errors/urban_malaria.err 
#SBATCH --job-name="urban_malaria_extract"       # Name of job





# unload any modules that carried over from your command line session
module purge

# add a project directory to your PATH (if needed)
#export PATH=$PATH:/projects/p20XXX/tools/

# load modules you need to use
module load R/4.0.3
module load gdal/3.1.3
module load proj/7.1.1
module load geos/3.8.1
module load udunits2/2.1.19



cd /home/ido0493/hbhi-dhs/nigeria_dhs/data_analysis/src/Research/urban_analysis/manuscript_scripts

# A command you actually want to execute:
R --vanilla -f 01_data_extractor.R
# Another command you actually want to execute, if needed:

