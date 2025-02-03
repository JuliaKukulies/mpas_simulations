"""

This script links ERA5 files from RDA on a monthly basis to the wps directory and ungribs
the files using ungrib.exe. A new job is created once the intermediate files for that month have
been successfully created. 

Email: kukulies@ucar.edu
Created: February 1st, 2025 

"""
import time
import calendar
import re
import glob
import os
import subprocess
from datetime import datetime, timedelta

# Configuration
base_dir = "/glade/work/kukulies/wpsv4.6.0"
grb_files_dir = "/glade/derecho/scratch/kukulies/era5"  # Directory containing the symbolic links to the .grb files (from RDA)
output_dir = "/glade/derecho/scratch/kukulies/era5"
namelist_path = os.path.join(base_dir, "namelist.wps")
batch_script_path = os.path.join(base_dir, "wps.sh")
link_grb_script_path = os.path.join(base_dir, "link_grib.csh")
start_year = 2020
start_month = 12
end_month = 12

# Function to extract the current start_date and end_date from namelist.wps
def extract_dates():
    with open(namelist_path, "r") as file:
        namelist = file.read()

    # Use regex to find start_date and end_date
    start_date_match = re.search(r"start_date\s*=\s*'([^']+)'", namelist)
    end_date_match = re.search(r"end_date\s*=\s*'([^']+)'", namelist)

    if not start_date_match or not end_date_match:
        raise ValueError("Could not find start_date or end_date in namelist.wps")

    current_start_date = start_date_match.group(1)
    current_end_date = end_date_match.group(1)

    return current_start_date, current_end_date

# Function to update the namelist.wps file
def update_namelist(year, month):
    # Calculate start and end dates for the month
    start_date = datetime(year, month, 1, 0, 0 )
    if month == 12:
        end_date = datetime(year + 1, 1,1,  0 ,0 )
    else:
        end_date = datetime(year, month + 1, 1, 0 , 0) 

    # Format dates as strings
    start_date_str = start_date.strftime("%Y-%m-%d_%H:%M:%S")
    end_date_str = end_date.strftime("%Y-%m-%d_%H:%M:%S")

    # Extract the current start_date and end_date from namelist.wps
    current_start_date, current_end_date = extract_dates()

    # Read the namelist.wps file
    with open(namelist_path, "r") as file:
        namelist = file.read()

    # Replace current start_date and end_date with new values
    namelist = namelist.replace(
        f"start_date = '{current_start_date}'",
        f"start_date = '{start_date_str}'"
    )
    namelist = namelist.replace(
        f"end_date   = '{current_end_date}'",
        f"end_date   = '{end_date_str}'"
    )

    # Write the updated namelist back to the file
    with open(namelist_path, "w") as file:
        file.write(namelist)

    print(f"Updated namelist.wps with start_date = '{start_date_str}' and end_date = '{end_date_str}'", flush = True)

    
# Function to link .grb files using link_grb.csh
def link_grb_files(year, month):
    month_str = f"{year:04d}{month:02d}"
    file_pattern = str(grb_files_dir) + f"/e5*{month_str}*"

    # Run the link_grb.csh script using tcsh and shell=True for wildcard expansion
    command = f"csh {link_grb_script_path} {file_pattern}"
    result = subprocess.run(command, cwd=base_dir, shell=True, check=True)

    if result.returncode == 0:
        print(f"Linked .grb files for {year:04d}-{month:02d}")
    else:
        raise RuntimeError(f"Failed to link .grb files for {year:04d}-{month:02d}")

# Function to submit the batch job
def submit_batch_job():
    subprocess.run(["qsub", batch_script_path])

def check_output_files(year, month):
    # Get the last day of the month
    last_day = calendar.monthrange(year, month)[1]
    last_day_str = f"{year:04d}-{month:02d}-{last_day:02d}"

    # Construct the file pattern for the last day
    file_pattern = os.path.join(output_dir, f"ERA5:{last_day_str}*")

    # Count the number of files matching the pattern
    output_files = glob.glob(os.path.join(output_dir, f"ERA5:{last_day_str}*" ))
    num_files = len(output_files)

    # Check if there are 8 files (3-hourly data for the last day)
    if num_files == 8:
        print(f"Output files for {year:04d}-{month:02d} are complete.")
        return True
    else:
        print(f"Waiting for output files for {year:04d}-{month:02d}... (found {num_files}/8 files for the last day)")
        return False

def cleanup_grib_links():
    # Find all files matching the pattern GRIBFILE.*
    grib_files = glob.glob(os.path.join(base_dir, "GRIBFILE.*" ))

    if len(grib_files) > 0:
        # Remove each file
        for file in grib_files:
            try:
                os.remove(file)
            except OSError as e:
                print(f"Error removing file {file}: {e}")

    else:
        print("no grib links found. ", flush = True)
                
###### MAIN PROGRAM ######
current_year, current_month = start_year, start_month
while (current_month <= end_month):
    print(f"Processing {current_year:04d}-{current_month:02d}", flush = True)

    # clean current grib links in WPS directory
    cleanup_grib_links()
    
    # Step 1: Link .grb files
    link_grb_files(current_year, current_month)

    # Step 2: Update namelist.wps
    update_namelist(current_year, current_month)

    # Step 3: Submit batch job
    submit_batch_job()

    # Step 4: Wait for output files to be created
    while not check_output_files(current_year, current_month):
        print("Waiting for output files...")
        time.sleep(1800)  # Check every minute

    print(f"Output files for {current_year:04d}-{current_month:02d} created.", flush= True)

    # Move to the next month
    #if current_month == 12:
    #    current_year += 1
    #    current_month = 1
    #else:
    current_month += 1

print("All months processed.", flush = True)
