#!/bin/csh
#
    # WW: Last modified 31/01/2025, Julia Kukulies
# To run this script: ./get-era5.csh YYYY
#                e.g. ./get-era5.csh 2020
#
#

set echo

# Check for correct number of arguments
if ( ${#argv} < 1 ) then
    echo "Usage: ./get-era5.csh YYYY"
    exit 1
endif

set LOGINNAME   = $USER
set TOPDIR      = /glade/derecho/scratch/${LOGINNAME}
set WPSDIR      = /glade/derecho/scratch/$LOGINNAME/era5
set BINDIR      = /glade/u/home/wrfhelp/bin

# Get the year from the input argument
set current_year = $1
set current_cen = `echo $current_year | cut -c1-2`

# Debug: Print the year
echo "Processing year: $current_year"

# Function to get the number of days in a month
set days_in_month = (31 28 31 30 31 30 31 31 30 31 30 31)

# Check if the year is a leap year
@ is_leap_year = 0
if ( ($current_year % 4 == 0 && $current_year % 100 != 0) || ($current_year % 400 == 0) ) then
    @ is_leap_year = 1
    set days_in_month[2] = 29
endif

# Loop through each month
set current_month = 1
while ( $current_month <= 12 )
    # Get the number of days in the current month
    set last_day_of_month = $days_in_month[$current_month]

    # Debug: Print the current month and number of days
    echo "Processing month: $current_month with $last_day_of_month days"

    # Set the directories for the current month
    set month_str = `printf "%02d" $current_month`
    set MSSDIR = /glade/campaign/collections/rda/data/ds633.0/e5.oper.an.pl/${current_year}${month_str}
    set MSSDIR2 = /glade/campaign/collections/rda/data/ds633.0/e5.oper.an.sfc/${current_year}${month_str}
    set MSSDIR0 = /glade/campaign/collections/rda/data/ds633.0/e5.oper.invariant/197901

    # Change to the working directory
    cd $WPSDIR

    # Obtain the time-invariant file: terrain height and land mask
    if ( ! -e e5.oper.invariant.128_129_z.ll025sc.1979010100_1979010100.grb ) then
        ln -sf $MSSDIR0/e5.oper.invariant.128_129_z.ll025sc.1979010100_1979010100.grb .
    endif

    if ( ! -e e5.oper.invariant.128_172_lsm.ll025sc.1979010100_1979010100.grb ) then
        ln -sf $MSSDIR0/e5.oper.invariant.128_172_lsm.ll025sc.1979010100_1979010100.grb .
    endif

    # Loop through each day in the month
    set current_day = 1
    while ( $current_day <= $last_day_of_month )
        # Loop through each hour (00 and 12)
        foreach current_hour ( 00 )
            # Construct the DataTime variable
            set day_str = `printf "%02d" $current_day`
            set DataTime = ${current_year}${month_str}${day_str}${current_hour}

            # Debug: Print the current date and time
            echo "Processing: $DataTime"

            # Process upperair data
            set file_time_s = ${current_year}${month_str}${day_str}00
            set file_time_e = ${current_year}${month_str}${day_str}23
            set data_type = e5.oper.an.pl.128
            set data_type_uv = ll025uv
            set data_type_sc = ll025sc

            ln -sf $MSSDIR/${data_type}_129_z.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR/${data_type}_130_t.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR/${data_type}_131_u.${data_type_uv}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR/${data_type}_132_v.${data_type_uv}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR/${data_type}_157_r.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR/${data_type}_133_q.${data_type_sc}.${file_time_s}_${file_time_e}.grb .

            # Process surface data
            set file_time_s = ${current_year}${month_str}0100
            set file_time_e = ${current_year}${month_str}${last_day_of_month}23
            set data_type_sfc = e5.oper.an.sfc.128

            ln -sf $MSSDIR2/${data_type_sfc}_034_sstk.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_039_swvl1.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_040_swvl2.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_041_swvl3.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_042_swvl4.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_139_stl1.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_170_stl2.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_183_stl3.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_236_stl4.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_165_10u.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_166_10v.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_167_2t.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_168_2d.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_134_sp.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_151_msl.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_235_skt.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_033_rsn.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
            ln -sf $MSSDIR2/${data_type_sfc}_141_sd.${data_type_sc}.${file_time_s}_${file_time_e}.grb .
        end

        # Increment the day
        @ current_day++
    end

    # Increment the month
    @ current_month++
end
