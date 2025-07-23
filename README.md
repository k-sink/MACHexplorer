# MACH Explorer 

## Overview
The MACH Explorer is a desktop application that enables users to evaluate and manipulate the MACH dataset. Developed by Katharine Sink, it leverages [Shiny](https://shiny.posit.co/) 
for an interactive interface. The MACH dataset is available at [zenodo](https://zenodo.org/records/15311986). This dataset contains daily climate data along with catchment attributes for 1,014 watersheds within the United States. Climate forcing data includes precipitation, 
minimum air temperature, maximum air temperature, mean air temperature, snow water equivalent, shortwave radiation, water vapor pressure, day length, potential evapotranspiration, and actual evapotranspiration. 
Hydrological data includes daily observed streamflow. Data coverage spans from January 1, 1980 to December 31, 2023. Catchment attribute categories are land cover, hydrology, geology, soil, regional, climate indices, 
and anthropogenic. MOPEX data is also included for 395 of the watersheds for January 1, 1948 to December 31, 1979 for precipitation, minimum and maximum air temperature, and observed streamflow. Basin identifiers are 
consist with the MOPEX and CAMELS datasets, based on USGS stream gaging stations.  

## Installation
1. Download the `MACH_Explorer_Installer.exe` from the [GitHub releases page](https://github.com/k-sink/MACHexplorer/releases).
2. Run the installer and follow the on-screen instructions.
3. The app will be installed to `%LocalAppData%\MACH Explorer App`.

## Requirements
- Windows operating system.
- No additional software required (includes portable R and Chrome).

## Support
- Report issues or suggest features at [https://github.com/k-sink/MACHexplorer/](https://github.com/k-sink/MACHexplorer/).
- Contact: katharine.sink@utdallas.edu

## License
This project is licensed under the MIT License - see the `LICENSE` file for details.

## Usage
1. Launch the app by double-clicking `run.bat` in the installation directory or by double-clicking on the desktop shortcut.
2. Download and save the `full_dataset.duckdb` database file from the [Github releases page](https://github.com/k-sink/MACHexplorer/releases).
3. Use the interface to filter and explore the MACH dataset.

### Data Import
The app uses a database management system called [DuckDB](https://duckdb.org/), which is portable and provides APIs for languages such as R. Prior to using the app, 
download the `full_dataset.duckdb` database file. When the app is launched, it will open on the **Data Import** tab (Figure 1). Use the **_Browse_** button to located the database file on 
your local machine. 

  <img width="1280px" height="640px" alt="Image" src="https://github.com/user-attachments/assets/f9ac815b-3344-4536-a3cd-ef74bd2c2bc9" />
*Figure 1: Data Import landing page* 


All tabs will be disabled until a connection is established with the database file. A progress bar will be displayed while the app is connecting to the database file. Once the app has connected successfully, 
a green Connected to Database status message will be displayed (Figure 2). 

