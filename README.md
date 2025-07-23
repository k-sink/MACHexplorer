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
To start the MACH Explorer, locate the installation directory and double-click `run.bat` or double-click the desktop shortcut if you elected to create one during app installation. 
- **What You’ll See**: When you run `run.bat`, a black Command Prompt window will appear with a message like "listening..." (*Figure 1*). This indicates that the app is starting and preparing to display its interface. The window must remain open for the app to work.
- **Browser Opening**: After a moment, the app will open in a web browser window (using a built-in Chrome instance). You can begin using the app from there.
- **Keeping the Command Window Open**: Do not close the Command Prompt window while using the app, as this will stop the app. To exit, simply close the browser window or click the "Stop" button if provided, then close the Command Prompt window.
- **Troubleshooting**: If the browser doesn’t open or the "listening..." message doesn’t appear, ensure all files are correctly installed and try running `run.bat` again. Check the Command Prompt for any error messages if issues persist.
<img width="1111" height="251" alt="Image" src="https://github.com/user-attachments/assets/9bf03e0a-e4b2-4351-bab6-f52d6db2b9c9" />
<i>Figure 1: Command prompt window that connects to Chrome browser.</i> 

## Getting Started
Users can browse and retrieve time series and/or attribute data for up to 1,014 catchments. 
1. Connect the app to the database file on the **Data Import** tab. 
2. Select sites on the **Site Selection** tab. All tabs in the app retrieve data for the sites selected here. 
3. After selecting sites, any of the following tabs can be used, independently of the others. Retrieve time series data for selected sites using the **Daily Data**, **Monthly Data**, and/or **Annual Data** tabs. Retrieve attribute data for selected sites using the **Attributes** and/or **Land Cover** tabs. Retrieve original MOPEX data for selected sites using the **MOPEX Data** tab. Note that the **_Retrieve and View Data_** buttons essentially query the database and must be pressed any time changes are made to filtering criterion or to site selections. 

### Data Import
The app uses a database management system called [DuckDB](https://duckdb.org/), which is portable and provides APIs for languages such as R. Prior to using the app, 
download the `full_dataset.duckdb` database file from the [Github releases page](https://github.com/k-sink/MACHexplorer/releases). When the app is launched, it will open on the **Data Import** tab (*Figure 2*). Use the **_Browse_** button to locate the database file on 
your local machine. 

<img width="1286" height="464" alt="Image" src="https://github.com/user-attachments/assets/322b8729-cd46-4734-8682-073d4dd8f250" />
<i>Figure 2: Data Import landing page.</i> 
<br></br>

All tabs will be disabled until a connection is established with the database file. A purple progress bar will be displayed while the app is connecting to the database file. Once the app has connected successfully, 
a green "Connected to Database" status message will be displayed (*Figure 3*).   

<img width="1286" height="618" alt="Image" src="https://github.com/user-attachments/assets/41ba74c0-c4f7-49ab-8647-3a1ede76e5f0" />
<i>Figure 3: Data Import page after database connection is established.</i>
<br></br>

### Site Selection
After the database connection is established, all additional tabs will be available. The **Site Selection** dashboard contains a location map for all 1,014 watersheds from the MACH dataset along with filtering options including 
spatial criterion (state, latitude, longitude) and general catchment attributes (mean elevation, drainage area, slope). <ins>All subsequent tabs in the application are dependent on the sites selected on this page. 
Sites selections are retained throughout the application.</ins> The dashboard consists of **"Filter Sites"**, **"Edit Individual Sites"**, **"USGS Station Locations"**, **"Discharge Record"**, and **"Selected Sites"** (*Figure 4*). The tables and map update 
in real time as filters are adjusted and represent the sites that will be used for data retrieval on subsequent tabs. 

<img width="1030" height="726" alt="Image" src="https://github.com/user-attachments/assets/04bf3eaa-e8d2-4a96-a1f1-def3ed1f9141" />
<i>Figure 4: Site Selection user interface.</i>
<br></br>

The **"Filter Sites"** box, *Figure 5*, contains spatial filters. To enable a selection, click in the checkbox. Clicking on the *State* box will display a drop-down menu. To select a state, click the name to add and to remove, click in the box and then use backspace. More than one state can be selected at a time. Slider bars can be used to determine a range of values for each numeric attribute, once it is selected. The ranges default to cover all possible values on record. *Latitude (N)* is decimal degrees north, *Longitude (W)* is decimal degrees west, *Mean Elevation (m)* is mean elevation above sea level in meters, *Drainage Area (km2)* is basin drainage area in square kilometers, and *Mean Slope (percent)* is overall mean basin slope in percent. The **_Reset Filters_** button will clear all selected filters and display all 1,014 sites. This button can be pressed at any time. 
  
<img width="464" height="553" alt="Image" src="https://github.com/user-attachments/assets/b7440c8a-d770-40e3-b5e4-b20e7942931f" />

<i>Figure 5: Spatial filtering options include state, latitude, longitude, mean elevation, drainage area, and slope.</i>
<br></br>


Individual sites can be added or removed based on user preference using the **"Edit Individual Sites"** box. For example, if a watershed is missing streamflow data for a specific period, it can be deleted from the selections using the 8-digit site number, shown in *Figure 6*. To remove a site, manually type the site number into the bottom box and then click the **_Remove Site_** button. To add a site, manually type the site number into the top box and then click the **_Add Site_** button. Leading zeroes should be included, if applicable 
to the site number. The **_Remove All Sites_** button will clear all selections, resulting in a blank display and tables. Individual sites can then be manually added. The **"Edit Individual Sites"** box lets you choose exactly which basins you want to obtain data from.

<img width="459" height="467" alt="Image" src="https://github.com/user-attachments/assets/a0840430-4646-4a6c-84c7-cf4696db4abb" />

<i>Figure 6: Individual sites can be added or removed from the filtered selections.</i>
<br></br>


The leaflet map **"USGS Station Locations"** displays the sites listed in the **"Selected Sites"** table and updates as filtering criterion are changed. The blue points represent the USGS stream gauging station locations. The default base map is *OpenStreetMap*, which includes geographical features such as roads, buildings, and trails. The basemap can be changed to *EsriTopo*, which provides labelled topographic contours and hillshade. Basin delineations can be toggled on and off by checking the *Basin Delineations* box. 
can be displayed by clicking the checkbox (*Figure 7*). The polygons represent the basin drainage area. 

<img width="3213" height="1646" alt="Image" src="https://github.com/user-attachments/assets/d64002b3-1799-4f74-85f0-09d1238448fb" />
<i>Figure 7: USGS stream gauging site locations along with basemap options.</i>
<br></br>

A point can be clicked on in the map to display a pop-up box containing the site number, site name, and coordinates (*Figure 8*).  
<img width="896" height="406" alt="Image" src="https://github.com/user-attachments/assets/6856c0f9-fcac-4061-adcd-49d4347e0c76" />
<i>Figure 8: Pop-up information box.</i>
<br></br>


Corresponding information for the filtered watersheds are included in two separate tables. Since not all basins have a complete streamflow record, the **"Discharge Record"** table (*Figure 9*) displays selected sites and the 
number of daily streamflow observations on record. The columns include the site number (*SITENO*), total number of records (*count_rec*), the first available date (*first_date*), the last available date (*last_date*), 
and the number of streamflow records for each calendar year. Leap years will contain 366 days. All sites in the MACH dataset have a minimum of 10 years of streamflow data. A complete record will contain 16,071 days. 

<img width="930" height="723" alt="Image" src="https://github.com/user-attachments/assets/ae7f8543-c6d5-4d12-9831-78a92d7065c6" />

<i>Figure 9: Streamflow records per site. The number of displayed records can be changed using the dropdown (5, 10, 20, 50). The search bar allows numbers and characters. Columns can be ordered using the diamond buttons near the header.</i>
<br></br>

The **"Selected Sites"** table displays sites based on **"Filter Sites"** and **"Edit Individual Sites"** information. The table includes the USGS site number (*SITENO*), site name (*NAME*), state (*STATE*), latitude in decimal degrees north (*LAT*), longitude
in decimal degrees west (*LONG*), mean elevation in meters above sea level (*ELEV*), basin drainage area in square kilometers (*AREA*), and overall mean basin slope (*SLOPE*) in percent (*Figure 10*). 

<img width="930" height="723" alt="Image" src="https://github.com/user-attachments/assets/60109517-1464-49cd-a5c4-0ab59362de1c" />

<i>Figure 10: Selected sites with SITENO, stream gauging site name (NAME), latitude (LAT), longitude (LONG), elevation (ELEV), drainage area (AREA), and mean overall slope (SLOPE).</i>
<br></br>

### Daily Data
Once the filtered basin selections are made, you have the option to evaluate data at the daily, monthly, and/or annual scale. For daily data, shown in *Figure 11*, each variable can be filtered by a range of values after the variable is selected. 
To select a variable, check the box under *"Select Variable(s)"*. Once selected, minimum and maximum value boxes will be displayed. If you want all possible values, you do not need to make any adjustments. The default values cover the range among the 1,014 sites. 
Variable abbreviations and units are: precipitation (*PRCP*) in millimeters per day, mean air temperature (*TAIR*), minimum air temperature (*TMIN*), and maximum air temperature (*TMAX*) in degrees Celcius, potential evapotranspiration (*PET*) in millimeters per day, actual evapotranspiration (*AET*) in millimeters per day, stream discharge (*OBSQ*) in millimeters per day, snow water equivalent (*SWE*) in  millimeters per day, shortwave radiation (*SRAD*) in watts per square meter, water vapor pressure (*VP*) Pascals, and day length (*DAYL*) in seconds per day. The data can also be filtered temporally. The *"Date Range"* enables you to select a beginning and ending date. The *"Calendar Year"* and *"Month"* can also be selected using the dropdown menu to further refine the temporal range. Calendar year is January 1 to December 31. Multiple calendar year and/or month selections can be made and subsequently removed by using backspace. If date range and calendar year are both selected, the calendar year must fall within the established range. An error message will be displayed if this occurs, which you can dismiss. No data will be displayed or queried from the database until the **_Retrieve and View Data_** button is pressed. The **_Reset Filters_** button will clear all selected variables and values. 


<img width="254" height="726" alt="Image" src="https://github.com/user-attachments/assets/e161f508-ae76-4fbb-8581-e2cc48e7c129" />

<i>Figure 11: Available climate variables in MACH. Daily data can be filtered by value range, date range, calendar year, and/or calendar month.</i>
<br></br>

The **"Filtered Daily Data"** table header will show the selected number of sites (*Figure 12*). If this number is not consistent with the **Site Selection** tab, make sure you have pressed the **_Retrieve and View Data_** button again to update the query. 
The table will display the selected variables and specified filters for the first 1,000 records only. All data will be available upon download. If you want to view data for a specific site before downloading, select that single site on the **Site Selection** tab. Again, 
only the first 1,000 records will be available for preview. The data preview is limited for this tab due to the large volume of available data (+16 million rows). 


<img width="3198" height="2487" alt="Image" src="https://github.com/user-attachments/assets/3a2d4dd6-397e-4b46-900f-5e20f9e9b99f" />






