# MACH Explorer 

## Overview
The **MACH Explorer** is a desktop application that enables users to evaluate and manipulate the **MACH** dataset (Sink, 2025). 
Developed by Katharine Sink, **MACH Explorer** leverages [Shiny](https://shiny.posit.co/) for an interactive interface. The **MACH** dataset is available at [zenodo](https://zenodo.org/records/16423231). **MACH** contains 
daily climate data along with catchment attributes for 1,014 watersheds within the United States. Climate forcing data includes precipitation, minimum air temperature, maximum air temperature, mean air temperature, 
snow water equivalent, shortwave radiation, water vapor pressure, day length, potential evapotranspiration, and actual evapotranspiration. Hydrological data includes daily observed streamflow. Data coverage spans 
from January 1, 1980 to December 31, 2023. Catchment attribute categories are land cover, hydrology, geology, soil, regional, climate indices, and anthropogenic. MOPEX data is also included for 395 of the 
watersheds for January 1, 1948 to December 31, 1979 for precipitation, minimum and maximum air temperature, and observed streamflow. Basin identifiers are consist with the MOPEX and CAMELS datasets, based on USGS stream gauging stations.  This repository 
contains the **MACH Explorer** executable and the _full_dataset.duckdb_ database file under **Releases**. 

If you use this software, please cite: Katharine Sink. (2025). k-sink/MACHexplorer: MACH Explorer version 1.0 (v1.0-machexplorer). Zenodo. https://doi.org/10.5281/zenodo.16585881

## Installation
1. Download the `MACH_Explorer_Installer.exe` from the [GitHub releases page](https://github.com/k-sink/MACHexplorer/releases).
2. Run the installer and follow the on-screen instructions. 
3. The app will be installed to `%LocalAppData%\MACH Explorer App`.

Launch the installer to begin setup. Click "Next" to install in default location. 

<img width="497" height="395" alt="Image" src="https://github.com/user-attachments/assets/61f5a220-5147-4147-9a33-593e77b03835" /> 
<br></br>

Click in the checkbox next to the "Create a desktop shortcut" if you want to create one, then click "Next". 

<img width="504" height="390" alt="Image" src="https://github.com/user-attachments/assets/9db1275c-d043-43b3-8454-5c818ec42907" />
<br></br>

The application is ready to install. Confirm the folder location and additional tasks, then click "Install". 

<img width="496" height="391" alt="Image" src="https://github.com/user-attachments/assets/a97c5ec7-d961-47e3-a855-ec97e6a1bcaa" />
<br></br>

The application includes a portable R and portable Chrome browser, creating a self-contained Shiny executable. Click "Finish" to exit the setup. MACH Explorer is now ready! 

<img width="502" height="395" alt="Image" src="https://github.com/user-attachments/assets/0a119881-4e94-4d53-9178-cf16da81399b" />
<br></br>

## Requirements
- Windows operating system.
- No additional software required (includes portable R and Chrome).
- Approximately 1.02 GB total. 

## Support
- Report issues or suggest features at [https://github.com/k-sink/MACHexplorer/](https://github.com/k-sink/MACHexplorer/).
- Contact: katharine.sink@utdallas.edu
<br></br>
Please note that I am not a software developer ... just a doctoral student who created an app. 😺

## License
This project is licensed under the MIT License - see the `LICENSE` file for details.

## Usage
To start the MACH Explorer, locate the installation directory and double-click `run.bat` or double-click the desktop shortcut if you elected to create one during app installation. 
- **What You’ll See**: When you run `run.bat`, a black Command Prompt window will appear with a message like "Listening..." (*Figure 1*). This indicates that the app is starting and preparing to display its interface. You do not need to do anything in the window. The window must remain open for the app to work.
- **Browser Opening**: After a moment, the app will open in a web browser window (using a built-in Chrome instance). You can begin using the app from there.
- **Keeping the Command Window Open**: Do not close the Command Prompt window while using the app, as this will stop the app. To exit, simply close the browser window or click the "Stop" button if provided, then close the Command Prompt window.
- **Troubleshooting**: If the browser doesn’t open or the "Listening..." message doesn’t appear, ensure all files are correctly installed and try running `run.bat` again. Check the Command Prompt for any error messages if issues persist.
<img width="1111" height="251" alt="Image" src="https://github.com/user-attachments/assets/9bf03e0a-e4b2-4351-bab6-f52d6db2b9c9" />
<i>Figure 1: Command prompt window that connects to Chrome browser and launches the R script.</i> 

## Getting Started
Users can browse and retrieve time series and/or attribute data for up to 1,014 catchments. 
1. Connect the app to the database file on the **Data Import** tab. 
2. Select sites on the **Site Selection** tab. All tabs in the app retrieve data for the sites selected here. 
3. After selecting sites, any of the following tabs can be used, independently of the others. Retrieve time series data for selected sites using the **Daily Data**, **Monthly Data**, and/or **Annual Data** tabs. Retrieve attribute data for selected sites using the **Attributes** and/or **Land Cover** tabs. Retrieve original MOPEX data for selected sites using the **MOPEX Data** tab. Note that the **_Retrieve and View Data_** buttons essentially query the database and must be pressed any time changes are made to filtering criterion or to site selections. 
<br></br>

### Data Import
The app uses a database management system called [DuckDB](https://duckdb.org/), which is portable and provides APIs for languages such as R. Prior to using the app, download the `full_dataset.duckdb` database file 
from the [Github releases page](https://github.com/k-sink/MACHexplorer/releases). The database file contains the same information available in the zenodo release, consolidated for efficient querying.  When the app is launched, it will open 
on the **Data Import** tab (*Figure 2*). Use the **_Browse_** button to locate the database file on your local machine. 

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
<i>Figure 4: Site Selection dashboard.</i>
<br></br>

The **"Filter Sites"** box, *Figure 5*, contains spatial filters. To enable a selection, click in the checkbox. Clicking on the *State* box will display a drop-down menu. To select a state, click the name to add and to remove, click in the box and then use backspace. More than one state can be selected at a time. Slider bars can be used to determine a range of values for each numeric attribute, once it is selected. The ranges default to cover all possible values on record. *Latitude (N)* is decimal degrees north, *Longitude (W)* is decimal degrees west, *Mean Elevation (m)* is mean elevation above sea level in meters, *Drainage Area (km2)* is basin drainage area in square kilometers, and *Mean Slope (percent)* is overall mean basin slope in percent. The **_Reset Filters_** button will clear all selected filters and display all 1,014 sites. This button can be pressed at any time. 
  
<img width="464" height="553" alt="Image" src="https://github.com/user-attachments/assets/b7440c8a-d770-40e3-b5e4-b20e7942931f" />

<i>Figure 5: Spatial filtering options include state, latitude, longitude, mean elevation, drainage area, and slope.</i>
<br></br>


Individual sites can be added or removed based on user preference using the **"Edit Individual Sites"** box. For example, if a watershed is missing streamflow data for a specific period, it can be deleted from the selections using the 8-digit site number, shown in *Figure 6*. To remove a site, manually type the site number into the bottom box and then click the **_Remove Site_** button. To add a site, manually type the site number into the top box and then click the **_Add Site_** button. Leading zeroes should be included, if applicable 
to the site number. The **_Remove All Sites_** button will clear all selections, resulting in a blank display and tables. Individual sites can then be manually added. The **"Edit Individual Sites"** box lets you customize exactly which basins you want to obtain data from.

<img width="459" height="467" alt="Image" src="https://github.com/user-attachments/assets/a0840430-4646-4a6c-84c7-cf4696db4abb" />

<i>Figure 6: Individual sites can be added or removed from the filtered selections.</i>
<br></br>


The leaflet map **"USGS Station Locations"** displays the sites listed in the **"Selected Sites"** table and updates as filtering criterion are changed. The blue points represent the USGS stream gauging station locations. The default base map is *OpenStreetMap*, which includes geographical features such as roads, buildings, and trails. The basemap can be changed to *EsriTopo*, which provides labelled topographic contours and hillshade. Basin delineations can be toggled on and off by checking the *Basin Delineations* box (*Figure 7*). The polygons represent the basin drainage area. 

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

The **"Selected Sites"** table (*Figure 10*) displays sites based on **"Filter Sites"** and **"Edit Individual Sites"** information. The table includes the USGS site number (*SITENO*), site name (*NAME*), state (*STATE*), latitude (*LAT*), longitude
(*LONG*), mean elevation (*ELEV*), basin drainage area (*AREA*), and overall mean basin slope (*SLOPE*). 

<img width="930" height="723" alt="Image" src="https://github.com/user-attachments/assets/60109517-1464-49cd-a5c4-0ab59362de1c" />

<i>Figure 10: Selected sites with SITENO, stream gauging site name (NAME), latitude (LAT), longitude (LONG), elevation (ELEV), drainage area (AREA), and mean overall slope (SLOPE).</i>
<br></br>

### Daily, Monthly, and Annual Data
Once the filtered basin selections are made, you have the option to evaluate time series at the daily, monthly, and/or annual scale. 

#### DAILY DATA
The **Daily Data** dashboard is shown in *Figure 11*. 

<img width="928" height="717" alt="Image" src="https://github.com/user-attachments/assets/3fcdab35-6546-4e00-98fb-0be273f2e473" />

<i>Figure 11: Daily Data dashboard.</i>
<br></br>

For daily data, shown in *Figure 12*, each variable can be filtered by a range of values after the variable is selected. 
To select a variable, check the box under *"Select Variable(s)"*. Once selected, minimum and maximum value boxes will be displayed. If you want all possible values, you do not need to make any adjustments. The default values cover the range among the 1,014 sites. 
Variable abbreviations and units are: precipitation (*PRCP*) in millimeters per day, mean air temperature (*TAIR*), minimum air temperature (*TMIN*), and maximum air temperature (*TMAX*) in degrees Celsius, potential evapotranspiration (*PET*) in millimeters per day, actual evapotranspiration (*AET*) in millimeters per day, stream discharge (*OBSQ*) in millimeters per day, snow water equivalent (*SWE*) in  millimeters per day, shortwave radiation (*SRAD*) in watts per square meter, water vapor pressure (*VP*) Pascals, and day length (*DAYL*) in seconds per day. The data can also be filtered temporally under *"Select Time Period(s)"*. The *Date Range* enables you to select a beginning and ending date. The *Calendar Year* and *Month* can also be selected using the dropdown menu to further refine the temporal range. *Calendar Year* is January 1 to December 31. Multiple *Calendar Year* and/or *Month* selections can be made and subsequently removed by using backspace. If *Date Range* and *Calendar Year* are both selected, the year must fall within the established range. An error message will be displayed if this occurs, which you can dismiss. No data will be displayed or queried from the database until the **_Retrieve and View Data_** button is pressed. The **_Reset Filters_** button will clear all selected variables and values. 


<img width="254" height="726" alt="Image" src="https://github.com/user-attachments/assets/e161f508-ae76-4fbb-8581-e2cc48e7c129" />

<i>Figure 12: Available climate variables in MACH. Daily data can be filtered by value range, date range, calendar year, and/or calendar month.</i>
<br></br>

The **"Filtered Daily Data"** table header will show the selected number of sites (*Figure 13*). If this number is not consistent with the **Site Selection** tab, make sure you have pressed the **_Retrieve and View Data_** button again to update the query. 
The table will display the selected variables and specified filters for the first 1,000 records only. All data will be available upon download. If you want to view data for a specific site before downloading, select that single site on the **Site Selection** tab. Again, 
only the first 1,000 records will be available for preview. The data preview is limited for this tab due to the large volume of available data (+16 million rows). If any streamflow data are missing, the cells in the OBSQ column will be blank and availability will correspond
to the **"Discharge Data"** results displayed on the **Site Selection** tab. Missing values in the downloaded csv files are indicated by NA. If ranges for selected variables are not applicable, the table will return *No data available in table*. 


<img width="3198" height="2487" alt="Image" src="https://github.com/user-attachments/assets/3a2d4dd6-397e-4b46-900f-5e20f9e9b99f" />
<i>Figure 13: Data preview for selected sites and variable(s). The table will always include the SITENO and DATE columns. </i>
<br></br>

Since the data preview is limited to 1,000 rows, the **"Data Summary"** table displays the selected sites and the number of rows (days) of data that will be returned upon download. In the example shown in *Figure 14*, precipitation was selected along with January for the 
entire period of record (1980 to 2023). Every January over a 44-year period is 1,364 days. 


<img width="456" height="723" alt="Image" src="https://github.com/user-attachments/assets/72eaeda6-c039-4632-ac53-79c2cb23f53e" />

<i>Figure 14: Data available for the selected sites.</i>
<br></br>

Each time series data tab has two download options as shown in *Figure 15*. The exported daily data will match what is depicted in the filtered table (SITENO, DATE, and variable columns). If more than one watershed is selected from the **Site Selection** tab, the **_Export as csv_** button will download all data as one single csv file, with each basin appended at the end of a previous basin's record. The exported data file naming convention is MACH_time_YYYY_MM_DD with the four-digit year, month, and day of the current date. The time corresponds to the tab name, either daily, monthly, or annual. If you would like to download data for each watershed separately, the **_Export as separate csv files_** button will result in a zip file containing individual csv files. The zip file naming convention is MACH_time_YYYY_MM_DD.zip and once extracted, will contain individual csv files, MACH_time_00000000.csv, where the zeroes represent the 8-digit site number. 

<img width="456" height="192" alt="Image" src="https://github.com/user-attachments/assets/b1eb05f1-b9d9-483a-a6ae-cda787ff3986" />

<i>Figure 15: Download options for time series data.</i>
<br></br>

A progress message will be displayed while data is downloading (*Figure 16*). Note that larger amounts of data may take a few minutes. Please wait until the download is complete before moving to a new query or tab, otherwise, the app may freeze. All csv files will contain SITENO as the first column (character) and DATE (MM/DD/YYYY) as the second, followed by selected variables. <ins>All data tabs are independent of each other, meaning that the variables and filters selected on one tab, for example daily, will not carry over to monthly or annual</ins>. 


<img width="404" height="111" alt="Image" src="https://github.com/user-attachments/assets/e6a510ed-3c51-445e-b8bc-8dcd11434a69" />

<i>Figure 16: Progress message during zip file creation.</i>
<br></br>

#### MONTHLY DATA
The **Monthly Data** tab contains all climate variables and temporal filters. Rather than a range of values for each variable, you have a choice of *Minimum*, *Maximum*, *Median*, *Mean*, or *Total*, shown in *Figure 17* in the **"Select Statistic"** box. Only one option can be selected at a time. The default is *Mean*. All statistics are based on the daily values over a month. For example, if *Maximum* is selected for precipitation, the maximum daily precipitation value for each individual month, January to December, will be returned for each calendar year. If *Total* is selected, all temperature variables will return the mean value.  

<img width="496" height="251" alt="Image" src="https://github.com/user-attachments/assets/0c7a5532-bde1-4983-b1ec-dd7a2e52ff12" />

<i>Figure 17: Statistics for monthly data. Includes minimum, maximum, median, mean, or total of daily values.</i>
<br></br>

Data can also be filtered by *Calendar Year* and/or *Month* under *"Select Time Period(s)"* as shown in *Figure 18*. The **"Filtered Monthly Data"** table will show all data returned and includes SITENO, YEAR, and MONTH columns along with any selected variables. These columns are what will be returned in downloaded monthly data csv files. 

<img width="994" height="718" alt="Image" src="https://github.com/user-attachments/assets/8f222f81-058d-4fa2-9637-c339b679304a" />
<i>Figure 18: Monthly data tabe with statistic options, variables, temporal filters, and table display.</i>
<br></br>


#### ANNUAL DATA
The **Annual Data** tab is similar to **Monthly Data**, except an *"Annual Aggregation"* option must be selected, either *Water Year* or *Calendar Year*, as shown in *Figure 19*. The default selection is *Water Year*. A water year begins on October 1 and ends on 
September 30 of the following year, for example, water year 1981 begins on October 1, 1980 and ends on September 30, 1981. The *"Select Statistic*" default is *Mean*. 

<img width="614" height="242" alt="Image" src="https://github.com/user-attachments/assets/c7c24fb9-2607-45fa-8a31-4de466ca8fe3" />

<i>Figure 19: Annual aggregation selection.</i>
<br></br>

The **Annual Data** dashboard is shown in *Figure 20*. 

<img width="898" height="718" alt="Image" src="https://github.com/user-attachments/assets/2fbdf354-df43-4d12-9917-f7066adf5f17" />

<i>Figure 20: Annual Data dashboard.</i>
<br></br>


Annual data can be filtered by year under *"Select Time Period(s)*", which will reflect the aggregation option selected under *"Annual Aggregation"* (*Figure 21*). Water year options do not include 1980 since the data begins
on January 1 1980. Water year 2023 ends on September 30, 2023. 

<img width="490" height="620" alt="Image" src="https://github.com/user-attachments/assets/b28936c3-e54a-4496-a08a-782568b44b50" />

<i>Figure 21: Annual temporal filter.</i>
<br></br>

### MOPEX Data 
MACH Explorer also enables retrieval of MOPEX data from the 395 basins within MACH also identified in the [MOPEX dataset](https://iahs.info/uploads/dms/13600.04-9-28-SCHAAKE.pdf). If any filtered watersheds from the **Site Selection** tab are labelled as MOPEX
in the *site_info.csv* (available on zenodo), they will appear on the **MOPEX Data** tab (*Figure 22*). In the example below, only 2 watersheds in Arizona (out of the 17 total sites) are also in MOPEX. The **"Stream Discharge Record"** is the same as the **"Discharge Record"** table, except it displays the total number of streamflow records per calendar year for the MOPEX sites (1948 to 1979). A complete record will have 11,688 days.  

<img width="1286" height="346" alt="Image" src="https://github.com/user-attachments/assets/5e6007f6-c164-4692-a37e-be16b5c41861" />

<i>Figure 22: MOPEX Data dashboard.</i>
<br></br>

There are two export options, shown in *Figure 23*. *MOPEX only* will download available stream discharge, precipitation, minimum temperature and maximum temperature for January 1, 1948 to December 31, 1979 for all identified MOPEX basins selected on the **Site Selection** tab. *MOPEX & MACH* will automatically append the MACH data beginning January 1, 1980 through to December 31, 2023 for stream discharge, precipitation, minimum temperature, and maximum temperature. Note that MOPEX data may have missing values for any variables.   

<img width="602" height="173" alt="Image" src="https://github.com/user-attachments/assets/572fc540-e591-4114-b48b-de8177ccae98" />

<i>Figure 23: MOPEX Data dashboard.</i>
<br></br>

The table results will not change based on the export option. After the export option is chosen, make sure to press the **_Retrieve and Confirm Data_** button to query the database. A confirmation message will be displayed that reflects the export option. If you change the export option, make sure to press the **_Retrieve and Confirm Data_** button again. 

For *MOPEX only* data, the message will indicate that MOPEX only data retrieved (*Figure 24*), while the *MOPEX & MACH* option will indicate that MACH data has been appended (*Figure 25*). 

<img width="484" height="331" alt="Image" src="https://github.com/user-attachments/assets/0fc998c2-b598-450d-9079-49f56c278605" />

<i>Figure 24: MOPEX only confirmation message.</i>
<br></br>

<img width="484" height="353" alt="Image" src="https://github.com/user-attachments/assets/505625af-dc9a-4321-b934-347d0221c5c0" />

<i>Figure 25: MOPEX & MACH data confirmation message.</i>
<br></br>

When the data is downloaded, the file naming convention for **_Export as csv_** is MOPEX_YYYY_MM_DD.csv, and for **_Export as separate csv files_**. The zip file will be MOPEX_YYYY_MM_DD.zip and the individual csv files will be MOPEX_00000000.csv where the zeroes
represent the 8-digit site number. The first column of the csv file will be SITENO (character), followed by DATE (MM/DD/YYYY), OBSQ (streamflow), PRCP (precipitation), TMAX (maximum air temperature), and TMIN (minimum air temperature). 

### Attributes
MACH Explorer also includes catchment attributes and you can select according to the number of attributes (single, monthly, or annual) and type. The retrieved attributes pertain only to the filtered sites from the **Site Selection** tab. Only one attribute type can be selected at a time under *"Select Attribute Type"*. Selected attributes will display in the **"Selected Attributes"** table with the SITENO as the first column followed by any selections. The table header will update based on the attribute type (single, monthly, annual). The **_Retrieve Attributes_** button must be pressed if any attribute selections are changed. A detailed *README.csv* file with attribute descriptions is located on [zenodo](https://zenodo.org/records/16423231). The options available in the drop-down menus correspond to the column names in the various attribute tables (i.e. climate, soil, geology). A portion of the *README.csv* file is shown in *Figure 26*. The file name correlates with the attribute type, for example, overall_climate is *Climate* under the overall (single attribute per site) option. 

Additional attributes are available outside of the MACH Explorer application, on zenodo, including detailed dam information, SSURGO soil data, and precipitation indices (dam_info.csv, soil_ssurgo.csv, Rx_prcp.csv). The contents are too extensive for inclusion in the app. 

<img width="1280" height="674" alt="Image" src="https://github.com/user-attachments/assets/3f7f22fd-1b2d-4b35-9267-07aa003edd11" />

<i>Figure 26: Screenshot of a portion of the README file contents.</i>
<br></br>

*Single Value per Site* display attributes that have one overall value (*Figure 27*). These categories under *"Select Overall Site Attribute(s)"* include *Catchment*, *Climate*, *Hydrology*, *Soil*, *Geology*, *Regional*, and *Anthropogenic*. 

<img width="1094" height="717" alt="Image" src="https://github.com/user-attachments/assets/3980c078-a29c-4dd6-8c4a-422cac38f0ba" />

<i>Figure 27: Single value per site for overall attributes.</i>
<br></br>

The *Monthly Value per Site* option pertains to climate attributes calculated using daily MACH data. The table for monthly attributes includes SITENO and MONTH columns, followed by attribute selections (*Figure 28*). 
 
<img width="1286" height="540" alt="Image" src="https://github.com/user-attachments/assets/89dd6b8c-c5dc-4956-8dd1-887be9fe873d" />

<i>Figure 28: Monthly value per site for monthly climate attributes.</i>
<br></br>

The *Annual Value per Site* option pertains to climate attributes calculated using daily MACH data (*Figure 29*). 

<img width="1286" height="544" alt="Image" src="https://github.com/user-attachments/assets/8be8e5b2-1850-4a71-85f4-13e61c395fd1" />

<i>Figure 29: Monthly value per site for monthly climate attributes.</i>
<br></br>

The **"Download Attributes"** option name will change based on the attribute type selection. All export buttons will download a single csv file that resembles the table output shown in the dashboard. The file naming convention is MACH_att_YYYY_MM_DD.csv for overall site attributes, MACH_monthly_att_YYYY_MM_DD.csv for monthly attributes, and MACH_annual_att_YYYY_MM_DD.csv for annual attributes. 

### Land Cover
The **Land Cover** tab retrieves land cover data for the selected sites (*Figure 30*). Available data begins in 1985 and ends in 2023. Values are percent coverage by basin area and all 16 classes will equal 100 percent. Specific years can be retrieved using *"Select Calendar Year(s)"* and 16 specific classes using *"Select Land Cover Class(es)"*, which represent the modified Anderson Level II classification system (Anderson et al., 1976). The **_Retrieve Land Cover Data_** button must be pressed each time the filters are changed. The results are displayed in the **"Land Cover Data"** table and include SITENO, YEAR, and the selected land cover classes. 

<img width="1286" height="544" alt="Image" src="https://github.com/user-attachments/assets/005fa4a4-d6f5-4fdb-8d3a-b7cae2778c24" />

<i>Figure 30: Land Cover Data dashboard.</i>
<br></br>

## Documentation
The MACH Explorer uses data and software downloaded from the following sources:
- **R**: [The R Project for Statistical Computing](https://www.r-project.org/) version 4.3.3. (Angel Food Cake). 
- **Chrome**: [Google Chrome Portable](https://portableapps.com/apps/internet/google_chrome_portable).
- **Daymet**: [Daymet Version 4](https://daymet.ornl.gov/) - climate variables (precipitation, minimum air temperature, maximum air temperature, snow water equivalent, vapor pressure, solar radiation, day length) for January 1, 1980 to December 31, 2023.
- **GLEAM4**: [Global Land Evaporation Amsterdam Model](https://www.gleam.eu/) - climate variables (actual evapotranspiration, potential evapotranspiration) for January 1, 1980 to December 31, 2023.
- **MOPEX**: [Model Parameter Estimation Experiment](https://doi.org/10.1016/j.jhydrol.2005.07.054) - climate variables (precipitation, minimum air temperature, maximum air temperature), streamflow for January 1, 1948 to December 31, 1979.
- **USGS NWIS**: [United States Geological Survey National Water Information System](https://waterdata.usgs.gov/nwis/rt) - streamflow data January 1, 1980 to December 31, 2023.
- **NHDPlus Version 2.1**: [National Hydrography Dataset](https://www.sciencebase.gov/catalog/item/5669a79ee4b08895842a1d47) - catchment attributes.
- **MRLC**: [Multi-Resolution Land Characteristics Consortium](https://www.mrlc.gov/) - land cover.
- **NID**: [US Army Corps of Engineers National Inventory of Dams](https://nid.sec.usace.army.mil/#/) - dam attributes.
- **NRCS**: [Web Soil Survey](https://websoilsurvey.nrcs.usda.gov/app/WebSoilSurvey.aspx) - soil attributes. 
- **MACH**: https://zenodo.org/records/16414465 (Katharine Sink, 2025). 
