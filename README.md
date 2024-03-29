# Hydrology Project
Years ago, I needed to analyze long-term climate data eries for a hydrology project. For short climate records, it is possible to use spreadsheets like Excel. However, it is more convenient to use high-level programming languages when dealing with large datasets. Such a code can preprocess data by filtering and searching for possible errors or missing data. 


## Fortran Program
To analyze the data, I wrote a computer program in FORTRAN (current repo). This program is able to read long term data of Pullman station from a text file, pre-process (filter) and fill in missing places using data from a nearby station (i.e. Moscow). The second part of the program computes the average monthly precipitation (AMP), average number of rainy days in a month (ANRDM), average daily rainfall intensity for a month (ADRIM), monthly average maximum temperature (MAMaxT), monthly average minimum temperature (MAMinT), probability of a wet day for a specific month (PWDM), and annual precipitation (AP). 

The program is divided to two main sections: 
1) Pre-processing
2) Calculation

In the first section, the Pullman and Moscow (Pullman.txt and Moscow.txt) data files are read as input, and missing or corrupted data of Moscow is replaced with corresponding values from the Pullman station. This process assumes no corresponding data in Pullman is missing or corrupted. The result is then written in another file named MOS2.TXT for clarification. 

The second part of the code aims to determine aome hydrologic parameters using a simple math and available definitions of the variables. Except for the annual precipitation which is only one value, the rest of the parameters are calculated for a 12-month period (i.e. January thru December). 