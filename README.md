# WB-Commitments-Project Description (intened goal of the code)
# this description mostly describes the cumulative_commitments.R file
"""
1. The data used for this project has was downloaded from the World Bank’s website on 06-11-2025: https://projects.worldbank.org/en/projects-operations/projects-list?os=0 

- Projects with the status of “dropped” are excluded from the dataset 

- We evaluated projects based on their board approval dates: “boardapprovaldate” 
If “boardapprovaldate” did not have a date (was NA), then the date in “loan_effective_date” was used. If “loan_effective_date” was also NA, the date in “public_disclosure_date” was used. Projects missing all three date variables were ignored. 

- The “curr_total_commitments” variable tells us the WB’s total commitment amount to a project and originally included IDA commitments, IBRD commitments, and third party grant amounts – however we decided to remove third party grant amounts from this calculation, as we are only interested in the WB’s IDA and IBRD commitments. Thus “curr_total_commitments” = “idacommant” + “curr_ibrd_commitment”, where the variable 

- “idacommant” represents the IDA commitment amount to a project and the variable “curr_ibrd_commitment” represents the IBRD commitment amount to a project. The variable “grantamt” refers to non WB third party funding, thus in our new calculation of “curr_total_commitments”, it is not included.  
Projects with either a NA or value of zero for both “idacommamt” and “curr_ibrd_commitment” are removed from the data.

2. The “idacommamt”, “curr_ibrd_commitment”, “curr_total_commitment” for each project in the same country and year are then summed 

- This drops all irrelevant columns from the dataset – the only columns left now should be “countryshortname”, “year”, “idacommamt”, “curr_ibrd_commitment”, and “curr_total_commitment”. This table will appear so that each country has a singular IDA, IBRD, and total amount for each year. Example: all of the projects funded by IDA commitments in 2014 have been summed into the 2014’s “idacommamt” for that country.  

- For each country, we add a a cumulative variable so that the IDA, IBRD, and total commitment values appear in new columns of the table.  

- cum_ida_including_year: Cumulative IDA commitment through the year in the year column 

- cum_ibrd_including_year: Cumulative IBRD commitment through the year in the year column 
- cum_total_including_year: Cumulative WB commitment through the year in the year column 
- cum_ida_before_year: Cumulative IDA commitments in the up to, but not including, the year in the year column 
- cum_ibrd_before_year: Cumulative IBRD commitments in the up to, but not including, the year in the year column 

- cum_total_before_year: Cumulative WB commitments in the up to, but not including, the year in the year column 

- Missing years, years without any new WB projects between years, are added with the same commitment values as the prior year. Years before 2009 and after 2023 are removed from the display data set so that only years 2010-2023 appear on the table.  

2.5 (I have not actually done this step yet) Each country year’s share of IBRD and IDA lending are calculated and new columns are added to the table. 

- Ibrd_share_t: share of total cumulative WB commitments from the IBRD through the year in the year column.  - Calculated as: ibrd_cumulative_t / wb_commitments_cumulative_t 

- Ida_share_t: share of total cumulative WB commitments from the IDA through the year in the year column. - - - Calculated as: ida_cumulative_t / wb_commitments_cumulative_t 

- Share_wb_lending_t: For each country-year pair, this variable shows that country’s cumulative World Bank lending amount as a percentage of the Bank’s total cumulative commitments up to that year. It is calculated as country’s cumulative WB commitments through year t / total WB commitments to all countries through year t 

3. Lagged years are added for the cumulative values of variables and new columns are titled “ida_cumulative_t_1” (for ida commitments in the year t-1), ibrd_cumulative_t_1” (for ibrd commitments in the year t-1), and wb_commitments_cumulative_t_1) (for total wb commitments in the year t-1). 
ida_cumulative_t_1: Cumulative IDA commitments in the up to, but not including, the year in the year column 
Ibrd_cumulative_t_1: Cumulative IBRD commitments in the up to, but not including, the year in the year column 
wb_commitments_cumulative_t_1: Cumulative WB commitments in the up to, but not including, the year in the year column 

4. Finally, columns in time t are renamed for clarity -  “curr_ibrd_commitment” to “ibrd_cumulative_t”, “idacommamt” to “ida_cumulative_t”, and “curr_total_commitment” to “wb_commitments_cumulative_t”. 
The result should be a table with 11 variables/columns.  
This datatable can now be exported to a csv file, excel, google sheets, etc.  
 
 
 
Variables Recap: 
Countryshortname: The name of the country or region receiving the commitment 
year: the year the WB commitment the funds - gotten from the process above 
ida_cumulative_t: Cumulative IDA commitment through the year in the year column 
ibrd_cumulative_t: Cumulative IBRD commitment through the year in the year column 
wb_commitments_cumulative_t: Cumulative WB commitment through the year in the year column 
ida_cumulative_t_1: Cumulative IDA commitments in the up to, but not including, the year in the year column 
Ibrd_cumulative_t_1: Cumulative IBRD commitments in the up to, but not including, the year in the year column 
wb_commitments_cumulative_t_1: Cumulative WB commitments in the up to, but not including, the year in the year column 
Ibrd_share_t: share of total cumulative WB commitments from the IBRD through the year in the year column. Calculated as: ibrd_cumulative_t / wb_commitments_cumulative_t 
Ida_share_t: share of total cumulative WB commitments from the IDA through the year in the year column. 
Calculated as: ida_cumulative_t / wb_commitments_cumulative_t 
share_wb_lending_t: Percentage of cumulative WB commitments through the year in the year column to the country in the county column.  






"""
