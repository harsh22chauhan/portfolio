proc import datafile = "/home/u38470770/OnlineRetail (1).csv" out = project;
run;

/* Create below transaction and fact tables using the raw transaction data provided:
•	Transaction table: Invoice number, customer id, product id, quantity, rate, date, time
•	Customer table: customer id, country
•	Product table: Product id, product name
*/

proc sql outobs = 4 ;
select * from project ;
quit ;

*Transaction table;
proc sql;
create table Transition as 
select Invoiceno, customerid, Stockcode, quantity, unitprice, invoicedate from project ;
quit ;

*Customer table;
proc sql;
create table customertable as
select customerid,country from project;
quit;

*Product table;
proc sql;
create table producttable as
select stockcode,description from project;
quit;

/* Create a report to get the country-wise breakup of:
1.	most popular product/ most purchased product
2.	least popular product/ least purchased product
3.	monthly sale */
proc freq data = project;
table country;
run;

*most popular product/ most purchased product;
proc sql;
create table report as
select country, stockcode,count(stockcode) as cp
from project
group by country, stockcode 
order by country, cp desc;
quit;

proc sort data = report;
by country ;
run;

data report2;
set report;
by country;
if first.country;
run;


*least popular product/ least purchased product;
data report3;
set report;
by country;
if last.country;
run;


/*•	Arrange the customers as per most loyal */
proc tabulate data = project out = p1 ;
class customerid;
var  quantity;
table  customerid ALL , quantity(N) / box = "proc tabulate" ;
run;

proc sql ;
create table loyal as
select customerid,N from p1
order by N desc;
quit;


*Divide them in 3 equal categories according to the
 total number of customers to provide them with 
 different promotional offers.;
proc rank data=loyal out=promotion groups=3;
var N;
ranks rank_size;
run;

proc sql;
create table months as
select * from project
 where invoicedate >= date('2011-10-31') and invoicedate <= date('2011-08-31');
quit;

