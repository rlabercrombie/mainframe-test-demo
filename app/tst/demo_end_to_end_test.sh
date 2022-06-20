#!/bin/bash

# Set up a test table (clear out any existing data)
psql -h db-test -p5432 -U postgres -d postgres -c "delete from demo_table" --quiet

# create a data file with sample content for 5 days
python ../util/create_data_file.py

# Run the demo_writer application to insert data from input file
../exe/demo_writer TESTENDTOEND
if [ $? -ne 0 ]; then
	echo "failed to run demo_writer..."
	exit 1;
fi

# Run the demo_report application
yest=$(date --date="yesterday" +"%Y%m%d")
../exe/demo_report $yest TESTENDTOEND
if [ $? -ne 0 ]; then
	echo "failed to run demo_report..."
	exit 1;
fi

# test the output report
echo "LOOKING FOR SUBTITLE DATE"
rpt_date_count="$(grep -c -E "[0-9]{4}-[0-9]{2}-[0-9]{2}" ../src/resources/out/DEMO_REPORT.TXT)"
rpt_date="$(grep -E "[0-9]{4}-[0-9]{2}-[0-9]{2}" ../src/resources/out/DEMO_REPORT.TXT)"
if [ $rpt_date_count -eq 1 ]; then
	echo "FOUND DATE: $rpt_date"
	echo "TEST PASSED"
else 
	echo "TEST FAILED"
	exit 1;
fi

yyyy=$(echo $rpt_date | cut -c1-4)
mm=$(echo $rpt_date | cut -c6-7)
dd=$(echo $rpt_date | cut -c9-10)
rec_date="${mm}/${dd}/${yyyy}"

#regex to determine a record line
record_count="$(grep -c -E "$rec_date  -  [A-Z]  -  [0-9]{3}" ../src/resources/out/DEMO_REPORT.TXT)"
#testing that the report has 10 records, since we know our test data
#file only has 10 records for each day
echo "EXPECTED RECORDS: 10"
echo "RECORDS FOUND:    ${record_count}"
if [ $record_count -eq 10 ]; then
	echo "TEST PASSED"
else 
	echo "TEST FAILED"
	exit 1;
fi



