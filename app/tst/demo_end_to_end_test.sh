#!/bin/bash

# Set up a test table (clear out any existing data)
psql -h db-test -p5432 -U postgres -d postgres -c "delete from demo_table" --quiet

# create a data file with sample content for 5 days
python ../util/create_data_file.py

# Run the demo_writer application to insert data from input file
../exe/demo_writer TESTENDTOEND
if [ $? -ne 0 ]; then
	echo "failed to run demo_writer..."
fi

# Run the demo_report application
../exe/demo_report TESTENDTOEND
if [ $? -ne 0 ]; then
	echo "failed to run demo_report..."
fi

# test the output report

#regex to determine a record line
record_count="$(grep -c -E '[0-9]{2}/[0-9]{2}/[0-9]{4}  -  [A-Z]  -  [0-9]{3}' ../src/resources/out/DEMO_REPORT.TXT)"
#testing that the report has 10 records, since we know our test data
#file only has 10 records for each day
echo "EXPECTED RECORDS: 10"
echo "RECORDS FOUND:    ${record_count}"
if [ $record_count -eq 10 ]; then
	echo "TEST PASSED"
else 
	echo "TEST FAILED"
fi