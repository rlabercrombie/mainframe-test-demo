echo "running demo_writer unit tests"
../exe/demo_writer TESTUNIT
if [ $? -ne 0 ]; then
	echo "TEST FAILED - demo_writer unit test"
	echo "review output above to see why..."
	exit 1;
fi

echo "running demo_report unit tests"
../exe/demo_report TESTUNIT
if [ $? -ne 0 ]; then
	echo "TEST FAILED - demo_report unit test"
	echo "review output above to see why..."
	exit 1;
fi

echo "running demo_writer integration tests"
../exe/demo_writer TESTINTEGRATION
if [ $? -ne 0 ]; then
	echo "TEST FAILED - demo_writer integration test"
	echo "review output above to see why..."
	exit 1;
fi

echo "running demo_report integration tests"
../exe/demo_report TESTINTEGRATION
if [ $? -ne 0 ]; then
	echo "TEST FAILED - demo_report integration test"
	echo "review output above to see why..."
	exit 1;
fi


echo "running end-to-end tests"
./demo_end_to_end_test.sh
if [ $? -ne 0 ]; then
	echo "TEST FAILED - demo end-to-end test"
	echo "review output above to see why..."
	exit 1;
fi

echo "tests completed successfully"