# Example of testing in Cobol (without testing frameworks)

The purpose of this demo is to show that application testing can be done anywhere and that you dont need a testing framework to accomplish it (though they can obviously help!)

## Requirements to run on your own

- Docker - to quickly set up the environment
- Git - to quickly move between different versions of the code

## The application to be tested

We start out with two Cobol programs located in /app/src.  

The first, 'demo_writer' reads from a data file, makes a few small checks (to ensure valid dates, etc), makes a small update to the data, and then inserts the data into a simple database.

The second, 'demo_report' reads all the records from the database with a date matching the current day, formats them, and writes a report.  

An example data file:

![image](https://user-images.githubusercontent.com/1844696/174794091-b3b36e4f-f80a-4e17-b58e-0a31a003cce9.png)

What the database table looks like:

![image](https://user-images.githubusercontent.com/1844696/174794682-1ed218f2-e711-4f6b-ad01-4617f590cce2.png)

An example output report:

![image](https://user-images.githubusercontent.com/1844696/174794170-358cac47-4d0f-48a6-87cf-264dd8807277.png)

## Step 0 

### Environment setup

This step includes downloading and setting up the standard application code with no tests.

Clone the repository to download it to your workstation and checkout the step0 branch in a 'Git Bash terminal':
```
cd /your/project/directory
git clone https://github.com/rlabercrombie/mainframe-test-demo.git
cd mainframe-test-demo
git checkout step0
```

Set up the environments using docker-compose:
```
docker-compose build
docker-compose up
```

In another terminal (I'll refer to this as the container terminal), move to the new mainframe-test-demo directory created earlier.  SSH into the linux demo container:  
```
docker exec -it mainframe-test-demo_demo_1 /bin/bash
```
Note that if the container name is different than mainframe-test-demo_demo_1 you can use ```docker container ls``` to view container names on your system.  Additionally, instead of the coommand above, you can find the container within docker desktop and click the 'cli' button.

### Manually test the application

Now that the environment is ready, we can step through the process of running the application.  These steps should all be done in the container terminal:

Compile the cobol programs
```
cd /home/util
./comp.sh demo_writer
./comp.sh demo_report
```
Create a data file at '/home/src/resources/DEMO_DATA.DAT'.  The first 8 characters should be the date in YYYYMMDD format.  The next should be two characters, I normally just reuse the two-character-day.  Next should be the character 'A'.  Next should be a three digit sequence number, like '001'.  Create one record for todays date and one for yesterday's date.  So if today's date were 6/21/2022, then my data file should look like 
```
2022062121A001
2022062020A001
```

Run a query against the database:
```sql
psql -h db -p5432 -U postgres -d postgres -c "select * from demo_table"
```

You should get an output like this with 0 rows:
![image](https://user-images.githubusercontent.com/1844696/174828276-1afe9b8a-a744-416b-ac78-efa0dd49a916.png)

Run the demo writer:
```
cd /home/exe
./demo_writer
```

Now run the query again - viola you should see the new rows of data from your data file appear in the database:
```sql
psql -h db -p5432 -U postgres -d postgres -c "select * from demo_table"
```

Run the demo report:
```
cd /home/exe
./demo_report
```

You should have a new report located at this path '/home/src/resources/out/DEMO_REPORT.TXT'.  This file should be a report with one record in it matching todays date.

So that's it, now we've verified that the application works correctly.  If you were new to this project and didnt know how to create the datafile or that we're connecting to a postgres database or that the report should only show records for the current date, you might imagine this was pretty hard to test.  Even if you know all this, there's a few steps in the process and if you forget one then you might inadvertantly deploy code with bugs in it.  Manual tests are great for spot checking, but what if we could automate a lot of these steps?

## Step 1

In this step we are making changes to add an end-to-end test (located at '.../tst/demo_end_to_end_test.sh') that will 
1) Clear out any existing records in our test database (which has the same schema as our live one)
2) Create a data file for us with records for today's date and other dates (this is a python script, but could have been any code that can perform the logic to write the data file). 
3) Run our demo_writer program, except using an argument that tells the program to use the test database, not the live one.
4) Run our demo_report program, except using an argument that tells the program to use the test database, not the live one.
5) Make some test assertions on the output report.  Essentially we parse the date in the subtitle, reformat it to the format the records use and see if we have 10 records in our report.  We know that there should be 10, since the data file that we created automatically will generate 10 records per date. If we do not have 10 records, we know something is wrong. 

You can see the exact changes made by [clicking here](https://github.com/rlabercrombie/mainframe-test-demo/compare/step0...step1) (The Pull Request screen just helps us compare the changes, you dont need to submit the PR).  They include creating the end-to-end test script, modifying our cobol programs to take in a test argument and code that uses the argument to decide whether we are hitting the live database or the test database, and the python script that will generate our test file. 

Now lets use the new tests...

In your git bash terminal, navigate to the mainframe-test-demo folder and checkout step1.  This will update your file system with the changes described above:
```
git checkout step1
```

In your container terminal, compile the cobol programs and then run the test script:
```
cd /home/util
./comp.sh demo_writer
./comp.sh demo_reader
cd /home/tst
./demo_end_to_end_test.sh
```

That's it! The output will tell you if the test was successful or not.  Thats a good start, but we probably also want some unit and integration tests as well.  The reason we started with end-to-end tests is because you generally dont have to make many code changes to get them running.  When adding unit and integration tests to legacy code that don't have any other tests, a lot of time you will have to refactor code to separate business logic and other non-business logic.  Before you do that, having a nice end-to-end test to verify you didnt majorly break anything while refactoring is nice.

## Step 2 

In this step we are adding unit and integration tests to our cobol programs.  These are added directly to the program and will alter the workflow to only test specific procedures in the code (the ones with the business logic) and avoid hitting the database or a file system, etc.  Notice the branch that hits B9000_TEST instead of B1000_GENERAL_LOGIC.  We also create a script that will run all of our unit/itegration tests for both programs followed by the end to end test create in Step 1.

You can see the exact changes made by [clicking here](https://github.com/rlabercrombie/mainframe-test-demo/compare/step1...step2)

Now to use our new tests...
