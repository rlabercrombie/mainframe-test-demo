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

This step includes downloading and setting up the standard application code with no tests.

Clone the repository to download it to your workstation and checkout the step0 branch:
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

In another terminal (I'll refer to this as the container terminal), move to the new mainframe-test-demo directory created earlier.  SSH into the linux demo container.  Note that if the container name is different than below you can use ```docker container ls``` to view container names on your system:
```
docker exec -it mainframe-test-demo_demo_1 /bin/bash
```

Now that the environment is ready, we can step through the process of running the application.  These steps should all be done in the container terminal:

Compile the cobol programs
```
cd /home/util
./comp.sh demo_writer
./comp.sh demo_report
```

Run a query against the database:
```sql
psql -h db -p5432 -U postgres -d postgres -c "select * from demo_table"
```

You should get an output like this:



Run the demo writer:

Run the demo report:




