       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO-WRITER.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEMO_DATA ASSIGN TO "../src/resources/DEMO_DATA.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       
       FD DEMO_DATA.
           01 DEMO_DATA_RECORD     PIC X(50).
           
       
       WORKING-STORAGE SECTION.

       *> ===========================================================
       *> This program updates a database based off of information 
       *> read from an input data file.

       *> example argument usage:

       *> run live program
       *>      ./demo_writer

       *> for running as part of end-to-end tests
       *>      ./demo_writer TESTENDTOEND 
       
       *> for running only unit tests
       *>      ./demo_writer TESTUNIT 
       
       *> for running only integration tests
       *>      ./demo_writer TESTINTEGRATION
       
       *> for running unit and integration tests 
       *>      ./demo_writer TEST
       *>      ./demo_writer TESTALL
       
       *> ===========================================================

       01 WS_PARAMS.
           05 WS_PARAMS_TEST_SWITCH        PIC X(04) VALUE 'N   '.
               88 WS_PARAMS_TEST               VALUE 'TEST'.        
           05 WS_PARAMS_TEST_NAME          PIC X(25) VALUE 'ALL'.    

       *> end of file definitions
       01 WS_EOF                           PIC X(1) VALUE 'N'. 
           88 WS_EOF_FALSE                     VALUE 'N'.
           88 WS_EOF_TRUE                      VALUE 'Y'.
       
       *> other switches
       01 WS_GOOD_DATE                           PIC X(1) VALUE 'F'. 
           88 WS_GOOD_DATE_FALSE                     VALUE 'F'.
           88 WS_GOOD_DATE_TRUE                      VALUE 'T'.
       
       01 WS_GOOD_STRING                           PIC X(1) VALUE 'F'. 
           88 WS_GOOD_STRING_FALSE                     VALUE 'F'.
           88 WS_GOOD_STRING_TRUE                      VALUE 'T'.

       *> test counters
       01 WS_TEST_PASSED                           PIC 9(2) VALUE ZERO.
       01 WS_TEST_FAILED                            PIC 9(2) VALUE ZERO.

       *> ocesql declarations
       01  DB_TABLENAME                PIC X(15).
       
       01 DB_FINISHED_FETCHING_FLAG         PIC X(1) VALUE 'F'. 
           88 DB_FINISHED_FETCHING                   VALUE 'T'.
       
       01  DEMO_REC.
           05 DEMO_DATE.
                   15 DEMO_DATE_YYYY PIC X(04).
                   15 DEMO_DATE_MM   PIC X(02).
                   15 DEMO_DATE_DD   PIC X(02).
           05 DEMO_STRING.
               10 DEMO_STRING_DAY              PIC X(02).
               10 DEMO_STRING_SWITCH         PIC X(01).
               10 DEMO_STRING_COUNTER        PIC 9(03).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       01  DB_RECORD_COUNT         PIC 9(04).
       01  DB_RECORDS.
           05  DB_DATE             PIC X(10).
           05  DB_STRING           PIC X(12).
               
       EXEC SQL END DECLARE SECTION END-EXEC.
       
       EXEC SQL INCLUDE SQLCA END-EXEC.
       *> end ocesql declaration
       
       PROCEDURE DIVISION.
       
       *> MAIN/TEST PROGRAM LOGIC STARTUP
       *> If program is called with the TEST argument, then we 
       *> will use test database connection info instead of a  
       *> database containing live data
           
           ACCEPT WS_PARAMS FROM COMMAND-LINE.
           IF WS_PARAMS_TEST
               MOVE "postgres@db-test:5432" TO DBNAME
               MOVE "postgres"         TO USERNAME
               MOVE "postgres"         TO PASSWD
           ELSE
               MOVE "postgres@db:5432" TO DBNAME
               MOVE "postgres"         TO USERNAME
               MOVE "postgres"         TO PASSWD
           END-IF.

        *> A normal run and an end-to-end test will act functionally 
        *> the same except for using live vs test databases whereas
        *> integration tests and unit tests have their own workflows

           IF NOT WS_PARAMS_TEST OR 
           (WS_PARAMS_TEST AND WS_PARAMS_TEST_NAME = "ENDTOEND")

               PERFORM B1000_GENERAL_LOGIC
           ELSE

               PERFORM B9000_TEST
           END-IF.

           STOP RUN.
       
       B1000_GENERAL_LOGIC.
           *> SETUP 
           PERFORM B3100_CONNECT
           
           *> GENERAL LOGIC            
           PERFORM B4000_ITERATE_FILE_ENTRIES

           PERFORM B3500_FETCH_ROWS_INIT

           PERFORM UNTIL DB_FINISHED_FETCHING
                PERFORM B3501_FETCH_ROWS_READ_NEXT 

           END-PERFORM
           
           *> FINISH UP
           PERFORM B3900_DISCONNECT
           
           EXIT.
               
       *> DATABASE PROCEDURES
       B3100_CONNECT.

           EXEC SQL
               CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
           END-EXEC.
           IF  SQLCODE NOT = ZERO PERFORM B8000_SQL_ERROR STOP RUN.
   
           EXIT.

       B3400_INSERT_ROW.

            STRING DEMO_DATE_YYYY DELIMITED BY SIZE,
                '-' DELIMITED BY SIZE,
                DEMO_DATE_MM DELIMITED BY SIZE,
                '-' DELIMITED BY SIZE,
                DEMO_DATE_DD DELIMITED BY SIZE 
            INTO DB_DATE

            MOVE DEMO_STRING TO DB_STRING

            *> INSERT STATEMENT

            EXEC SQL
                INSERT INTO demo_table
                    (report_date, report_text)
                VALUES (:DB_DATE, :DB_STRING)
            END-EXEC

            IF SQLCODE NOT = ZERO 
                PERFORM B8000_SQL_ERROR STOP RUN.

           EXIT.
       
       B3500_FETCH_ROWS_INIT.

            *> DECLARE CURSOR
            EXEC SQL
                DECLARE C1 CURSOR FOR
                    SELECT report_date, report_text
                    FROM demo_table
            END-EXEC

            
            EXEC SQL
               OPEN C1
            END-EXEC.
            IF  SQLSTATE NOT = ZERO PERFORM B8000_SQL_ERROR STOP RUN.
   
           MOVE 'F' TO DB_FINISHED_FETCHING_FLAG
   
           EXIT.
       
       B3501_FETCH_ROWS_READ_NEXT.
               
           EXEC SQL
                   FETCH C1 INTO :DB_DATE, :DB_STRING
           END-EXEC

           MOVE  DB_DATE(1:4)   TO    DEMO_DATE_YYYY
           MOVE  DB_DATE(6:2)   TO    DEMO_DATE_MM
           MOVE  DB_DATE(9:2)   TO    DEMO_DATE_DD
           MOVE  DB_STRING      TO    DEMO_STRING
           
           IF SQLCODE NOT = ZERO    
               IF SQLSTATE = "02000" 
                   SET DB_FINISHED_FETCHING TO TRUE
                   
               ELSE 
                   PERFORM B8000_SQL_ERROR STOP RUN
               END-IF
           END-IF
       
           EXIT.

       B3600_CLEAR_TEST_TABLE.
           EXEC SQL 
                DELETE FROM demo_table
           END-EXEC.

           IF  SQLSTATE NOT = ZERO PERFORM B8000_SQL_ERROR STOP RUN.
           EXIT.

       B3900_DISCONNECT.
           EXEC SQL COMMIT WORK END-EXEC.

           EXEC SQL 
               CLOSE C1
           END-EXEC

           EXEC SQL
               DISCONNECT ALL
           END-EXEC.

           EXIT.
       
       *> DATA LAYER
       B4000_ITERATE_FILE_ENTRIES.
           OPEN INPUT DEMO_DATA
               PERFORM UNTIL WS_EOF_TRUE
                   READ DEMO_DATA INTO DEMO_REC
                       AT END MOVE 'Y' TO WS_EOF
                       NOT AT END
                           
                           PERFORM B4100_PROCESS_FILE_RECORD
                           PERFORM B3400_INSERT_ROW
       
                   END-READ
               END-PERFORM.
           CLOSE DEMO_DATA.

           EXIT.
       
       *> BUSINESS LOGIC
       B4100_PROCESS_FILE_RECORD.
           PERFORM B4200_DATE_CHECK
           PERFORM B4300_STRING_CHECK.
           IF WS_GOOD_STRING_TRUE AND WS_GOOD_DATE_TRUE
                   PERFORM B4301_UPDATE_STRING_SWITCH    
           END-IF.
           
           EXIT.
       
       B4200_DATE_CHECK.
           MOVE 'T' TO WS_GOOD_DATE
           IF DEMO_DATE NOT NUMERIC 
               MOVE 'F' TO WS_GOOD_DATE
           EXIT.
       
       B4300_STRING_CHECK.
           MOVE 'T' TO WS_GOOD_STRING
           IF DEMO_STRING = SPACES OR DEMO_STRING = LOW-VALUE
               MOVE 'F' TO WS_GOOD_STRING
           END-IF.
       
           EXIT.
       
       B4301_UPDATE_STRING_SWITCH.
           MOVE 'Z' TO DEMO_STRING_SWITCH.
           EXIT.
       
       *> ERRORS
       B8000_SQL_ERROR.
          DISPLAY "*** SQL ERROR ***".
          DISPLAY "SQLCODE: " SQLCODE " " NO ADVANCING.
          EVALUATE SQLCODE
             WHEN  +10
                DISPLAY "Record not found"
             WHEN  -01
                DISPLAY "Connection falied"
             WHEN  -20
                DISPLAY "Internal error"
             WHEN  -30
                DISPLAY "PostgreSQL error"
                DISPLAY "ERRCODE: "  SQLSTATE
                DISPLAY SQLERRMC
             *> TO RESTART TRANSACTION, DO ROLLBACK.
                EXEC SQL
                    ROLLBACK
                END-EXEC
             WHEN  OTHER
                DISPLAY "Undefined error"
                DISPLAY "ERRCODE: "  SQLSTATE
                DISPLAY SQLERRMC
          END-EVALUATE.

          MOVE 1 TO RETURN-CODE.

          EXIT.

       *> TESTS
       B9000_TEST.
           IF WS_PARAMS_TEST_NAME = 'ALL'
               PERFORM B9100_UNIT_TESTS
               PERFORM B9200_INTEGRATION_TESTS
           ELSE
               IF WS_PARAMS_TEST_NAME = 'UNIT'
                   PERFORM B9100_UNIT_TESTS
               ELSE IF WS_PARAMS_TEST_NAME = 'INTEGRATION'
                   PERFORM B9200_INTEGRATION_TESTS
               END-IF
           END-IF.
       
       
           DISPLAY "PASSING TESTS: ", WS_TEST_PASSED.
           DISPLAY "FAILING TESTS: ", WS_TEST_FAILED.

           IF WS_TEST_FAILED > 0
               MOVE 1 TO RETURN-CODE
           END-IF.
           
           EXIT.
       
       B9100_UNIT_TESTS.
           DISPLAY 'RUNNING UNIT TESTS!'
       
           PERFORM B9001_DEMO_DATE_IS_NUMERIC_DATE
           PERFORM B9002_DEMO_STRING_ISNT_EMPTY
           PERFORM B9003_DEMO_STRING_SWITCH_TO_Z
       
           EXIT.
       
       B9200_INTEGRATION_TESTS.
           DISPLAY 'RUNNING INTEGRATION TESTS!'           
       
           DISPLAY "we are connecting to a test table with the same "
           DISPLAY "schema to test inserts by immediately performing "
           DISPLAY "a read to after an insert to verify it exists "
           DISPLAY "in the database "
           
           DISPLAY "CONNECT TO DB..."
           PERFORM B3100_CONNECT

           DISPLAY 'CLEARING OUT EXISTING DATA IN THE TEST TABLE...'
           PERFORM B3600_CLEAR_TEST_TABLE

           MOVE '20220516' TO DEMO_DATE
           MOVE '16A001' TO DEMO_STRING
           DISPLAY 'INSERTING A RECORD: ', DEMO_REC
           PERFORM B3400_INSERT_ROW

           MOVE SPACES TO DEMO_REC
           DISPLAY 'RETRIEVING THE RECORD: '
           PERFORM B3500_FETCH_ROWS_INIT
           PERFORM B3501_FETCH_ROWS_READ_NEXT 
           DISPLAY 'RETRIEVED A RECORD: ', DEMO_REC

           DISPLAY 'TESTING EQUALITY...'
           IF DEMO_DATE = '20220516'
             AND DEMO_STRING = '16A001'

                PERFORM B9901_TEST_PASSED
           ELSE 
                PERFORM B9902_TEST_FAILED
           END-IF

           DISPLAY "DISCONNECTING"

           PERFORM B3900_DISCONNECT

           EXIT.
       
       B9001_DEMO_DATE_IS_NUMERIC_DATE.
           DISPLAY "FIRST 8 CHARACTERS IS NUMERIC "
           DISPLAY "SETS WS_GOOD_DATE TO TRUE".
           MOVE SPACES TO DEMO_REC
           MOVE '20220101' TO DEMO_DATE
           PERFORM B4200_DATE_CHECK
           IF WS_GOOD_DATE_TRUE 
               PERFORM B9901_TEST_PASSED
           ELSE 
               PERFORM B9902_TEST_FAILED
           END-IF
       
           DISPLAY "FIRST 8 CHARACTERS IS NOT NUMERIC "
           DISPLAY "SETS WS_GOOD_DATE TO FALSE".
           MOVE SPACES TO DEMO_REC
           MOVE 'A' TO DEMO_DATE
           PERFORM B4200_DATE_CHECK
           IF WS_GOOD_DATE_FALSE 
               PERFORM B9901_TEST_PASSED
           ELSE 
               PERFORM B9902_TEST_FAILED
           END-IF
           EXIT.
       
       B9002_DEMO_STRING_ISNT_EMPTY.
           DISPLAY "EMPTY STRING SETS WS_GOOD_STRING TO FALSE".
           MOVE SPACES TO DEMO_REC
           PERFORM B4300_STRING_CHECK
           IF WS_GOOD_STRING_FALSE 
               PERFORM B9901_TEST_PASSED
           ELSE 
               PERFORM B9902_TEST_FAILED
           END-IF
       
           DISPLAY "NONEMPTY STRING SETS WS_GOOD_STRING TO TRUE".
           MOVE SPACES TO DEMO_REC
           MOVE '123456' TO DEMO_STRING
           PERFORM B4300_STRING_CHECK
           IF WS_GOOD_STRING_TRUE
               PERFORM B9901_TEST_PASSED
           ELSE 
               PERFORM B9902_TEST_FAILED
           END-IF
       
           EXIT.
       
       B9003_DEMO_STRING_SWITCH_TO_Z.
           DISPLAY "STRING SWITCH GETS CHANGED".
           MOVE SPACES TO DEMO_REC
           
           PERFORM B4301_UPDATE_STRING_SWITCH.
           IF DEMO_STRING_SWITCH = 'Z' 
               PERFORM B9901_TEST_PASSED
           ELSE 
               PERFORM B9902_TEST_FAILED
           END-IF
       
           EXIT.
       
       B9901_TEST_PASSED.
           DISPLAY "TEST PASSED"
           ADD 1 TO WS_TEST_PASSED
           DISPLAY "------"
           EXIT.
       
       B9902_TEST_FAILED.
           DISPLAY "TEST FAILED"
           ADD 1 TO WS_TEST_FAILED
           DISPLAY "------"
           EXIT.
