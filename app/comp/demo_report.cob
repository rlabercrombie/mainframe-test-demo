       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO-REPORT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DEMO_REPORT ASSIGN TO 
                "../src/resources/out/DEMO_REPORT.TXT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD DEMO_REPORT.
           01 DEMO_REPORT_RECORD     PIC X(72).

       WORKING-STORAGE SECTION.
       *> ===========================================================
       *> This program reads from a database to generate a report

       *> example argument usage:

       *> run live program
       *>      ./demo_report

       *> for running as part of end-to-end tests
       *>      ./demo_report TESTENDTOEND 
       
       *> for running only unit tests
       *>      ./demo_report TESTUNIT 
       
       *> for running only integration tests
       *>      ./demo_report TESTINTEGRATION
       
       *> for running unit and integration tests 
       *>      ./demo_report TEST
       *>      ./demo_report TESTALL
       
       *> ===========================================================
       01 WS_PARAMS.
           05 WS_PARAMS_TEST_SWITCH        PIC X(04) VALUE 'N   '.
               88 WS_PARAMS_TEST               VALUE 'TEST'.        
           05 WS_PARAMS_TEST_NAME          PIC X(25) VALUE 'ALL'. 

       *> test counters
       01 WS_TEST_PASSED                           PIC 9(2) VALUE ZERO.
       01 WS_TEST_FAILED                            PIC 9(2) VALUE ZERO.

       01 WS_TEST_EXPECTED_DATE                  PIC X(10).   

       *> test counters
       01 WS_CURRENT_DATE_DATA.
         05  WS_CURRENT_DATE.
             10  WS_CURRENT_YEAR         PIC 9(04).
             10  WS_CURRENT_MONTH        PIC 9(02).
             10  WS_CURRENT_DAY          PIC 9(02).
         05  WS_CURRENT_TIME.
             10  WS_CURRENT_HOURS        PIC 9(02).
             10  WS_CURRENT_MINUTE       PIC 9(02).
             10  WS_CURRENT_SECOND       PIC 9(02).
             10  WS_CURRENT_MILLISECONDS PIC 9(02).

       01 WS_COUNTER                     PIC 9(5) VALUE ZERO.

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

OCESQL*EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01  DBNAME                  PIC  X(30) VALUE SPACE.
       01  USERNAME                PIC  X(30) VALUE SPACE.
       01  PASSWD                  PIC  X(10) VALUE SPACE.
       01  DB_RECORD_COUNT         PIC 9(04).
       01  DB_RECORDS.
           05  DB_DATE             PIC X(10).
           05  DB_STRING           PIC X(12).
               
OCESQL*EXEC SQL END DECLARE SECTION END-EXEC.
       
OCESQL*EXEC SQL INCLUDE SQLCA END-EXEC.
OCESQL     copy "sqlca.cbl".
       *> end ocesql declaration

       *> report working storage
       01  WS_RPT_TITLE.
           05  FILLER                     PIC X(31) VALUE SPACES.
           05  FILLER                     PIC X(11) VALUE "DEMO REPORT".
           05  FILLER                     PIC X(30) VALUE SPACES.
       01  WS_RPT_SUBTITLE.
           05  FILLER                       PIC X(31) VALUE SPACES.
           05  WS_RPT_SUBTITLE_DATE         PIC X(10).
           05  FILLER                       PIC X(31) VALUE SPACES.
       01  WS_RPT_LINE_BREAK.
           05  WS_RPT_LINE_BREAK_SPACES     PIC X(72) VALUE ALL SPACES.
           05  WS_RPT_LINE_STARS            PIC X(72) VALUE ALL "*".
       01  WS_RPT_DATA.
           05  WS_RPT_DATA_DATE.
               10  WS_RPT_DATA_DATE_MM      PIC X(02).
               10  FILLER                   PIC X(01) VALUE "/".
               10  WS_RPT_DATA_DATE_DD      PIC X(02).
               10  FILLER                   PIC X(01) VALUE "/".
               10  WS_RPT_DATA_DATE_YYYY    PIC X(04).
           05  FILLER                       PIC X(05) VALUE "  -  ".
           05  WS_RPT_DATA_SWITCH           PIC X(01).
           05  FILLER                       PIC X(05) VALUE "  -  ".
           05  WS_RPT_DATA_COUNTER          PIC X(03).
       01  WS_RPT_SUMMARY.
           05  FILLER                 PIC X(30) VALUE SPACES.
           05  FILLER                 PIC X(15) VALUE "TOTAL RECORDS: ".
           05  WS_RPT_SUMMARY_COUNT   PIC X(05).

OCESQL*
OCESQL 01  SQ0001.
OCESQL     02  FILLER PIC X(067) VALUE "INSERT INTO demo_table (report"
OCESQL  &  "_date, report_text) VALUES ( $1, $2 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0002.
OCESQL     02  FILLER PIC X(074) VALUE "SELECT report_date, report_tex"
OCESQL  &  "t FROM demo_table WHERE report_date = ( $1 )".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0003.
OCESQL     02  FILLER PIC X(022) VALUE "DELETE FROM demo_table".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
OCESQL 01  SQ0004.
OCESQL     02  FILLER PIC X(014) VALUE "DISCONNECT ALL".
OCESQL     02  FILLER PIC X(1) VALUE X"00".
OCESQL*
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

           MOVE FUNCTION CURRENT-DATE TO WS_CURRENT_DATE_DATA.

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
           PERFORM B3100_CONNECT.
           OPEN OUTPUT DEMO_REPORT.

           *> GENERAL LOGIC 
           PERFORM B5000_INITIALIZE_REPORT.
           PERFORM B4000_READ_DB_ENTRIES_INTO_REPORT.

           PERFORM B5100_WRITE_REPORT_SUMMARY.

           *> FINISH UP
           PERFORM B3900_DISCONNECT.
           CLOSE DEMO_REPORT.

           EXIT.

       *> DATABASE PROCEDURES
       B3100_CONNECT.

OCESQL*    EXEC SQL
OCESQL*        CONNECT :USERNAME IDENTIFIED BY :PASSWD USING :DBNAME 
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLConnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE USERNAME
OCESQL          BY VALUE 30
OCESQL          BY REFERENCE PASSWD
OCESQL          BY VALUE 10
OCESQL          BY REFERENCE DBNAME
OCESQL          BY VALUE 30
OCESQL     END-CALL.
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

OCESQL*     EXEC SQL
OCESQL*         INSERT INTO demo_table
OCESQL*             (report_date, report_text)
OCESQL*         VALUES (:DB_DATE, :DB_STRING)
OCESQL*     END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DB_DATE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 12
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DB_STRING
OCESQL     END-CALL
OCESQL     CALL "OCESQLExecParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0001
OCESQL          BY VALUE 2
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

            IF SQLCODE NOT = ZERO 
                PERFORM B8000_SQL_ERROR STOP RUN.

           EXIT.
       
       B3500_FETCH_ROWS_INIT.

            STRING WS_CURRENT_YEAR DELIMITED BY SIZE,
                '-' DELIMITED BY SIZE,
                WS_CURRENT_MONTH DELIMITED BY SIZE,
                '-' DELIMITED BY SIZE,
                WS_CURRENT_DAY DELIMITED BY SIZE 
            INTO DB_DATE
            
            *> DECLARE CURSOR
OCESQL*     EXEC SQL
OCESQL*         DECLARE C1 CURSOR FOR
OCESQL*             SELECT report_date, report_text
OCESQL*             FROM demo_table
OCESQL*             WHERE report_date = (:DB_DATE)
OCESQL*     END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetSQLParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DB_DATE
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorDeclareParams" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "demo_report_C1" & x"00"
OCESQL          BY REFERENCE SQ0002
OCESQL          BY VALUE 1
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

            
OCESQL*     EXEC SQL
OCESQL*        OPEN C1
OCESQL*     END-EXEC.
OCESQL     CALL "OCESQLCursorOpen" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "demo_report_C1" & x"00"
OCESQL     END-CALL.
            IF  SQLSTATE NOT = ZERO PERFORM B8000_SQL_ERROR STOP RUN.
   
           MOVE 'F' TO DB_FINISHED_FETCHING_FLAG
   
           EXIT.
       
       B3501_FETCH_ROWS_READ_NEXT.
               
OCESQL*    EXEC SQL
OCESQL*            FETCH C1 INTO :DB_DATE, :DB_STRING
OCESQL*    END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 10
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DB_DATE
OCESQL     END-CALL
OCESQL     CALL "OCESQLSetResultParams" USING
OCESQL          BY VALUE 16
OCESQL          BY VALUE 12
OCESQL          BY VALUE 0
OCESQL          BY REFERENCE DB_STRING
OCESQL     END-CALL
OCESQL     CALL "OCESQLCursorFetchOne" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "demo_report_C1" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL

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
           ELSE
               ADD 1 TO WS_COUNTER

           END-IF
       
           EXIT.

       B3600_CLEAR_TEST_TABLE.
OCESQL*    EXEC SQL 
OCESQL*         DELETE FROM demo_table
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE SQ0003
OCESQL     END-CALL.

           IF  SQLSTATE NOT = ZERO PERFORM B8000_SQL_ERROR STOP RUN.
           EXIT.
       
       B3900_DISCONNECT.
OCESQL*    EXEC SQL 
OCESQL*        CLOSE C1
OCESQL*    END-EXEC
OCESQL     CALL "OCESQLCursorClose"  USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "demo_report_C1" & x"00"
OCESQL     END-CALL
OCESQL    

OCESQL*    EXEC SQL
OCESQL*        DISCONNECT ALL
OCESQL*    END-EXEC.
OCESQL     CALL "OCESQLDisconnect" USING
OCESQL          BY REFERENCE SQLCA
OCESQL     END-CALL.

           EXIT.

       *> DATA LAYER
       B4000_READ_DB_ENTRIES_INTO_REPORT.

           PERFORM B3500_FETCH_ROWS_INIT
           PERFORM UNTIL DB_FINISHED_FETCHING 

               PERFORM B3501_FETCH_ROWS_READ_NEXT 
               
               IF NOT DB_FINISHED_FETCHING
                  PERFORM B4100_PROCESS_DB_RECORD
               END-IF

           END-PERFORM

           EXIT.

       B4100_PROCESS_DB_RECORD.

           PERFORM B5100_WRITE_REPORT_RECORD.
           MOVE SPACES TO DEMO_REC.

           EXIT.

       *> REPORT WRITING LOGIC
       B5000_INITIALIZE_REPORT.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_TITLE.

           STRING WS_CURRENT_YEAR DELIMITED BY SIZE,
               '-' DELIMITED BY SIZE,
               WS_CURRENT_MONTH DELIMITED BY SIZE,
               '-' DELIMITED BY SIZE,
               WS_CURRENT_DAY DELIMITED BY SIZE 
           INTO WS_RPT_SUBTITLE_DATE.

           WRITE DEMO_REPORT_RECORD FROM WS_RPT_SUBTITLE.

           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_BREAK_SPACES.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_STARS.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_BREAK_SPACES.

           EXIT.

       B5100_WRITE_REPORT_RECORD.
           
           PERFORM B5101_CREATE_RPT_REC_FROM_DEMO_DATA.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_DATA.

           EXIT.

       B5101_CREATE_RPT_REC_FROM_DEMO_DATA.

           MOVE DEMO_DATE_MM TO WS_RPT_DATA_DATE_MM.
           MOVE DEMO_DATE_DD TO WS_RPT_DATA_DATE_DD.
           MOVE DEMO_DATE_YYYY TO WS_RPT_DATA_DATE_YYYY.
           MOVE DEMO_STRING_SWITCH TO WS_RPT_DATA_SWITCH.
           MOVE DEMO_STRING_COUNTER TO WS_RPT_DATA_COUNTER.

           EXIT.

       B5100_WRITE_REPORT_SUMMARY.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_BREAK_SPACES.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_STARS.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_LINE_BREAK_SPACES.

           PERFORM B5101_CREATE_SUMMARY_LINE.
           WRITE DEMO_REPORT_RECORD FROM WS_RPT_SUMMARY.
           EXIT.

       B5101_CREATE_SUMMARY_LINE.
           MOVE WS_COUNTER TO WS_RPT_SUMMARY_COUNT
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
OCESQL*         EXEC SQL
OCESQL*             ROLLBACK
OCESQL*         END-EXEC
OCESQL     CALL "OCESQLStartSQL"
OCESQL     END-CALL
OCESQL     CALL "OCESQLExec" USING
OCESQL          BY REFERENCE SQLCA
OCESQL          BY REFERENCE "ROLLBACK" & x"00"
OCESQL     END-CALL
OCESQL     CALL "OCESQLEndSQL"
OCESQL     END-CALL
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

           PERFORM B9101_CORRECTLY_CREATE_REPORT_RECORD.
           PERFORM B9102_CORRECTLY_SET_SUMMARY_COUNT.
           *> CORRECTLY CREATES REPORT LINE FROM SAMPLE DB QUERY
           *>        TEST WITH FULL SUMMARY LINE 
           *>       05/20/2022  -  Z  - 001
           *> CORRECTLY CREATES SUMMARY LINE FROM SAMPLE COUNT
       
           EXIT.

       B9101_CORRECTLY_CREATE_REPORT_RECORD.
           DISPLAY 'CORRECTLY CREATE REPORT RECORD'.

           MOVE "12" TO DEMO_DATE_MM.
           MOVE "01" TO DEMO_DATE_DD.
           MOVE "2020" TO DEMO_DATE_YYYY.
           MOVE "Z" TO DEMO_STRING_SWITCH.
           MOVE "001" TO DEMO_STRING_COUNTER.
           PERFORM B5101_CREATE_RPT_REC_FROM_DEMO_DATA.

           DISPLAY "EXPECTED: ",
               "12/01/2020  -  Z  -  001".

           DISPLAY "ACTUAL:   ", WS_RPT_DATA.

           IF WS_RPT_DATA = "12/01/2020  -  Z  -  001"
               PERFORM B9901_TEST_PASSED
           ELSE
               PERFORM B9902_TEST_FAILED
           EXIT.

       B9102_CORRECTLY_SET_SUMMARY_COUNT.
           DISPLAY 'CORRECTLY CREATE REPORT SUMMARY LINE'.

           MOVE "00001" TO WS_COUNTER.
           PERFORM B5101_CREATE_SUMMARY_LINE.

           DISPLAY "EXPECTED: ",
               "00001".

           DISPLAY "ACTUAL:   ", WS_RPT_SUMMARY_COUNT.

           IF WS_RPT_SUMMARY_COUNT = "00001"
               PERFORM B9901_TEST_PASSED
           ELSE
               PERFORM B9902_TEST_FAILED
           EXIT.

           EXIT.

       B9200_INTEGRATION_TESTS.
           DISPLAY 'RUNNING INTEGRATION TESTS!'
              
           DISPLAY "we are connecting to a test table with the same "
           DISPLAY "schema to insert some test data and verify it "
           DISPLAY "translates correctly into a report line"
           
           DISPLAY "CONNECT TO DB..."
           PERFORM B3100_CONNECT

           DISPLAY 'CLEARING OUT EXISTING DATA IN THE TEST TABLE...'
           PERFORM B3600_CLEAR_TEST_TABLE

           MOVE WS_CURRENT_DATE TO DEMO_DATE
           MOVE '16Z001' TO DEMO_STRING
           DISPLAY 'INSERTING A RECORD: ', DEMO_REC
           PERFORM B3400_INSERT_ROW

           MOVE SPACES TO DEMO_REC
           DISPLAY 'RETRIEVING THE RECORD: '
           PERFORM B3500_FETCH_ROWS_INIT
           PERFORM B3501_FETCH_ROWS_READ_NEXT 
           DISPLAY 'RETRIEVED A RECORD: ', DEMO_REC

           DISPLAY 'TESTING REPORT LINE...'
           PERFORM B5101_CREATE_RPT_REC_FROM_DEMO_DATA

            STRING WS_CURRENT_MONTH DELIMITED BY SIZE,
                '/' DELIMITED BY SIZE,
                WS_CURRENT_DAY DELIMITED BY SIZE,
                '/' DELIMITED BY SIZE,
                WS_CURRENT_YEAR DELIMITED BY SIZE 
            INTO WS_TEST_EXPECTED_DATE

            DISPLAY "EXPECTED: ", WS_TEST_EXPECTED_DATE
            DISPLAY "ACTUAL:   ", WS_RPT_DATA_DATE

           IF WS_RPT_DATA_DATE = WS_TEST_EXPECTED_DATE
             AND WS_RPT_DATA_SWITCH = 'Z'
             AND WS_RPT_DATA_COUNTER = '001'

                PERFORM B9901_TEST_PASSED
           ELSE 
                PERFORM B9902_TEST_FAILED
           END-IF

           DISPLAY 'TESTING COUNTER...'
           DISPLAY "EXPECTING: 1".
           DISPLAY "ACTUAL:    ", WS_COUNTER.
           IF WS_COUNTER = 1
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
           EXIT.
           EXIT.
