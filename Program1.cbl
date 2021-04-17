       identification division.
       program-id. A6-DataValidation.
       author. Kaifkhan Vakil.
       date-written. 2020-03-25.
      *Program description 
      *This code is for making an error report which will be showing 
      *all the error that a record might contain, if it does not contain 
      *any error than we would display the error. 

       environment division.
       configuration section.
       input-output section.
      *Declaring file control for input and output file. 
       file-control.
           select input-file
           assign to "../../../A6.dat"
           organization is line sequential.

           select error-report 
           assign to "../../../A6-DataValidation.out".


       data division.
       file section. 
      *File declaration of input file for reading the records from out 
      *input file. 
       fd input-file
           data record is input-line
           record contains 24 characters.
       01 input-line.
         05 il-maint-code               pic x.
               88 il-class-valid                   value 'A', 'C', 'D'. 
         05 il-part-number             pic 9(3).
         05 il-part-description        pic x(10).
         05 il-par-unit-price          pic 9(2)v99.
               88 il-price-lss-50                  value 1.00 thru 
               50.00.
         05 il-vendor-number.
           10 vendor-number-1          pic 9(1).
               88 vendor-number-less-3             value 1 thru 3.
           10 vendor-number-5          pic 9(5).

      *File declaration of error report which will be displaying in 
      *our report. 
       fd error-report
           data record is error-line
           record contains 50 characters.
       01 error-line                   pic x(50).

       working-storage section.

      *Declaring report headings which will display my name and 
      *assignment name
       01 ws-report-heading1.
         05 filler                     pic x(15)   value 
         "Kaifkhan vakil,".
         05 filler                     pic x(5)    value spaces.
         05 filler                     pic x(12)   value "Assignment 6".

      *This heading will display page heading. 
       01 ws-report-heading2.
         05 filler                     pic x(4)    value spaces.
         05 filler                     pic x(12)   value "ERROR REPORT".

      *This heading will display column headings. 
       01 ws-report-heading3.
           05 filler                   pic x(6)    value "RECORD".
           05 filler                   pic x(2)    value spaces.
           05 filler                   pic x(24)   value 
           "--------Raw Data--------".

      *This heading will display another line of column heading.
       01 ws-report-heading4.
         05 filler                     pic x(6)    value "NUMBER".
         05 filler                     pic x(5)    value spaces.
         05 filler                     pic x(18)   value 
         "and Error Messages".

      *This line is for adding a blank line at the end of the report
       01 ws-blank-line                pic x(120)  value spaces.

0     *This is the detail line which will be showing any false records 
       01 ws-detail-line.
         05 filler                     pic x(2)    value spaces.
         05 ws-record-number pic 99.
         05 filler                     pic x(4)    value spaces.
         05 ws-file-line               pic x(24).

      *This is the summary line which will show total data in the report
       01 ws-summary-line1.
         05 filler                     pic x(10)   value "TOTAL DATA".
         05 filler                     pic x(3)    value spaces.
         05 ws-total-data              pic 99      value 0.

      *This is the summary line which will show the total good data
       01 ws-summary-line2.
         05 filler                     pic x(9)    value "GOOD DATA".
         05 filler                     pic x(3)    value spaces.
         05 ws-good-data               pic 99      value 0.

      *This is the summary line which will show the total error data
       01 ws-summary-line3.
         05 filler                     pic x(8)    value "BAD DATA".
         05 filler                     pic x(3)    value spaces.
         05 ws-bad-data                pic 99      value 0.

      *This is the summary line which will show good adds of the file.
       01 ws-summary-line4.
         05 filler                     pic x(9)    value "GOOD ADDS".
         05 filler                     pic x(3)    value spaces.
         05 ws-good-add                pic 99      value 0.

      *This is the summary line which will show good changes in the 
      *record.
       01 ws-summary-line5.
         05 filler                     pic x(12)   value "GOOD CHANGES".
         05 filler                     pic x(3)    value spaces.
         05 ws-good-changes            pic 99      value 0.

      *This is the summary line which will show deltes changes in the 
      *record.
       01 ws-summary-line6.
         05 filler                     pic x(12)   value "GOOD DELETES".
         05 filler                     pic x(3)    value spaces.
         05 ws-good-deletes            pic 99      value 0.

      *This will show error line when there is any error in teh record.
       01 ws-error-line.
         05 filler                     pic x(8)    value spaces.
         05 ws-error-code-desc         pic x(40).

      *this is the eof flag declaration for paging.
       01 ws-flags.
         05 ws-eof-flag                pic x       value space.
          
      *This is boolean constant to see the proper end of file.
       01 ws-boolean-cnst.
         05 ws-true-cnst               pic x       value "Y".
         05 ws-false-cnst              pic x       value "N".

      *This is showing all the constant error messages of the record.
       01 ws-error-text-cnst.
         05 ws-error-text-1-cnst       pic x(40)   value
                             "PART NO. NOT NUMERIC          ".
         05 ws-error-text-2-cnst       pic x(40)   value
                             "WRONG MAINT CODE      ".
         05 ws-error-text-3-cnst       pic x(40)   value
                             "PRICE IN TROUBLE    ".
         05 ws-error-text-4-cnst       pic x(40)   value
                             "DESCRIPTION MISSING        ".
         05 ws-error-text-5-cnst       pic x(40)   value
                             "NON ALPHA IN DESC  ".
         05 ws-error-text-6-cnst       pic x(40)   value
                             "WRONG VENDOR SERIES  ".

      *These are the constants checking for the class 
       01 ws-class-cnst.
         05 ws-class-A-cnst            pic x       value 'A'.
         05 ws-class-C-cnst            pic x       value 'C'.
         05 ws-class-D-cnst            pic x       value 'D'.
      *A page counter to check the records.
       01 ws-line-page-counters.
         05 ws-line-count              pic 999     value 0.
         05 ws-page-count              pic 999     value 0.
         05 ws-lines-per-page-cnst     pic 999     value 15.
      *
      *Contants for use of counting 
       77 ws-record-count              pic 99      value 0.
       77 ws-good-count                pic 99      value 0.
       77 ws-bad-count                 pic 99      value 0.
       77 ws-good-A-count              pic 99      value 0.
       77 ws-good-C-count              pic 99      value 0.
       77 ws-good-D-count              pic 99      value 0.

       procedure division.
       000-main.
           move ws-false-cnst to ws-eof-flag.

      *Opening the files
           open input  input-file.
           open output error-report.

      *Reading the files
           read input-file 
           at end move ws-true-cnst            to ws-eof-flag.
           write error-line                    from ws-report-heading1
           before advancing 1 line.
      *processing the lines and output them
           perform 100-process-input
             until ws-eof-flag = ws-true-cnst.
           
      *     SUmmary line.  
          perform 200-summary-line.
           
      *Closing the file.
           close input-file.
           close error-report.

           display "Press enter to continue".
           accept return-code.

           goback.

      *This paragraph will be taking care of the processing each record
      *and outputting them if there are any errors. 
       100-process-input.

         
           add 1 to ws-record-count.
           if((il-par-unit-price 
             not equal spaces))
               if (il-part-number is not numeric) or (not il-class-valid
                 ) or
                 (il-par-unit-price is not numeric) or
                 (not il-price-lss-50) or (il-part-description is equal
                 spaces) or (not il-part-description is alphabetic) or
                 (not vendor-number-less-3)
                 then
                   add 1               to ws-bad-count
                   add 1               to ws-line-count
                   if (ws-line-count > ws-lines-per-page-cnst or
                     ws-page-count =
                     0)
                       move 0          to ws-line-count
                       add 1           to ws-page-count

                       if ws-page-count > 1 then
                           write error-line from ws-blank-line
                             after page
                       end-if
                    end-if
                   write error-line        from ws-report-heading2
                     after advancing 2 line
                   write error-line        from ws-report-heading3
                     after advancing 2 lines
                   write error-line        from ws-report-heading4
                   move spaces to error-line

                   if il-part-number is not numeric then
                       move ws-record-count        to ws-record-number
                       move input-line             to ws-file-line
                       write error-line            from ws-detail-line
                         after advancing 2 lines
                       move ws-error-text-1-cnst   to ws-error-code-desc
                       write error-line            from  ws-error-line
                         after advancing 1 line
                   end-if

                   if not il-class-valid then
                       move ws-record-count        to ws-record-number
                       if (error-line equals spaces)
                           move input-line         to ws-file-line
                           write error-line        from ws-detail-line
                             after advancing 2 lines
                       end-if
                       move ws-error-text-2-cnst   to ws-error-code-desc
                       write error-line            from ws-error-line
                       after advancing 1 line

                   end-if

                   if not (il-maint-code is equal 'D')
                       if (il-par-unit-price is not numeric) or not
                         (il-price-lss-50) then
                           move ws-record-count    to ws-record-number
                           if (error-line equals spaces)
                               move input-line     to ws-file-line
                               write error-line    from ws-detail-line
                                 after advancing 2 lines
                           end-if
                           move ws-error-text-3-cnst to
                             ws-error-code-desc
                           write error-line        from ws-error-line
                             after advancing 1 line
                       end-if

                       if (il-part-description is equal spaces) then
                           move ws-record-count    to ws-record-number
                           if (error-line equals spaces)
                               move input-line     to ws-file-line
                               write error-line    from ws-detail-line
                                 after advancing 2 lines
                           end-if
                           move ws-error-text-4-cnst to
                             ws-error-code-desc
                           write error-line          from 
                           ws-error-line
                             after advancing 1 line
                       end-if

                       if not (il-part-description is alphabetic)
                           move ws-record-count     to 
                           ws-record-number
                           if (error-line equals spaces)
                               move input-line     to ws-file-line
                               write error-line    from ws-detail-line
                                 after advancing 2 lines
                           end-if
                           move ws-error-text-5-cnst to
                             ws-error-code-desc
                           write error-line        from ws-error-line
                             after advancing 1 line
                       end-if

                       if not (vendor-number-less-3)
                           move ws-record-count    to ws-record-number
                           if (error-line equals spaces)
                               move input-line     to ws-file-line
                               write error-line    from ws-detail-line
                                 after advancing 2 lines
                           end-if
                           move ws-error-text-6-cnst to
                             ws-error-code-desc
                           write error-line        from ws-error-line
                             after advancing 1 line
                       end-if
                   end-if
               end-if
           else
               add 1 to ws-good-count
               if(il-maint-code equals 'A')
                   add 1 to ws-good-A-count
               end-if
               if (il-maint-code equals 'C')
                   add 1 to ws-good-C-count
               end-if
               if (il-maint-code equals 'D')
                   add 1 to ws-good-D-count
               end-if
           end-if.
           
           

              read input-file 
              at end move ws-true-cnst to ws-eof-flag.

      *Writing summary line.
       200-summary-line.
           move ws-record-count    to ws-total-data.
           move ws-good-count      to ws-good-data.
           move ws-bad-count       to ws-bad-data.
           move ws-good-A-count    to ws-good-add.
           move ws-good-C-count    to ws-good-changes.
           move ws-good-D-count    to ws-good-deletes.

           write error-line    from ws-summary-line1
           after advancing 2 lines.
           write error-line    from ws-summary-line2
           after advancing 2 lines.
           write error-line    from ws-summary-line3
             after advancing 2 lines.
           write error-line    from ws-summary-line4
             after advancing 2 lines.
           write error-line    from ws-summary-line5
             after advancing 2 lines.
           write error-line    from ws-summary-line6
             after advancing 2 lines.


       end program A6-DataValidation.