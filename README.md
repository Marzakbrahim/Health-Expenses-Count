# Health-Expenses-Count
## Objectives
- Implementation of sequential file reading and writing.
- Passing parameters to a program.
## Treatment
Goal: Develop a program to read a file containing health expense records and produce a file summarizing the total expenses for each month of each year.
## Specifications
The file "DCPT-S.txt" contains a list of one thousand health expense records. The file format is as follows:
- Product Code: X(2)
- Policy Number: X(12)
- Date of Treatment: 9(7)
- Amount reimbursed by the health insurance: 9(7)V99

The objective of the program is to produce a table that lists the total expenses for each month of each year. The resulting file will have the following format:
MOIS * ANNE * MONTANT TOTAL REMBOURSE
01 2003 2.801,87
02 2003 3.103,92

## Program Implementation
The provided code aims to achieve the project's objectives. It reads the input file, processes the records, and writes the summarized data to an output file. Below are key points regarding the implementation:
- File Handling:
    - Input file: "DCPT-S.txt"
    - Output file: "MODIF-DCPT-S.txt"
    - Error file: "DCPT-S-Erreur.txt"
    - Sequential file organization is used for input, output, and error files.
- Variables and Structures:
    - Various data structures are defined to store record fields.
    - File status variables are used for error handling.
- Processing Steps:
    - Initialization: Open input and output files, initialize variables, and write the header to the output file.
    - Processing: Read each record, perform necessary calculations, and write summarized data to the output file.
    - Error Handling: Check for errors in record fields and log them in the error file.
- Conclusion:
    - The provided code is designed to efficiently process health expense records and generate a summarized report.
    - It handles file operations and errors gracefully.
    - After execution, the program displays the number of input and output records, as well as any encountered errors.
## Conclusion
This program effectively accomplishes the task of summarizing health expense records by month and year. It reads input data, performs necessary calculations, handles errors, and produces the desired output.
