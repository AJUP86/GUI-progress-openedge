
/*------------------------------------------------------------------------
    File        : ttCustomer.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Alejandro
    Created     : Tue Sep 12 17:43:09 CEST 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table ttCustomer{&sufix} no-undo like Customer
field rowIdent as rowid
index roIdent rowIdent.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
