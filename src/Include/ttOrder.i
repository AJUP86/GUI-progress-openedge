
/*------------------------------------------------------------------------
    File        : ttOrder.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Alejandro
    Created     : Wed Sep 20 16:35:51 CEST 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table ttOrder{&sufix} no-undo like Order
    field rowIdent as rowid
    index roIdent rowIdent.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
