
/*------------------------------------------------------------------------
    File        : ttState.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Alejandro
    Created     : Wed Oct 04 11:50:12 CEST 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define temp-table ttState{&sufix} no-undo like State
    field rowIdent as rowid
    index roIdent rowIdent.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
