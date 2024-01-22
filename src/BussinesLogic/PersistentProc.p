&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    Library     : PersistentProc.p
    Purpose     : library of frequently used functions or procecedures. 

    Syntax      :

    Description : any common functions or procecedures are in here. This can be 
                  referred to from any program through the handle of this
                  peristent procecedures

    Author(s)   : Andrew Ferguson
    Created     : Aug 2000
    Notes       : Modified Stan Swiercz Nov 2000.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-RunPersistent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD RunPersistent Procedure 
function RunPersistent returns handle
    ( input pcProcedureName as character )  forward.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* Shutdown when user exits application */
subscribe to "Shutdown":U anywhere.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Shutdown) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Shutdown Procedure 
procedure Shutdown :
    /*------------------------------------------------------------------------------
      Purpose: Delete this procedure from memory when the user exits the applicaion.    
      Parameters:  <none>
      Notes: Usually executes in response to Shutdown event.      
    ------------------------------------------------------------------------------*/
    if this-procedure:persistent then 
        delete procedure this-procedure.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-RunPersistent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION RunPersistent Procedure 
function RunPersistent returns handle
    ( input pcProcedureName as character ) :
    /*------------------------------------------------------------------------------
      Purpose:  This function checks to see if a persistent procedure is already 
                running. If not, it runs the procedure persistently. It returns
                the handle to the persistent procedure.
       Notes:  
    ------------------------------------------------------------------------------*/
    define variable hproc as handle no-undo.

    hProc = session:first-procedure.
    do while valid-handle(hProc) and hProc:file-name ne pcProcedureName:
        hProc = hProc:next-sibling.
    end.

    if not valid-handle(hProc) then
        run VALUE(pcProcedureName) persistent set hProc.

    return hProc.   /* Function return value. */

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

