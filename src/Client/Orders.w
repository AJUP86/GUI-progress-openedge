&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttOrder NO-UNDO LIKE Order
       field rowIdent as rowid index rowIdent rowIdent.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

create widget-pool.

/* ***************************  Definitions  ************************** */
subscribe to "Shutdown":U anywhere.
/* Parameters Definitions ---                                           */
define input  parameter            phProcLib       as handle      no-undo.
define input  parameter            ghDataUtil      as handle      no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BrOrders

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttOrder

/* Definitions for BROWSE BrOrders                                      */
&Scoped-define FIELDS-IN-QUERY-BrOrders ttOrder.Ordernum ttOrder.OrderDate ~
ttOrder.OrderStatus 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BrOrders 
&Scoped-define QUERY-STRING-BrOrders FOR EACH ttOrder NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BrOrders OPEN QUERY BrOrders FOR EACH ttOrder NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BrOrders ttOrder
&Scoped-define FIRST-TABLE-IN-QUERY-BrOrders ttOrder


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BrOrders}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BrOrders BtnDone 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BrOrders FOR 
      ttOrder SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BrOrders
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BrOrders C-Win _STRUCTURED
  QUERY BrOrders NO-LOCK DISPLAY
      ttOrder.Ordernum FORMAT "zzzzzzzzz9":U WIDTH 13.6
      ttOrder.OrderDate FORMAT "99/99/99":U WIDTH 11.6
      ttOrder.OrderStatus FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 73 BY 10.51 ROW-HEIGHT-CHARS .64 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BrOrders AT ROW 1.51 COL 5 WIDGET-ID 200
     BtnDone AT ROW 12.8 COL 63 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.23
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttOrder T "?" NO-UNDO Sports2000 Order
      ADDITIONAL-FIELDS:
          field rowIdent as rowid index rowIdent rowIdent
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Orders"
         HEIGHT             = 13.23
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BrOrders 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BrOrders
/* Query rebuild information for BROWSE BrOrders
     _TblList          = "Temp-Tables.ttOrder"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ttOrder.Ordernum
"ttOrder.Ordernum" ? ? "integer" ? ? ? ? ? ? no ? no no "13.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ttOrder.OrderDate
"ttOrder.OrderDate" ? ? "date" ? ? ? ? ? ? no ? no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.ttOrder.OrderStatus
     _Query            is OPENED
*/  /* BROWSE BrOrders */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON end-error OF C-Win /* Orders */
or endkey of {&WINDOW-NAME} anywhere 
    do:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        if this-procedure:persistent then return no-apply.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON window-close OF C-Win /* Orders */
do:
        /* This event will close the window and terminate the procedure.  */
        apply "CLOSE":U to this-procedure.
        return no-apply.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON choose OF BtnDone IN FRAME DEFAULT-FRAME /* Done */
do:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
        apply "CLOSE":U to this-procedure.
  &ENDIF
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BrOrders
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
assign CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
on close of this-procedure 
    run disable_UI.

/* Best default for GUI applications is...                              */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
    on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
    if not this-procedure:persistent then
        wait-for close of this-procedure.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  ENABLE BrOrders BtnDone 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects C-Win 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/
   

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReopenQuery C-Win 
PROCEDURE ReopenQuery :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    query brOrders:query-prepare(  "for each ttOrder").
    query brOrders:query-open().

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Shutdown C-Win 
PROCEDURE Shutdown PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
apply "close" to this-procedure.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChanged C-Win 
PROCEDURE ValueChanged :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    define input parameter icustNum as integer no-undo.
    define input  parameter CustName as character no-undo.
    run GetOrderData in ghDataUtil (output TABLE ttOrder, input icustNum).
    run ReopenQuery.
    run enable_ui. 
    {&WINDOW-NAME}:title = CustName.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

