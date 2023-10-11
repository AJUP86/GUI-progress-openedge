&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomer NO-UNDO LIKE Customer
       field rowIdent as rowid index rowIdent RowIdent.



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

/* Parameters Definitions ---                                           */
define input parameter ghProcLib as handle no-undo. 


/*define input parameter rowRowId as rowid no-undo.*/

/* Local Variable Definitions ---                                       */
define input parameter ghDataUtil as handle        no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME detailsFrame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomer

/* Definitions for FRAME detailsFrame                                   */
&Scoped-define FIELDS-IN-QUERY-detailsFrame ttCustomer.CustNum ~
ttCustomer.Contact ttCustomer.Name ttCustomer.Phone ttCustomer.Country ~
ttCustomer.EmailAddress ttCustomer.PostalCode ttCustomer.City ~
ttCustomer.SalesRep ttCustomer.State ttCustomer.Discount ~
ttCustomer.CreditLimit ttCustomer.Address ttCustomer.Address2 ~
ttCustomer.Comments 
&Scoped-define ENABLED-FIELDS-IN-QUERY-detailsFrame ttCustomer.CustNum ~
ttCustomer.Contact ttCustomer.Name ttCustomer.Phone ttCustomer.Country ~
ttCustomer.EmailAddress ttCustomer.PostalCode ttCustomer.City ~
ttCustomer.SalesRep ttCustomer.State ttCustomer.Discount ~
ttCustomer.CreditLimit ttCustomer.Address ttCustomer.Address2 ~
ttCustomer.Comments 
&Scoped-define ENABLED-TABLES-IN-QUERY-detailsFrame ttCustomer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-detailsFrame ttCustomer
&Scoped-define QUERY-STRING-detailsFrame FOR EACH ttCustomer SHARE-LOCK
&Scoped-define OPEN-QUERY-detailsFrame OPEN QUERY detailsFrame FOR EACH ttCustomer SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-detailsFrame ttCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-detailsFrame ttCustomer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ttCustomer.CustNum ttCustomer.Contact ~
ttCustomer.Name ttCustomer.Phone ttCustomer.Country ttCustomer.EmailAddress ~
ttCustomer.PostalCode ttCustomer.City ttCustomer.SalesRep ttCustomer.State ~
ttCustomer.Discount ttCustomer.CreditLimit ttCustomer.Address ~
ttCustomer.Address2 ttCustomer.Comments 
&Scoped-define ENABLED-TABLES ttCustomer
&Scoped-define FIRST-ENABLED-TABLE ttCustomer
&Scoped-Define ENABLED-OBJECTS BtnDone BtnFirst BtnPrev BtnNext BtnLast 
&Scoped-Define DISPLAYED-FIELDS ttCustomer.CustNum ttCustomer.Contact ~
ttCustomer.Name ttCustomer.Phone ttCustomer.Country ttCustomer.EmailAddress ~
ttCustomer.PostalCode ttCustomer.City ttCustomer.SalesRep ttCustomer.State ~
ttCustomer.Discount ttCustomer.CreditLimit ttCustomer.Address ~
ttCustomer.Address2 ttCustomer.Comments 
&Scoped-define DISPLAYED-TABLES ttCustomer
&Scoped-define FIRST-DISPLAYED-TABLE ttCustomer


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "Close" 
     SIZE 16 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnFirst 
     IMAGE-UP FILE "media/first.bmp":U
     LABEL "&First" 
     SIZE 4 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnLast 
     IMAGE-UP FILE "media/last.bmp":U
     LABEL "&Last" 
     SIZE 4 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnNext 
     IMAGE-UP FILE "media/next.bmp":U
     LABEL "&Next" 
     SIZE 4 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnPrev 
     IMAGE-UP FILE "media/prev.bmp":U
     LABEL "&Prev" 
     SIZE 4 BY 1.13
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY detailsFrame FOR 
      ttCustomer SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME detailsFrame
     BtnDone AT ROW 1.26 COL 86 WIDGET-ID 40
     BtnFirst AT ROW 2.54 COL 86 WIDGET-ID 32
     BtnPrev AT ROW 2.54 COL 90 WIDGET-ID 34
     BtnNext AT ROW 2.54 COL 94 WIDGET-ID 36
     BtnLast AT ROW 2.54 COL 98 WIDGET-ID 38
     ttCustomer.CustNum AT ROW 1.51 COL 14 COLON-ALIGNED WIDGET-ID 16
           VIEW-AS TEXT 
          SIZE 7.7 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.Contact AT ROW 1.51 COL 59 COLON-ALIGNED WIDGET-ID 10
           VIEW-AS TEXT 
          SIZE 21 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.Name AT ROW 2.8 COL 14 COLON-ALIGNED WIDGET-ID 22
           VIEW-AS TEXT 
          SIZE 31.2 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.Phone AT ROW 2.8 COL 59 COLON-ALIGNED WIDGET-ID 24
           VIEW-AS TEXT 
          SIZE 21.2 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.Country AT ROW 4.08 COL 14 COLON-ALIGNED WIDGET-ID 12
           VIEW-AS TEXT 
          SIZE 21.2 BY .64
          FONT 6
     ttCustomer.EmailAddress AT ROW 4.08 COL 59 COLON-ALIGNED WIDGET-ID 20
           VIEW-AS TEXT 
          SIZE 27 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.PostalCode AT ROW 5.36 COL 14 COLON-ALIGNED WIDGET-ID 26
           VIEW-AS TEXT 
          SIZE 14.5 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.City AT ROW 6.64 COL 14 COLON-ALIGNED WIDGET-ID 6
           VIEW-AS TEXT 
          SIZE 26.2 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.SalesRep AT ROW 6.64 COL 59 COLON-ALIGNED WIDGET-ID 28
           VIEW-AS TEXT 
          SIZE 8.5 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.State AT ROW 7.92 COL 14 COLON-ALIGNED WIDGET-ID 30
           VIEW-AS TEXT 
          SIZE 21.2 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.Discount AT ROW 7.92 COL 59 COLON-ALIGNED WIDGET-ID 18
           VIEW-AS TEXT 
          SIZE 6.4 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.CreditLimit AT ROW 9.21 COL 59 COLON-ALIGNED WIDGET-ID 14
           VIEW-AS TEXT 
          SIZE 14.2 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.Address AT ROW 9.46 COL 14 COLON-ALIGNED WIDGET-ID 2
           VIEW-AS TEXT 
          SIZE 29 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.Address2 AT ROW 11.26 COL 13 COLON-ALIGNED WIDGET-ID 4
           VIEW-AS TEXT 
          SIZE 31 BY .64
          FGCOLOR 9 FONT 6
     ttCustomer.Comments AT ROW 12.8 COL 14 COLON-ALIGNED WIDGET-ID 8
           VIEW-AS TEXT 
          SIZE 81.2 BY .64
          FGCOLOR 9 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104.8 BY 13.41
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttCustomer T "?" NO-UNDO Sports2000 Customer
      ADDITIONAL-FIELDS:
          field rowIdent as rowid index rowIdent RowIdent
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 13.41
         WIDTH              = 104.8
         MAX-HEIGHT         = 22.97
         MAX-WIDTH          = 126.2
         VIRTUAL-HEIGHT     = 22.97
         VIRTUAL-WIDTH      = 126.2
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
/* SETTINGS FOR FRAME detailsFrame
   FRAME-NAME                                                           */
ASSIGN 
       ttCustomer.Address:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.Address2:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.City:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.Comments:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.Contact:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.Country:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.CreditLimit:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.CustNum:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.Discount:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.EmailAddress:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.Name:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.Phone:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.PostalCode:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.SalesRep:READ-ONLY IN FRAME detailsFrame        = TRUE.

ASSIGN 
       ttCustomer.State:READ-ONLY IN FRAME detailsFrame        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME detailsFrame
/* Query rebuild information for FRAME detailsFrame
     _TblList          = "Temp-Tables.ttCustomer"
     _Query            is OPENED
*/  /* FRAME detailsFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON end-error OF C-Win /* <insert window title> */
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
ON window-close OF C-Win /* <insert window title> */
do:
        /* This event will close the window and terminate the procedure.  */
        apply "CLOSE":U to this-procedure.
        return no-apply.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON choose OF BtnDone IN FRAME detailsFrame /* Close */
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


&Scoped-define SELF-NAME BtnFirst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFirst C-Win
ON choose OF BtnFirst IN FRAME detailsFrame /* First */
do:
        publish "getFirstCustomer".
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnLast
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnLast C-Win
ON choose OF BtnLast IN FRAME detailsFrame /* Last */
do:
        publish "getLastCustomer".
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNext
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNext C-Win
ON choose OF BtnNext IN FRAME detailsFrame /* Next */
do:
  
        publish "getNextCustomer".  
  

    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnPrev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnPrev C-Win
ON choose OF BtnPrev IN FRAME detailsFrame /* Prev */
do:
        publish "getPrevCustomer".
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   // run InitializeObjects.
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

  {&OPEN-QUERY-detailsFrame}
  GET FIRST detailsFrame.
  IF AVAILABLE ttCustomer THEN 
    DISPLAY ttCustomer.CustNum ttCustomer.Contact ttCustomer.Name ttCustomer.Phone 
          ttCustomer.Country ttCustomer.EmailAddress ttCustomer.PostalCode 
          ttCustomer.City ttCustomer.SalesRep ttCustomer.State 
          ttCustomer.Discount ttCustomer.CreditLimit ttCustomer.Address 
          ttCustomer.Address2 ttCustomer.Comments 
      WITH FRAME detailsFrame IN WINDOW C-Win.
  ENABLE BtnDone BtnFirst BtnPrev BtnNext BtnLast ttCustomer.CustNum 
         ttCustomer.Contact ttCustomer.Name ttCustomer.Phone ttCustomer.Country 
         ttCustomer.EmailAddress ttCustomer.PostalCode ttCustomer.City 
         ttCustomer.SalesRep ttCustomer.State ttCustomer.Discount 
         ttCustomer.CreditLimit ttCustomer.Address ttCustomer.Address2 
         ttCustomer.Comments 
      WITH FRAME detailsFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-detailsFrame}
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
    //ghDataUtil = dynamic-function('RunPersistent' in ghProcLib, "DataUtil.p":U).
    
    

/*    run GetCustRecord in ghDataUtil(output table ttCustomer, input rowRowId).*/


    
   
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButtons C-Win 
PROCEDURE setButtons :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define input parameter firstRow as rowid no-undo.
    define input parameter lastRow as rowid no-undo.

    do with frame {&frame-name}:
    
        assign  
            BtnFirst:sensitive = (ttCustomer.rowIdent <> firstRow)
            BtnLast:sensitive  = (ttCustomer.rowIdent <> lastRow)
            BtnPrev:sensitive  = BtnFirst:sensitive
            BtnNext:sensitive  = BtnLast:sensitive.
    end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChanged C-Win 
PROCEDURE ValueChanged :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define input  parameter prowCustRow as rowid no-undo.

    run getCustRecord in ghDataUtil ( output table ttCustomer,
        input prowCustRow ).

    run enable_UI.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

