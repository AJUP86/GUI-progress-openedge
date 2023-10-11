&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttOrder NO-UNDO LIKE Order
       field RowIdent as ROWID Index RowIdent RowIdent.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
define variable ghProcLib  as handle no-undo.
define variable ghDataUtil as handle no-undo. 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttOrder

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame ttOrder.Ordernum ~
ttOrder.CustNum ttOrder.OrderDate ttOrder.OrderStatus ttOrder.ShipDate ~
ttOrder.PromiseDate ttOrder.ShipToID ttOrder.BillToID ttOrder.WarehouseNum ~
ttOrder.Carrier ttOrder.PO ttOrder.Creditcard ttOrder.Terms ~
ttOrder.SalesRep ttOrder.Instructions 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ttOrder.Ordernum ~
ttOrder.CustNum ttOrder.OrderDate ttOrder.OrderStatus ttOrder.ShipDate ~
ttOrder.PromiseDate ttOrder.ShipToID ttOrder.BillToID ttOrder.WarehouseNum ~
ttOrder.Carrier ttOrder.PO ttOrder.Creditcard ttOrder.Terms ~
ttOrder.SalesRep ttOrder.Instructions 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame ttOrder
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame ttOrder
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ttOrder SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ttOrder SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ttOrder
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ttOrder


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ttOrder.Ordernum ttOrder.CustNum ~
ttOrder.OrderDate ttOrder.OrderStatus ttOrder.ShipDate ttOrder.PromiseDate ~
ttOrder.ShipToID ttOrder.BillToID ttOrder.WarehouseNum ttOrder.Carrier ~
ttOrder.PO ttOrder.Creditcard ttOrder.Terms ttOrder.SalesRep ~
ttOrder.Instructions 
&Scoped-define ENABLED-TABLES ttOrder
&Scoped-define FIRST-ENABLED-TABLE ttOrder
&Scoped-Define ENABLED-OBJECTS BtnNext-2 Btn_OK 
&Scoped-Define DISPLAYED-FIELDS ttOrder.Ordernum ttOrder.CustNum ~
ttOrder.OrderDate ttOrder.OrderStatus ttOrder.ShipDate ttOrder.PromiseDate ~
ttOrder.ShipToID ttOrder.BillToID ttOrder.WarehouseNum ttOrder.Carrier ~
ttOrder.PO ttOrder.Creditcard ttOrder.Terms ttOrder.SalesRep ~
ttOrder.Instructions 
&Scoped-define DISPLAYED-TABLES ttOrder
&Scoped-define FIRST-DISPLAYED-TABLE ttOrder


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnNext-2 
     IMAGE-UP FILE "adeicon/next.bmp":U
     LABEL "&Next" 
     SIZE 15 BY 1.13 TOOLTIP "next"
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ttOrder SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ttOrder.Ordernum AT ROW 2.03 COL 11 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
     ttOrder.CustNum AT ROW 2.03 COL 59 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 7.7 BY 1
     ttOrder.OrderDate AT ROW 3.31 COL 11 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ttOrder.OrderStatus AT ROW 3.31 COL 59 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "Ordered","Back Ordered","Partially Shipped","Shipped" 
          DROP-DOWN-LIST
          SIZE 24.8 BY 1
     ttOrder.ShipDate AT ROW 4.33 COL 59 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
     ttOrder.PromiseDate AT ROW 4.59 COL 11 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ttOrder.ShipToID AT ROW 5.62 COL 59 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
     ttOrder.BillToID AT ROW 5.87 COL 11 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
     ttOrder.WarehouseNum AT ROW 6.9 COL 59 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
     ttOrder.Carrier AT ROW 7.15 COL 11 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 26.2 BY 1
     ttOrder.PO AT ROW 8.44 COL 11 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     ttOrder.Creditcard AT ROW 9.72 COL 11 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "Visa","American Express","Master Card" 
          DROP-DOWN-LIST
          SIZE 24.8 BY 1
     ttOrder.Terms AT ROW 9.97 COL 61 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     ttOrder.SalesRep AT ROW 10.74 COL 11 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 8.5 BY 1
     ttOrder.Instructions AT ROW 12.28 COL 11 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 51.2 BY 1
     BtnNext-2 AT ROW 15.1 COL 70 WIDGET-ID 34
     Btn_OK AT ROW 16.38 COL 70
     SPACE(3.29) SKIP(0.40)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Order Information"
         DEFAULT-BUTTON Btn_OK WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttOrder T "?" NO-UNDO Sports2000 Order
      ADDITIONAL-FIELDS:
          field RowIdent as ROWID Index RowIdent RowIdent
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "Temp-Tables.ttOrder"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON window-close OF FRAME Dialog-Frame /* Order Information */
do:
  apply "END-ERROR":U to self.
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnNext-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnNext-2 Dialog-Frame
ON choose OF BtnNext-2 IN FRAME Dialog-Frame /* Next */
do:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN notify IN THIS-PROCEDURE ("get-next") NO-ERROR.
    &ELSE
      PUBLISH "fetchNext":U.
    &ENDIF
  &ELSEIF "{&TABLES-IN-QUERY-{&FRAME-NAME}}" ne "" &THEN
  /* This is a simple NEXT RECORD navigation button, useful for building
     test screens quickly.  NOTE: if there are no tables in the query, then
     this code will not compile; so use the preprocessor to skip it. */
      get next {&FRAME-NAME}.
      if not available {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}}
          then get last {&FRAME-NAME}.
      if available {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} then do:
          display {&FIELDS-IN-QUERY-{&FRAME-NAME}} with frame {&FRAME-NAME}.
   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      end.
  &ENDIF
end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
if valid-handle(active-window) and frame {&FRAME-NAME}:PARENT eq ?
then frame {&FRAME-NAME}:PARENT = active-window.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
   on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
  run InitializeObjects.
  run enable_UI.
  wait-for go of frame {&FRAME-NAME}.
end.
run disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  IF AVAILABLE ttOrder THEN 
    DISPLAY ttOrder.Ordernum ttOrder.CustNum ttOrder.OrderDate ttOrder.OrderStatus 
          ttOrder.ShipDate ttOrder.PromiseDate ttOrder.ShipToID ttOrder.BillToID 
          ttOrder.WarehouseNum ttOrder.Carrier ttOrder.PO ttOrder.Creditcard 
          ttOrder.Terms ttOrder.SalesRep ttOrder.Instructions 
      WITH FRAME Dialog-Frame.
  ENABLE ttOrder.Ordernum ttOrder.CustNum ttOrder.OrderDate ttOrder.OrderStatus 
         ttOrder.ShipDate ttOrder.PromiseDate ttOrder.ShipToID ttOrder.BillToID 
         ttOrder.WarehouseNum ttOrder.Carrier ttOrder.PO ttOrder.Creditcard 
         ttOrder.Terms ttOrder.SalesRep ttOrder.Instructions BtnNext-2 Btn_OK 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects Dialog-Frame 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    run PersistentProc.p persistent set ghProcLib. 
    ghDataUtil = dynamic-function('RunPersistent' in
        ghProcLib, "DataUtil.p":U). 
    run GetOrderData in ghDataUtil (output TABLE ttOrder).

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

