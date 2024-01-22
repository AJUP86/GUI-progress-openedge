&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomer NO-UNDO LIKE Customer
       field rowIdent as rowid index rowIdent rowIdent.
DEFINE TEMP-TABLE ttSalesrep NO-UNDO LIKE Salesrep
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
{ttSort.i}
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---     
                                  */
define variable ghOrder     as handle    no-undo.
define variable glResponse  as logical   no-undo.                                
define variable ghProcLib   as handle    no-undo.  
define variable ghDataUtil  as handle    no-undo.
define variable ghDetails   as handle    no-undo.
define variable cSortMethod as character no-undo.
define temp-table ttCustomerUpd no-undo like ttCustomer.

define variable gcWhereClause  as character     no-undo.
define variable gcSortByClause as character     no-undo.
define variable gfirstRowIdent as rowid         no-undo.
define variable gLastRowIdent  as rowid         no-undo.


define variable ascendingSort  as integer       no-undo initial 1.
define variable descendingSort as integer       no-undo initial 2.
define variable noSort         as integer       no-undo initial 3.
define variable columnNr       as integer       no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME brCustomers

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomer

/* Definitions for BROWSE brCustomers                                   */
&Scoped-define FIELDS-IN-QUERY-brCustomers ttCustomer.CustNum ~
ttCustomer.Name ttCustomer.Comments 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brCustomers 
&Scoped-define QUERY-STRING-brCustomers FOR EACH ttCustomer NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brCustomers OPEN QUERY brCustomers FOR EACH ttCustomer NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brCustomers ttCustomer
&Scoped-define FIRST-TABLE-IN-QUERY-brCustomers ttCustomer


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-brCustomers}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brCustomers fiCustNum fiCustName fiComment ~
orders-btn fiRepName 
&Scoped-Define DISPLAYED-OBJECTS fiCustNum fiCustName fiComment fiRepName 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Customer 
       MENU-ITEM m_New          LABEL "New"           
       MENU-ITEM m_Modify       LABEL "Modify"        
       MENU-ITEM m_Delete       LABEL "Delete"        
       RULE
       MENU-ITEM m_Exit         LABEL "Exit"           ACCELERATOR "CTRL-Q".

DEFINE MENU MENU-BAR-C-Win MENUBAR
       SUB-MENU  m_Customer     LABEL "Customer"      .


/* Definitions of the field level widgets                               */
DEFINE BUTTON orders-btn 
     LABEL "Orders" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE fiComment AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 TOOLTIP "Search keyword" NO-UNDO.

DEFINE VARIABLE fiCustName AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 TOOLTIP "Search Name" NO-UNDO.

DEFINE VARIABLE fiCustNum AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 TOOLTIP "Search customer Number" NO-UNDO.

DEFINE VARIABLE fiRepName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rep Name" 
      VIEW-AS TEXT 
     SIZE 14 BY .64 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brCustomers FOR 
      ttCustomer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brCustomers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brCustomers C-Win _STRUCTURED
  QUERY brCustomers NO-LOCK DISPLAY
      ttCustomer.CustNum FORMAT ">>>>9":U WIDTH 11.6
      ttCustomer.Name FORMAT "x(30)":U
      ttCustomer.Comments FORMAT "x(80)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 13.85 ROW-HEIGHT-CHARS .67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     brCustomers AT ROW 1.77 COL 6 WIDGET-ID 200
     fiCustNum AT ROW 15.87 COL 4 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     fiCustName AT ROW 15.87 COL 16 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fiComment AT ROW 15.87 COL 47 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     orders-btn AT ROW 18.44 COL 87 WIDGET-ID 8
     fiRepName AT ROW 17.92 COL 11 COLON-ALIGNED WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.4 BY 19.87 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttCustomer T "?" NO-UNDO Sports2000 Customer
      ADDITIONAL-FIELDS:
          field rowIdent as rowid index rowIdent rowIdent
      END-FIELDS.
      TABLE: ttSalesrep T "?" NO-UNDO Sports2000 Salesrep
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
         TITLE              = "Customers"
         HEIGHT             = 19.87
         WIDTH              = 102.4
         MAX-HEIGHT         = 19.87
         MAX-WIDTH          = 102.4
         VIRTUAL-HEIGHT     = 19.87
         VIRTUAL-WIDTH      = 102.4
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

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-Win:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB brCustomers 1 DEFAULT-FRAME */
ASSIGN 
       brCustomers:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       fiRepName:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brCustomers
/* Query rebuild information for BROWSE brCustomers
     _TblList          = "Temp-Tables.ttCustomer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ttCustomer.CustNum
"ttCustomer.CustNum" ? ? "integer" ? ? ? ? ? ? no ? no no "11.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.ttCustomer.Name
     _FldNameList[3]   = Temp-Tables.ttCustomer.Comments
     _Query            is OPENED
*/  /* BROWSE brCustomers */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON end-error OF C-Win /* Customers */
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
ON window-close OF C-Win /* Customers */
do:
        /* This event will close the window and terminate the procedure.  */
        apply "CLOSE":U to this-procedure.
        return no-apply.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brCustomers
&Scoped-define SELF-NAME brCustomers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomers C-Win
ON default-action OF brCustomers IN FRAME DEFAULT-FRAME
do:
   
        if not valid-handle ( ghDetails ) then
        do: 
            run details.w persistent set ghDetails ( input ghProcLib, input ghDataUtil ).
            run ValueChanged in ghDetails       ( ttCustomer.RowIdent). 
    
            subscribe "getNextCustomer" in ghDetails.
            subscribe "getPrevCustomer" in ghDetails.
            subscribe "getFirstCustomer" in ghDetails.
            subscribe "getLastCustomer" in ghDetails.
    
    
        end. 
        apply "value-changed" to brCustomers in frame default-frame.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomers C-Win
ON start-search OF brCustomers IN FRAME DEFAULT-FRAME
do :
        define variable currentRow as rowid no-undo.
        define buffer bfSort for ttSort.
        define variable ColumnHandle as handle no-undo.
    
        ColumnHandle = {&BROWSE-NAME}:CURRENT-COLUMN.

        find bfSort where bfSort.ColumnHandle = ColumnHandle.
        case bfSort.SortType:
            when NoSort         then 
                bfSort.SortType = AscendingSort.
            when ascendingSort  then 
                bfSort.SortType = DescendingSort.
            when DescendingSort then 
                bfSort.SortType = NoSort.
        end case.
    
        gcSortByClause = substitute( "BY &1 &2",
            ColumnHandle:name, 
            ( if bfSort.SortType = DescendingSort
            then " DESCENDING"
            else "" )).
        for each bfSort:
      
            if bfSort.ColumnHandle = ColumnHandle or 
                bfSort.SortType = NoSort then 
                next.
        
            gcSortByClause = substitute( "&1 BY &2 &3",
                gcSortByClause,
                bfSort.ColumnHandle:NAME,
                ( if bfSort.SortType = DescendingSort
                then " DESCENDING"
                else "" ) ). 
                       
        end.

        currentRow = rowid(ttCustomer).
        run ReopenQuery(currentRow).
        
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brCustomers C-Win
ON value-changed OF brCustomers IN FRAME DEFAULT-FRAME
do:
        define variable repName as character no-undo.
        if not available ttCustomer then return.

        if valid-handle( ghDetails ) then run ValueChanged in ghDetails ( ttcustomer.RowIdent ). 
        if valid-handle (ghOrder) then run ValueChanged in ghOrder (ttCustomer.CustNum, ttCustomer.Name).
        assign 
            fiRepName = "No Customer.".
        if ttCustomer.SalesRep <> "" then run GetRepData in ghDataUtil (output repName, input ttcustomer.SalesRep).
        fiRepName = repName.

        display fiRepName with frame Default-frame in window C-win.
        run setButtons.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiComment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiComment C-Win
ON value-changed OF fiComment IN FRAME DEFAULT-FRAME
do:
        run setSearchQuery.  
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustName C-Win
ON value-changed OF fiCustName IN FRAME DEFAULT-FRAME
do:
        run setSearchQuery.  
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiCustNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiCustNum C-Win
ON value-changed OF fiCustNum IN FRAME DEFAULT-FRAME
do:
        run setSearchQuery.
    
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_New
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_New C-Win
ON choose OF MENU-ITEM m_New /* New */
do:
        define variable rowRowIdent as rowid no-undo. 
        run custMaint.w (input "New",
            input ghProcLib,
            input ?,
            output TABLE ttCustomerUpd,
            input ghDataUtil). 
        find first ttCustomerUpd.
        rowRowIdent = ttCustomerUpd.rowIdent.  
        if rowRowIdent <> ? then 
        do:
            create ttCustomer. 
            buffer-copy ttCustomerUpd to ttCustomer. 
            run ReopenQuery(rowid(ttCustomer)).
        end. 
          
    end.

on choose of menu-item m_Modify in sub-menu m_Customer   
    do:
        define variable rowRowIdent as rowid no-undo.

        run custMaint.w (input "Mod":U,  
            input ghProcLib,
            input ttCustomer.RowIdent,
            output TABLE ttCustomerUpd,
        input ghDataUtil).
        if return-value matches "*deleted*" then
        do :
            delete ttCustomer.
            brCustomers:DELETE-CURRENT-ROW() in frame {&frame-name}.
        end.
        else
        do:
            find first ttCustomerUpd.                    /* Make returned record available */
            buffer-copy ttCustomerUpd to ttCustomer.      /* Update local record */    
            run ReopenQuery(rowid(ttCustomer)).                             /* Reopen Query */
        end.
    end.

on choose of menu-item m_Delete in sub-menu m_Customer
    do:
        message "Are you sure you want to delete this Client record?"
            view-as alert-box question buttons yes-no
            update glResponse. 
        if glResponse then
        do:
            run DeleteCustomer in ghDataUtil(input ttCustomer.RowIdent).
            if return-value = "" then
            do:
                delete ttCustomer.
                brCustomers:DELETE-CURRENT-ROW() in frame {&frame-name}.
            end.
            else
                message return-value
                    view-as alert-box information buttons ok.
        end.  
    end.

on choose of menu-item m_Exit in sub-menu m_Customer
    do:
        apply "CLOSE" to this-procedure.
        return no-apply.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME orders-btn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL orders-btn C-Win
ON choose OF orders-btn IN FRAME DEFAULT-FRAME /* Orders */
do:
    
        if not valid-handle(ghOrder) then 
        do:
            run Orders.w persistent set ghOrder( input ghProcLib , input ghDataUtil ). 
            run valuechanged in ghOrder( ttCustomer.CustNum, ttCustomer.Name ). 
        end.    
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
do:
    publish "Shutdown".
    run disable_UI.
end.    
    

/* Best default for GUI applications is...                              */
pause 0 before-hide.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
do on error   undo MAIN-BLOCK, leave MAIN-BLOCK
    on end-key undo MAIN-BLOCK, leave MAIN-BLOCK:
    run InitializeObjects.
    run enable_UI.
    run ReopenQuery(?).
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
  DISPLAY fiCustNum fiCustName fiComment fiRepName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE brCustomers fiCustNum fiCustName fiComment orders-btn fiRepName 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFirstAndLastRowId C-Win 
PROCEDURE GetFirstAndLastRowId :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    gfirstRowIdent = ?.
    gLastRowIdent = ?.
    if not available ttCustomer then return.
    gfirstRowIdent = ttCustomer.rowIdent.
    if browse brCustomers:query:get-last () then 
    do: 
        gLastRowIdent = ttCustomer.rowIdent.
        browse brCustomers:query:get-first ().
    end.    
       
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFirstCustomer C-Win 
PROCEDURE getFirstCustomer :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    if available ttCustomer then
        apply "Home"          to brCustomers in frame {&frame-name}.
    apply "value-changed" to brCustomers in frame {&frame-name}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getLastCustomer C-Win 
PROCEDURE getLastCustomer :
/*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/
    if available ttCustomer then
        apply "end" to brCustomers in frame {&frame-name}.
    apply "value-changed" to brCustomers in frame {&frame-name}.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNextCustomer C-Win 
PROCEDURE getNextCustomer :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    if available ttCustomer then 
        
    apply "cursor-down" to browse {&BROWSE-NAME}.
    apply "value-changed" to brCustomers in frame {&frame-name}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPrevCustomer C-Win 
PROCEDURE getPrevCustomer :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    if available ttCustomer then
        browse brCustomers:select-prev-row () no-error.
    apply "value-changed" to brCustomers in frame {&frame-name}.

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTtSort C-Win 
PROCEDURE GetTtSort :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    do ColumnNr = 1 to browse {&BROWSE-NAME}:NUM-COLUMNS: 
        create ttSort.
        assign 
            ttSort.ColumnHandle = browse {&BROWSE-NAME}:GET-BROWSE-COLUMN( ColumnNr )
            ttSort.SortType     = (if ttSort.ColumnHandle:name = "CustNum" then ascendingSort else noSort).
        .
    end.


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects C-Win 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
             Purpose:
             Notes:
            ------------------------------------------------------------------------------*/

    run persistentproc.p persistent set ghProcLib.
    ghDataUtil = dynamic-function('RunPersistent' in ghProcLib, "DataUtil.p":U).
    run GetCustData in ghDataUtil (output TABLE ttCustomer). 
    run GetTtSort.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReopenQuery C-Win 
PROCEDURE ReopenQuery :
/*------------------------------------------------------------------------------*/
    /* Purpose:                                                                       */
    /* Notes:                                                                         */
    /*------------------------------------------------------------------------------*/
    define variable focusedRow as integer no-undo.
    define input parameter CurrentRowid as rowid   no-undo.
   
    focusedRow = browse brCustomers:focused-row.
    query brCustomers:query-close ().
    query brCustomers:query-prepare (                                               
        substitute("FOR EACH ttCustomer NO-LOCK &1 &2                                
                    ", gcWhereClause, gcSortByClause)         
        ).
   
    query brCustomers:query-open ().
    run GetFirstAndLastRowId.
    
    if currentRowid <> ? then
    do:
        browse brCustomers:set-repositioned-row (focusedRow) no-error.   
        browse brCustomers:query:reposition-to-rowid (CurrentRowid) no-error.
        

    end.
    apply "value-changed" to brCustomers in frame default-frame. 
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButtons C-Win 
PROCEDURE setButtons :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    if not available ttCustomer then return.
    if valid-handle(ghDetails) then run setButtons in ghDetails(gfirstRowIdent, gLastRowIdent).
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSearchQuery C-Win 
PROCEDURE setSearchQuery :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    gcWhereClause = "WHERE true".
    do with frame {&FRAME-NAME}: 
    end.
    assign  fiCustNum fiCustName fiComment.
    if fiCustNum = 0 and fiCustName = "" and fiComment = ""then
        return.
    if fiCustNum  <>  0 then gcWhereClause = gcWhereClause + substitute(" and ttCustomer.custNum >= '&1'", fiCustNum).
    if fiCustName <> "" then gcWhereClause = gcWhereClause + substitute(" and ttCustomer.name begins '&1'", fiCustName).
    if fiComment  <> "" then gcWhereClause = gcWhereClause + SUBSTITUTE(" and ttCustomer.comments matches('*&1*')" , fiComment).      
    run ReopenQuery(?).


end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

