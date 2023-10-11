&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE ttCustomerUpd NO-UNDO LIKE Customer
       field rowIdent as rowid index rowdIdent rowIdent.



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
{ttState.i}
/* Parameters Definitions ---                                           */
define variable isUsa as logical no-undo initial true.
define input parameter pcMode           as character    no-undo.
define input parameter phProcLib        as handle       no-undo. 
define input parameter prowRowId        as rowid        no-undo.
define output parameter TABLE for ttCustomerUpd.
define input parameter ghDataUtil     as handle    no-undo.
/* Local Variable Definitions ---                                       */

define variable ghField        as handle    no-undo.
define variable ghCountryField as handle    no-undo.
define variable validNames     as character no-undo.
validNames = "Name,Phone, Country,EmailAddress,PostalCode,City,Address".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttCustomerUpd

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame ttCustomerUpd.Contact ~
ttCustomerUpd.Name ttCustomerUpd.Phone ttCustomerUpd.Country ~
ttCustomerUpd.EmailAddress ttCustomerUpd.PostalCode ttCustomerUpd.SalesRep ~
ttCustomerUpd.City ttCustomerUpd.Discount ttCustomerUpd.State ~
ttCustomerUpd.CreditLimit ttCustomerUpd.Address ttCustomerUpd.Address2 ~
ttCustomerUpd.Comments ttCustomerUpd.CustNum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ttCustomerUpd.Contact ~
ttCustomerUpd.Name ttCustomerUpd.Phone ttCustomerUpd.Country ~
ttCustomerUpd.EmailAddress ttCustomerUpd.PostalCode ttCustomerUpd.SalesRep ~
ttCustomerUpd.City ttCustomerUpd.Discount ttCustomerUpd.CreditLimit ~
ttCustomerUpd.Address ttCustomerUpd.Address2 ttCustomerUpd.Comments 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame ttCustomerUpd
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame ttCustomerUpd
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ttCustomerUpd SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ttCustomerUpd SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ttCustomerUpd
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ttCustomerUpd


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ttCustomerUpd.Contact ttCustomerUpd.Name ~
ttCustomerUpd.Phone ttCustomerUpd.Country ttCustomerUpd.EmailAddress ~
ttCustomerUpd.PostalCode ttCustomerUpd.SalesRep ttCustomerUpd.City ~
ttCustomerUpd.Discount ttCustomerUpd.CreditLimit ttCustomerUpd.Address ~
ttCustomerUpd.Address2 ttCustomerUpd.Comments 
&Scoped-define ENABLED-TABLES ttCustomerUpd
&Scoped-define FIRST-ENABLED-TABLE ttCustomerUpd
&Scoped-Define ENABLED-OBJECTS Btn_Save Btn_Cancel 
&Scoped-Define DISPLAYED-FIELDS ttCustomerUpd.Contact ttCustomerUpd.Name ~
ttCustomerUpd.Phone ttCustomerUpd.Country ttCustomerUpd.EmailAddress ~
ttCustomerUpd.PostalCode ttCustomerUpd.SalesRep ttCustomerUpd.City ~
ttCustomerUpd.Discount ttCustomerUpd.State ttCustomerUpd.CreditLimit ~
ttCustomerUpd.Address ttCustomerUpd.Address2 ttCustomerUpd.Comments ~
ttCustomerUpd.CustNum 
&Scoped-define DISPLAYED-TABLES ttCustomerUpd
&Scoped-define FIRST-DISPLAYED-TABLE ttCustomerUpd


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CapitalizeFirstLetter Dialog-Frame 
FUNCTION CapitalizeFirstLetter returns character
    ( input pString as character, input hCountry as handle ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormatPhone Dialog-Frame 
FUNCTION FormatPhone returns character
    ( inputPhone as character ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormattedCountry Dialog-Frame 
FUNCTION FormattedCountry returns character
    (rawCountry as character ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateEmail Dialog-Frame 
FUNCTION ValidateEmail returns character
    ( input email as character, input labelEmail as character) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidatePostalCode Dialog-Frame 
FUNCTION ValidatePostalCode returns character
    ( input rawPostal as character ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateTextFields Dialog-Frame 
FUNCTION ValidateTextFields returns character
    ( input rawInput as character, input fieldLabel as character, input hCountry as handle ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn_Save AUTO-GO 
     LABEL "Save" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ttCustomerUpd SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ttCustomerUpd.Contact AT ROW 2.03 COL 64 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Btn_Save AT ROW 2.03 COL 91
     ttCustomerUpd.Name AT ROW 3.31 COL 14 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 31.2 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Phone AT ROW 3.31 COL 64 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 14 
     Btn_Cancel AT ROW 3.31 COL 91
     ttCustomerUpd.Country AT ROW 4.59 COL 14 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
          BGCOLOR 14 
     ttCustomerUpd.EmailAddress AT ROW 4.59 COL 64 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
          BGCOLOR 14 
     ttCustomerUpd.PostalCode AT ROW 5.87 COL 14 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 14.5 BY 1
          BGCOLOR 14 
     ttCustomerUpd.SalesRep AT ROW 7.15 COL 64 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 8.5 BY 1
     ttCustomerUpd.City AT ROW 8 COL 14 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 26.2 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Discount AT ROW 8.44 COL 64 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     ttCustomerUpd.State AT ROW 9.21 COL 14 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "None","None",
                     "Web","Web"
          DROP-DOWN-LIST
          SIZE 16 BY 1
     ttCustomerUpd.CreditLimit AT ROW 9.72 COL 64 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
     ttCustomerUpd.Address AT ROW 10.49 COL 14 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 36.2 BY 1
          BGCOLOR 14 
     ttCustomerUpd.Address2 AT ROW 11.77 COL 14 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 36.2 BY 1
     ttCustomerUpd.Comments AT ROW 14.08 COL 14 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 81.2 BY 1
     ttCustomerUpd.CustNum AT ROW 2.03 COL 14 COLON-ALIGNED WIDGET-ID 18
           VIEW-AS TEXT 
          SIZE 7.7 BY .64
          FGCOLOR 9 
     SPACE(83.29) SKIP(13.68)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON Btn_Save CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttCustomerUpd T "?" NO-UNDO Sports2000 Customer
      ADDITIONAL-FIELDS:
          field rowIdent as rowid index rowdIdent rowIdent
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

/* SETTINGS FOR FILL-IN ttCustomerUpd.CustNum IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       ttCustomerUpd.CustNum:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR COMBO-BOX ttCustomerUpd.State IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "Temp-Tables.ttCustomerUpd"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON window-close OF FRAME Dialog-Frame /* <insert dialog title> */
do:
        apply "END-ERROR":U to self.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save Dialog-Frame
ON choose OF Btn_Save IN FRAME Dialog-Frame /* Save */
do:
    define variable hFieldName    as handle no-undo.
    define variable hFieldPhone   as handle no-undo.
    define variable hFieldCountry as handle no-undo.
    define variable hFieldEmail   as handle no-undo.
    define variable hFieldPostal  as handle no-undo.
    define variable hFieldCity    as handle no-undo.
    define variable hFieldAddress as handle no-undo.
    hFieldName = ttCustomerUpd.Name:handle.
    hFieldPhone = ttCustomerUpd.Phone:handle.
    hFieldCountry = ttCustomerUpd.Country:handle.
    hFieldEmail = ttCustomerUpd.EmailAddress:handle.
    hFieldPostal = ttCustomerUpd.PostalCode:handle.
    hFieldCity = ttCustomerUpd.City:handle.
    hFieldAddress = ttCustomerUpd.Address:handle.
        if not valid-handle(ghField) then 
        do:
            message "You haven't input any data yet!"
                view-as alert-box.
            return no-apply.
        end.
        if    hFieldName:screen-value <> "" and
        hFieldPhone:screen-value <> "" and 
        hFieldCountry:screen-value <> "" and
        hFieldEmail:screen-value <> "" and
        hFieldPostal:screen-value <> "" and
        hFieldCity:screen-value <> "" and
        hFieldAddress:screen-value <> "" then 
            
        do: 
            assign {&DISPLAYED-FIELDS}. 
            run SaveCustRecord in ghDataUtil (input-output TABLE ttCustomerUpd,input pcMode). 
            find first ttCustomerUpd.
            if pcMode = "New":U then 
            do:
                pcMode = "Mod":U. /* Set mode to Modify after creation */
                disable ttCustomerUpd.custNum with frame {&FRAME-NAME}.
            end. /* IF pcMode = "New" */ 
            display {&DISPLAYED-FIELDS} with frame {&FRAME-NAME}. 
            run reopenQuery . 
        end.    
        else 
        do:
            message "Unable to save! There are still empty required fields, complete them  and try again."
                view-as alert-box.
            return no-apply.
        end.
                                                                    
   
    
    end.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.Name,ttCustomerUpd.Phone,ttCustomerUpd.Country,ttCustomerUpd.EmailAddress,ttCustomerUpd.PostalCode,ttCustomerUpd.City,ttCustomerUpd.Address Dialog-Frame


on leave of ttCustomerUpd.Name,ttCustomerUpd.Phone, ttCustomerUpd.Country,ttCustomerUpd.EmailAddress,ttCustomerUpd.PostalCode,ttCustomerUpd.City,ttCustomerUpd.Address  in frame Dialog-Frame
    do: 
        ghField = self.
        ghCountryField = ttCustomerUpd.Country:handle.
        run FormatField(ghField, ghCountryField).
        if return-value = "Error" then 
        do:
            apply "entry" to self.
            return no-apply.
        end.
    
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.Country
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.Country Dialog-Frame
ON value-changed OF ttCustomerUpd.Country IN FRAME Dialog-Frame /* Country */
do:
        if  isUSA = true then
        do with frame {&frame-name}:
            enable ttCustomerUpd.State.
      
        end.   
        else 
        do with frame {&frame-name}: 
            disable ttCustomerUpd.State.
        end.
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
  IF AVAILABLE ttCustomerUpd THEN 
    DISPLAY ttCustomerUpd.Contact ttCustomerUpd.Name ttCustomerUpd.Phone 
          ttCustomerUpd.Country ttCustomerUpd.EmailAddress 
          ttCustomerUpd.PostalCode ttCustomerUpd.SalesRep ttCustomerUpd.City 
          ttCustomerUpd.Discount ttCustomerUpd.State ttCustomerUpd.CreditLimit 
          ttCustomerUpd.Address ttCustomerUpd.Address2 ttCustomerUpd.Comments 
          ttCustomerUpd.CustNum 
      WITH FRAME Dialog-Frame.
  ENABLE ttCustomerUpd.Contact Btn_Save ttCustomerUpd.Name ttCustomerUpd.Phone 
         Btn_Cancel ttCustomerUpd.Country ttCustomerUpd.EmailAddress 
         ttCustomerUpd.PostalCode ttCustomerUpd.SalesRep ttCustomerUpd.City 
         ttCustomerUpd.Discount ttCustomerUpd.CreditLimit ttCustomerUpd.Address 
         ttCustomerUpd.Address2 ttCustomerUpd.Comments 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FormatField Dialog-Frame 
PROCEDURE FormatField :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define input parameter hField as handle no-undo.
    define input parameter hCountry as handle no-undo.
    define variable validFieldList as character no-undo.
    define variable errorMessage   as character no-undo.

    validFieldList = replace(validNames, " ", ""). 

    case true:
        when not valid-handle(hField)                   then 
            errorMessage = "no valid".
        when lookup (hfield:name, validFieldList ) = 0  then 
            errorMessage =  "Not Valid field, shame on you!".
        when hfield:screen-value =  ""  then 
            errorMessage =  substitute("&1 is a Required Field, please write a &1 to continue.", hField:name).
                                                                                
        when hField:name = "Phone"                      then 
            do:
                define variable rawPhone    as character no-undo.
                define variable phoneNumber as integer   no-undo.
                rawPhone = hField:screen-value.
                rawPhone = replace(rawPhone, "-", "").
                phoneNumber = integer(rawPhone) no-error.
                if error-status:error then errorMessage = substitute("The field &1 only takes digists.", hfield:name).
                else hField:screen-value = rawPhone.
                                                                        
            end. 
        when hField:name = "Country"                then 
            do:
                define variable rawCountry as character no-undo.
                rawCountry = hField:screen-value.
                if rawCountry = "USA" then 
                do:
                    isUsa = true.
                    apply "value-changed" to ttCustomerUpd.Country in frame Dialog-Frame.
                end. 
                else 
                do:
                    isUsa = false.
                    apply "value-changed" to ttCustomerUpd.Country in frame Dialog-Frame.
                end.
                hField:screen-value = FormattedCountry(input rawCountry). 
            end.  
        when hField:name = "EmailAddress"                  then 
            do: 
                                                               
                define variable rawEmail   as character no-undo.
                define variable labelEmail as character no-undo.
                define variable emailError as character no-undo.
                                                        
                labelEmail = hField:name.
                rawEmail= hField:screen-value.
                hField:screen-value = ValidateEmail(rawEmail,labelEmail).
            end.
        when hField:name =  "PostalCode" and hCountry:screen-value = "Netherlands" then 
            do:
                define variable rawPostalCode  as character no-undo.
                define variable formatedPostal as character no-undo.
                rawPostalCode = hField:screen-value.
                formatedPostal = ValidatePostalCode(rawPostalCode).
                if formatedPostal <> "UNKNOWN" then
                    hField:screen-value = formatedPostal.
                else errorMessage = substitute("&1 is not a valid Dutch postal code.", rawPostalCode).
                                                                
            end.   
        when hfield:screen-value <> "" and (hField:name <> "Phone" and hField:name <> "PostalCode"  and hField:name <> "EmailAddress") then 
            do:
                define variable rawRequiredInput      as character no-undo.
                define variable FormatedRequiredInput as character no-undo.
                define variable labelField            as character no-undo.
                labelField = hField:name.
                rawRequiredInput = hField:screen-value.
                FormatedRequiredInput = ValidateTextFields(rawRequiredInput,labelField, hCountry ).
                case true:
                    when FormatedRequiredInput = "INVALID NAME" then 
                        errorMessage = "Wrong name format, you should provide name and last name".
                    when FormatedRequiredInput = "INVALID CITY" then 
                        errorMessage = "The City should exist!".
                    when FormatedRequiredInput = "INVALID ADDRESS" then 
                        errorMessage = "Not a valid Address".   
                end case. 
             
                if errorMessage > "" then 
                do: 
                    message errorMessage
                        view-as alert-box.
                    return "error".
                end.
                else  hfield:screen-value =  FormatedRequiredInput.
            end.                                                                                                                                                            
    end case. 
                                                            
    if errorMessage > "" then 
    do: 
        message errorMessage
            view-as alert-box.
        return "error".
    end.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObjects Dialog-Frame 
PROCEDURE InitializeObjects :
/*------------------------------------------------------------------------------
                 Purpose:
                 Notes:
                ------------------------------------------------------------------------------*/
    run GetStates in ghDataUtil(output table ttState).
    do with frame {&frame-name}:
        ttCustomerUpd.State:delimiter=";".
        for each ttState:
            ttCustomerUpd.State:add-last (ttState.StateName, ttState.State).
        end.    
    end.
    apply "value-changed" to ttCustomerUpd.Country in frame {&frame-name}.
    empty temp-table ttCustomerUpd no-error. 
    if pcMode = "Mod":U then
    do:
        run GetCustRecord in ghDataUtil(output TABLE ttCustomerUpd,
            input prowRowid).
        if return-value = "" then
        do:                              
            find first ttCustomerUpd.
        end.
    end.   
    else
        if pcMode = "New" then
        do: 
            create ttCustomerUpd.
            enable ttCustomerUpd.CustNum with frame {&FRAME-NAME}.
        end. 
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReopenQuery Dialog-Frame 
PROCEDURE ReopenQuery :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    {&OPEN-QUERY-Dialog-Frame}

end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CapitalizeFirstLetter Dialog-Frame 
FUNCTION CapitalizeFirstLetter returns character
    ( input pString as character, input hCountry as handle ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable wIndex  as integer   no-undo.
    define variable wResult as character no-undo.
    define variable wWord   as character no-undo.
    define variable isDutch as logical   no-undo initial false.

    /* Check if the country is Netherlands */
    isDutch = (hCountry:screen-value = "Netherlands").

    /* Split the string by spaces */
    do wIndex = 1 to num-entries(pString, ' '):

        /* Get each word, trim any extra spaces and capitalize the first letter */
        wWord = trim(entry(wIndex, pString, ' ')).

        /* If country is Netherlands, check for exceptions */
        if isDutch then 
        do:
            if lookup(wWord, "van,de,der,ter", ",") > 0 then
                wWord = LOWER(wWord).
            else
                wWord = caps(substring(wWord,1,1)) + LOWER(substring(wWord,2)).
        end.
        else
            wWord = caps(substring(wWord,1,1)) + LOWER(substring(wWord,2)).

        /* Build the result string */
        wResult = wResult + wWord + if wIndex < NUM-ENTRIES(pString, ' ') then ' ' else ''.

    end.

    return wResult.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FormatPhone Dialog-Frame 
FUNCTION FormatPhone returns character
    ( inputPhone as character ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable result as character no-undo.
    define variable i      as integer   no-undo.
    do i = 1 to length(inputPhone) by 4:
        result = result + SUBSTRING( inputPhone, i, 4) + "-".                                           
    end.
    /* Remove the trailing hyphen */
    result = substring(result, 1, length(result) - 1).

    return result.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FormattedCountry Dialog-Frame 
FUNCTION FormattedCountry returns character
    (rawCountry as character ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable result as character no-undo.
    if substring(rawCountry, 1, 2) = "nl" then
        result = "Netherlands" + SUBSTRING(rawCountry, 3).
    else if substring(rawCountry, 1, 3) = "n l" then
            result = "Netherlands" + SUBSTRING(rawCountry, 4).
        else
            result = rawCountry.
    return result.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateEmail Dialog-Frame 
FUNCTION ValidateEmail returns character
    ( input email as character, input labelEmail as character):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable result         as character no-undo.
    define variable formattedEmail as character no-undo.
    define variable errorMessage   as character no-undo.
    formattedEmail = LOWER(replace(email, " ", "")).
    if index(formattedEmail, "@") = 0 or INDEX(formattedEmail, ".") <= INDEX(formattedEmail, "@") then
    do:
        errorMessage =  substitute("&1 Shoud be valid email.", labelEmail).

        message errorMessage
            view-as alert-box.        
    end.
    else 
    do:
        result = formattedEmail.
    end.
    return result.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidatePostalCode Dialog-Frame 
FUNCTION ValidatePostalCode returns character
    ( input rawPostal as character ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable cleanedPostal as character no-undo.
    define variable digits        as integer   no-undo.
    define variable letters       as character no-undo.
    define variable AscNumber     as integer   no-undo.
    define variable counter       as integer   no-undo.
    cleanedPostal = UPPER(replace(rawPostal, " ", "")).
    cleanedPostal = replace(cleanedPostal, "-", "").

    if length(cleanedPostal) = 6 then 
    do:
        digits = integer(substring(cleanedPostal, 1,4)) no-error.
        if error-status:error then return "UNKNOWN".
        letters = substring(cleanedPostal, 5,2).
         
        do counter = 1 to 2:
            AscNumber = asc(substring(letters, counter,1)).
            if  AscNumber < asc("A") or AscNumber > asc("Z") then 
                return "UNKNOWN".
        end.       
         
         
        return cleanedPostal.
    end. 
        
    else 
        return "UNKNOWN".

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateTextFields Dialog-Frame 
FUNCTION ValidateTextFields returns character
    ( input rawInput as character, input fieldLabel as character, input hCountry as handle ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable formatedInput as character no-undo.
    case true:
        when fieldLabel = "Name" then 
            do:
                define variable trimmedInput as character no-undo.
                define variable formatInput  as character no-undo.
                trimmedInput = trim(rawInput).
                formatInput = CapitalizeFirstLetter(trimmedInput, hCountry).
                if length(formatInput) <= 6 or INDEX(formatInput, " ") = 0 then  return "INVALID NAME".
                else   formatedInput = formatInput.           
            end.   
        when fieldLabel = "City" then 
            do:
                define variable CapitalizeCity as character no-undo.
                CapitalizeCity = CapitalizeFirstLetter(rawInput, hCountry).
                if length(CapitalizeCity) < 3 then return "INVALID CITY".
                else formatedInput = CapitalizeCity.
            end.  
        when fieldLabel = "Address" then 
            do:
                if length(rawInput) <= 9 or index(rawInput, " ") = 0 then return "INVALID ADDRESS".
                else formatedInput = CapitalizeFirstLetter(rawInput, hCountry).
            end.                                                         
    end case.    

    return formatedInput.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

