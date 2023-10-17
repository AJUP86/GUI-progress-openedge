&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
define temp-table ttCustomerUpd no-undo like Customer
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
function CapitalizeFirstLetter returns character
    ( input pString as character, input hCountry as handle ) forward.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormatPhone Dialog-Frame 
function FormatPhone returns character
    ( hField as handle ) forward.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FormattedCountry Dialog-Frame 
function FormattedCountry returns character
    (rawCountry as character ) forward.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateEmail Dialog-Frame 
function ValidateEmail returns character
    ( input hEmail as handle) forward.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidatePostalCode Dialog-Frame 
function ValidatePostalCode returns character
    ( input rawPostal as character ) forward.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ValidateTextFields Dialog-Frame 
function ValidateTextFields returns character
    ( input hRawInput as handle, input hCountry as handle ) forward.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
define button Btn_Cancel auto-end-key 
     label "Cancel" 
     size 15 by 1.13
     bgcolor 8 .

define button Btn_Save auto-go 
     label "Save" 
     size 15 by 1.13
     bgcolor 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
define query Dialog-Frame for 
      ttCustomerUpd scrolling.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

define frame Dialog-Frame
     ttCustomerUpd.Contact at row 2.03 col 64 colon-aligned widget-id 12
          view-as fill-in 
          size 22 by 1 no-tab-stop 
     Btn_Save at row 2.03 col 91 no-tab-stop 
     ttCustomerUpd.Name at row 3.31 col 14 colon-aligned widget-id 24
          view-as fill-in 
          size 31.2 by 1
          bgcolor 14 
     ttCustomerUpd.Phone at row 3.31 col 64 colon-aligned widget-id 26
          view-as fill-in 
          size 21.2 by 1
          bgcolor 14 
     Btn_Cancel at row 3.31 col 91
     ttCustomerUpd.Country at row 4.59 col 14 colon-aligned widget-id 14
          view-as fill-in 
          size 21.2 by 1
          bgcolor 14 
     ttCustomerUpd.EmailAddress at row 4.59 col 64 colon-aligned widget-id 22
          view-as fill-in 
          size 24 by 1
          bgcolor 14 
     ttCustomerUpd.PostalCode at row 5.87 col 14 colon-aligned widget-id 28
          view-as fill-in 
          size 14.5 by 1
          bgcolor 14 
     ttCustomerUpd.SalesRep at row 7.15 col 64 colon-aligned widget-id 30
          view-as fill-in 
          size 8.5 by 1
     ttCustomerUpd.City at row 8 col 14 colon-aligned widget-id 8
          view-as fill-in 
          size 26.2 by 1
          bgcolor 14 
     ttCustomerUpd.Discount at row 8.44 col 64 colon-aligned widget-id 20
          view-as fill-in 
          size 6.4 by 1
     ttCustomerUpd.State at row 9.21 col 14 colon-aligned widget-id 34
          view-as combo-box inner-lines 5
          list-item-pairs "None","None",
                     "Web","Web"
          drop-down-list
          size 16 by 1
     ttCustomerUpd.CreditLimit at row 9.72 col 64 colon-aligned widget-id 16
          view-as fill-in 
          size 14.2 by 1
     ttCustomerUpd.Address at row 10.49 col 14 colon-aligned widget-id 2
          view-as fill-in 
          size 36.2 by 1
          bgcolor 14 
     ttCustomerUpd.Address2 at row 11.77 col 14 colon-aligned widget-id 4
          view-as fill-in 
          size 36.2 by 1
     ttCustomerUpd.Comments at row 14.08 col 14 colon-aligned widget-id 10
          view-as fill-in 
          size 81.2 by 1
     ttCustomerUpd.CustNum at row 2.03 col 14 colon-aligned widget-id 18
           view-as text 
          size 7.7 by .64
          fgcolor 9 
     space(83.29) skip(13.68)
    with view-as dialog-box keep-tab-order 
         side-labels no-underline three-d  scrollable 
         title "<insert dialog title>"
         default-button Btn_Save cancel-button Btn_Cancel widget-id 100.


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
assign 
       frame Dialog-Frame:SCROLLABLE       = false
       frame Dialog-Frame:HIDDEN           = true.

/* SETTINGS FOR FILL-IN ttCustomerUpd.CustNum IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
assign 
       ttCustomerUpd.CustNum:READ-ONLY in frame Dialog-Frame        = true.

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
on window-close of frame Dialog-Frame /* <insert dialog title> */
do:
        apply "END-ERROR":U to self.
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Save Dialog-Frame
on choose of Btn_Save in frame Dialog-Frame /* Save */
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
        catch e as Progress.Lang.Error :
              message e:GetMessage(1)
                view-as alert-box. 
              apply "entry" to self.
              return no-apply.
        end catch.
    
    end.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ttCustomerUpd.Country
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttCustomerUpd.Country Dialog-Frame
on value-changed of ttCustomerUpd.Country in frame Dialog-Frame /* Country */
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
    frame  Dialog-frame:title = ttCustomerUpd.Name.
    wait-for go of frame {&FRAME-NAME}.
end.
run disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
procedure disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  hide frame Dialog-Frame.
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
procedure enable_UI :
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
  get first Dialog-Frame.
  if available ttCustomerUpd then 
    display ttCustomerUpd.Contact ttCustomerUpd.Name ttCustomerUpd.Phone 
          ttCustomerUpd.Country ttCustomerUpd.EmailAddress 
          ttCustomerUpd.PostalCode ttCustomerUpd.SalesRep ttCustomerUpd.City 
          ttCustomerUpd.Discount ttCustomerUpd.State ttCustomerUpd.CreditLimit 
          ttCustomerUpd.Address ttCustomerUpd.Address2 ttCustomerUpd.Comments 
          ttCustomerUpd.CustNum 
      with frame Dialog-Frame.
  enable ttCustomerUpd.Contact Btn_Save ttCustomerUpd.Name ttCustomerUpd.Phone 
         Btn_Cancel ttCustomerUpd.Country ttCustomerUpd.EmailAddress 
         ttCustomerUpd.PostalCode ttCustomerUpd.SalesRep ttCustomerUpd.City 
         ttCustomerUpd.Discount ttCustomerUpd.CreditLimit ttCustomerUpd.Address 
         ttCustomerUpd.Address2 ttCustomerUpd.Comments 
      with frame Dialog-Frame.
  view frame Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
end procedure.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FormatField Dialog-Frame 
procedure FormatField :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define input parameter hField as handle no-undo.
    define input parameter hCountry as handle no-undo.
    define variable validFieldList as character no-undo.
    define variable errorMessage   as character no-undo.
    define variable rawInput       as character no-undo.

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
                if FormatPhone(hField) <> "INVALID" then hField:screen-value = FormatPhone(hField).
                else errorMessage = substitute("&1 must contain only digits", hField:name).                                                        
            end. 
        when hField:name = "Country"                then 
            do:
                rawInput = hField:screen-value.
                if rawInput = "USA" then 
                do:
                    isUsa = true.
                    apply "value-changed" to ttCustomerUpd.Country in frame Dialog-Frame.
                end. 
                else 
                do:
                    isUsa = false.
                    apply "value-changed" to ttCustomerUpd.Country in frame Dialog-Frame.
                end.
                hField:screen-value = FormattedCountry( rawInput). 
            end.  
        when hField:name = "EmailAddress"                  then 
                 hField:screen-value = ValidateEmail(hField).
        when hField:name =  "PostalCode" and hCountry:screen-value = "Netherlands" then 
            do:
                rawInput = hField:screen-value.
                hField:screen-value = ValidatePostalCode(rawInput).                                                      
            end.   
        when hfield:screen-value <> "" and (hField:name <> "Phone" and hField:name <> "PostalCode"  and hField:name <> "EmailAddress") then 
            do:
                hfield:screen-value = ValidateTextFields(hField, hCountry ).
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
procedure InitializeObjects :
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
procedure ReopenQuery :
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
function CapitalizeFirstLetter returns character
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
function FormatPhone returns character
    ( hField as handle ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable result as character no-undo.
    define variable phoneNumber as integer no-undo.
   result = hField:screen-value.
   result = replace(result, "-", "").
    phoneNumber = integer(result) no-error.
    if error-status:error then 
        undo, throw new Progress.Lang.AppError(substitute("&1 must contain only digits", hField:name)).
    return result.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FormattedCountry Dialog-Frame 
function FormattedCountry returns character
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
function ValidateEmail returns character
    ( input hEmail as handle):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable result         as character no-undo.
    define variable formattedEmail as character no-undo.
    
    formattedEmail = LOWER(replace(hEmail:screen-value, " ", "")).
    if index(formattedEmail, "@") = 0 or INDEX(formattedEmail, ".") <= INDEX(formattedEmail, "@") then 
        undo, throw new Progress.Lang.AppError(substitute("&1 Shoud be valid email.", hEmail:name)).
    result = formattedEmail.
    return result.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidatePostalCode Dialog-Frame 
function ValidatePostalCode returns character
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
        if error-status:error then 
            undo, throw new Progress.Lang.AppError("INVALID POSTAL CODE! First four characters should be numbers",1).
        letters = substring(cleanedPostal, 5,2).
         
        do counter = 1 to 2:
            AscNumber = asc(substring(letters, counter,1)).
            if  AscNumber < asc("A") or AscNumber > asc("Z") then 
                undo, throw new Progress.Lang.AppError("INVALID POSTAL CODE! Dutch postal codes contains 2 letters at the end",1).
        end.       
         
         
        return cleanedPostal.
    end. 
        
    else 
        undo, throw new Progress.Lang.AppError("INVALID POSTAL CODE! Dutch postal codes contains 6 characters always.",1).

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ValidateTextFields Dialog-Frame 
function ValidateTextFields returns character
    ( input hRawInput as handle, input hCountry as handle ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable formatedInput as character no-undo.
    case true:
        when hRawInput:name = "Name" then 
            do:
                define variable trimmedInput as character no-undo.
                define variable formatInput  as character no-undo.
                trimmedInput = trim(hRawInput:screen-value).
                formatInput = CapitalizeFirstLetter(trimmedInput, hCountry).
                if length(formatInput) <= 6 or INDEX(formatInput, " ") = 0 then  
                undo, throw new Progress.Lang.AppError("Wrong name format, you should provide name and last name",1).
                formatedInput = formatInput.           
            end.   
        when hRawInput:name = "City" then 
            do:
                define variable CapitalizeCity as character no-undo.
                CapitalizeCity = CapitalizeFirstLetter(hRawInput:screen-value, hCountry).
                if length(CapitalizeCity) < 3 then
                    undo, throw new Progress.Lang.AppError("The City should exist!",1).
                formatedInput = CapitalizeCity.
            end.  
        when hRawInput:name = "Address" then 
            do:
                if length(hRawInput:screen-value) <= 9 or index(hRawInput:screen-value, " ") = 0 then
                    undo, throw new Progress.Lang.AppError("Not a valid Address",1).
                formatedInput = CapitalizeFirstLetter(hRawInput:screen-value, hCountry).
            end.                                                         
    end case.    

    return formatedInput.

end function.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

