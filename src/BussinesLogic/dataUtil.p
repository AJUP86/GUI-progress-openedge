
/*------------------------------------------------------------------------
    File        : dataUtil.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Alejandro
    Created     : Tue Sep 12 16:43:54 CEST 2023
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ttCustomer.i}
{ttOrder.i}
{ttState.i}


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
subscribe to "Shutdown":U anywhere.


/* **********************  Internal Procedures  *********************** */

procedure DeleteCustomer:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define input parameter prRowIdent as rowid no-undo.
    
    find Customer where rowid(Customer) = prRowIdent exclusive-lock no-error.
    if available Customer then 
    do:
        delete Customer.
        return.
    end.
    else if locked (Customer) then
            return "The record is locked.  Try later.".
        else
            return "The record has already been deleted!".

end procedure.

procedure GetCustData:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define output parameter TABLE for ttCustomer.

    empty temp-table ttCustomer.
    
    for each Customer no-lock : 
        create ttCustomer.
        buffer-copy Customer to ttCustomer.
        assign 
            ttCustomer.RowIdent = rowid(Customer).
    end.

end procedure.

procedure GetCustRecord:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define output parameter TABLE for ttCustomer.
    define input  parameter prCustomerRow as rowid no-undo.

    empty temp-table ttCustomer no-error.

    if prCustomerRow <> ? then
        find Customer where rowid(Customer) = prCustomerRow no-lock no-error.
    else
        find last Customer no-lock no-error.  
    if available Customer then
    do:
        create ttCustomer.
        buffer-copy Customer to ttCustomer.
        assign 
            ttCustomer.RowIdent = rowid(Customer).
        return.
    end.
    else
        return "Record has been deleted!".

end procedure.

procedure GetOrderData:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define output parameter TABLE for ttOrder.
    define input  parameter piKeyValue  as integer    no-undo.

    empty temp-table ttOrder no-error.

    for each Order where Order.CustNum = piKeyValue no-lock: 
        create ttOrder.
        buffer-copy Order to ttOrder.
        assign 
            ttOrder.rowIdent = rowid(Order).
    end.

end procedure.

procedure GetRepData:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define output parameter repName as character no-undo.
    define input  parameter cSalesRep as character no-undo.

    find Salesrep where Salesrep.SalesRep = cSalesrep.
    repName = Salesrep.RepName.

end procedure.

procedure GetStates:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define output parameter TABLE for ttState.

    empty temp-table ttState.
    
    for each State no-lock : 
        create ttState.
        buffer-copy State to ttState.
        assign 
            ttState.RowIdent = rowid(State).
    end.

end procedure.

procedure SaveCustRecord:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define input-output parameter TABLE for  ttCustomer.
    define input parameter pcMode as character  no-undo.

    find first ttCustomer.
    do transaction:
        if pcMode = "New" then 
            create Customer.
        else
            find Customer where rowid(Customer) = ttCustomer.RowIdent 
                exclusive-lock no-wait no-error.
        /* Do the following for both new and modified records */
        if available Customer then
            buffer-copy ttCustomer except RowIdent CustNum to Customer.
        else
            if locked (Customer) then
                return "Record is locked.  Try later.".
            else 
                return "Record has been deleted!".
    end. /* Transaction */
    find current Customer no-lock.  /* Remove the lock */
    buffer-copy Customer to ttCustomer.
    ttCustomer.rowIdent = rowid(Customer).
    return.

end procedure.

