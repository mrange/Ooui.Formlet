open System
open Ooui

open Ooui.Formlets
  module Test =
    type Address =
      {
        CarryOver     : string
        Address       : string
        Zip           : string
        City          : string
        County        : string
        Country       : string
      }
      static member New co address zip city county country : Address = 
        {
          CarryOver     = co
          Address       = address
          Zip           = zip
          City          = city
          County        = county
          Country       = country
        }

    type Customer =
      {
        FirstName     : string
        LastName      : string
        SocialNo      : string
      }
      static member New fn ln sno : Customer = 
        {
          FirstName     = fn
          LastName      = ln
          SocialNo      = sno
        }

    type Company =
      {
        CompanyName   : string
        CompanyNo     : string
      }
      static member New cn cno : Company = 
        {
          CompanyName   = cn
          CompanyNo     = cno
        }

    type Entity =
      | Customer  of Customer
      | Company   of Company
    

    type Registration =
      {
        Entity          : Entity
        InvoiceAddress  : Address
        DeliveryAddress : Address option
      }
      static member New e ia da : Registration =
        {
          Entity          = e
          InvoiceAddress  = ia
          DeliveryAddress = da
        }

    let label     w lbl t   = t |> Enhance.withLabel lbl |> Surround.withElement Div (sprintf "form-group col-md-%d" w)
    let input     v w lbl   = Inputs.text lbl ""    |> v |> label w lbl
    let nonEmpty  w lbl     = input Formlet.validateNonEmpty w lbl
    let any       w lbl     = input id w lbl
    let checkBox  lbl       = Inputs.checkBox false |> label 12 lbl
    let group     lbl t     = t |> Formlet.tag lbl |> Enhance.withGroupBox lbl
    let test (node : Node) =
      let address lbl =
        Formlet.value Address.New
        <*> any       12  "C/O"
        <*> nonEmpty  12  "Address"
        <*> nonEmpty  6   "Zip"
        <*> nonEmpty  6   "City"
        <*> any       6   "County"
        <*> any       6   "Country"
        |> group lbl

      let customer =
        Formlet.value Customer.New
        <*> nonEmpty  6   "First name"
        <*> nonEmpty  6   "Last name"
        <*> nonEmpty  12  "Social no"
        |>> Customer
        |> group "Customer"
        
      let company =
        Formlet.value Company.New
        <*> nonEmpty  12  "Company name"
        <*> nonEmpty  12  "Company no"
        |>> Company
        |> group "Company"
 
      let entity =
        formlet {
          let! isCompany  = checkBox "Is company?"

          let! entity     =
            if isCompany then
              company
            else
              customer

          let! invoiceAddress = address "Invoice Address"

          let! useSeparateDeliveryAddress = checkBox "Separate delivery address?"

          let! deliveryAddress = 
            if useSeparateDeliveryAddress then
              address "Delivery Address" |>> Some
            else
              Formlet.value None

          return Registration.New entity invoiceAddress deliveryAddress
        }

      let form = 
        entity 
        |> Enhance.withSubmit
        |> Surround.withElement Form ""

      View.attachTo form node
[<EntryPoint>]
let main argv =
  
  UI.Port <- 8800

  let div = Div ()
  div.Style.Margin <- "10px"

  let fr = Test.test div

  printfn "%A" fr
  
  UI.Publish ("/formlet", div)

  Console.ReadLine () |> ignore

  0
