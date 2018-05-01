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

    let label   lbl t = t |> Enhance.withLabel lbl |> Surround.withElement Div "form-group"
    let input     lbl = Inputs.text lbl ""    |> Formlet.validateNonEmpty |> label lbl
    let checkBox  lbl = Inputs.checkBox false |> label lbl
    let test (node : Node) =
      let address =
        Formlet.value Address.New
        <*> input     "C/O"
        <*> input     "Address"
        <*> input     "Zip"
        <*> input     "City"
        <*> input     "County"
        <*> input     "Country"
        |> Enhance.withGroupBox "Address"

      let customer =
        Formlet.value Customer.New
        <*> input     "First name"
        <*> input     "Last name"
        <*> input     "Social no"
        |>> Customer
        |> Enhance.withGroupBox "Customer"
        
      let company =
        Formlet.value Company.New
        <*> input     "Company name"
        <*> input     "Company no"
        |>> Company
        |> Enhance.withGroupBox "Company"
 
      let entity =
        formlet {
          let! isCompany  = checkBox "Is company?"
          let! entity     =
            if isCompany then
              company
            else
              customer
          let! address    = address
          return entity, address
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
