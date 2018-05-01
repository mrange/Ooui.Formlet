open System
open Ooui

open Ooui.Formlets
  module Test =
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
          return entity
        }

      View.attachTo (entity |> Surround.withElement Form "")  node
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
