open System
open Ooui
open System.Text.RegularExpressions

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

    let regexSocialNo       = Regex ("\d{8}-\d{4}", RegexOptions.CultureInvariant ||| RegexOptions.Compiled ||| RegexOptions.Singleline)

    let row       t         = t |> Surround.withElement Div "form-row"
    let label     lbl t     =
      t
      |> Enhance.withLabel lbl
      |> row
    let input     v lbl     = Inputs.text lbl "" |> v |> Enhance.withValidation |> label lbl
    let notEmpty  lbl       = input Validate.notEmpty lbl
    let socialNo  lbl       = input (Validate.notEmpty >> Validate.regex regexSocialNo "Social no should look like this: 19601201-1234") lbl
    let any       lbl       = input id lbl
    let checkBox  lbl       = Inputs.checkBox false |> label lbl
    let group     lbl t     = t |> Formlet.tag lbl |> Enhance.withGroupBox lbl
    let test (node : Node) =
      let address lbl =
        Formlet.value Address.New
        <*> any       "C/O"
        <*> notEmpty  "Address"
        <*> notEmpty  "Zip"
        <*> notEmpty  "City"
        <*> any       "County"
        <*> any       "Country"
        |> group lbl

      let customer =
        Formlet.value Customer.New
        <*> notEmpty  "First name"
        <*> notEmpty  "Last name"
        <*> socialNo  "Social no"
        |>> Customer
        |> group "Customer"

      let company =
        Formlet.value Company.New
        <*> notEmpty  "Company name"
        <*> notEmpty  "Company no"
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

  UI.HeadHtml <- """<link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css" />""";
  UI.Publish ("/formlet", div)

  Console.ReadLine () |> ignore

  0
