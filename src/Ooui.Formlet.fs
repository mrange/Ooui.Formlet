namespace Ooui

module Formlets =

  type FormletContext = FC of unit

  type [<Struct>] FormletChangeNotification = FCN of (unit -> unit)

  type FormletElement = Ooui.Node

  type [<RequireQualifiedAccess>] FormletTree = 
    | Empty
    | Element of FormletElement
    | Adorner of FormletElement*FormletElement*FormletTree
    | Fork    of FormletTree*FormletTree

  type [<Struct>] FormletFailureContext = 
    | FFC of (string list)

    with
      member x.Append name : FormletFailureContext =
        let (FFC names) = x
        FFC (name::names)

  type [<RequireQualifiedAccess>] FormletFailureTree = 
    | Empty
    | Failure of FormletFailureContext*string
    | Fork    of FormletFailureTree*FormletFailureTree

    static member Join l r : FormletFailureTree =
      match l, r with 
      | Empty , Empty -> Empty
      | _     , Empty -> l
      | Empty , _     -> r
      | _     , _     -> Fork (l, r)

  type [<Struct>] FormletResult<'T> = FR of 'T*FormletFailureTree*FormletTree

  type [<Struct>] Formlet<'T> = 
    | FL of (FormletContext -> FormletChangeNotification -> FormletFailureContext -> FormletTree -> FormletResult<'T>)

  module Details =
    let inline fadapt (FL t) = OptimizedClosures.FSharpFunc<_, _, _, _, _>.Adapt t

    let inline finvoke (t : OptimizedClosures.FSharpFunc<_, _, _, _, _>) fc fcn ffc ft = t.Invoke (fc, fcn, ffc, ft)

    let rec findElement (ft : FormletTree) =
      match ft with
      | FormletTree.Element (:? Element as e)       -> Some e
      | FormletTree.Adorner (:? Element as e, _, _) -> Some e
      | FormletTree.Empty                           -> None
      | FormletTree.Element e                       -> None
      | FormletTree.Adorner (_, _, sft)             -> findElement sft
      | FormletTree.Fork (lft, rft)                 ->
        match findElement lft with
        | None  -> findElement rft
        | r     -> r

  open Details

  module Formlet =
    open FSharp.Core.Printf

    let value (v : 'T) : Formlet<'T> =
      FL <| fun fc fcn ffc ft ->
        FR (v, FormletFailureTree.Empty, FormletTree.Empty)

    let failWith (fv : 'T) msg : Formlet<'T>  =
      FL <| fun fc fcn ffc ft ->
        FR (fv, FormletFailureTree.Failure (ffc, msg), FormletTree.Empty)

    let failWithf fv fmt = kprintf (failWith fv) fmt

    let bind (t : Formlet<'T>) (uf : 'T -> Formlet<'U>) : Formlet<'U> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let tft, uft =
          match ft with
          | FormletTree.Fork (tft, uft) -> tft              , uft
          | _                           -> FormletTree.Empty, FormletTree.Empty

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc tft

        let u  = uf tv
        let uf = fadapt u

        let (FR (uv, ufft, uft)) = finvoke uf fc fcn ffc uft

        FR (uv, FormletFailureTree.Join tfft ufft, FormletTree.Fork (tft, uft))

    let apply (f: Formlet<'T -> 'U>) (t : Formlet<'T>) : Formlet<'U> = 
      let ff = fadapt f
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let fft, tft =
          match ft with
          | FormletTree.Fork (fft, tft) -> fft              , tft
          | _                           -> FormletTree.Empty, FormletTree.Empty

        let (FR (ff, ffft, fft)) = finvoke ff fc fcn ffc fft
        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc tft

        FR (ff tv, FormletFailureTree.Join ffft tfft, FormletTree.Fork (fft, tft))

    let map m (t : Formlet<'T>) : Formlet<'U> = 
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft

        FR (m tv, tfft, tft)

    let validate (validator : 'T -> string option) (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let tfr = finvoke tf fc fcn ffc ft
        let (FR (tv, tfft, tft)) = tfr

        match validator tv with
        | Some failure  -> 
          FR (tv, FormletFailureTree.Join tfft (FormletFailureTree.Failure (ffc, failure)), tft)
        | None          -> 
          tfr

    let validateNonEmpty (t : Formlet<string>) : Formlet<string> =
      let validator (v : string) =
        if v.Length > 0 then
          None
        else
          Some "Input must not be empty"
      validate validator t

    type Builder () =
      member inline x.Bind       (t, uf)   = bind t uf
      member inline x.Return     v         = value v
      member inline x.ReturnFrom t         = t         : Formlet<_>
      member inline x.Zero       ()        = value ()

  let formlet = Formlet.Builder ()

  type Formlet<'T> with 
    static member inline (>>=) (t, uf)  = Formlet.bind    t uf
    static member inline (|>>) (t, m)   = Formlet.map     m t
    static member inline (<*>) (f, t)   = Formlet.apply   f t
(*
    static member inline (<|>) (l, r)   = Formlet.orElse  l r
    static member inline (<&>) (l, r)   = Formlet.andAlso l r
*)

  // TODO: Reinitialize controls as needed

  module Surround =

    let withElement (creator : unit -> #Element) ``class`` (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->
          
        let e, ft =
          match ft with
          | FormletTree.Adorner ((:? #Element as e), _, sft) ->
            e, sft
          | _ ->
            let e = creator ()
            e.ClassName <- ``class`` 
            e, FormletTree.Empty

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft

        FR (tv, tfft, FormletTree.Adorner (e, e, tft))

  module Enhance =
    module Details =
      type GroupBox (group : string) =
        inherit Div ()

        let title   = Div()
        let content = Div()

        member x.Initialize () =
          if x.Children.Count = 0 then
            x.ClassName       <- "panel panel-default"
            title.ClassName   <- "panel-heading"
            content.ClassName <- "panel-body"
            x.AppendChild title                 |> ignore
            x.AppendChild content               |> ignore
            title.AppendChild (TextNode group)  |> ignore

        member x.Content = content
    open Details

    let withClass ``class`` (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->

        let tfr = finvoke tf fc fcn ffc ft          
        let (FR (tv, tfft, tft)) = tfr

        match findElement tft with
        | Some element  -> element.ClassName <- ``class``
        | None          -> ()

        tfr

    let withLabel label (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->
          
        let e, ft =
          match ft with
          | FormletTree.Fork (FormletTree.Element (:? Label as e), sft) ->
            e, sft
          | _ ->
            let e = Label label
            e, FormletTree.Empty

        e.Text <- label

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn (ffc.Append label) ft

        match findElement tft with
        | Some element  -> e.For <- element
        | None          -> ()

        FR (tv, tfft, FormletTree.Fork (FormletTree.Element e, tft))

    let withGroupBox label (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->
          
        let e, ft =
          match ft with
          | FormletTree.Adorner (:? GroupBox as e, _, sft) ->
            e, sft
          | _ ->
            let e = GroupBox label
            e.Initialize ()
            e, FormletTree.Empty

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn (ffc.Append label) ft

        FR (tv, tfft, FormletTree.Adorner (e, e.Content, tft))

  module Inputs =

    type [<Struct>] Input<'T> =
      {
        Type        : InputType
        Initializer : Input -> unit
        ValueGetter : Input -> 'T
      }

    let input (input : Input<'T>) : Formlet<'T> = 
      FL <| fun fc fcn ffc ft ->
        let e =
          match ft with
          | FormletTree.Element (:? Input as e) when e.Type = input.Type ->
            e
          | _ ->
            let e = Input input.Type
            let (FCN fcn) = fcn
            e.Change.Add (fun _ -> fcn ())
            input.Initializer e
            e

        FR (input.ValueGetter e, FormletFailureTree.Empty, FormletTree.Element e)

    let text placeholder initial : Formlet<string> = 
      {
        Type        = InputType.Text
        Initializer = fun input -> 
          input.ClassName   <- "form-control"
          input.Placeholder <- placeholder
          input.Value       <- initial
        ValueGetter = fun input -> input.Value
      } |> input

    let checkBox initial : Formlet<bool> = 
      {
        Type        = InputType.Checkbox
        Initializer = fun input -> input.IsChecked <- initial
        ValueGetter = fun input -> input.IsChecked
      } |> input

  module View =
    let attachTo (t : Formlet<'T>) (node : Node) : unit =
      let rec buildTree (children : ResizeArray<Node>) (ft : FormletTree) =
        match ft with
        | FormletTree.Empty -> 
          ()
        | FormletTree.Element e -> 
          children.Add e
        | FormletTree.Adorner (e, se, ft) ->
          buildSubTree se ft
          children.Add e
        | FormletTree.Fork (lft, rft) -> 
          buildTree children lft
          buildTree children rft
      and buildSubTree (node : Node) (ft : FormletTree) =
        let nestedChildren = ResizeArray node.Children.Count
        buildTree nestedChildren ft
        node.ReplaceChildren nestedChildren 
      let mutable ft = FormletTree.Empty
      let fc = FC ()
      let ffc = FFC []
      let tf = fadapt t
      let rec fcn = FCN (fun () -> 
          printfn "Change request"
          update ()
        )
      and update () =
        printfn "FormletTree(Before): %A" ft
        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft
        printfn "Formlet Tree(After): %A" tft
        printfn "Value: %A" tv
        printfn "Failure Tree: %A" tfft
        buildSubTree node tft
        ft <- tft
      update ()

  module Test =
    type Customer =
      {
        FirstName     : string
        LastName      : string
        CompanyOwner  : bool
        CompanyNo     : string
      }
      static member New fn ln co cno : Customer = 
        {
          FirstName     = fn
          LastName      = ln
          CompanyOwner  = co
          CompanyNo     = cno
        }
    let label   lbl t = t |> Enhance.withLabel lbl |> Surround.withElement Div "form-group"
    let input     lbl = Inputs.text lbl ""    |> Formlet.validateNonEmpty |> label lbl
    let checkBox  lbl = Inputs.checkBox false |> label lbl
    let test (node : Node) =
      let t =
        Formlet.value Customer.New
        <*> input     "First name"
        <*> input     "Last name"
        <*> checkBox  "Is company owner"
        <*> input     "Company No"
        |> Enhance.withGroupBox "Customer"
        

      View.attachTo (t |> Surround.withElement Form "")  node