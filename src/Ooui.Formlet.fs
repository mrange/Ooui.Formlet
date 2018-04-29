namespace Ooui

module Formlets =

  type FormletContext = FC of unit

  type [<Struct>] FormletChangeNotification = FCN of (unit -> unit)

  type FormletElement = Ooui.Node

  type [<RequireQualifiedAccess>] FormletTree = 
    | Empty
    | Element of FormletElement
    | Adorner of FormletElement*FormletTree
    | Fork    of FormletTree*FormletTree

  type [<Struct>] FormletFailureContext = FFC of (string list)

  type [<RequireQualifiedAccess>] FormletFailureTree = 
    | Empty
    | Failure of FormletFailureContext*string
    | Fork    of FormletFailureTree*FormletFailureTree

    static member Join l r : FormletFailureTree =
      match l, r with 
      | Empty , Empty -> Empty
      | l     , Empty -> r
      | _     , _     -> Fork (l, r)

  type [<Struct>] FormletResult<'T> = FR of 'T*FormletFailureTree*FormletTree

  type [<Struct>] Formlet<'T> = 
    | FL of (FormletContext -> FormletChangeNotification -> FormletFailureContext -> FormletTree -> FormletResult<'T>)

  module Details =
    let inline fadapt (FL t) = OptimizedClosures.FSharpFunc<_, _, _, _, _>.Adapt t

    let inline finvoke (t : OptimizedClosures.FSharpFunc<_, _, _, _, _>) fc fcn ffc ft = t.Invoke (fc, fcn, ffc, ft)

    let rec findElement (ft : FormletTree) =
      match ft with
      | FormletTree.Element (:? Element as e)     -> Some e
      | FormletTree.Adorner (:? Element as e, _)  -> Some e
      | FormletTree.Empty                         -> None
      | FormletTree.Element e                     -> None
      | FormletTree.Adorner (_, sft)              -> findElement sft
      | FormletTree.Fork (lft, rft)               ->
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

    // TODO: Optimize

    let apply f t = bind f (fun ff -> bind t (fun tv -> value (ff tv)))

    let map m t = bind t (fun tv -> value tv)

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

  module Adorn =
    let withDiv (t : Formlet<'T>) : Formlet<'T> =
      let tf = fadapt t
      FL <| fun fc fcn ffc ft ->
          
        let e, ft =
          match ft with
          | FormletTree.Adorner ((:? Div as e), sft) ->
            e, sft
          | _ ->
            let e = Div ()
            e, FormletTree.Empty

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft

        FR (tv, tfft, FormletTree.Adorner (e, tft))

  module Enhance =

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
            e.Style.Width <- "100px"
            e, FormletTree.Empty

        let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft
        
        match findElement tft with
        | Some element  -> e.For <- element.Id
        | None          -> ()

        FR (tv, tfft, FormletTree.Fork (FormletTree.Element e, tft))


  module Inputs =
    let text initial : Formlet<string> = 
      FL <| fun fc fcn ffc ft ->
        let e =
          match ft with
          | FormletTree.Element (:? Input as e) when e.Type = InputType.Text ->
            e
          | _ ->
            let e = Input InputType.Text
            let (FCN fcn) = fcn
            e.Change.Add (fun _ -> fcn ())
            e.Value <- initial
            e
        FR (e.Value, FormletFailureTree.Empty, FormletTree.Element e)

    let checkBox initial : Formlet<bool> = 
      FL <| fun fc fcn ffc ft ->
        let e =
          match ft with
          | FormletTree.Element (:? Input as e) when e.Type = InputType.Checkbox ->
            e
          | _ ->
            let e = Input InputType.Checkbox
            let (FCN fcn) = fcn
            e.Change.Add (fun _ -> fcn ())
            e.IsChecked <- initial
            e
        FR (e.IsChecked, FormletFailureTree.Empty, FormletTree.Element e)

  module View =
    let attachTo (t : Formlet<'T>) (form : Form) : unit =
      let rec buildTree (children : ResizeArray<Node>) (ft : FormletTree) =
        match ft with
        | FormletTree.Empty -> 
          ()
        | FormletTree.Element e -> 
          children.Add e
        | FormletTree.Adorner (e, ft) ->
          buildSubTree e ft
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
        buildSubTree form tft
        ft <- tft

      update ()

  module Test =
    let label   lbl t = t |> Enhance.withLabel lbl |> Adorn.withDiv
    let input     lbl = Inputs.text ""        |> label lbl
    let checkBox  lbl = Inputs.checkBox false |> label lbl
    let test (form : Form) =
      let t = 
        formlet {
          let! f = input    "Hello"
          let! s = input    "There"
          let! t = checkBox "Check me!"
          return f, s, t
        }
      View.attachTo t form