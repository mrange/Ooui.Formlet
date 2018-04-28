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

  module Formlet =
    open FSharp.Core.Printf

    module Details =
      let inline fadapt (FL t) = OptimizedClosures.FSharpFunc<_, _, _, _, _>.Adapt t

      let inline finvoke (t : OptimizedClosures.FSharpFunc<_, _, _, _, _>) fc fcn ffc ft = t.Invoke (fc, fcn, ffc, ft)

    open Details

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

        let (FR (uv, ufft, uft)) = finvoke uf fc fcn ffc tft

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


  module View =
    open Formlet.Details
    let update (t : Formlet<'T>) (ft : FormletTree) (div : Div) =
      let rec buildTree (parent : Node) (position : int) (ft: FormletTree) =
        match ft with
        | FormletTree.Empty -> 
        ()
      let fc = FC ()
      let fcn = FCN (fun () -> printfn "Change request")
      let ffc = FFC []
      let tf = fadapt t
      let (FR (tv, tfft, tft)) = finvoke tf fc fcn ffc ft
      buildTree div 0 tft

      struct (tv, tfft, tft)
