module MyApp.Client.Main
open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Json
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

/// La url asigna cada item
type Page =
    | [<EndPoint "/">] Home 
    | [<EndPoint "/contador">] Counter
    | [<EndPoint "/data">] Data
    | [<EndPoint "/prueba">] Prueba 
     /// ---------- C R U D ---------------------///
    | [<EndPoint "/todo">] All
    | [<EndPoint "/activo">] Activo 
    | [<EndPoint "/completar">] Complete
 

/// Model de Elmish
/// declara los tipos de datos con su respectiva variable de toda la pg. web
/// 
type Key = int 
type Model =
    {
        page: Page
        counter: int
        books: Book[]option
        error: string option
        username: string
        password: string
        signedInAs: option<string>
        signInFailed: bool
        prueba: Page
        editing : option<string>
        //-----nuevo
        //secretData: string option
        name: string
        //----- C R U D --------
        Id : Key
        Task : string
        IsCompleted : bool
        Editing : option<string>
        //---- L I S T A --- D O S ------
        EndPoint : Page
        NewTask : string
        Entries : list<Model>
        NextKey : Key 
    }

and Book =
    {
        title: string
        author: string
        [<DateTimeFormat "yyyy-MM-dd">]
        publishDate: DateTime
        isbn: string
    }
// Inicializamos los datos del modelo
let initModel (key: Key) (task: string) =
    {
        page = Home
        counter = 0
        books = None
        error = None
        username = ""
        password = ""   
        signedInAs = None
        signInFailed = false
        prueba = Prueba
        editing = None
        //----nuevo
        //secretData = None
        name = ""
        //--- C R U D --------
        Id = key
        Task = task
        IsCompleted = false
        Editing = None
        //---- s e g u n d a --- p a r t e --------
        EndPoint = All
        NewTask = ""        
        Entries = []
        NextKey = 0
    }

/// Definición del sericio remoto
type BookService =
    {
//---------------------- C A R G A R   D A T A --------------------------------        
        /// Trae la lista de todos los libro de la colección
        getBooks: unit-> Async<Book[]>
        /// Agregra un libro en la coleción
        addBook: Book -> Async<unit>
        /// Elimina un libro de la colección, identificado por su ISBN.
        removeBookByIsbn: string -> Async<unit>

//---------------------- I N I C I A R  S E S I Ó N --------------------------
        /// Iniciar sesión en la aplicación
        signIn : string * string -> Async<option<string>>
        //------ NEW
        //signIn : string -> Async<unit>
        /// Trae el nombre del usuario, (o ninguno si no esdta registrados) 
        getUsername : unit -> Async<string>
        /// Salir de la aplicación
        signOut : unit -> Async<unit>
        //getSecretData: unit -> Async<string>
    }
    interface IRemoteService with
        member this.BasePath = "/books"


/// Mensajes de actualización de la aplicación
type Message =
    | SetPage of Page
    | Increment
    | Decrement
    | SetCounter of int
    | GetBooks
    | GotBooks of Book[]
    // //--Agregar ---
    // | GetaddBook
    // | GotaddBook of Book[]
    // | SetaddBook of string
    //--Iniciar sesión --
    | SetUsername of string
    | SetPassword of string
    | GetSignedInAs
    | RecvSignedInAs of option<string>
    | SendSignIn
    | RecvSignIn of option<string>
    | SendSignOut
    | RecvSignOut
    | Error of exn // exn detecta expresiones de error que no pretendias detectar
    | ClearError
    //---- nuevo
  //  | GetSecretData
    //| GotSecretData of data : string
    | SetName of string
     //------  C R U D ----------
    | Remove 
    | StartEdit
    | Edit of text: string
    | CommitEdit
    | CancelEdit
    | SetCompleted of completed: bool
    //---- S E G U N D A ---- P A R T E -----
    | EditNewTask of text : string
    | AddEntry
    | ClearCompleted
    | SetAllCompleted of completed : bool
    | EntryMessage of key: Key * message: Message
    | SetEndPoint of Page
    
let update remote  message  model =
//-------Lógica de Iniciar Sesión --------------
    let onSignIn = function
        | Some _ -> Cmd.ofMsg GetBooks
        | None -> Cmd.none
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
//--------Lógica del contador -----------
    | Increment ->
        { model with counter = model.counter + 1 }, Cmd.none
    | Decrement ->
        { model with counter = model.counter - 1 }, Cmd.none
    | SetCounter value ->
        { model with counter = value }, Cmd.none
//---DATA----
//--------Metodos para traer,modifica,elimar,y agregar la data--------------
//----Trae la coleccion de libros y detecta errores que no se pretende detectar
    | GetBooks ->
        let cmd = Cmd.ofAsync remote.getBooks () GotBooks Error
        { model with books = None }, cmd
//-----Trae toda la coleccion de libros sin errores -----------        
    | GotBooks books ->
        { model with books = Some books }, Cmd.none
//----LOGIN -----
//---- La variable en la que me va a almacenar el username-------------
    | SetUsername s ->
        { model with username = s }, Cmd.none
//---- La variable en la que me va a almacenar el password-------------
    | SetPassword s ->
        { model with password = s }, Cmd.none
//---- Ver si el usuario ingresado es autorizado o no  y detecta errores -------    
    | GetSignedInAs ->
        model, Cmd.ofAuthorized remote.getUsername () RecvSignedInAs Error
//---- Inicia sesion con el usuario correcto y me trae la coleccion de libros --------------      
    | RecvSignedInAs username ->
        { model with signedInAs = username }, onSignIn username
//---- Verificar usuario y contraseña para poder inciar sesión detectando errores----
    | SendSignIn ->
        model, Cmd.ofAsync remote.signIn (model.username, model.password) RecvSignIn Error
//---- Verificar usuario si son correcto los datos inicia sesión----
    | RecvSignIn username ->
        { model with signedInAs = username; signInFailed = Option.isNone username }, onSignIn username
//---- Enviar cerrar sesión detectando errores----
    | SendSignOut ->
        model, Cmd.ofAsync remote.signOut () (fun () -> RecvSignOut) Error
//---- Cerrar sesión es que este iniciado sesión caso contrario retorna falso   ----
    | RecvSignOut ->
        { model with signedInAs = None; signInFailed = false }, Cmd.none
//---- Excepcion remota no autorizada muestra el error y lo desconecta ----
    | Error RemoteUnauthorizedException ->
        { model with error = Some "You have been logged out."; signedInAs = None }, Cmd.none
//---- detecta errores que no se pretende detectar ---         
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
//---- Limpar error ----
    | ClearError ->
        { model with error = None }, Cmd.none
    // | StartEdit ->
    //     { model with editing = Some model.books }
    // | Edit value ->
    //     { model with editing = Some books.editing |> Option.map (fun _ -> value) }

   //---- nuevo----
    // | GetSecretData ->
    //     model,
    //     Cmd.ofAsync remote.getSecretData () GotSecretData Error
    // | GotSecretData data -> { model with secretData = Some data }, []
    // | Error RemoteUnauthorizedException -> { model with secretData = None }, []
    // | Error exn -> { model with error = Some exn.Message }, []

    //---------- C R U D ----------------
    
    | StartEdit ->
        { model with Editing = Some model.Task }, Cmd.none 
    | Edit value ->
        { model with Editing =  model.Editing  |> Option.map (fun _-> value ) }, Cmd.none
    | CommitEdit ->
        { model with 
                Task = model.Editing |> Option.defaultValue model.Task
                Editing = None }, Cmd.none
    | CancelEdit ->
        { model with Editing = None},Cmd.none
    | SetCompleted value -> 
        { model with IsCompleted= value}, Cmd.none
//------------- S E G U N D A ---- P A R T E ----
    | EditNewTask value ->
        { model with NewTask = value } , Cmd.none     
    // | AddEntry ->
    //     { model with 
    //         NewTask = ""
    //         Entries = model.Entries @ [ New model.NextKey model.NewTask ]
    //         NextKey = model.NextKey + 1 }, Cmd.none
    | ClearCompleted ->
        { model with Entries = List.filter (fun e -> not e.IsCompleted)model.Entries}, Cmd.none
    | SetAllCompleted c ->
        { model with Entries = List.map (fun e -> { e with IsCompleted = c}) model.Entries }, Cmd.none
    // | EntryMessage (key, msg) ->
    //     let updateEntry (e: Model) =
    //         if e.Id = key then msg e else Some e
    //     { model with Entries = List.choose updateEntry model.Entries }, Cmd.none
    | SetEndPoint ep ->
        { model with EndPoint = ep}, Cmd.none 

    
//Conecta el sistema de enrutamiento a la aplicación Elmish.
//---- Infiere el router --- 
let router = Router.infer SetPage (fun model -> model.page)
//---- Llama a la plantilla de la carpeta wwroot -----
type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main.Home().Elt()

let counterPage model dispatch =
    Main.Counter()
        .Decrement(fun _ -> dispatch Decrement)
        .Increment(fun _ -> dispatch Increment)
        .Value(model.counter, fun v -> dispatch (SetCounter v))
        .Elt()

let dataPage model (username: string) dispatch =
    Main.Data()
        .Reload(fun _ -> dispatch GetBooks)
        .Username(username)
        .SignOut(fun _ -> dispatch SendSignOut)
        .Rows(cond model.books <| function
            | None ->
                Main.EmptyData().Elt()
            | Some books ->
                forEach books <| fun book ->
                    tr [] [
                        td [] [text book.title]
                        td [] [text book.author]
                        td [] [text (book.publishDate.ToString("yyyy-MM-dd"))]
                        td [] [text book.isbn]
                    ]) 
        .Elt()
let pruebaPage model dispatch =
    // let countNotCompleted = 
    //     state.Entries 
    //     |> List.filter ( fun e -> not e.IsCompleted)
    //     |> List.length
    Main.Prueba()
    //     .Name(model.name, fun n -> dispatch (SetName n) )
    //     .Id("hello")
    //     //.Who("angie")
    //     .Who(b [] [text "Angie"])
    //     //---Mayusculas----
    //     .Class("heading")
    //     .Greet(fun _ -> printfn "Hello, world!")
    //     //.Greet(fun e -> printfn "Clicked at (%i, %i)" e.ClientX e.ClientY)
        .Label(text model.Task)
        .CssAttrs(
            attr.classes [
                if model.IsCompleted then yield "completed"
                if model.Editing.IsSome then yield "editing"
                match  page, model.IsCompleted with
                | Page.Completed, false
                | Page.Active, true -> yield "hidden"           
                | _-> ()
            ]
        )
        .EditingTask(
            model.Editing |> Option.defaultValue "",
            fun text -> dispatch (Message.Edit text)
        )
        .EditBlur(fun _-> dispatch Message.CommitEdit)
        .EditKeyup(fun e -> 
            match e.Key with
            | "Enter" -> dispatch Message.CommitEdit
            | "Escape" -> dispatch Message.CancelEdit
            | _-> ()
        )
        .IsCompleted(
            model.IsCompleted,
            fun x -> dispatch (Message.SetCompleted x)
        )
        .Remove(fun _-> dispatch Message.Remove)
        .StartEdit(fun _-> dispatch Message.StartEdit)
        .HiddenIfNoEntries(if List.isEmpty state.Entries then "hidden" else "")
        .Entries(
            forEach state.Entries <| fun entry -> 
                let entryDispatch msg = dispatch (EntryMessage (entry.Id, msg))
                ecomp<Entry.Component,_,_> (state.EndPoint, entry ) entryDispatch
        )
        .ClearCompleted(fun _-> dispatch Message.ClearCompleted )
        .IsCompleted(
            (countNotCompleted = 0),
            fun c -> dispatch (Message.SetAllCompleted c)
        )
        .Task( 
            state.NewTask,
            fun text -> dispatch (Message.EditNewTask text)
        )
        .Edit( fun e ->
            if e.Key = "Enter" && state.NewTask <> "" then 
                dispatch Message.AddEntry)
        .ItemsLeft(
            match countNotCompleted with
            | 1 -> "1 item left"
            | n -> string n + " items left"
        )
        .CssFilterAll(attr.``class`` (if state.EndPoint = EndPoint.All then "selected" else null))
        .CssFilterActive(attr.``class`` (if state.EndPoint = EndPoint.Active then "selected" else null))
        .CssFilterCompleted(attr.``class`` (if state.EndPoint = EndPoint.Completed then "selected" else null))
        .Elt()

// type Key = int
// let Render (endpoint,entry) dispatch=
//     Main.Todo()
//         .Label(text entry.Task)
//         .CssAttrs(
//             attr.classes[
//                 if entry.IsCompleted then yield "completed"
//                 if entry.Editing.IsSome then yield "editing"
//                 match endpoint, entry.IsCompleted with
//                 | Page.Completed, false
//                 | Page.Active, true -> yield "hidden"
//                 | _-> ()
//                 ]
//         )
//         .EditingTask(
//             entry.Editing |> Option.defaultValue "",
//             fun text -> dispatch (Message.Edit text)
//         )
//         .EditBlur(fun _-> dispatch Message.CommitEdit)
//         .EditKeyup(fun e ->
//             match e.Key with
//             | "Enter" -> dispatch Message.CommitEdit
//             | "Escape" -> dispatch Message.CancelEdit
//             | _ -> ()
//         )
//         .IsCompleted(
//             entry.IsCompleted,
//             fun x -> dispatch (Message.SetCompleted x)
//         )
//         .StartEdit(fun _-> dispatch Message.StartEdit)
//         .Elt()

type Component()=
    inherit ElmishComponent<Model,Message>()
    override this.ShouldRender(oldModel , newModel) = oldModel <> newModel
    override this.View model dispatch = pruebaPage model dispatch
//---- Pagina de inicion de sesión ----

let signInPage model dispatch =
    Main.SignIn()
        .Username(model.username, fun s -> dispatch (SetUsername s))
        .Password(model.password, fun s -> dispatch (SetPassword s))
        .SignIn(fun _ -> dispatch SendSignIn)
        .ErrorNotification(
            cond model.signInFailed <| function
            | false -> empty
            | true ->
                Main.ErrorNotification()
                    .HideClass("is-hidden")
                    .Text("Sign in failed. Use any username and the password \"12345\".")
                    .Elt()
        )
        .Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat [
            menuItem model Home "Home"
            menuItem model Counter "Counter"
            menuItem model Data "Download data"
            menuItem model Prueba "Prueba"
        ])
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
            | Counter -> counterPage model dispatch
            | Data ->
                cond model.signedInAs <| function
                | Some username -> dataPage model username dispatch
                | None -> signInPage model dispatch
            | Prueba -> pruebaPage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let bookService = this.Remote<BookService>()
        let update = update bookService
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetSignedInAs) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReloading
#endif
