port module Main exposing (..)

import Html exposing (button, div, input, li, span, text, ul)
import Html.Attributes exposing (class, href, placeholder, rel, value)
import Html.Events exposing (onClick, onInput)
import String.Format exposing (format3)


main : Program (Maybe Model) Model Message
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- { model = model
-- , view = view
-- , update = update
-- }
-- TYPES


type alias Book =
    { title : String
    , author : String
    , genre : String
    }


type alias Model =
    { books : List Book
    , form : Book
    }


type Message
    = Submit
    | Title String
    | Author String
    | Genre String
    | Remove Book



-- MODEL


emptyBook : Book
emptyBook =
    { title = ""
    , author = ""
    , genre = ""
    }


inititalModel : Model
inititalModel =
    { books = []
    , form = emptyBook
    }


init : Maybe Model -> ( Model, Cmd msg )
init model =
    case model of
        Just model ->
            ( model, Cmd.none )

        Nothing ->
            ( inititalModel, Cmd.none )



-- VIEW


css : String -> Html.Html Message
css name =
    Html.node "link"
        [ rel "stylesheet", href name ]
        []


bookDescription : Book -> Html.Html Message
bookDescription { author, title, genre } =
    text
        (format3
            "{1} ({2}) // {3}"
            ( title, author, genre )
        )


bookRemoveButton : Book -> Html.Html Message
bookRemoveButton book =
    span
        [ class "book__remove", onClick (Remove book) ]
        [ text "✕" ]


bookListItem : Book -> Html.Html Message
bookListItem book =
    li
        [ class "book" ]
        [ bookDescription book, bookRemoveButton book ]


bookList : List Book -> Html.Html Message
bookList books =
    ul
        [ class "books" ]
        (List.map bookListItem books)


field : String -> (String -> Message) -> String -> Html.Html Message
field placeholder_ onInput_ value_ =
    input
        [ placeholder placeholder_
        , onInput onInput_
        , value value_
        ]
        []


bookSubmit : Html.Html Message
bookSubmit =
    button [ onClick Submit ] [ text "Add book" ]


view : Model -> Html.Html Message
view { books, form } =
    div
        []
        [ css "00-books.css"
        , bookList books
        , field "title" Title form.title
        , field "author" Author form.author
        , field "genre" Genre form.genre
        , bookSubmit
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Message
subscriptions model =
    Sub.none



-- PORTS


port setStorage : Model -> Cmd msg



-- UPDATE


copyBookFromForm : Model -> Model
copyBookFromForm model =
    { model | books = model.form :: model.books }


clearForm : Model -> Model
clearForm model =
    { model | form = emptyBook }


setForm : Book -> Model -> Model
setForm form model =
    { model | form = form }


asFormIn : Model -> Book -> Model
asFormIn =
    flip setForm


setAuthor : String -> Book -> Book
setAuthor author form =
    { form | author = author }


asAuthorIn : Book -> String -> Book
asAuthorIn =
    flip setAuthor


setGenre : String -> Book -> Book
setGenre genre form =
    { form | genre = genre }


asGenreIn : Book -> String -> Book
asGenreIn =
    flip setGenre


setTitle : String -> Book -> Book
setTitle title form =
    { form | title = title }


asTitleIn : Book -> String -> Book
asTitleIn =
    flip setTitle


removeBook : Book -> Model -> Model
removeBook book model =
    { model | books = List.filter (\b -> b /= book) model.books }


update : Message -> Model -> ( Model, Cmd Message )
update message model =
    case message of
        Submit ->
            let
                newModel =
                    model
                        |> copyBookFromForm
                        |> clearForm
            in
            ( newModel, setStorage newModel )

        Title title ->
            let
                newModel =
                    title
                        |> asTitleIn model.form
                        |> asFormIn model
            in
            ( newModel, setStorage newModel )

        Author author ->
            let
                newModel =
                    author
                        |> asAuthorIn model.form
                        |> asFormIn model
            in
            ( newModel, setStorage newModel )

        Genre genre ->
            let
                newModel =
                    genre
                        |> asGenreIn model.form
                        |> asFormIn model
            in
            ( newModel, setStorage newModel )

        Remove book ->
            let
                newModel =
                    model
                        |> removeBook book
            in
            ( newModel, setStorage newModel )
