module Main exposing (..)

import Html exposing (button, div, input, li, span, text, ul)
import Html.Attributes exposing (class, href, placeholder, rel, value)
import Html.Events exposing (onClick, onInput)
import String.Format exposing (format3)


main : Program Never Model Message
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- TYPES


type alias Form =
    { title : String
    , author : String
    , genre : String
    }


type alias Index =
    Int


type alias Indexed a =
    { a | index : Index }


type alias Book =
    Indexed Form


type alias Model =
    { books : List Book
    , form : Form
    }


type Message
    = Submit
    | Title String
    | Author String
    | Genre String
    | Remove Int



-- MODEL


emptyForm : Form
emptyForm =
    { title = ""
    , author = ""
    , genre = ""
    }


model : Model
model =
    { books = []
    , form = emptyForm
    }



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


bookRemoveButton : Indexed a -> Html.Html Message
bookRemoveButton { index } =
    span
        [ class "book__remove", onClick (Remove index) ]
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



-- UPDATE


bookFromForm : Form -> Index -> Book
bookFromForm { author, genre, title } index =
    { author = author
    , title = title
    , genre = genre
    , index = index
    }


extractBook : Model -> Book
extractBook model =
    bookFromForm model.form (List.length model.books)


copyBookFromForm : Model -> Model
copyBookFromForm model =
    { model | books = extractBook model :: model.books }


clearForm : Model -> Model
clearForm model =
    { model | form = emptyForm }


setForm : Form -> Model -> Model
setForm form model =
    { model | form = form }


asFormIn : Model -> Form -> Model
asFormIn =
    flip setForm


setAuthor : String -> Form -> Form
setAuthor author form =
    { form | author = author }


asAuthorIn : Form -> String -> Form
asAuthorIn =
    flip setAuthor


setGenre : String -> Form -> Form
setGenre genre form =
    { form | genre = genre }


asGenreIn : Form -> String -> Form
asGenreIn =
    flip setGenre


setTitle : String -> Form -> Form
setTitle title form =
    { form | title = title }


asTitleIn : Form -> String -> Form
asTitleIn =
    flip setTitle


removeBook : Int -> Model -> Model
removeBook index model =
    { model | books = List.filter (\x -> x.index /= index) model.books }


update : Message -> Model -> Model
update message model =
    case message of
        Submit ->
            model
                |> copyBookFromForm
                |> clearForm

        Title title ->
            title
                |> asTitleIn model.form
                |> asFormIn model

        Author author ->
            author
                |> asAuthorIn model.form
                |> asFormIn model

        Genre genre ->
            genre
                |> asGenreIn model.form
                |> asFormIn model

        Remove index ->
            model
                |> removeBook index
