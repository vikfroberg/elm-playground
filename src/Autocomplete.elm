module Autocomplete exposing (main)
import Async exposing (Async)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Process
import Queue exposing (Queue)
import Task exposing (Task)


type alias Model =
    { search : String
    , result : Async (List String)
    }


type Msg
    = SearchChanged String
    | SearchResponse Async.Id (List String)
    | DebouncedSearchChanged String


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { search = "", result = Async.empty }
    , Cmd.none
    )



-- Update

-- mapResult stateFn cmdFn ( model, cmd ) =
--     let
--         ( state, msg ) = stateFn model.result
--     in
--     ( { model | result = state }
--     , Cmd.batch [ cmd, newCmd ]
--     )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ = Debug.log "msg" msg
    in
    case msg of
        SearchChanged search ->
            ( model, Cmd.none )
        --     let
        --         ( state, cmd ) =
        --             Async.debounce (DebouncedSearchChanged search) model.result
        --     in
        --     ( { model | search = search, result = state }, cmd )

        DebouncedSearchChanged search ->
            let
                ( state, cmd ) =
                    Async.push
                        (\id -> Task.perform (SearchResponse id) (searchCountry search))
                        model.result
            in
            ( { model | search = search, result = state }
            , cmd
            )


        SearchResponse id items ->
            ( { model | result = Async.set id items model.result }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    let
        result =
            model.result
                |> Async.takeLast
    in
    div []
        [ input
            [ type_ "text"
            , value model.search
            , onInput DebouncedSearchChanged
            ]
            []
        , case result of
            Just items ->
                case items of
                    [] ->
                        div [] [ text "No result" ]

                    xs ->
                        div
                            []
                            (List.map (div [] << List.singleton << text) items)

            Nothing ->
                case model.search of
                    "" ->
                        div [] [ text "Search for a country" ]

                    _ ->
                        div [] [ text "Loading..." ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Helpers


searchCountry : String -> Task Never (List String)
searchCountry search =
    let
        filteredCountries =
            List.filter (String.contains search) countries
    in
    Process.sleep 1000
        |> Task.map (always filteredCountries)


countries =
    [ "Afghanistan"
    , "Albania"
    , "Algeria"
    , "Andorra"
    , "Angola"
    , "Anguilla"
    , "Antigua &amp; Barbuda"
    , "Argentina"
    , "Armenia"
    , "Aruba"
    , "Australia"
    , "Austria"
    , "Azerbaijan"
    , "Bahamas"
    , "Bahrain"
    , "Bangladesh"
    , "Barbados"
    , "Belarus"
    , "Belgium"
    , "Belize"
    , "Benin"
    , "Bermuda"
    , "Bhutan"
    , "Bolivia"
    , "Bosnia &amp; Herzegovina"
    , "Botswana"
    , "Brazil"
    , "British Virgin Islands"
    , "Brunei"
    , "Bulgaria"
    , "Burkina Faso"
    , "Burundi"
    , "Cambodia"
    , "Cameroon"
    , "Canada"
    , "Cape Verde"
    , "Cayman Islands"
    , "Chad"
    , "Chile"
    , "China"
    , "Colombia"
    , "Congo"
    , "Cook Islands"
    , "Costa Rica"
    , "Cote D Ivoire"
    , "Croatia"
    , "Cruise Ship"
    , "Cuba"
    , "Cyprus"
    , "Czech Republic"
    , "Denmark"
    , "Djibouti"
    , "Dominica"
    , "Dominican Republic"
    , "Ecuador"
    , "Egypt"
    , "El Salvador"
    , "Equatorial Guinea"
    , "Estonia"
    , "Ethiopia"
    , "Falkland Islands"
    , "Faroe Islands"
    , "Fiji"
    , "Finland"
    , "France"
    , "French Polynesia"
    , "French West Indies"
    , "Gabon"
    , "Gambia"
    , "Georgia"
    , "Germany"
    , "Ghana"
    , "Gibraltar"
    , "Greece"
    , "Greenland"
    , "Grenada"
    , "Guam"
    , "Guatemala"
    , "Guernsey"
    , "Guinea"
    , "Guinea Bissau"
    , "Guyana"
    , "Haiti"
    , "Honduras"
    , "Hong Kong"
    , "Hungary"
    , "Iceland"
    , "India"
    , "Indonesia"
    , "Iran"
    , "Iraq"
    , "Ireland"
    , "Isle of Man"
    , "Israel"
    , "Italy"
    , "Jamaica"
    , "Japan"
    , "Jersey"
    , "Jordan"
    , "Kazakhstan"
    , "Kenya"
    , "Kuwait"
    , "Kyrgyz Republic"
    , "Laos"
    , "Latvia"
    , "Lebanon"
    , "Lesotho"
    , "Liberia"
    , "Libya"
    , "Liechtenstein"
    , "Lithuania"
    , "Luxembourg"
    , "Macau"
    , "Macedonia"
    , "Madagascar"
    , "Malawi"
    , "Malaysia"
    , "Maldives"
    , "Mali"
    , "Malta"
    , "Mauritania"
    , "Mauritius"
    , "Mexico"
    , "Moldova"
    , "Monaco"
    , "Mongolia"
    , "Montenegro"
    , "Montserrat"
    , "Morocco"
    , "Mozambique"
    , "Namibia"
    , "Nepal"
    , "Netherlands"
    , "Netherlands Antilles"
    , "New Caledonia"
    , "New Zealand"
    , "Nicaragua"
    , "Niger"
    , "Nigeria"
    , "Norway"
    , "Oman"
    , "Pakistan"
    , "Palestine"
    , "Panama"
    , "Papua New Guinea"
    , "Paraguay"
    , "Peru"
    , "Philippines"
    , "Poland"
    , "Portugal"
    , "Puerto Rico"
    , "Qatar"
    , "Reunion"
    , "Romania"
    , "Russia"
    , "Rwanda"
    , "Saint Pierre &amp; Miquelon"
    , "Samoa"
    , "San Marino"
    , "Satellite"
    , "Saudi Arabia"
    , "Senegal"
    , "Serbia"
    , "Seychelles"
    , "Sierra Leone"
    , "Singapore"
    , "Slovakia"
    , "Slovenia"
    , "South Africa"
    , "South Korea"
    , "Spain"
    , "Sri Lanka"
    , "St Kitts &amp; Nevis"
    , "St Lucia"
    , "St Vincent"
    , "St. Lucia"
    , "Sudan"
    , "Suriname"
    , "Swaziland"
    , "Sweden"
    , "Switzerland"
    , "Syria"
    , "Taiwan"
    , "Tajikistan"
    , "Tanzania"
    , "Thailand"
    , "Timor L'Este"
    , "Togo"
    , "Tonga"
    , "Trinidad &amp; Tobago"
    , "Tunisia"
    , "Turkey"
    , "Turkmenistan"
    , "Turks &amp; Caicos"
    , "Uganda"
    , "Ukraine"
    , "United Arab Emirates"
    , "United Kingdom"
    , "United States"
    , "United States Minor Outlying Islands"
    , "Uruguay"
    , "Uzbekistan"
    , "Venezuela"
    , "Vietnam"
    , "Virgin Islands (US)"
    , "Yemen"
    , "Zambia"
    , "Zimbabwe"
    ]
