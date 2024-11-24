module View.Posts exposing (..)

import Html exposing (Html, table, thead, tbody, tr, th, td, text, a, div, select, option, input, label)
import Html.Attributes exposing (class, href, id, type_, checked, selected, for, value)
import Html.Events exposing (onCheck, onInput)
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time exposing (formatTime, durationBetween, formatDuration)



{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}


-- Generates an HTML table displaying a list of posts
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable _ now posts =
    table []
        [ tableHeader
        , tableBody now posts
        ]

tableHeader : Html Msg
tableHeader =
    thead []
        [ tr []
            [ th [] [ text "Score" ]
            , th [] [ text "Title" ]
            , th [] [ text "Type" ]
            , th [] [ text "Posted Date" ]
            , th [] [ text "Link" ]
            ]
        ]

tableBody : Time.Posix -> List Post -> Html Msg
tableBody now posts =
    tbody []
        (List.map (postRow now) posts)


-- Creates a single table row for a post with relevant data
postRow : Time.Posix -> Post -> Html Msg
postRow now post =
    let
        timePassed =
            durationBetween post.time now
                |> Maybe.map formatDuration
                |> Maybe.withDefault ""
    in
    tr []
        [ 
          td [ class "post-score" ] [ text (String.fromInt post.score) ]
        , td [ class "post-title" ] [ text post.title ]
        , td [ class "post-type" ] [ text post.type_ ]
        , td [ class "post-time" ] [ text (formatTime Time.utc post.time ++ " (" ++ timePassed ++ ")") ]
        , td [ class "post-url" ]
            [ case post.url of
                Just url -> a [ href url, class "post-link" ] [ text "Link" ]
                Nothing -> text ""
            ]
        ]

{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}

postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    div []
        [
          div []
            [ label [ for "select-posts-per-page" ] [ text "Posts per page: " ]
            , select
                [ id "select-posts-per-page", onInput (ConfigChanged << ChangePostsToShow << Maybe.withDefault 10 << String.toInt) ]
                (List.map (\n -> option 
                        [ value (String.fromInt n ), selected (config.postsToShow == n) ]
                        [ text (String.fromInt n) ]) [10, 25, 50])
            ]

        , div []
            [ label [ for "select-sort-by" ] [ text "Sort by: " ]
            , select
                [ id "select-sort-by", onInput (ConfigChanged << ChangeSortBy << Maybe.withDefault None << sortFromString) ]
                (List.map(\sortOption -> option
                        [ selected (config.sortBy == sortOption) ]
                        [ text (sortToString sortOption) ])
                    sortOptions
                )
            ]

        , div []
            [ label [ for "checkbox-show-job-posts" ] [ text "Show job posts: " ]
            , input
                [ id "checkbox-show-job-posts"
                , type_ "checkbox"
                , checked config.showJobs
                , onCheck (ConfigChanged << ChangeShowJobs)
                ]
                []
            ]

        , div []
            [ label [ for "checkbox-show-text-only-posts" ] [ text "Show text-only posts: " ]
            , input
                [ id "checkbox-show-text-only-posts"
                , type_ "checkbox"
                , checked config.showTextOnly
                , onCheck (ConfigChanged << ChangeShowTextOnly)
                ]
                []
            ]
        ]

