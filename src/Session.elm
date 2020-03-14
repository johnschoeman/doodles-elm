module Session exposing
    ( Model
    , WithSession
    , initial
    , navKey
    )

import Browser.Navigation as Nav


type alias WithSession a =
    { a | session : Model }


type alias Model =
    { key : Nav.Key
    }


initial : Nav.Key -> Model
initial key =
    { key = key
    }


navKey : Model -> Nav.Key
navKey { key } =
    key
