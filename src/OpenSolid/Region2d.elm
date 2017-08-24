module OpenSolid.Region2d
    exposing
        ( boundaries
        , extrusion
        , extrusionWith
        , fromRectangle
        , fromRectangleWith
        , fuse
        , mirrorAcross
        , rectangle
        , rectangleWith
        , revolution
        , revolutionWith
        , rotateAround
        , roundedRectangle
        , toMesh
        , translateBy
        )

import OpenSolid.Axis2d as Axis2d exposing (Axis2d)
import OpenSolid.BoundaryType as BoundaryType
import OpenSolid.Curve2d as Curve2d
import OpenSolid.Frame2d as Frame2d exposing (Frame2d)
import OpenSolid.LineSegment2d as LineSegment2d exposing (LineSegment2d)
import OpenSolid.Mesh exposing (Mesh)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Point2d as Point2d exposing (Point2d)
import OpenSolid.Rectangle2d as Rectangle2d exposing (Rectangle2d)
import OpenSolid.Vector2d as Vector2d exposing (Vector2d)


rectangle : { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Region2d
rectangle extrema =
    fromRectangle (Rectangle2d.with extrema)


rectangleWith : { left : BoundaryType, right : BoundaryType, top : BoundaryType, bottom : BoundaryType } -> { minX : Float, maxX : Float, minY : Float, maxY : Float } -> Region2d
rectangleWith edgeTypes extrema =
    fromRectangleWith edgeTypes (Rectangle2d.with extrema)


fromRectangle : Rectangle2d -> Region2d
fromRectangle =
    fromRectangleWith
        { left = BoundaryType.exterior
        , right = BoundaryType.exterior
        , top = BoundaryType.exterior
        , bottom = BoundaryType.exterior
        }


fromRectangleWith : { left : BoundaryType, right : BoundaryType, top : BoundaryType, bottom : BoundaryType } -> Rectangle2d -> Region2d
fromRectangleWith edgeTypes rectangle =
    Implementation.RectangleRegion rectangle edgeTypes


extrusion : Curve2d -> Vector2d -> Region2d
extrusion =
    extrusionWith
        { start = BoundaryType.exterior
        , end = BoundaryType.exterior
        , left = BoundaryType.exterior
        , right = BoundaryType.exterior
        }


extrusionWith : { start : BoundaryType, end : BoundaryType, left : BoundaryType, right : BoundaryType } -> Curve2d -> Vector2d -> Region2d
extrusionWith =
    Implementation.regionExtrusionWith


revolution : Curve2d -> Point2d -> Float -> Region2d
revolution =
    revolutionWith
        { start = BoundaryType.exterior
        , end = BoundaryType.exterior
        , inside = BoundaryType.exterior
        , outside = BoundaryType.exterior
        }


revolutionWith : { start : BoundaryType, end : BoundaryType, inside : BoundaryType, outside : BoundaryType } -> Curve2d -> Point2d -> Float -> Region2d
revolutionWith =
    Implementation.regionRevolutionWith


boundaries : Region2d -> List Curve2d
boundaries =
    Implementation.regionBoundaries


toMesh : Float -> Region2d -> Mesh Point2d
toMesh =
    Implementation.regionToMesh


translateBy : Vector2d -> Region2d -> Region2d
translateBy =
    Implementation.regionTranslateBy


rotateAround : Point2d -> Float -> Region2d -> Region2d
rotateAround =
    Implementation.regionRotateAround


mirrorAcross : Axis2d -> Region2d -> Region2d
mirrorAcross =
    Implementation.regionMirrorAcross


fuse : List Region2d -> Region2d
fuse =
    Implementation.Fused


roundedRectangle : Rectangle2d -> Float -> Region2d
roundedRectangle rectangle cornerRadius =
    let
        ( width, height ) =
            Rectangle2d.dimensions rectangle

        halfWidth =
            width / 2

        halfHeight =
            height / 2

        frame =
            Rectangle2d.axes rectangle

        xAxis =
            Frame2d.xAxis frame

        yAxis =
            Frame2d.yAxis frame

        cornerCenter =
            Point2d.in_ frame
                ( halfWidth - cornerRadius
                , halfHeight - cornerRadius
                )

        cornerStart =
            Point2d.in_ frame ( halfWidth, halfHeight - cornerRadius )

        topRightCorner =
            revolutionWith
                { start = BoundaryType.interior
                , end = BoundaryType.interior
                , inside = BoundaryType.interior
                , outside = BoundaryType.exterior
                }
                (Curve2d.lineSegment <|
                    LineSegment2d.from cornerCenter cornerStart
                )
                cornerCenter
                (degrees 90)

        rightRectangle =
            fromRectangleWith
                { left = BoundaryType.interior
                , right = BoundaryType.exterior
                , top = BoundaryType.interior
                , bottom = BoundaryType.interior
                }
                (Rectangle2d.in_ frame
                    { minX = halfWidth - cornerRadius
                    , maxX = halfWidth
                    , minY = -halfHeight + cornerRadius
                    , maxY = halfHeight - cornerRadius
                    }
                )

        topRectangle =
            fromRectangleWith
                { left = BoundaryType.interior
                , right = BoundaryType.interior
                , top = BoundaryType.exterior
                , bottom = BoundaryType.interior
                }
                (Rectangle2d.in_ frame
                    { minX = -halfWidth + cornerRadius
                    , maxX = halfWidth - cornerRadius
                    , minY = halfHeight - cornerRadius
                    , maxY = halfHeight
                    }
                )

        leftRectangle =
            rightRectangle |> mirrorAcross yAxis

        bottomRectangle =
            topRectangle |> mirrorAcross xAxis

        topLeftCorner =
            topRightCorner |> mirrorAcross yAxis

        bottomRightCorner =
            topRightCorner |> mirrorAcross xAxis

        bottomLeftCorner =
            topLeftCorner |> mirrorAcross xAxis

        innerRectangle =
            fromRectangleWith
                { left = BoundaryType.interior
                , right = BoundaryType.interior
                , top = BoundaryType.interior
                , bottom = BoundaryType.interior
                }
                (Rectangle2d.in_ frame
                    { minX = -halfWidth + cornerRadius
                    , maxX = halfWidth - cornerRadius
                    , minY = -halfHeight + cornerRadius
                    , maxY = halfHeight - cornerRadius
                    }
                )
    in
    fuse
        [ topLeftCorner
        , topRectangle
        , topRightCorner
        , leftRectangle
        , innerRectangle
        , rightRectangle
        , bottomLeftCorner
        , bottomRectangle
        , bottomRightCorner
        ]
