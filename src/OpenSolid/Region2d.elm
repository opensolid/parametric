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
        , toMesh
        , translateBy
        )

import OpenSolid.BoundaryType as BoundaryType
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.Mesh exposing (Mesh)
import OpenSolid.Parametric.Implementation as Implementation
import OpenSolid.Parametric.Types exposing (..)
import OpenSolid.Rectangle2d as Rectangle2d


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
