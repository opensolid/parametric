module OpenSolid.Parametric.Implementation exposing (..)

import OpenSolid.Arc2d as Arc2d
import OpenSolid.Arc3d as Arc3d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.CubicSpline3d as CubicSpline3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d
import OpenSolid.SketchPlane3d as SketchPlane3d
import OpenSolid.Vector3d as Vector3d


type Curve2d
    = LineSegment2dCurve LineSegment2d
    | Arc2dCurve Arc2d
    | QuadraticSpline2dCurve QuadraticSpline2d
    | CubicSpline2dCurve CubicSpline2d
    | ProjectedCurve2d Curve3d SketchPlane3d


type Curve3d
    = LineSegment3dCurve LineSegment3d
    | Arc3dCurve Arc3d
    | QuadraticSpline3dCurve QuadraticSpline3d
    | CubicSpline3dCurve CubicSpline3d
    | PlacedCurve3d Curve2d SketchPlane3d
    | ProjectedCurve3d Curve3d Plane3d


type Surface3d
    = ExtrusionSurface Curve3d Vector3d
    | RevolutionSurface Curve3d Frame3d Float
    | ParallelogramSurface Point3d Vector3d Vector3d


curve2dPointOn : Curve2d -> Float -> Point2d
curve2dPointOn curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2d.interpolate lineSegment2d

        Arc2dCurve arc2d ->
            Arc2d.point arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.point quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.point cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            curve3dPointOn curve3d >> Point3d.projectInto sketchPlane


curve2dPlaceIn : Frame2d -> Curve2d -> Curve2d
curve2dPlaceIn frame curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve (LineSegment2d.placeIn frame lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve (Arc2d.placeIn frame arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.placeIn frame quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve (CubicSpline2d.placeIn frame cubicSpline2d)

        ProjectedCurve2d curve3d sketchPlane ->
            ProjectedCurve2d curve3d
                (Frame2d.xy
                    |> Frame2d.relativeTo frame
                    |> Frame2d.placeOnto sketchPlane
                )


curve2dRelativeTo : Frame2d -> Curve2d -> Curve2d
curve2dRelativeTo frame curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve (LineSegment2d.relativeTo frame lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve (Arc2d.relativeTo frame arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.relativeTo frame quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve (CubicSpline2d.relativeTo frame cubicSpline2d)

        ProjectedCurve2d curve3d sketchPlane ->
            ProjectedCurve2d curve3d (Frame2d.placeOnto sketchPlane frame)


curve2dPlaceOnto : SketchPlane3d -> Curve2d -> Curve3d
curve2dPlaceOnto sketchPlane curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment3dCurve
                (LineSegment2d.placeOnto sketchPlane lineSegment2d)

        Arc2dCurve arc2d ->
            Arc3dCurve
                (Arc2d.placeOnto sketchPlane arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline3dCurve
                (QuadraticSpline2d.placeOnto sketchPlane quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline3dCurve
                (CubicSpline2d.placeOnto sketchPlane cubicSpline2d)

        ProjectedCurve2d curve3d projectionSketchPlane ->
            if sketchPlane == projectionSketchPlane then
                curve3d
            else
                PlacedCurve3d curve2d sketchPlane


curve3dPointOn : Curve3d -> Float -> Point3d
curve3dPointOn curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3d.interpolate lineSegment3d

        Arc3dCurve arc3d ->
            Arc3d.point arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.point quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.point cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            curve2dPointOn curve2d >> Point2d.placeOnto sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            curve3dPointOn unprojectedCurve3d >> Point3d.projectOnto plane


curve3dProjectOnto : Plane3d -> Curve3d -> Curve3d
curve3dProjectOnto plane curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3dCurve
                (LineSegment3d.projectOnto plane lineSegment3d)

        Arc3dCurve _ ->
            ProjectedCurve3d curve3d plane

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.projectOnto plane quadraticSpline3d)

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3dCurve
                (CubicSpline3d.projectOnto plane cubicSpline3d)

        PlacedCurve3d _ _ ->
            ProjectedCurve3d curve3d plane

        ProjectedCurve3d unprojectedCurve3d originalProjectionPlane ->
            if plane == originalProjectionPlane then
                curve3d
            else
                ProjectedCurve3d curve3d plane


curve3dPlaceIn : Frame3d -> Curve3d -> Curve3d
curve3dPlaceIn frame curve =
    case curve of
        LineSegment3dCurve lineSegment ->
            LineSegment3dCurve (LineSegment3d.placeIn frame lineSegment)

        Arc3dCurve arc ->
            Arc3dCurve (Arc3d.placeIn frame arc)

        QuadraticSpline3dCurve quadraticSpline ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.placeIn frame quadraticSpline)

        CubicSpline3dCurve cubicSpline ->
            CubicSpline3dCurve
                (CubicSpline3d.placeIn frame cubicSpline)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d curve2d (SketchPlane3d.placeIn frame sketchPlane)

        ProjectedCurve3d unprojectedCurve projectionPlane ->
            ProjectedCurve3d
                (curve3dPlaceIn frame unprojectedCurve)
                (Plane3d.placeIn frame projectionPlane)


curve3dRelativeTo : Frame3d -> Curve3d -> Curve3d
curve3dRelativeTo frame curve =
    case curve of
        LineSegment3dCurve lineSegment ->
            LineSegment3dCurve (LineSegment3d.relativeTo frame lineSegment)

        Arc3dCurve arc ->
            Arc3dCurve (Arc3d.relativeTo frame arc)

        QuadraticSpline3dCurve quadraticSpline ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.relativeTo frame quadraticSpline)

        CubicSpline3dCurve cubicSpline ->
            CubicSpline3dCurve
                (CubicSpline3d.relativeTo frame cubicSpline)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d curve2d (SketchPlane3d.relativeTo frame sketchPlane)

        ProjectedCurve3d unprojectedCurve projectionPlane ->
            ProjectedCurve3d
                (curve3dRelativeTo frame unprojectedCurve)
                (Plane3d.relativeTo frame projectionPlane)


curve3dExtrudeBy : Vector3d -> Curve3d -> Surface3d
curve3dExtrudeBy vector curve =
    case curve of
        LineSegment3dCurve lineSegment3d ->
            let
                ( p0, p1 ) =
                    LineSegment3d.endpoints lineSegment3d
            in
            ParallelogramSurface p0 (Point3d.vectorFrom p0 p1) vector

        _ ->
            ExtrusionSurface curve vector


curve3dRevolveAround : Axis3d -> Float -> Curve3d -> Surface3d
curve3dRevolveAround axis angle curve =
    let
        zDirection =
            Axis3d.direction axis

        ( xDirection, yDirection ) =
            Direction3d.perpendicularBasis zDirection

        frame =
            Frame3d
                { originPoint = Axis3d.originPoint axis
                , xDirection = xDirection
                , yDirection = yDirection
                , zDirection = zDirection
                }
    in
    RevolutionSurface (curve3dRelativeTo frame curve) frame angle


surface3dPointOn : Surface3d -> Point2d -> Point3d
surface3dPointOn surface =
    case surface of
        ParallelogramSurface point uVector vVector ->
            let
                ( x0, y0, z0 ) =
                    Point3d.coordinates point

                ( xu, yu, zu ) =
                    Vector3d.components uVector

                ( xv, yv, zv ) =
                    Vector3d.components vVector
            in
            \(Point2d ( u, v )) ->
                Point3d
                    ( x0 + u * xu + v * xv
                    , y0 + u * yu + v * yv
                    , z0 + u * zu + v * zv
                    )

        ExtrusionSurface curve vector ->
            let
                pointOnCurve =
                    curve3dPointOn curve
            in
            \(Point2d ( u, v )) ->
                pointOnCurve u
                    |> Point3d.translateBy (Vector3d.scaleBy v vector)

        RevolutionSurface localCurve frame angle ->
            let
                pointOnCurve =
                    curve3dPointOn localCurve
            in
            \(Point2d ( u, v )) ->
                let
                    ( x0, y0, z ) =
                        Point3d.coordinates (pointOnCurve u)

                    theta =
                        v * angle

                    cosTheta =
                        cos theta

                    sinTheta =
                        sin theta

                    x =
                        x0 * cosTheta - y0 * sinTheta

                    y =
                        y0 * cosTheta + x0 * sinTheta
                in
                Point3d.in_ frame ( x, y, z )
