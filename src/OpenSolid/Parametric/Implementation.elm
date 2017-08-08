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
import OpenSolid.Vector2d as Vector2d
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
            Arc2d.pointOn arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.pointOn quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.pointOn cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            curve3dPointOn curve3d >> Point3d.projectInto sketchPlane


curve2dEvaluate : Curve2d -> Float -> ( Point2d, Vector2d )
curve2dEvaluate curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            let
                derivative =
                    LineSegment2d.vector lineSegment2d
            in
            \t -> ( LineSegment2d.interpolate lineSegment2d t, derivative )

        Arc2dCurve arc2d ->
            Arc2d.evaluate arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.evaluate quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.evaluate cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            curve3dEvaluate curve3d
                >> (\( unprojectedPoint3d, unprojectedVector3d ) ->
                        ( Point3d.projectInto sketchPlane unprojectedPoint3d
                        , Vector3d.projectInto sketchPlane unprojectedVector3d
                        )
                   )


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


curve2dMaxSecondDerivativeMagnitude : Curve2d -> Float
curve2dMaxSecondDerivativeMagnitude curve2d =
    case curve2d of
        LineSegment2dCurve _ ->
            0

        Arc2dCurve arc2d ->
            let
                sweptAngle =
                    Arc2d.sweptAngle arc2d
            in
            Arc2d.radius arc2d * sweptAngle * sweptAngle

        QuadraticSpline2dCurve quadraticSpline2d ->
            let
                ( p1, p2, p3 ) =
                    QuadraticSpline2d.controlPoints quadraticSpline2d

                v1 =
                    Point2d.vectorFrom p1 p2

                v2 =
                    Point2d.vectorFrom p2 p3
            in
            2 * Vector2d.length (Vector2d.difference v2 v1)

        CubicSpline2dCurve cubicSpline2d ->
            let
                ( p1, p2, p3, p4 ) =
                    CubicSpline2d.controlPoints cubicSpline2d

                u1 =
                    Point2d.vectorFrom p1 p2

                u2 =
                    Point2d.vectorFrom p2 p3

                u3 =
                    Point2d.vectorFrom p3 p4

                v1 =
                    Vector2d.difference u2 u1

                v2 =
                    Vector2d.difference u3 u2
            in
            6 * max (Vector2d.length v1) (Vector2d.length v2)

        ProjectedCurve2d curve3d _ ->
            curve3dMaxSecondDerivativeMagnitude curve3d


curve2dNumSegments : Float -> Curve2d -> Int
curve2dNumSegments tolerance curve2d =
    if tolerance > 0 then
        let
            maxSecondDerivativeMagnitude =
                curve2dMaxSecondDerivativeMagnitude curve2d
        in
        max 1 (ceiling (sqrt (maxSecondDerivativeMagnitude / (8 * tolerance))))
    else
        1


curve2dToPolyline : Float -> Curve2d -> Polyline2d
curve2dToPolyline tolerance curve2d =
    let
        numSegments =
            curve2dNumSegments tolerance curve2d

        parameterValues =
            List.range 0 numSegments
                |> List.map (\index -> toFloat index / toFloat numSegments)

        points =
            parameterValues |> List.map (curve2dPointOn curve2d)
    in
    Polyline2d points


curve3dPointOn : Curve3d -> Float -> Point3d
curve3dPointOn curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3d.interpolate lineSegment3d

        Arc3dCurve arc3d ->
            Arc3d.pointOn arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.pointOn quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.pointOn cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            curve2dPointOn curve2d >> Point2d.placeOnto sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            curve3dPointOn unprojectedCurve3d >> Point3d.projectOnto plane


curve3dEvaluate : Curve3d -> Float -> ( Point3d, Vector3d )
curve3dEvaluate curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            let
                derivative =
                    LineSegment3d.vector lineSegment3d
            in
            \t -> ( LineSegment3d.interpolate lineSegment3d t, derivative )

        Arc3dCurve arc3d ->
            Arc3d.evaluate arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.evaluate quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.evaluate cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            curve2dEvaluate curve2d
                >> (\( point2d, vector2d ) ->
                        ( Point2d.placeOnto sketchPlane point2d
                        , Vector2d.placeOnto sketchPlane vector2d
                        )
                   )

        ProjectedCurve3d unprojectedCurve3d plane ->
            curve3dEvaluate unprojectedCurve3d
                >> (\( unprojectedPoint3d, unprojectedVector3d ) ->
                        ( Point3d.projectOnto plane unprojectedPoint3d
                        , Vector3d.projectOnto plane unprojectedVector3d
                        )
                   )


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


curve3dMaxSecondDerivativeMagnitude : Curve3d -> Float
curve3dMaxSecondDerivativeMagnitude curve3d =
    case curve3d of
        LineSegment3dCurve _ ->
            0

        Arc3dCurve arc3d ->
            let
                sweptAngle =
                    Arc3d.sweptAngle arc3d
            in
            Arc3d.radius arc3d * sweptAngle * sweptAngle

        QuadraticSpline3dCurve quadraticSpline3d ->
            let
                ( p1, p2, p3 ) =
                    QuadraticSpline3d.controlPoints quadraticSpline3d

                v1 =
                    Point3d.vectorFrom p1 p2

                v2 =
                    Point3d.vectorFrom p2 p3
            in
            2 * Vector3d.length (Vector3d.difference v2 v1)

        CubicSpline3dCurve cubicSpline3d ->
            let
                ( p1, p2, p3, p4 ) =
                    CubicSpline3d.controlPoints cubicSpline3d

                u1 =
                    Point3d.vectorFrom p1 p2

                u2 =
                    Point3d.vectorFrom p2 p3

                u3 =
                    Point3d.vectorFrom p3 p4

                v1 =
                    Vector3d.difference u2 u1

                v2 =
                    Vector3d.difference u3 u2
            in
            6 * max (Vector3d.length v1) (Vector3d.length v2)

        ProjectedCurve3d unprojectedCurve3d _ ->
            curve3dMaxSecondDerivativeMagnitude unprojectedCurve3d

        PlacedCurve3d curve2d _ ->
            curve2dMaxSecondDerivativeMagnitude curve2d


curve3dNumSegments : Float -> Curve3d -> Int
curve3dNumSegments tolerance curve3d =
    if tolerance > 0 then
        let
            maxSecondDerivativeMagnitude =
                curve3dMaxSecondDerivativeMagnitude curve3d
        in
        max 1 (ceiling (sqrt (maxSecondDerivativeMagnitude / (8 * tolerance))))
    else
        1


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
