module OpenSolid.Parametric.Implementation exposing (..)

import OpenSolid.Arc2d as Arc2d
import OpenSolid.Arc3d as Arc3d
import OpenSolid.Axis3d as Axis3d
import OpenSolid.Circle2d as Circle2d
import OpenSolid.CubicSpline2d as CubicSpline2d
import OpenSolid.CubicSpline3d as CubicSpline3d
import OpenSolid.Direction3d as Direction3d
import OpenSolid.Frame2d as Frame2d
import OpenSolid.Frame3d as Frame3d
import OpenSolid.Geometry.Types exposing (..)
import OpenSolid.LineSegment2d as LineSegment2d
import OpenSolid.LineSegment3d as LineSegment3d
import OpenSolid.Mesh as Mesh exposing (Mesh)
import OpenSolid.Plane3d as Plane3d
import OpenSolid.Point2d as Point2d
import OpenSolid.Point3d as Point3d
import OpenSolid.Polyline2d as Polyline2d
import OpenSolid.QuadraticSpline2d as QuadraticSpline2d
import OpenSolid.QuadraticSpline3d as QuadraticSpline3d
import OpenSolid.Rectangle2d as Rectangle2d
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
    | PlanarSurface Region2d SketchPlane3d


type EdgeType
    = Interior
    | Exterior


type Region2d
    = RectangleRegion Rectangle2d { left : EdgeType, right : EdgeType, top : EdgeType, bottom : EdgeType }
    | ExtrusionRegion Curve2d Vector2d { start : EdgeType, end : EdgeType, left : EdgeType, right : EdgeType }
    | RevolutionRegion Curve2d Point2d Float { start : EdgeType, end : EdgeType, inside : EdgeType, outside : EdgeType }
    | FanRegion Point2d Curve2d { start : EdgeType, end : EdgeType, curve : EdgeType }
    | Fused (List Region2d)


curve2dStartPoint : Curve2d -> Point2d
curve2dStartPoint curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2d.startPoint lineSegment2d

        Arc2dCurve arc2d ->
            Arc2d.startPoint arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.startPoint quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.startPoint cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            curve3dStartPoint curve3d |> Point3d.projectInto sketchPlane


curve2dEndPoint : Curve2d -> Point2d
curve2dEndPoint curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2d.endPoint lineSegment2d

        Arc2dCurve arc2d ->
            Arc2d.endPoint arc2d

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2d.endPoint quadraticSpline2d

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2d.endPoint cubicSpline2d

        ProjectedCurve2d curve3d sketchPlane ->
            curve3dEndPoint curve3d |> Point3d.projectInto sketchPlane


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


curve2dReverse : Curve2d -> Curve2d
curve2dReverse curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve (LineSegment2d.reverse lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve (Arc2d.reverse arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve (QuadraticSpline2d.reverse quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve (CubicSpline2d.reverse cubicSpline2d)

        ProjectedCurve2d curve3d sketchPlane ->
            ProjectedCurve2d (curve3dReverse curve3d) sketchPlane


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


curve2dTranslateBy : Vector2d -> Curve2d -> Curve2d
curve2dTranslateBy displacement curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve
                (LineSegment2d.translateBy displacement lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve
                (Arc2d.translateBy displacement arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.translateBy displacement quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve
                (CubicSpline2d.translateBy displacement cubicSpline2d)

        ProjectedCurve2d curve3d projectionSketchPlane ->
            let
                sketchPlaneDisplacement =
                    Vector2d.flip displacement
                        |> Vector2d.placeOnto projectionSketchPlane

                translatedSketchPlane =
                    projectionSketchPlane
                        |> SketchPlane3d.translateBy sketchPlaneDisplacement
            in
            ProjectedCurve2d curve3d translatedSketchPlane


curve2dRotateAround : Point2d -> Float -> Curve2d -> Curve2d
curve2dRotateAround point angle curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve
                (LineSegment2d.rotateAround point angle lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve
                (Arc2d.rotateAround point angle arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.rotateAround point angle quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve
                (CubicSpline2d.rotateAround point angle cubicSpline2d)

        ProjectedCurve2d curve3d projectionSketchPlane ->
            let
                rotationAxis =
                    Axis3d
                        { originPoint =
                            Point2d.placeOnto projectionSketchPlane point
                        , direction =
                            SketchPlane3d.normalDirection projectionSketchPlane
                        }

                rotatedSketchPlane =
                    projectionSketchPlane
                        |> SketchPlane3d.rotateAround rotationAxis -angle
            in
            ProjectedCurve2d curve3d rotatedSketchPlane


curve2dMirrorAcross : Axis2d -> Curve2d -> Curve2d
curve2dMirrorAcross axis curve2d =
    case curve2d of
        LineSegment2dCurve lineSegment2d ->
            LineSegment2dCurve
                (LineSegment2d.mirrorAcross axis lineSegment2d)

        Arc2dCurve arc2d ->
            Arc2dCurve
                (Arc2d.mirrorAcross axis arc2d)

        QuadraticSpline2dCurve quadraticSpline2d ->
            QuadraticSpline2dCurve
                (QuadraticSpline2d.mirrorAcross axis quadraticSpline2d)

        CubicSpline2dCurve cubicSpline2d ->
            CubicSpline2dCurve
                (CubicSpline2d.mirrorAcross axis cubicSpline2d)

        ProjectedCurve2d curve3d projectionSketchPlane ->
            let
                mirroredSketchPlane =
                    Frame2d.xy
                        |> Frame2d.mirrorAcross axis
                        |> Frame2d.placeOnto projectionSketchPlane
            in
            ProjectedCurve2d curve3d mirroredSketchPlane


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
                    Vector2d.from p1 p2

                v2 =
                    Vector2d.from p2 p3
            in
            2 * Vector2d.length (Vector2d.difference v2 v1)

        CubicSpline2dCurve cubicSpline2d ->
            let
                ( p1, p2, p3, p4 ) =
                    CubicSpline2d.controlPoints cubicSpline2d

                u1 =
                    Vector2d.from p1 p2

                u2 =
                    Vector2d.from p2 p3

                u3 =
                    Vector2d.from p3 p4

                v1 =
                    Vector2d.difference u2 u1

                v2 =
                    Vector2d.difference u3 u2
            in
            6 * max (Vector2d.length v1) (Vector2d.length v2)

        ProjectedCurve2d curve3d _ ->
            curve3dMaxSecondDerivativeMagnitude curve3d


curveNumSegments : Float -> Float -> Int
curveNumSegments tolerance maxSecondDerivativeMagnitude =
    if tolerance > 0 then
        max 1 (ceiling (sqrt (maxSecondDerivativeMagnitude / (8 * tolerance))))
    else
        1


curve2dNumSegments : Float -> Curve2d -> Int
curve2dNumSegments tolerance curve2d =
    curveNumSegments tolerance (curve2dMaxSecondDerivativeMagnitude curve2d)


evenlySpacedParameterValues : Int -> List Float
evenlySpacedParameterValues numSegments =
    List.range 0 numSegments
        |> List.map (\index -> toFloat index / toFloat numSegments)


curve2dToPolyline : Float -> Curve2d -> Polyline2d
curve2dToPolyline tolerance curve2d =
    let
        numSegments =
            curve2dNumSegments tolerance curve2d

        parameterValues =
            evenlySpacedParameterValues numSegments

        points =
            parameterValues |> List.map (curve2dPointOn curve2d)
    in
    Polyline2d points


curve2dSamples : Float -> Curve2d -> List ( Point2d, Vector2d )
curve2dSamples tolerance curve2d =
    let
        numSegments =
            curve2dNumSegments tolerance curve2d

        parameterValues =
            evenlySpacedParameterValues numSegments
    in
    parameterValues |> List.map (curve2dEvaluate curve2d)


curve3dStartPoint : Curve3d -> Point3d
curve3dStartPoint curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3d.startPoint lineSegment3d

        Arc3dCurve arc3d ->
            Arc3d.startPoint arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.startPoint quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.startPoint cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            curve2dStartPoint curve2d |> Point2d.placeOnto sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            curve3dStartPoint unprojectedCurve3d |> Point3d.projectOnto plane


curve3dEndPoint : Curve3d -> Point3d
curve3dEndPoint curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3d.endPoint lineSegment3d

        Arc3dCurve arc3d ->
            Arc3d.endPoint arc3d

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3d.endPoint quadraticSpline3d

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3d.endPoint cubicSpline3d

        PlacedCurve3d curve2d sketchPlane ->
            curve2dEndPoint curve2d |> Point2d.placeOnto sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            curve3dEndPoint unprojectedCurve3d |> Point3d.projectOnto plane


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


curve3dReverse : Curve3d -> Curve3d
curve3dReverse curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment3d ->
            LineSegment3dCurve (LineSegment3d.reverse lineSegment3d)

        Arc3dCurve arc3d ->
            Arc3dCurve (Arc3d.reverse arc3d)

        QuadraticSpline3dCurve quadraticSpline3d ->
            QuadraticSpline3dCurve (QuadraticSpline3d.reverse quadraticSpline3d)

        CubicSpline3dCurve cubicSpline3d ->
            CubicSpline3dCurve (CubicSpline3d.reverse cubicSpline3d)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d (curve2dReverse curve2d) sketchPlane

        ProjectedCurve3d unprojectedCurve3d plane ->
            ProjectedCurve3d (curve3dReverse unprojectedCurve3d) plane


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


curve3dRotateAround : Axis3d -> Float -> Curve3d -> Curve3d
curve3dRotateAround axis angle curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment ->
            LineSegment3dCurve
                (LineSegment3d.rotateAround axis angle lineSegment)

        Arc3dCurve arc ->
            Arc3dCurve (Arc3d.rotateAround axis angle arc)

        QuadraticSpline3dCurve quadraticSpline ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.rotateAround axis angle quadraticSpline)

        CubicSpline3dCurve cubicSpline ->
            CubicSpline3dCurve
                (CubicSpline3d.rotateAround axis angle cubicSpline)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d curve2d
                (SketchPlane3d.rotateAround axis angle sketchPlane)

        ProjectedCurve3d unprojectedCurve projectionPlane ->
            ProjectedCurve3d
                (curve3dRotateAround axis angle unprojectedCurve)
                (Plane3d.rotateAround axis angle projectionPlane)


curve3dTranslateBy : Vector3d -> Curve3d -> Curve3d
curve3dTranslateBy displacement curve3d =
    case curve3d of
        LineSegment3dCurve lineSegment ->
            LineSegment3dCurve
                (LineSegment3d.translateBy displacement lineSegment)

        Arc3dCurve arc ->
            Arc3dCurve (Arc3d.translateBy displacement arc)

        QuadraticSpline3dCurve quadraticSpline ->
            QuadraticSpline3dCurve
                (QuadraticSpline3d.translateBy displacement quadraticSpline)

        CubicSpline3dCurve cubicSpline ->
            CubicSpline3dCurve
                (CubicSpline3d.translateBy displacement cubicSpline)

        PlacedCurve3d curve2d sketchPlane ->
            PlacedCurve3d curve2d
                (SketchPlane3d.translateBy displacement sketchPlane)

        ProjectedCurve3d unprojectedCurve projectionPlane ->
            ProjectedCurve3d
                (curve3dTranslateBy displacement unprojectedCurve)
                (Plane3d.translateBy displacement projectionPlane)


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
                    Vector3d.from p1 p2

                v2 =
                    Vector3d.from p2 p3
            in
            2 * Vector3d.length (Vector3d.difference v2 v1)

        CubicSpline3dCurve cubicSpline3d ->
            let
                ( p1, p2, p3, p4 ) =
                    CubicSpline3d.controlPoints cubicSpline3d

                u1 =
                    Vector3d.from p1 p2

                u2 =
                    Vector3d.from p2 p3

                u3 =
                    Vector3d.from p3 p4

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
    curveNumSegments tolerance (curve3dMaxSecondDerivativeMagnitude curve3d)


curve3dToPolyline : Float -> Curve3d -> Polyline3d
curve3dToPolyline tolerance curve3d =
    let
        numSegments =
            curve3dNumSegments tolerance curve3d

        parameterValues =
            evenlySpacedParameterValues numSegments

        points =
            parameterValues |> List.map (curve3dPointOn curve3d)
    in
    Polyline3d points


curve3dSamples : Float -> Curve3d -> List ( Point3d, Vector3d )
curve3dSamples tolerance curve3d =
    let
        numSegments =
            curve3dNumSegments tolerance curve3d

        parameterValues =
            evenlySpacedParameterValues numSegments
    in
    parameterValues |> List.map (curve3dEvaluate curve3d)


surface3dExtrusion : Curve3d -> Vector3d -> Surface3d
surface3dExtrusion curve vector =
    case curve of
        LineSegment3dCurve lineSegment3d ->
            let
                ( p0, p1 ) =
                    LineSegment3d.endpoints lineSegment3d
            in
            ParallelogramSurface p0 (Vector3d.from p0 p1) vector

        _ ->
            ExtrusionSurface curve vector


surface3dRevolution : Curve3d -> Axis3d -> Float -> Surface3d
surface3dRevolution curve axis angle =
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
                        Point3d.coordinates (pointOnCurve v)

                    theta =
                        u * angle

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

        PlanarSurface _ sketchPlane ->
            -- TODO: interpolate within region bounding box?
            Point2d.placeOnto sketchPlane


surface3dToMesh : Float -> Surface3d -> Mesh ( Point3d, Vector3d )
surface3dToMesh tolerance surface3d =
    case surface3d of
        ParallelogramSurface point uVector vVector ->
            let
                n =
                    Vector3d.normalize (Vector3d.crossProduct uVector vVector)

                p0 =
                    point

                p1 =
                    p0 |> Point3d.translateBy uVector

                p2 =
                    p1 |> Point3d.translateBy vVector

                p3 =
                    p0 |> Point3d.translateBy vVector

                vertices =
                    [ ( p0, n ), ( p1, n ), ( p2, n ), ( p3, n ) ]

                faceIndices =
                    [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
            in
            Mesh.fromList vertices faceIndices

        ExtrusionSurface curve3d extrusionVector ->
            let
                curveSamples =
                    curve3dSamples tolerance curve3d

                toVertex ( curvePoint, curveDerivative ) =
                    let
                        normalVector =
                            Vector3d.normalize <|
                                Vector3d.crossProduct
                                    curveDerivative
                                    extrusionVector
                    in
                    ( curvePoint, normalVector )

                startVertices =
                    List.map toVertex curveSamples

                toEndVertex ( startPoint, startNormal ) =
                    ( Point3d.translateBy extrusionVector startPoint
                    , startNormal
                    )

                endVertices =
                    List.map toEndVertex startVertices

                numColumns =
                    List.length startVertices - 1

                accumulate startVertices endVertices result =
                    case ( startVertices, endVertices ) of
                        ( startFirst :: startRest, endFirst :: endRest ) ->
                            accumulate
                                startRest
                                endRest
                                (startFirst :: endFirst :: result)

                        _ ->
                            result

                vertices =
                    accumulate startVertices endVertices []

                prependFaces columnIndex faces =
                    if columnIndex < numColumns then
                        let
                            offset =
                                2 * columnIndex

                            faceIndices1 =
                                ( offset + 2, offset, offset + 1 )

                            faceIndices2 =
                                ( offset + 2, offset + 1, offset + 3 )
                        in
                        prependFaces (columnIndex + 1)
                            (faceIndices1 :: faceIndices2 :: faces)
                    else
                        faces

                faceIndices =
                    prependFaces 0 []
            in
            Mesh.fromList vertices faceIndices

        RevolutionSurface localCurve3d frame sweptAngle ->
            let
                curveSamples =
                    curve3dSamples tolerance localCurve3d

                squaredRadius point =
                    let
                        ( x, y, _ ) =
                            Point3d.coordinates point
                    in
                    x * x + y * y

                maxRadius =
                    sqrt <|
                        List.foldl
                            (\( point, derivative ) currentMax ->
                                max currentMax (squaredRadius point)
                            )
                            0
                            curveSamples

                maxSecondDerivativeMagnitude =
                    maxRadius * sweptAngle * sweptAngle

                numRotationSteps =
                    curveNumSegments tolerance maxSecondDerivativeMagnitude

                toStartVertex ( point, vDerivative ) =
                    let
                        ( x, y, _ ) =
                            Point3d.coordinates point

                        uDerivative =
                            Vector3d ( sweptAngle * x, sweptAngle * y, 0 )

                        normalVector =
                            Vector3d.normalize <|
                                Vector3d.crossProduct uDerivative vDerivative
                    in
                    ( point, normalVector )

                startVertices =
                    List.map toStartVertex curveSamples

                rotateVertexBy angle =
                    let
                        cosAngle =
                            cos angle

                        sinAngle =
                            sin angle
                    in
                    \( point, normalVector ) ->
                        let
                            ( x, y, z ) =
                                Point3d.coordinates point

                            ( nx, ny, nz ) =
                                Vector3d.components normalVector
                        in
                        ( Point3d
                            ( x * cosAngle - y * sinAngle
                            , y * cosAngle + x * sinAngle
                            , z
                            )
                        , Vector3d
                            ( nx * cosAngle - ny * sinAngle
                            , ny * cosAngle + nx * sinAngle
                            , nz
                            )
                        )

                rotationAngles =
                    List.range 1 numRotationSteps
                        |> List.map
                            (\index ->
                                sweptAngle
                                    * (toFloat index / toFloat numRotationSteps)
                            )

                rotatedVertexLists =
                    rotationAngles
                        |> List.map
                            (\angle ->
                                startVertices |> List.map (rotateVertexBy angle)
                            )

                vertices =
                    List.concat (startVertices :: rotatedVertexLists)
                        |> List.map
                            (\( position, normal ) ->
                                ( Point3d.placeIn frame position
                                , Vector3d.placeIn frame normal
                                )
                            )

                numColumns =
                    List.length curveSamples - 1

                numRows =
                    numRotationSteps

                prependFaces rowIndex columnIndex faces =
                    if columnIndex < numColumns then
                        let
                            i1 =
                                rowIndex * (numColumns + 1) + columnIndex

                            i2 =
                                i1 + 1

                            i3 =
                                i2 + numColumns + 1

                            i4 =
                                i3 - 1

                            faceIndices1 =
                                ( i1, i2, i3 )

                            faceIndices2 =
                                ( i1, i3, i4 )
                        in
                        prependFaces
                            rowIndex
                            (columnIndex + 1)
                            (faceIndices1 :: faceIndices2 :: faces)
                    else if rowIndex < (numRows - 1) then
                        prependFaces (rowIndex + 1) 0 faces
                    else
                        faces

                faceIndices =
                    prependFaces 0 0 []
            in
            Mesh.fromList vertices faceIndices

        PlanarSurface region sketchPlane ->
            let
                normalVector =
                    SketchPlane3d.normalDirection sketchPlane
                        |> Direction3d.toVector

                toVertex3d point =
                    ( Point2d.placeOnto sketchPlane point
                    , normalVector
                    )
            in
            regionToMesh tolerance region |> Mesh.map toVertex3d


surface3dRotateAround : Axis3d -> Float -> Surface3d -> Surface3d
surface3dRotateAround axis angle surface =
    case surface of
        ExtrusionSurface curve3d extrusionVector ->
            ExtrusionSurface
                (curve3dRotateAround axis angle curve3d)
                (Vector3d.rotateAround axis angle extrusionVector)

        RevolutionSurface localCurve3d frame sweptAngle ->
            RevolutionSurface
                localCurve3d
                (Frame3d.rotateAround axis angle frame)
                sweptAngle

        ParallelogramSurface point uVector vVector ->
            let
                rotateVector =
                    Vector3d.rotateAround axis angle
            in
            ParallelogramSurface
                (Point3d.rotateAround axis angle point)
                (rotateVector uVector)
                (rotateVector vVector)

        PlanarSurface region sketchPlane ->
            PlanarSurface
                region
                (SketchPlane3d.rotateAround axis angle sketchPlane)


regionExtrusionWith : { start : EdgeType, end : EdgeType, left : EdgeType, right : EdgeType } -> Curve2d -> Vector2d -> Region2d
regionExtrusionWith edgeTypes curve2d extrusionVector =
    let
        startPoint =
            curve2dStartPoint curve2d

        endPoint =
            curve2dEndPoint curve2d

        crossProduct =
            Vector2d.crossProduct
                (Vector2d.from startPoint endPoint)
                extrusionVector

        leftToRight =
            crossProduct >= 0

        startCurve =
            if leftToRight then
                curve2d
            else
                curve2dReverse curve2d
    in
    ExtrusionRegion startCurve extrusionVector edgeTypes


regionRevolutionWith : { start : EdgeType, end : EdgeType, inside : EdgeType, outside : EdgeType } -> Curve2d -> Point2d -> Float -> Region2d
regionRevolutionWith edgeTypes curve2d centerPoint sweptAngle =
    let
        startPoint =
            curve2dStartPoint curve2d

        endPoint =
            curve2dEndPoint curve2d

        startSquaredRadius =
            Point2d.squaredDistanceFrom centerPoint startPoint

        endSquaredRadius =
            Point2d.squaredDistanceFrom centerPoint endPoint

        insideToOutside =
            startSquaredRadius <= endSquaredRadius

        startCurve =
            if insideToOutside then
                curve2d
            else
                curve2dReverse curve2d
    in
    RevolutionRegion startCurve centerPoint sweptAngle edgeTypes


regionBoundaries : Region2d -> List Curve2d
regionBoundaries region =
    case region of
        RectangleRegion rectangle edgeTypes ->
            let
                ( p1, p2, p3, p4 ) =
                    Rectangle2d.vertices rectangle

                boundaryCurve edgeType startPoint endPoint =
                    case edgeType of
                        Exterior ->
                            Just <|
                                LineSegment2dCurve <|
                                    LineSegment2d ( startPoint, endPoint )

                        Interior ->
                            Nothing
            in
            if Frame2d.isRightHanded (Rectangle2d.axes rectangle) then
                List.filterMap identity
                    [ boundaryCurve edgeTypes.bottom p1 p2
                    , boundaryCurve edgeTypes.right p2 p3
                    , boundaryCurve edgeTypes.top p3 p4
                    , boundaryCurve edgeTypes.left p4 p1
                    ]
            else
                List.filterMap identity
                    [ boundaryCurve edgeTypes.bottom p2 p1
                    , boundaryCurve edgeTypes.right p3 p2
                    , boundaryCurve edgeTypes.top p4 p3
                    , boundaryCurve edgeTypes.left p1 p4
                    ]

        ExtrusionRegion curve2d extrusionVector edgeTypes ->
            let
                startCurve =
                    case edgeTypes.start of
                        Exterior ->
                            Just curve2d

                        Interior ->
                            Nothing

                endCurve =
                    case edgeTypes.end of
                        Exterior ->
                            Just
                                (curve2d
                                    |> curve2dTranslateBy extrusionVector
                                    |> curve2dReverse
                                )

                        Interior ->
                            Nothing

                startPoint =
                    curve2dStartPoint curve2d

                endPoint =
                    curve2dEndPoint curve2d

                leftCurve =
                    case edgeTypes.left of
                        Exterior ->
                            let
                                lineSegment =
                                    LineSegment2d
                                        ( startPoint
                                        , Point2d.translateBy
                                            extrusionVector
                                            startPoint
                                        )
                            in
                            Just (LineSegment2dCurve lineSegment)

                        Interior ->
                            Nothing

                rightCurve =
                    case edgeTypes.right of
                        Exterior ->
                            let
                                lineSegment =
                                    LineSegment2d
                                        ( Point2d.translateBy
                                            extrusionVector
                                            endPoint
                                        , endPoint
                                        )
                            in
                            Just (LineSegment2dCurve lineSegment)

                        Interior ->
                            Nothing
            in
            List.filterMap identity
                [ startCurve
                , rightCurve
                , endCurve
                , leftCurve
                ]

        RevolutionRegion curve2d centerPoint sweptAngle edgeTypes ->
            let
                startCurve =
                    case edgeTypes.start of
                        Exterior ->
                            if sweptAngle >= 0.0 then
                                Just curve2d
                            else
                                Just (curve2dReverse curve2d)

                        Interior ->
                            Nothing

                endCurve =
                    case edgeTypes.end of
                        Exterior ->
                            let
                                rotated =
                                    curve2d
                                        |> curve2dRotateAround
                                            centerPoint
                                            sweptAngle
                            in
                            if sweptAngle >= 0.0 then
                                Just (curve2dReverse rotated)
                            else
                                Just rotated

                        Interior ->
                            Nothing

                insidePoint =
                    curve2dStartPoint curve2d

                outsidePoint =
                    curve2dEndPoint curve2d

                outsideCurve =
                    case edgeTypes.outside of
                        Exterior ->
                            let
                                arc =
                                    Arc2d
                                        { startPoint = outsidePoint
                                        , centerPoint = centerPoint
                                        , sweptAngle = sweptAngle
                                        }
                            in
                            if sweptAngle >= 0.0 then
                                Just (Arc2dCurve arc)
                            else
                                Just (Arc2dCurve (Arc2d.reverse arc))

                        Interior ->
                            Nothing

                insideCurve =
                    case edgeTypes.inside of
                        Exterior ->
                            let
                                arc =
                                    Arc2d
                                        { startPoint = insidePoint
                                        , centerPoint = centerPoint
                                        , sweptAngle = sweptAngle
                                        }
                            in
                            if sweptAngle >= 0.0 then
                                Just (Arc2dCurve (Arc2d.reverse arc))
                            else
                                Just (Arc2dCurve arc)

                        Interior ->
                            Nothing
            in
            List.filterMap identity
                [ startCurve
                , outsideCurve
                , endCurve
                , insideCurve
                ]

        FanRegion point curve2d edgeTypes ->
            let
                startCurve =
                    case edgeTypes.start of
                        Exterior ->
                            let
                                lineSegment =
                                    LineSegment2d
                                        ( point, curve2dStartPoint curve2d )
                            in
                            Just (LineSegment2dCurve lineSegment)

                        Interior ->
                            Nothing

                endCurve =
                    case edgeTypes.start of
                        Exterior ->
                            let
                                lineSegment =
                                    LineSegment2d
                                        ( curve2dEndPoint curve2d, point )
                            in
                            Just (LineSegment2dCurve lineSegment)

                        Interior ->
                            Nothing

                outsideCurve =
                    case edgeTypes.curve of
                        Exterior ->
                            Just curve2d

                        Interior ->
                            Nothing
            in
            List.filterMap identity
                [ startCurve
                , outsideCurve
                , endCurve
                ]

        Fused regions ->
            List.concat (List.map regionBoundaries regions)


regionToMesh : Float -> Region2d -> Mesh Point2d
regionToMesh tolerance region =
    case region of
        RectangleRegion rectangle _ ->
            let
                ( p0, p1, p2, p3 ) =
                    Rectangle2d.vertices rectangle

                vertexList =
                    [ p0, p1, p2, p3 ]

                faceIndices =
                    if Frame2d.isRightHanded (Rectangle2d.axes rectangle) then
                        [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]
                    else
                        [ ( 0, 2, 1 ), ( 0, 3, 2 ) ]
            in
            Mesh.fromList vertexList faceIndices

        ExtrusionRegion curve2d extrusionVector _ ->
            let
                startVertices =
                    curve2dToPolyline tolerance curve2d
                        |> Polyline2d.vertices

                endVertices =
                    List.map (Point2d.translateBy extrusionVector) startVertices

                numColumns =
                    List.length startVertices - 1

                accumulate startVertices endVertices result =
                    case ( startVertices, endVertices ) of
                        ( startFirst :: startRest, endFirst :: endRest ) ->
                            accumulate
                                startRest
                                endRest
                                (startFirst :: endFirst :: result)

                        _ ->
                            result

                vertices =
                    accumulate startVertices endVertices []

                prependFaces columnIndex faces =
                    if columnIndex < numColumns then
                        let
                            offset =
                                2 * columnIndex

                            faceIndices1 =
                                ( offset, offset + 2, offset + 3 )

                            faceIndices2 =
                                ( offset, offset + 3, offset + 1 )
                        in
                        prependFaces (columnIndex + 1)
                            (faceIndices1 :: faceIndices2 :: faces)
                    else
                        faces

                faceIndices =
                    prependFaces 0 []
            in
            Mesh.fromList vertices faceIndices

        RevolutionRegion curve2d centerPoint sweptAngle _ ->
            let
                insidePoint =
                    curve2dStartPoint curve2d

                outsidePoint =
                    curve2dEndPoint curve2d

                outsideRadius =
                    Point2d.distanceFrom centerPoint outsidePoint

                numRotationSteps =
                    curveNumSegments tolerance
                        (outsideRadius * sweptAngle * sweptAngle)

                startCurve =
                    if sweptAngle >= 0.0 then
                        curve2d
                    else
                        curve2dReverse curve2d

                startVertices =
                    curve2dToPolyline tolerance startCurve
                        |> Polyline2d.vertices

                rotationAngles =
                    List.range 1 numRotationSteps
                        |> List.map
                            (\index ->
                                sweptAngle
                                    * (toFloat index / toFloat numRotationSteps)
                            )

                rotatedVertexLists =
                    rotationAngles
                        |> List.map
                            (\angle ->
                                startVertices
                                    |> List.map
                                        (Point2d.rotateAround centerPoint angle)
                            )

                vertices =
                    List.concat (startVertices :: rotatedVertexLists)

                numColumns =
                    List.length startVertices - 1

                numRows =
                    numRotationSteps

                prependFaces rowIndex columnIndex faces =
                    if columnIndex < numColumns then
                        let
                            i1 =
                                rowIndex * (numColumns + 1) + columnIndex

                            i2 =
                                i1 + 1

                            i4 =
                                i2 + numColumns

                            i3 =
                                i4 + 1

                            faceIndices1 =
                                ( i1, i2, i3 )

                            faceIndices2 =
                                ( i1, i3, i4 )
                        in
                        prependFaces
                            rowIndex
                            (columnIndex + 1)
                            (faceIndices1 :: faceIndices2 :: faces)
                    else if rowIndex < (numRows - 1) then
                        prependFaces (rowIndex + 1) 0 faces
                    else
                        faces

                faceIndices =
                    prependFaces 0 0 []
            in
            Mesh.fromList vertices faceIndices

        FanRegion point curve2d _ ->
            let
                curveVertices =
                    curve2dToPolyline tolerance curve2d |> Polyline2d.vertices

                numFaces =
                    List.length curveVertices - 1

                toFaceIndices faceIndex =
                    ( 0, faceIndex + 1, faceIndex + 2 )

                faceIndices =
                    List.range 0 (numFaces - 1) |> List.map toFaceIndices
            in
            Mesh.fromList (point :: curveVertices) faceIndices

        Fused regions ->
            Mesh.merge (List.map (regionToMesh tolerance) regions)


regionTranslateBy : Vector2d -> Region2d -> Region2d
regionTranslateBy displacement region =
    case region of
        RectangleRegion rectangle edgeTypes ->
            RectangleRegion
                (Rectangle2d.translateBy displacement rectangle)
                edgeTypes

        ExtrusionRegion curve2d extrusionVector edgeTypes ->
            ExtrusionRegion
                (curve2dTranslateBy displacement curve2d)
                extrusionVector
                edgeTypes

        RevolutionRegion curve2d centerPoint sweptAngle edgeTypes ->
            RevolutionRegion
                (curve2dTranslateBy displacement curve2d)
                (Point2d.translateBy displacement centerPoint)
                sweptAngle
                edgeTypes

        FanRegion point curve2d edgeTypes ->
            FanRegion
                (Point2d.translateBy displacement point)
                (curve2dTranslateBy displacement curve2d)
                edgeTypes

        Fused regions ->
            Fused (List.map (regionTranslateBy displacement) regions)


regionRotateAround : Point2d -> Float -> Region2d -> Region2d
regionRotateAround point angle region =
    case region of
        RectangleRegion rectangle edgeTypes ->
            RectangleRegion
                (Rectangle2d.rotateAround point angle rectangle)
                edgeTypes

        ExtrusionRegion curve2d extrusionVector edgeTypes ->
            ExtrusionRegion
                (curve2dRotateAround point angle curve2d)
                (Vector2d.rotateBy angle extrusionVector)
                edgeTypes

        RevolutionRegion curve2d centerPoint sweptAngle edgeTypes ->
            RevolutionRegion
                (curve2dRotateAround point angle curve2d)
                (Point2d.rotateAround point angle centerPoint)
                sweptAngle
                edgeTypes

        FanRegion fanPoint curve2d edgeTypes ->
            FanRegion
                (Point2d.rotateAround point angle fanPoint)
                (curve2dRotateAround point angle curve2d)
                edgeTypes

        Fused regions ->
            Fused (List.map (regionRotateAround point angle) regions)


regionMirrorAcross : Axis2d -> Region2d -> Region2d
regionMirrorAcross axis region =
    case region of
        RectangleRegion rectangle edgeTypes ->
            RectangleRegion
                (Rectangle2d.mirrorAcross axis rectangle)
                edgeTypes

        ExtrusionRegion curve2d extrusionVector edgeTypes ->
            ExtrusionRegion
                (curve2d |> curve2dMirrorAcross axis |> curve2dReverse)
                (Vector2d.mirrorAcross axis extrusionVector)
                edgeTypes

        RevolutionRegion curve2d centerPoint sweptAngle edgeTypes ->
            RevolutionRegion
                (curve2d |> curve2dMirrorAcross axis)
                (Point2d.mirrorAcross axis centerPoint)
                -sweptAngle
                edgeTypes

        FanRegion point curve2d edgeTypes ->
            FanRegion
                (Point2d.mirrorAcross axis point)
                (curve2d |> curve2dMirrorAcross axis |> curve2dReverse)
                edgeTypes

        Fused regions ->
            Fused (List.map (regionMirrorAcross axis) regions)
